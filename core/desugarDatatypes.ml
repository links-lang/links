open CommonTypes
open Types
open SourceCode
open SourceCode.WithPos
open Sugartypes
open Utility
open List
open Errors
open Parse

module SEnv = Env.String


let tygroup_counter = ref 0
let fresh_tygroup_id = function () ->
  let ret = !tygroup_counter in
  tygroup_counter := ret + 1;
  ret

(* Check that no datatype is left undesugared. *)
let all_datatypes_desugared =
object (self)
  inherit SugarTraversals.predicate as super

  val all_desugared = true
  method satisfied = all_desugared

  method! datatype' = function
      (_, None) -> {< all_desugared = false >}
    | _ -> self

  method! phrasenode = function
    | TableLit (_, (_, None), _, _, _) -> {< all_desugared = false >}
    | p -> super#phrasenode p
end

let concrete_subkind =
  function
  | Some subkind -> subkind
  | None         -> default_subkind

(* Find all unbound type variables in a term *)
let typevars =
object (self)
  inherit SugarTraversals.fold as super

  val tyvar_list : name list = []
  val tyvars : type_variable StringMap.t = StringMap.empty

  (* fill in subkind with the default *)
  val fill =
    function
    | (_, (_, Some _), _) as tv -> tv
    | (name, (pk, None), rest) -> (name, (pk, Some default_subkind), rest)

  method tyvar_list =
    List.map (fun name -> fill (StringMap.find name tyvars)) (List.rev tyvar_list)

  method add_name name = {< tyvar_list = name :: tyvar_list >}
  method register ((name, _, _) as tv) = {< tyvars = StringMap.add name tv tyvars >}
  method bind tv = self#register tv

  method add ((name, (pk, sk), freedom) as tv) =
    if StringMap.mem name tyvars then
      let (_, (pk', sk'), freedom') = StringMap.find name tyvars in
      (* monotonically increase subkinding information *)
      let (sk, sk') =
        match sk, sk' with
        | Some sk, None  -> Some sk, Some sk
        | None, Some sk' -> Some sk', Some sk'
        | _, _           -> sk, sk' in
      let tv = (name, (pk, sk), freedom) in
      let tv' = (name, (pk', sk'), freedom') in
      (* check that duplicate type variables have the same kind *)
      if tv <> tv' then
        failwith ("kind mismatch in type variable: " ^
                  Sugartypes.show_type_variable tv ^ " vs: " ^
                  Sugartypes.show_type_variable tv');
      self#register tv
    else
      (self#register tv)#add_name name


  method! bindingnode = function
    (* type declarations bind variables; exclude those from the
       analysis. *)
    | Typenames _  -> self
    (* Don't traverse into modules, we call typevars on them separately
       once we desugar them *)
    | Module _     -> self
    | b            -> super#bindingnode b

  method! datatypenode = let open Datatype in
    function
    | TypeVar (x, k, freedom) -> self#add (x, (pk_type, k), freedom)
    | Mu (v, t)       -> let o = self#bind (v, (pk_type, None), `Rigid) in o#datatype t
    | Forall (qs, t)  ->
        let o = List.fold_left (fun o q -> o#bind (rigidify q)) self qs in
        o#datatype t
    | dt -> super#datatypenode dt

  method! row_var = let open Datatype in function
    | Closed               -> self
    | Open (x, k, freedom) -> self#add (x, (pk_row, k), freedom)
    | Recursive (s, r)     -> let o = self#bind (s, (pk_row, None), `Rigid) in o#row r

  method! fieldspec = let open Datatype in function
    | Absent -> self
    | Present t -> self#datatype t
    | Var (x, k, freedom) -> self#add (x, (pk_presence, k), freedom)
end

type var_env = { tenv : Types.meta_type_var StringMap.t;
                 renv : Types.meta_row_var StringMap.t;
                 penv : Types.meta_presence_var StringMap.t }

let empty_env = {tenv = StringMap.empty; renv = StringMap.empty; penv = StringMap.empty}

exception UnexpectedFreeVar of string
exception UnexpectedOperationEffects of string

module Desugar =
struct
  (* Desugars quantifiers into Types.quantifiers,
   * returning updated variable environment.
   * Lifted / deduplicated from `typename` and `Forall` desugaring. *)
  let desugar_quantifiers (var_env: var_env) (qs: Sugartypes.quantifier list) :
      (Types.quantifier list * var_env) =
      ListLabels.fold_right ~init:([], var_env) qs
      ~f:(fun (name, (primarykind, subkind), _freedom)
              (args, {tenv; renv; penv}) ->
            let var = Types.fresh_raw_variable () in
            let subkind = concrete_subkind subkind in
            match primarykind with
              | PrimaryKind.Type ->
                  let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
                    ((var, subkind, `Type point)::args,
                     {tenv=StringMap.add name point tenv; renv; penv})
              | PrimaryKind.Row ->
                  let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
                    ((var, subkind, `Row point)::args,
                     {tenv; renv=StringMap.add name point renv; penv})
              | PrimaryKind.Presence ->
                  let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
                    ((var, subkind, `Presence point))::args,
                     {tenv; renv; penv=StringMap.add name point penv})

  let rec datatype var_env (type_env : FrontendTypeEnv.t) t' =
    let datatype var_env t' = datatype var_env type_env t' in
    match t' with
    | {node = t; pos} ->
      let lookup_type t = StringMap.find t var_env.tenv in
      let open Datatype in
      match t with
        | TypeVar (s, _, _) -> (try `MetaTypeVar (lookup_type s)
                                with NotFound _ -> raise (UnexpectedFreeVar s))
        | Function (f, e, t) ->
            `Function (Types.make_tuple_type (List.map (datatype var_env) f),
                      effect_row var_env type_env e,
                      datatype var_env t)
        | Lolli (f, e, t) ->
            `Lolli (Types.make_tuple_type (List.map (datatype var_env) f),
                       effect_row var_env type_env e,
                       datatype var_env t)
        | Mu (name, t) ->
            let var = Types.fresh_raw_variable () in
            (* FIXME: shouldn't we support other subkinds for recursive types? *)
            let point = Unionfind.fresh (`Var (var, default_subkind, `Flexible)) in
            let tenv = StringMap.add name point var_env.tenv in
            let _ = Unionfind.change point (`Recursive (var, datatype {var_env with tenv=tenv} t)) in
              `MetaTypeVar point
        | Forall (qs, t) ->
            let (qs: Types.quantifier list), var_env = desugar_quantifiers var_env qs in
            let t = datatype var_env t in
              `ForAll (Types.box_quantifiers qs, t)
        | Unit -> Types.unit_type
        | Tuple ks ->
            let labels = map string_of_int (Utility.fromTo 1 (1 + length ks)) in
            let unit = Types.make_empty_closed_row () in
            let present (s, x) = (s, `Present x)
            in
              `Record (fold_right2 (curry (Types.row_with -<- present)) labels (map (datatype var_env) ks) unit)
        | Record r -> `Record (row var_env type_env r)
        | Variant r -> `Variant (row var_env type_env r)
        | Effect r -> `Effect (row var_env type_env r)
        | Table (r, w, n) -> `Table (datatype var_env r, datatype var_env w, datatype var_env n)
        | List k -> `Application (Types.list, [`Type (datatype var_env k)])
        | TypeApplication (qtycon, ts) ->
            (* Matches kinds of the quantifiers against the type arguments.
             * Returns Types.type_args based on the given frontend type arguments. *)

            (* We expand the qualified name to account for module opening.
               This is only used for printing the types, the environments
               keep track of the expansion
               However, this is slightly dodgy from a usability experience:
               Subsequent opens may invalidate the string name we fix here,
               meaning that they are only valid at the point of their creation.

               FIXME: Discuss what to do with this.
               One test assumes expaning the string names in the aliases,
               but doing it consistently would break many tests
               Maybe would become Prelude.Maybe
               *)
            let _qtycon_expanded_name =
              try
                begin match FrontendTypeEnv.lookup_tycons_with_orig_path type_env qtycon with
                  | None, _ -> QualifiedName.canonical_name qtycon
                  | Some prefix, _ ->
                     QualifiedName.canonical_name (QualifiedName.append prefix qtycon)
                end
              with | FrontendTypeEnv.ModuleNotFound _
                   | FrontendTypeEnv.TyConsNotFound _ ->
                      (* We ignore these errors for now, they will be handled further down *)
                      QualifiedName.canonical_name qtycon
            in
            let match_quantifiers qs =
              let match_kinds i (q, t) =
                let primary_kind_of_type_arg : Datatype.type_arg -> PrimaryKind.t = function
                  | Type _ -> PrimaryKind.Type
                  | Row _ -> PrimaryKind.Row
                  | Presence _ -> PrimaryKind.Presence
                in
                let q_kind = primary_kind_of_quantifier q in
                let t_kind = primary_kind_of_type_arg t in
                if q_kind <> t_kind then
                  raise (TypeApplicationKindMismatch {pos;
                    name=QualifiedName.canonical_name qtycon;
                    tyarg_number=i;
                    expected=PrimaryKind.to_string q_kind;
                    provided=PrimaryKind.to_string t_kind})
                else (q, t)
              in
              let type_arg' var_env type_env = function
                | Row r -> `Row (effect_row var_env type_env r)
                | t -> type_arg var_env type_env t
              in
              begin
                try
                  let ts = ListUtils.zip' qs ts in
                  List.mapi
                    (fun i (q,t) ->
                      let (q, t) = match_kinds i (q, t) in
                      match subkind_of_quantifier q with
                      | (_, Restriction.Effect) -> type_arg' var_env type_env t
                      | _ -> type_arg var_env type_env t) ts
                with
                | ListUtils.Lists_length_mismatch ->
                    raise (TypeApplicationArityMismatch {pos;
                      name=QualifiedName.canonical_name qtycon;
                      expected=List.length qs; provided=List.length ts})
              end in

            begin match FrontendTypeEnv.find_tycons type_env qtycon with
              | None -> raise (UnboundTyCon (pos, qtycon))
              | Some (`Alias (qs, _dt)) ->
                  let ts = match_quantifiers qs in
                  Instantiate.alias qtycon ts type_env
              | Some (`Abstract abstype) ->
                  (* TODO: check that the kinds match up *)
                  `Application (abstype, List.map (type_arg var_env type_env) ts)
              | Some (`Mutual (qs, tygroup_ref)) ->
                  (* Check that the quantifiers / kinds match up, then generate
                   * a `RecursiveApplication. *)
                  let r_args = match_quantifiers qs in
                  (* In a group of recursive type defs, references of other
                     types must be unqualified *)
                  let tycon = QualifiedName.unqualify qtycon in
                  let r_unwind args dual =
                    let (_, body) = StringMap.find tycon !tygroup_ref.type_map in
                    let body = Instantiate.recursive_application tycon qs args body in
                    if dual then dual_type body else body in
                  let r_unique_name = tycon ^ (string_of_int !tygroup_ref.id) in
                  let r_linear () =
                    StringMap.lookup tycon !tygroup_ref.linearity_map in
                  `RecursiveApplication
                    { r_name = tycon;
                      r_dual = false;
                      r_unique_name;
                      r_args;
                      r_unwind;
                      r_linear }
            end
        | Primitive k -> `Primitive k
        | DB -> `Primitive Primitive.DB
        | (Input _ | Output _ | Select _ | Choice _ | Dual _ | End) as s -> session_type var_env type_env s
  and session_type var_env type_env =
    (* let lookup_type t = StringMap.find t var_env.tenv in  -- used only in commented code *)
    (* HACKY *)
    let open Datatype in
    function
    | Input (t, s)  -> `Input (datatype var_env type_env t, datatype var_env type_env s)
    | Output (t, s) -> `Output (datatype var_env type_env t, datatype var_env type_env s)
    | Select r      -> `Select (row var_env type_env r)
    | Choice r      -> `Choice (row var_env type_env r)
    | Dual s        -> `Dual (datatype var_env type_env s)
    | End           -> `End
    | _ -> assert false
  and fieldspec var_env type_env =
    let lookup_flag = flip StringMap.find var_env.penv in
      let open Datatype in function
        | Absent -> `Absent
        | Present t -> `Present (datatype var_env type_env t)
        | Var (name, _, _) ->
            begin
              try `Var (lookup_flag name)
              with NotFound _ -> raise (UnexpectedFreeVar name)
            end
  and row var_env type_env (fields, rv) =
    let lookup_row = flip StringMap.find var_env.renv in
    let seed =
      let open Datatype in
      match rv with
        | Closed -> Types.make_empty_closed_row ()
        | Open (rv, _, _) ->
            begin
              try (StringMap.empty, lookup_row rv, false)
              with NotFound _ -> raise (UnexpectedFreeVar rv)
            end
        | Recursive (name, r) ->
            let var = Types.fresh_raw_variable () in
            let point = Unionfind.fresh (`Var (var, default_subkind, `Flexible)) in
            let renv = StringMap.add name point var_env.renv in
            let _ = Unionfind.change point (`Recursive (var, row {var_env with renv=renv} type_env r)) in
              (StringMap.empty, point, false) in
    let fields =
        List.map
          (fun (k, p) ->
            (k, fieldspec var_env type_env p))
          fields
    in
    fold_right Types.row_with fields seed
  and effect_row var_env type_env (fields, rv) =
    let fields =
      (* Closes any empty, open arrow rows on user-defined
         operations. Note any row which can be closed will have an
         unbound effect variable.  *)
      try List.map
            (let open Datatype in function
            | (name, Present { node = Function (domain, (fields, rv), codomain); pos}) as op
                when not (TypeUtils.is_builtin_effect name) ->
               (* Elaborates `Op : a -> b' to `Op : a {}-> b' *)
               begin match rv, fields with
               | Closed, [] -> op
               | Open _, []
               | Recursive _, [] -> (* might need an extra check on recursive rows *)
                  (name, Present (WithPos.make ~pos (Function (domain, ([], Closed), codomain))))
               | _,_ -> raise (UnexpectedOperationEffects name)
               end
            | x -> x)
            fields
      with
        UnexpectedOperationEffects op_name ->
          failwith (Printf.sprintf "The abstract operation %s has unexpected effects in its signature. The effect signature on an abstract operation arrow is always supposed to be empty, since any effects it might have are ultimately conferred by its handler." op_name)
    in
    let (fields, rho, dual) = row var_env type_env (fields, rv) in
    let fields =
      StringMap.mapi
        (fun name ->
          function
          | `Present t
              when not (TypeUtils.is_builtin_effect name || TypeUtils.is_function_type t) ->
             (* Elaborates `Op : a' to `Op : () {}-> a' *)
             let eff = Types.make_empty_closed_row () in
             `Present (Types.make_function_type [] eff t)
          | t -> t)
        fields
    in
    (fields, rho, dual)
  and type_arg var_env type_env =
    let open Datatype in function
    | Type t -> `Type (datatype var_env type_env t)
    | Row r -> `Row (row var_env type_env r)
    | Presence f -> `Presence (fieldspec var_env type_env f)

  (* pre condition: all subkinds have been filled in *)
  let generate_var_mapping (vars : type_variable list) : (Types.quantifier list * var_env) =
    let addt x t envs = {envs with tenv = StringMap.add x t envs.tenv} in
    let addr x r envs = {envs with renv = StringMap.add x r envs.renv} in
    let addf x f envs = {envs with penv = StringMap.add x f envs.penv} in
    let vars, var_env =
      List.fold_left
        (fun (vars, envs) ->
           let var = Types.fresh_raw_variable () in
             fun (x, kind, freedom) ->
             match (kind, freedom) with
             | (PrimaryKind.Type, Some subkind), freedom ->
               let t = Unionfind.fresh (`Var (var, subkind, freedom)) in
                 (var, subkind, `Type t)::vars, addt x t envs
             | (PrimaryKind.Row, Some subkind), freedom ->
               let r = Unionfind.fresh (`Var (var, subkind, freedom)) in
                 (var, subkind, `Row r)::vars, addr x r envs
             | (PrimaryKind.Presence, Some subkind), freedom ->
               let f = Unionfind.fresh (`Var (var, subkind, freedom)) in
                 (var, subkind, `Presence f)::vars, addf x f envs
             | (_, None), _ ->
               (* Shouldn't occur; we are assuming that all subkinds have been
                * filled in*)
               assert false)
        ([], empty_env)
        vars
    in
      List.rev vars, var_env

  let datatype' map type_env (dt, _ : datatype') =
    (dt, Some (datatype map type_env dt))

  (* Desugar a foreign function declaration.  Foreign declarations
     cannot use type variables from the context.  Any type variables
     found are implicitly universally quantified at this point. *)
  let foreign type_env dt =
    let tvars = (typevars#datatype' dt)#tyvar_list in
      datatype' (snd (generate_var_mapping tvars)) type_env dt


 (* Desugar a table literal.  No free variables are allowed here.
    We generate both read and write types by looking for readonly constraints *)
  let tableLit type_env constraints dt =
    try
      let read_type = match datatype' empty_env type_env (dt, None) with
        | (_, Some read_type) -> read_type
        | _ -> assert false in
      let write_row, needed_row =
        match TypeUtils.concrete_type read_type with
        | `Record (fields, _, _) ->
           StringMap.fold
             (fun label t (write, needed) ->
              match lookup label constraints with
              | Some cs ->
                 if List.exists ((=) Readonly) cs then
                   (write, needed)
                 else (* if List.exists ((=) `Default) cs then *)
                   (Types.row_with (label, t) write, needed)
              | _  ->
                 let add = Types.row_with (label, t) in
                 (add write, add needed))
             fields
             (Types.make_empty_closed_row (), Types.make_empty_closed_row ())
        | _ -> failwith "Table types must be record types"
      in
      (* We deliberately don't concretise the returned read_type in
          the hope of improving error messages during type
          inference. *)
      read_type, `Record write_row, `Record needed_row
    with UnexpectedFreeVar x ->
      failwith ("Free variable ("^ x ^") in table literal")
end


(* convert a syntactic type into a semantic type, using `map' to resolve free type variables *)
let desugar (initial_type_env : FrontendTypeEnv.t) (initial_map : var_env) =
object (self : 'self_type)
  inherit SugarTraversals.fold_map as super

  val tyvar_map = initial_map

  val type_env = initial_type_env

  (* Typing environment collecting additions to the typing environment in the
     current module *)
  val cur_module_additions = FrontendTypeEnv.empty_typing_environment

  method get_additions () = cur_module_additions

  method reset_additions () =
    ({< cur_module_additions = FrontendTypeEnv.empty_typing_environment >},
     cur_module_additions)

  method! patternnode = function
    | Pattern.HasType (pat, dt) ->
        let o, pat = self#pattern pat in
          o, Pattern.HasType (pat, Desugar.datatype' tyvar_map type_env dt)
    | p -> super#patternnode p


  method! phrasenode = function
    | Block (bs, p) ->
        (* aliases bound in `bs'
           should not escape the scope of the block *)
        let o       = {<>} in
        let o, bs  = o#list (fun o -> o#binding) bs in
        let _o, p  = o#phrase p in
          (* NB: we return `self' rather than `_o' in order to return
             to the outer scope; any aliases bound in _o are
             unreachable from outside the block *)
          self, Block (bs, p)
    | TypeAnnotation (p, dt) ->
        let o, p = self#phrase p in
          o, TypeAnnotation (p, Desugar.datatype' tyvar_map self#get_type_env dt)
    | Upcast (p, dt1, dt2) ->
        let o, p = self#phrase p in
          o, Upcast (p, Desugar.datatype' tyvar_map type_env dt1, Desugar.datatype' tyvar_map type_env dt2)
    | TableLit (t, (dt, _), cs, keys, p) ->
        let read, write, needed = Desugar.tableLit type_env cs dt in
        let o, t = self#phrase t in
    let o, keys = o#phrase keys in
        let o, p = o#phrase p in
          o, TableLit (t, (dt, Some (read, write, needed)), cs, keys, p)
    (* Switch and receive type annotations are never filled in by
       this point, so we ignore them.  *)
    | p -> super#phrasenode p

  method! bindingnode = function
    | Typenames ts ->
        (* Maps syntactic types in the recursive group to semantic types. *)
        (* This must be empty to start off with, because there's a cycle
         * in calculating the semantic types: we need the alias environment
         * populated with all types in the group in order to calculate a
         * semantic type. We populate the reference in a later pass. *)
        let tygroup_ref = ref {
          id = fresh_tygroup_id ();
          type_map = StringMap.empty;
          linearity_map = StringMap.empty
        } in

        (* Each type will have its own variable environment, used in
         * later passes.*)
        let venvs_map = StringMap.empty in

        (* Collect all type declarations in the group as mutuals.
           Quantifiers need to be desugared. *)
        let (new_mutual_env, venvs_map) =
          List.fold_left (fun (alias_env, venvs_map) (t, args, _, _) ->
            let qs = List.map (fst) args in
            let qs, var_env =  Desugar.desugar_quantifiers empty_env qs in
            let venvs_map = StringMap.add t var_env venvs_map in
            (SEnv.bind alias_env (t, `Mutual (qs, tygroup_ref)), venvs_map) )
            (SEnv.empty, venvs_map) ts in

        let mutual_type_env =
          SEnv.fold
            (fun name d env -> FrontendTypeEnv.bind_tycons env (name, d))
            new_mutual_env
            type_env in


        (* Desugar all DTs, given the temporary new alias environment. *)
        let desugared_mutuals : Sugartypes.typename list =
          List.map (fun (name, args, dt, pos) ->
            let sugar_qs = List.map (fst) args in

            (* Semantic quantifiers have already been constructed,
             * so retrieve them *)
            let sem_qs =
                begin
                  match SEnv.find new_mutual_env name with
                    | Some (`Mutual (qs, _)) -> qs
                    | _ -> assert false
                end in

            let args =
              ListUtils.zip' sugar_qs sem_qs
                |> List.map (fun (sq, q) -> (sq, Some(q))) in

            (* Desugar the datatype *)
            let var_env = StringMap.find name venvs_map in
            let dt' = Desugar.datatype' var_env mutual_type_env dt in
            (* Check if the datatype has actually been desugared *)
            let (t, dt) =
              (match dt' with
                   | (t, Some dt) -> (t, dt)
                   | _ -> assert false) in
            (name, args, (t, Some dt), pos)
          ) ts in

        (* Given the desugared datatypes, we now need to handle linearity. *)
        (* First, calculate linearity up to recursive application, and a
         * dependency graph. *)
        let (linearity_env, dep_graph) =
          List.fold_left (fun (lin_map, dep_graph) (name, _, (_, dt), _) ->
            let dt = OptionUtils.val_of dt in
            let lin_map = StringMap.add name (not @@ is_unl_type dt) lin_map in
            let deps = recursive_applications dt in
            let dep_graph = (name, deps) :: dep_graph in
            (lin_map, dep_graph)
          ) (StringMap.empty, []) desugared_mutuals in
        (* Next, topo-sort the dependency graph. We need to reverse since we
         * propagate linearity information downwards from the SCCs which everything
         * depends on, rather than upwards. *)
        let sorted_graph = Graph.topo_sort_sccs dep_graph |> List.rev in
        (* Next, propagate the linearity information through the graph,
           in order to construct the final linearity map.
         * Given the topo-sorted dependency graph, we propagate linearity based
         * on the following rules:
         * 1. If any type in a SCC is linear, then all types in that SCC must
         *    also be linear.
         * 2. If a type depends on a linear type, then it must also be linear.
         * 3. Otherwise, the type is unrestricted.
         *
         * Given that we have a topo-sorted graph, as soon as we come across a
         * linear SCC, we know that the remaining types are also linear. *)
        let (linearity_map, _) =
          List.fold_right (fun scc (acc, lin_found) ->
            let scc_linear =
              lin_found || List.exists (fun x -> StringMap.find x linearity_env) scc in
            let acc =
              List.fold_left (fun acc x -> StringMap.add x scc_linear acc) acc scc in
            (acc, scc_linear)) sorted_graph (StringMap.empty, false) in

        (* Finally, construct environment of new aliases, and populate the map from
         * strings to the desugared datatypes which in turn allows recursive type
         * unwinding in unification. *)
        (* NB: type aliases are scoped; we allow shadowing.
           We also allow type aliases to shadow abstract types. *)
        let new_aliases_env =
          List.fold_left (fun alias_env (t, args, (_, dt'), _) ->
            let dt = OptionUtils.val_of dt' in
            let semantic_qs = List.map (snd ->- val_of) args in
            let alias_env =
              SEnv.bind alias_env (t, (None, `Alias (List.map (snd ->- val_of) args, dt))) in
            tygroup_ref :=
              { !tygroup_ref with
                  type_map = (StringMap.add t (semantic_qs, dt) !tygroup_ref.type_map);
                  linearity_map };
            alias_env
        ) SEnv.empty desugared_mutuals in

        let additions_only_tenv =
          {FrontendTypeEnv.empty_typing_environment with
            FrontendTypeEnv.tycon_env = new_aliases_env} in


        (* Updated both typing environments carried in the object *)
        let updated_type_env =
           FrontendTypeEnv.extend_typing_environment
             ~effects:type_env.FrontendTypeEnv.effect_row
             type_env
             additions_only_tenv in
        let updated_additions_env =
          FrontendTypeEnv.extend_typing_environment
            ~effects:cur_module_additions.FrontendTypeEnv.effect_row
            cur_module_additions
            additions_only_tenv in

        ({< type_env = updated_type_env;
            cur_module_additions = updated_additions_env >},
         Typenames desugared_mutuals)
    | Val (pat, (tyvars, p), loc, dt) ->
        let o, pat = self#pattern pat in
        let o, p   = o#phrase p in
        let o, loc = o#location loc in
          o, Val (pat, (tyvars, p), loc, opt_map (Desugar.datatype' tyvar_map type_env) dt)
    | Fun (bind, lin, (tyvars, fl), loc, dt) ->
        let o, bind = self#binder bind in
        let o, fl   = o#funlit fl in
        let o, loc  = o#location loc in
          o, Fun (bind, lin, (tyvars, fl), loc, opt_map (Desugar.datatype' tyvar_map type_env) dt)
    | Funs binds ->
        let o, binds =
          super#list
            (fun o (bind, lin, (tyvars, fl), loc, dt, pos) ->
               let o, bind = o#binder bind in
               let o, fl   = o#funlit fl in
               let o, loc  = o#location loc in
               let    dt   = opt_map (Desugar.datatype' tyvar_map type_env) dt in
               let o, pos  = o#position pos
               in (o, (bind, lin, (tyvars, fl), loc, dt, pos)))
            binds
        in o, Funs binds
    | Foreign (bind, raw_name, lang, file, dt) ->
        let _, bind = self#binder bind in
        let dt' = Desugar.foreign type_env dt in
        self, Foreign (bind, raw_name, lang, file, dt')
    | Module (name, module_t, bs) ->
       assert (module_t = None);
       let pre_module_type_env = type_env in
       let (o, pre_module_additions) = self#reset_additions () in

       let desugar_module_binding
           : ('self_type * binding list) -> binding -> ('self_type * binding list) =
         fun (o, bs) b ->
         (* Approximates the existing (broken) scoping of type variables
            used in DesugarDatatypes:
            The scope of each type variable is the "toplevel" binding
            it appears in, where toplevel either means the toplevel of
            a file (as usual), but it also includes any binding
            that appears as an immediate child of a module.
            To achieve this, we re-create the map from
            syntactic to semantic type variables for all
            bindings of the module *)
         let tyvars = (typevars#binding b)#tyvar_list in
         let new_tyvar_map = snd (Desugar.generate_var_mapping tyvars) in
         let o = o#update_ty_var_map new_tyvar_map in
         let o, b = o#binding b in
         o, (b :: bs)
       in

       let o, name = o#name name in
       let o, bs_rev =
         List.fold_left
           desugar_module_binding
           (o, [])
           bs in
       let bs = List.rev bs_rev in

       let additions = o#get_additions () in
       let alias_additions =
         Env.String.fold
           (fun tycon (_, tspec) map -> StringMap.add tycon tspec map)
           additions.FrontendTypeEnv.tycon_env
           StringMap.empty in
       let module_additions =
         Env.String.fold
           (fun module_name (_, module_t) map -> StringMap.add module_name module_t  map)
           additions.FrontendTypeEnv.module_env
           StringMap.empty in
       let new_module : Types.module_t =
         {fields  = StringMap.empty;
          tycons  = alias_additions;
          modules = module_additions} in

       (* prerr_endline ("Aliases in module " ^ name);
       StringMap.iter (fun n _ -> print_endline n) new_module.tycons; *)

       let updated_type_env =
         FrontendTypeEnv.bind_module pre_module_type_env (name, new_module) in
       let updated_additions_env =
         FrontendTypeEnv.bind_module pre_module_additions (name, new_module) in

       ({< type_env = updated_type_env;
           cur_module_additions = updated_additions_env>},
        Module (name, module_t, bs))
    | Import qname ->
       ({< type_env = FrontendTypeEnv.open_module qname type_env type_env >}, Import qname)

    | b -> super#bindingnode b

  method! sentence =
    (* return any aliases bound to the interactive loop so that they
       are available to future input.  The default definition will
       do fine here *)
    super#sentence

  method! program (bindings, e) =
    (* as with a block, bindings should not escape here *)
    let o           = {<>} in
    let o, bindings = o#list (fun o -> o#binding) bindings in
    let _o, e       = o#option (fun o -> o#phrase) e in
      self, (bindings, e)

  method update_ty_var_map new_tyvar_map =
    {< tyvar_map = new_tyvar_map >}

  method get_type_env = type_env
end

let phrase type_env p =
  let tvars = (typevars#phrase p)#tyvar_list in
  let tyvar_mapping = (snd (Desugar.generate_var_mapping tvars)) in
    (desugar type_env tyvar_mapping )#phrase p

let binding type_env b =
  let tvars = (typevars#binding b)#tyvar_list in
  let tyvar_mapping = snd (Desugar.generate_var_mapping tvars) in
  (desugar type_env tyvar_mapping)#binding b

let toplevel_bindings type_env bs =
  let type_env, bnds =
    List.fold_left
      (fun (type_env, bnds) bnd ->
         let o, bnd = binding type_env bnd in
           (o#get_type_env, bnd::bnds))
    (type_env, [])
      bs
  in (type_env, List.rev bnds)

let program typing_env (bindings, p : Sugartypes.program) :
    (FrontendTypeEnv.t * Sugartypes.program) =
  let res_ty_env, bindings =
    toplevel_bindings typing_env bindings in
  (res_ty_env, (bindings, opt_map ((phrase typing_env) ->- snd) p))

let sentence typing_env = function
  | Definitions bs ->
      let typing_env', bs' = toplevel_bindings typing_env bs in
        typing_env', Definitions bs'
  | Expression  p  -> let o, p = phrase typing_env p in
      o#get_type_env, Expression p
  | Directive   d  -> typing_env, Directive d

let read ~aliases s =
  let dt, _ = parse_string ~in_context:(LinksLexer.fresh_context ()) datatype s in
  let vars, var_env = Desugar.generate_var_mapping (typevars#datatype dt)#tyvar_list in
  let type_env =
    {FrontendTypeEnv.empty_typing_environment with
      FrontendTypeEnv.tycon_env = aliases } in
  let () = List.iter Generalise.rigidify_quantifier vars in
    (Types.for_all (vars, Desugar.datatype var_env type_env dt))
