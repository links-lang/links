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

let internal_error message = Errors.internal_error ~filename:"desugarDatatypes.ml" ~message

let typevar_primary_kind_mismatch pos var ~expected ~actual =
  Type_error
    ( pos,
      Printf.sprintf "Mismatch in kind for type variable `%s'.\n" var
      ^ Printf.sprintf "  This has kind `%s', but we require a `%s' here."
          (CommonTypes.PrimaryKind.to_string actual)
          (CommonTypes.PrimaryKind.to_string expected) )

let typevar_mismatch pos (v1 : type_variable) (v2 : type_variable) =
  let var, _, _ = v1 in
  Type_error
    ( pos,
      Printf.sprintf "Mismatch in kind for type variable `%s'.\n" var
      ^ Printf.sprintf "  Declared as `%s' and `%s'."
          (string_of_type_variable v1)
          (string_of_type_variable v2) )

let duplicate_var pos var =
  Type_error (pos, Printf.sprintf "Multiple definitions of type variable `%s'." var)

(** Construct an error for when a type alias has free variables within it. *)
let free_type_variable pos name =
  if name.[0] = '_' || name.[0] = '$' then Type_error (pos, "Unbound anonymous type variable.")
  else Type_error (pos, Printf.sprintf "Unbound type variable `%s'." name)

let tygroup_counter = ref 0

let fresh_tygroup_id () =
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

(** Replaces all placeholder variables with a fresh one. *)
class basic_freshener = object
  inherit SugarTraversals.map as super

  method! known_type_variable =
    let module SC = SugarConstructors.SugartypesPositions in
    function
    | (("$" | "$anon"), None, freedom) ->
       SC.fresh_known_type_variable freedom
       |> super#known_type_variable
    | v ->  super#known_type_variable v
end

let freshen_vars =
  let module SC = SugarConstructors.SugartypesPositions in
  (* Determine if this is an "anonymous" effect type variable, and so introduced
     within a simple arrow (->, ~>, -@, ~>) *)
  let is_anon_effect = function
    | Datatype.Open ("$anon", None, `Rigid) -> true
    | _ -> false in
  (* If this function type is exclusively composed of anonymous effect type
     variables. Or rather, there are no explicitly mentioned effect variables. *)
  let all_anon_effects = object
    inherit SugarTraversals.predicate as super

    val all_anon = true
    method satisfied = all_anon

    method! datatypenode = let open Datatype in
      function
      | Function (_, (_, eff_var), _) | Lolli (_, (_, eff_var), _)
           when not (is_anon_effect eff_var) -> {<all_anon = false>}
      | ty -> super#datatypenode ty
  end in

  let basic_refresh = new basic_freshener
  and shared_refresh var = object
    inherit basic_freshener as super
    method! datatypenode = let open Datatype in
      function
      | Function (args, (effects, eff_var), ret) when is_anon_effect eff_var
        -> super#datatypenode (Function (args, (effects, var), ret))
      | Lolli (args, (effects, eff_var), ret) when is_anon_effect eff_var
        -> super#datatypenode (Lolli (args, (effects, var), ret))
      | ty -> super#datatypenode ty
  end in

  object
    inherit basic_freshener as super
    method! datatypenode = let open Datatype in
      function
      | (Function _ | Lolli _) as dt ->
         (* If effect_sugar is enabled, and all variables are fresh (and so not
            explicitly named), then we will substitute the same variable across
            all arrows. Otherwise every arrow gets its own row variable, as
            normal. *)
         if Settings.get_value Basicsettings.Types.effect_sugar
            && (all_anon_effects#datatypenode dt)#satisfied then
           let var = Datatype.Open (SC.fresh_known_type_variable `Rigid) in
           (shared_refresh var)#datatypenode dt
         else
           basic_refresh#datatypenode dt
      | dt -> super#datatypenode dt
  end

(** Ensure this variable has some kind, if {!Basicsettings.Types.infer_kinds} is disabled. *)
let ensure_kinded = function
  | name, (None, subkind), freedom when not (Settings.get_value Basicsettings.Types.infer_kinds) ->
      (name, (Some pk_type, subkind), freedom)
  | v -> v

(* Find all unbound type variables in a term *)
let typevars =
object (self)
  inherit SugarTraversals.fold as super

  val tyvar_list : name list = []
  val tyvars : type_variable StringMap.t = StringMap.empty

  (* fill in subkind with the default *)
  val fill = fun (name, (kind, subkind), freedom) ->
    (name,
     (Some (from_option PrimaryKind.Type kind), Some (from_option default_subkind subkind)),
     freedom)

  method tyvar_list =
    List.map (fun name -> fill (StringMap.find name tyvars)) (List.rev tyvar_list)

  method tyvars = StringMap.map fill tyvars;

  method add_name name = {<tyvar_list = name :: tyvar_list>}

  method register ((name, _, _) as tv) = {<tyvars = StringMap.add name tv tyvars>}

  method replace name map = {<tyvars = StringMap.update name (fun _ -> StringMap.find_opt name map) tyvars>}

  method bind tv = self#register tv

  method add ((name, (pk, sk), freedom) as tv) =
    if StringMap.mem name tyvars then
      begin
        let (_, (pk', sk'), freedom') = StringMap.find name tyvars in
        let union = function
          | Some x, None | None, Some x -> Some x, Some x
          | x, y -> x, y
        in
        (* monotonically increase subkinding information *)
        let (sk, sk') = union (sk, sk') in
        let (pk, pk') = union (pk, pk') in
        let tv = (name, (pk, sk), freedom) in
        let tv' = (name, (pk', sk'), freedom') in
        (* check that duplicate type variables have the same kind *)
        if tv <> tv' then
          raise (typevar_mismatch Position.dummy tv tv');
        self#register tv
      end
    else (self#register tv)#add_name name

  method quantified action qs =
    let o = List.fold_left (fun o q ->
      let q = ensure_kinded (rigidify q) in
      o#bind (rigidify q)) self qs
    in
    let o = action o in
    List.fold_left (fun o (q, _, _) -> o#replace q tyvars) o qs

  method! bindingnode = function
    (* Type declarations bind variables; exclude those from the
       analysis. We'll handle these specially later on *)
    | Typenames _  -> self
    | b            -> super#bindingnode b

  method! datatype ({ pos; _ } as ty) =
    try super#datatype ty
    with Type_error (pos', msg) when pos' = Position.dummy -> raise (Type_error (pos, msg))

  method! datatypenode = let open Datatype in
    function
    | TypeVar (x, k, freedom) -> self#add (x, (Some pk_type, k), freedom)
    | Mu (v, t)       -> self#quantified (fun o -> o#datatype t) [(v, (Some pk_type, None), `Rigid)]
    | Forall (qs, t)  -> self#quantified (fun o -> o#datatype t) qs
    | dt -> super#datatypenode dt

  method! row_var = let open Datatype in function
    | Closed               -> self
    | Open (x, k, freedom) -> self#add (x, (Some pk_row, k), freedom)
    | Recursive (s, r)     -> self#quantified (fun o -> o#row r) [(s, (Some pk_row, None), `Rigid)]

  method! fieldspec = let open Datatype in function
    | Absent -> self
    | Present t -> self#datatype t
    | Var (x, k, freedom) -> self#add (x, (Some pk_presence, k), freedom)
end

type var_env = { tyvars : meta_var StringMap.t } [@@unboxed]

let empty_env = { tyvars = StringMap.empty }

let kind_error pos var expected = function
  | Some d ->
      let actual =
        match d with
        | `Type _ -> PrimaryKind.Type
        | `Row _ -> PrimaryKind.Row
        | `Presence _ -> PrimaryKind.Presence
      in
      typevar_primary_kind_mismatch pos var ~expected ~actual
  | None -> free_type_variable pos var

let lookup_tvar pos t { tyvars } =
  match StringMap.find_opt t tyvars with
  | Some (`Type v) -> v
  | x -> raise (kind_error pos t PrimaryKind.Type x)

let lookup_rvar pos t { tyvars } =
  match StringMap.find_opt t tyvars with
  | Some (`Row v) -> v
  | x -> raise (kind_error pos t PrimaryKind.Row x)

let lookup_pvar pos t { tyvars } =
  match StringMap.find_opt t tyvars with
  | Some (`Presence v) -> v
  | x -> raise (kind_error pos t PrimaryKind.Presence x)

module Desugar = struct
  (* Desugars quantifiers into Types.quantifiers,
   * returning updated variable environment.
   * Lifted / deduplicated from `typename` and `Forall` desugaring. *)
  let desugar_quantifiers (var_env: var_env) (qs: Sugartypes.quantifier list) body pos :
      (Types.quantifier list * var_env) =
    (* Bind all quantified variables, and then do a naive {!typevars} pass over this set to infer
       any unannotated kinds, and verify existing kinds/subkinds match up.

       Also verify no duplicate variables exist. *)
    let tvs, _ = List.fold_left
      (fun (o, names) ((name, _, _) as v) ->
        if StringSet.mem name names then raise (duplicate_var pos name);
        (o#bind (ensure_kinded v), StringSet.add name names)) (typevars, StringSet.empty) qs in
    let tvs = (tvs#datatype body)#tyvars in
    let qs = List.map (fun (name, _, _) -> StringMap.find name tvs) qs in

    ListLabels.fold_right ~init:([], var_env) qs
      ~f:(fun (name, (primarykind, subkind), _freedom)
              (args, { tyvars }) ->
            let var = Types.fresh_raw_variable () in
            let subkind = concrete_subkind subkind in
            let quant, def =
              match primarykind with
              | Some PrimaryKind.Type ->
                  let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
                  ((var, subkind, `Type point), `Type point)
              | Some PrimaryKind.Row ->
                  let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
                  ((var, subkind, `Row point), `Row point)
              | Some PrimaryKind.Presence ->
                  let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
                  ((var, subkind, `Presence point), `Presence point)
              | None -> raise (internal_error "Undesugared kind")
            in
            (quant :: args,
             { tyvars = StringMap.add name def tyvars }) )

  let rec datatype var_env (alias_env : Types.tycon_environment) t' =
    let datatype var_env t' = datatype var_env alias_env t' in
    match t' with
    | { node = t; pos } ->
      let open Datatype in
      match t with
        | TypeVar (s, _, _) -> `MetaTypeVar (lookup_tvar pos s var_env)
        | QualifiedTypeApplication _ -> assert false (* will have been erased *)
        | Function (f, e, t) ->
            `Function (Types.make_tuple_type (List.map (datatype var_env) f),
                      effect_row var_env alias_env e t,
                      datatype var_env t)
        | Lolli (f, e, t) ->
            `Lolli (Types.make_tuple_type (List.map (datatype var_env) f),
                       effect_row var_env alias_env e t,
                       datatype var_env t)
        | Mu (name, t) ->
            let var = Types.fresh_raw_variable () in
            (* FIXME: shouldn't we support other subkinds for recursive types? *)
            let point = Unionfind.fresh (`Var (var, default_subkind, `Flexible)) in
            let tyvars = StringMap.add name (`Type point) var_env.tyvars in
            let _ = Unionfind.change point (`Recursive (var, datatype { tyvars } t)) in
              `MetaTypeVar point
        | Forall (qs, t) ->
            let (qs: Types.quantifier list), var_env = desugar_quantifiers var_env qs t pos in
            let t = datatype var_env t in
              `ForAll (Types.box_quantifiers qs, t)
        | Unit -> Types.unit_type
        | Tuple ks ->
            let labels = map string_of_int (Utility.fromTo 1 (1 + length ks)) in
            let unit = Types.make_empty_closed_row () in
            let present (s, x) = (s, `Present x)
            in
              `Record (fold_right2 (curry (Types.row_with -<- present)) labels (map (datatype var_env) ks) unit)
        | Record r -> `Record (row var_env alias_env r t')
        | Variant r -> `Variant (row var_env alias_env r t')
        | Effect r -> `Effect (row var_env alias_env r t')
        | Table (r, w, n) -> `Table (datatype var_env r, datatype var_env w, datatype var_env n)
        | List k -> `Application (Types.list, [ `Type (datatype var_env k) ])
        | TypeApplication (tycon, ts) ->
            (* Matches kinds of the quantifiers against the type arguments.
             * Returns Types.type_args based on the given frontend type arguments. *)
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
                  raise
                    (TypeApplicationKindMismatch
                       { pos;
                         name = tycon;
                         tyarg_number = i;
                         expected = PrimaryKind.to_string q_kind;
                         provided = PrimaryKind.to_string t_kind
                       })
                else (q, t)
              in
              let type_arg' var_env alias_env = function
                | Row r -> `Row (effect_row var_env alias_env r t')
                | t -> type_arg var_env alias_env t t'
              in
              begin
                try
                  let ts = ListUtils.zip' qs ts in
                  List.mapi
                    (fun i (q,t) ->
                      let (q, t) = match_kinds i (q, t) in
                      match subkind_of_quantifier q with
                      | (_, Restriction.Effect) -> type_arg' var_env alias_env t
                      | _ -> type_arg var_env alias_env t t') ts
                with
                  ListUtils.Lists_length_mismatch ->
                    raise
                    (TypeApplicationArityMismatch
                       { pos; name = tycon; expected = List.length qs; provided = List.length ts }) end in
            begin match SEnv.find alias_env tycon with
              | None -> raise (UnboundTyCon (pos, tycon))
              | Some (`Alias (qs, _dt)) ->
                  let ts = match_quantifiers qs in
                  Instantiate.alias tycon ts alias_env
              | Some (`Abstract abstype) ->
                  (* TODO: check that the kinds match up *)
                  `Application (abstype, List.map (fun ta -> type_arg var_env alias_env ta t') ts)
              | Some (`Mutual (qs, tygroup_ref)) ->
                  (* Check that the quantifiers / kinds match up, then generate
                   * a `RecursiveApplication. *)
                  let r_args = match_quantifiers qs in
                  let r_unwind args dual =
                    let _, body = StringMap.find tycon !tygroup_ref.type_map in
                    let body = Instantiate.recursive_application tycon qs args body in
                    if dual then dual_type body else body
                  in
                  let r_unique_name = tycon ^ string_of_int !tygroup_ref.id in
                  let r_linear () = StringMap.lookup tycon !tygroup_ref.linearity_map in
                  `RecursiveApplication
                    { r_name = tycon; r_dual = false; r_unique_name; r_args; r_unwind; r_linear }
            end
        | Primitive k -> `Primitive k
        | DB -> `Primitive Primitive.DB
        | (Input _ | Output _ | Select _ | Choice _ | Dual _ | End) as s ->
            session_type var_env alias_env s t'

  and session_type var_env alias_env st (node : 'a WithPos.t) =
    (* let lookup_type t = StringMap.find t var_env.tenv in -- used only in commented code *)
    (* HACKY *)
    let open Datatype in
    match st with
    | Input (t, s)  -> `Input (datatype var_env alias_env t, datatype var_env alias_env s)
    | Output (t, s) -> `Output (datatype var_env alias_env t, datatype var_env alias_env s)
    | Select r      -> `Select (row var_env alias_env r node)
    | Choice r      -> `Choice (row var_env alias_env r node)
    | Dual s        -> `Dual (datatype var_env alias_env s)
    | End           -> `End
    | _ -> assert false

  and fieldspec var_env alias_env fs { pos; _ } =
    let open Datatype in
    match fs with
    | Absent -> `Absent
    | Present t -> `Present (datatype var_env alias_env t)
    | Var (name, _, _) -> `Var (lookup_pvar pos name var_env)

  and row var_env alias_env (fields, rv) (node : 'a WithPos.t) =
    let seed =
      let open Datatype in
      match rv with
        | Closed -> Types.make_empty_closed_row ()
        | Open (rv, _, _) -> (StringMap.empty, lookup_rvar node.pos rv var_env, false)
        | Recursive (name, r) ->
            let var = Types.fresh_raw_variable () in
            let point = Unionfind.fresh (`Var (var, default_subkind, `Flexible)) in
            let tyvars = StringMap.add name (`Row point) var_env.tyvars in
            let _ = Unionfind.change point (`Recursive (var, row { tyvars } alias_env r node)) in
            (StringMap.empty, point, false)
    in
    let fields = List.map (fun (k, p) -> (k, fieldspec var_env alias_env p node)) fields in
    fold_right Types.row_with fields seed

  and effect_row var_env alias_env (fields, rv) node =
    let fields =
      (* Closes any empty, open arrow rows on user-defined
         operations. Note any row which can be closed will have an
         unbound effect variable.  *)
      List.map
        (let open Datatype in
        function
        | (name, Present { node = Function (domain, (fields, rv), codomain); pos }) as op
          when not (TypeUtils.is_builtin_effect name) -> (
          (* Elaborates `Op : a -> b' to `Op : a {}-> b' *)
          match (rv, fields) with
          | Closed, [] -> op
          | Open _, [] | Recursive _, [] ->
              (* might need an extra check on recursive rows *)
              (name, Present (WithPos.make ~pos (Function (domain, ([], Closed), codomain))))
          | _, _ ->
              raise
                (Type_error
                   ( pos,
                     "The abstract operation " ^ name ^ " has unexpected "
                     ^ "effects in its signature. The effect signature on an "
                     ^ "abstract operation arrow is always supposed to be empty, "
                     ^ "since any effects it might have are ultimately conferred by its handler."
                   )) )
        | x -> x)
        fields
    in
    let (fields, rho, dual) = row var_env alias_env (fields, rv) node in
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

  and type_arg var_env alias_env ta node =
    let open Datatype in
    match ta with
    | Type t -> `Type (datatype var_env alias_env t)
    | Row r -> `Row (row var_env alias_env r node)
    | Presence f -> `Presence (fieldspec var_env alias_env f node)

  (* pre condition: all subkinds have been filled in *)
  let generate_var_mapping (vars : type_variable list) : Types.quantifier list * var_env =
    let addt x t envs = { tyvars = StringMap.add x (`Type t) envs.tyvars } in
    let addr x r envs = { tyvars = StringMap.add x (`Row r) envs.tyvars } in
    let addf x f envs = { tyvars = StringMap.add x (`Presence f) envs.tyvars } in
    let vars, var_env =
      List.fold_left
        (fun (vars, envs) ->
           let var = Types.fresh_raw_variable () in
             fun (x, kind, freedom) ->
             match (kind, freedom) with
             | (Some PrimaryKind.Type, Some subkind), freedom ->
               let t = Unionfind.fresh (`Var (var, subkind, freedom)) in
                 (var, subkind, `Type t)::vars, addt x t envs
             | (Some PrimaryKind.Row, Some subkind), freedom ->
               let r = Unionfind.fresh (`Var (var, subkind, freedom)) in
                 (var, subkind, `Row r)::vars, addr x r envs
             | (Some PrimaryKind.Presence, Some subkind), freedom ->
               let f = Unionfind.fresh (`Var (var, subkind, freedom)) in
                 (var, subkind, `Presence f)::vars, addf x f envs
             | (_, None), _ | (None, _), _ ->
               (* Shouldn't occur; we are assuming that all subkinds have been
                * filled in*)
               assert false)
        ([], empty_env)
        vars
    in
      List.rev vars, var_env

  let datatype' map alias_env ((dt, _) : datatype') = (dt, Some (datatype map alias_env dt))

  (* Desugar a foreign function declaration. Foreign declarations cannot use type variables from
     the context. Any type variables found are implicitly universally quantified at this point. *)
  let foreign alias_env dt =
    let tvars = (typevars#datatype' dt)#tyvar_list in
    datatype' (snd (generate_var_mapping tvars)) alias_env dt

  (* Desugar a table literal. No free variables are allowed here. We generate both read and write
     types by looking for readonly constraints *)
  let tableLit alias_env constraints dt =
    let read_type =
      match datatype' empty_env alias_env (dt, None) with
      | _, Some read_type -> read_type
      | _ -> assert false
    in
    let write_row, needed_row =
      match TypeUtils.concrete_type read_type with
      | `Record (fields, _, _) ->
          StringMap.fold
            (fun label t (write, needed) ->
              match lookup label constraints with
              | Some cs ->
                  if List.exists (( = ) Readonly) cs then (write, needed)
                  else
                    (* if List.exists ((=) `Default) cs then *)
                    (Types.row_with (label, t) write, needed)
              | _ ->
                  let add = Types.row_with (label, t) in
                  (add write, add needed) )
            fields
            (Types.make_empty_closed_row (), Types.make_empty_closed_row ())
      | _ -> raise (internal_error "Table types must be record types")
    in
    (* We deliberately don't concretise the returned read_type in the hope of improving error
       messages during type inference. *)
    (read_type, `Record write_row, `Record needed_row)
end


(* convert a syntactic type into a semantic type, using `map' to resolve free type variables *)
let desugar initial_alias_env map =
object (self)
  inherit SugarTraversals.fold_map as super

  val alias_env = initial_alias_env

  method! datatype' node = (self, Desugar.datatype' map alias_env node)

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
    | TableLit (t, (dt, _), cs, keys, p) ->
        let read, write, needed = Desugar.tableLit alias_env cs dt in
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

        (* Add all type declarations in the group to the alias
         * environment, as mutuals. Quantifiers need to be desugared. *)
        let (mutual_env, venvs_map) =
          List.fold_left (fun (alias_env, venvs_map) (t, args, (d, _), pos) ->
            let qs = List.map fst args in
            let qs, var_env =  Desugar.desugar_quantifiers empty_env qs d pos in
            let venvs_map = StringMap.add t var_env venvs_map in
            (SEnv.bind alias_env (t, `Mutual (qs, tygroup_ref)), venvs_map) )
            (alias_env, venvs_map) ts in

        (* Desugar all DTs, given the temporary new alias environment. *)
        let desugared_mutuals =
          List.map (fun (name, args, dt, pos) ->
            let sugar_qs = List.map (fst) args in

            (* Semantic quantifiers have already been constructed,
             * so retrieve them *)
            let sem_qs =
                begin
                  match SEnv.find mutual_env name with
                    | Some (`Mutual (qs, _)) -> qs
                    | _ -> assert false
                end in

            let args =
              ListUtils.zip' sugar_qs sem_qs
                |> List.map (fun (sq, q) -> (sq, Some(q))) in

            (* Desugar the datatype *)
            let var_env = StringMap.find name venvs_map in
            let dt' = Desugar.datatype' var_env mutual_env dt in
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
            let lin_map = StringMap.add name (not @@ Unl.is_type dt) lin_map in
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

        (* Finally, construct a new alias environment, and populate the map from
         * strings to the desugared datatypes which in turn allows recursive type
         * unwinding in unification. *)
        (* NB: type aliases are scoped; we allow shadowing.
           We also allow type aliases to shadow abstract types. *)
        let alias_env =
          List.fold_left (fun alias_env (t, args, (_, dt'), _) ->
            let dt = OptionUtils.val_of dt' in
            let semantic_qs = List.map (snd ->- val_of) args in
            let alias_env =
              SEnv.bind alias_env (t, `Alias (List.map (snd ->- val_of) args, dt)) in
            tygroup_ref :=
              { !tygroup_ref with
                  type_map = (StringMap.add t (semantic_qs, dt) !tygroup_ref.type_map);
                  linearity_map };
            alias_env
        ) alias_env desugared_mutuals in

        ({< alias_env = alias_env >}, Typenames desugared_mutuals)
    | Foreign (bind, raw_name, lang, file, dt) ->
        let _, bind = self#binder bind in
        let dt' = Desugar.foreign alias_env dt in
        self, Foreign (bind, raw_name, lang, file, dt')
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

  method aliases = alias_env
end

let phrase alias_env p =
  let p = freshen_vars#phrase p in
  let tvars = (typevars#phrase p)#tyvar_list in
    (desugar alias_env (snd (Desugar.generate_var_mapping tvars)))#phrase p

let binding alias_env ({ node; pos } as b : binding) =
  match node with
  | Funs bnds ->
      let bnds =
        List.map
          (fun bnd ->
            let bnd = freshen_vars#recursive_function bnd in
            let tvars = (typevars#bindingnode (Funs [ bnd ]))#tyvar_list in
            (desugar alias_env (snd (Desugar.generate_var_mapping tvars)))#recursive_function bnd
            |> snd )
          bnds
      in
      (alias_env, WithPos.make ~pos (Funs bnds))
  | _ ->
      let b = freshen_vars#binding b in
      let tvars = (typevars#binding b)#tyvar_list in
      let o, b = (desugar alias_env (snd (Desugar.generate_var_mapping tvars)))#binding b in
      (o#aliases, b)

let toplevel_bindings alias_env bs =
  let alias_env, bnds =
    List.fold_left
      (fun (alias_env, bnds) bnd ->
         let aliases, bnd = binding alias_env bnd in
           (aliases, bnd::bnds))
    (alias_env, [])
      bs
  in (alias_env, List.rev bnds)

let program typing_env (bindings, p : Sugartypes.program) :
    Sugartypes.program =
  let alias_env = typing_env.tycon_env in
  let alias_env, bindings =
    toplevel_bindings alias_env bindings in
  (* let typing_env = { typing_env with tycon_env = alias_env } in *)
  (bindings, opt_map ((phrase alias_env) ->- snd) p)

let sentence typing_env = function
  | Definitions bs ->
      let _alias_env, bs' = toplevel_bindings typing_env.tycon_env bs in
        Definitions bs'
  | Expression  p  -> let _o, p = phrase typing_env.tycon_env p in
      Expression p
  | Directive   d  -> Directive d

let read ~aliases s =
  let dt, _ = parse_string ~in_context:(LinksLexer.fresh_context ()) datatype s in
  let dt = freshen_vars#datatype dt in
  let vars, var_env = Desugar.generate_var_mapping (typevars#datatype dt)#tyvar_list in
  let () = List.iter Generalise.rigidify_quantifier vars in
    (Types.for_all (vars, Desugar.datatype var_env aliases dt))
