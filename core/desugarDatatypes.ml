(** Desugar datatypes converts types from their syntactic representation
   (defined in {!Sugartypes}) to the semantic one defined in {!Types}. It also
   handles some limited  forms of syntactic sugar within types, and translates
   them appropriately.
 *)

module Transform' = Transform (* One of the modules below defines a
                                 module named 'Transform' which
                                 shadows the compilation unit
                                 'Transform'. *)
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


let found_non_var_meta_var =
  internal_error "Every meta_*_var in a SugarTypeVar must be a Var at this point"

let tygroup_counter = ref 0

let fresh_tygroup_id () =
  let ret = !tygroup_counter in
  tygroup_counter := ret + 1;
  ret

(** Check that no datatype is left undesugared. *)
let all_datatypes_desugared =
object (self)
  inherit SugarTraversals.predicate as super

  val all_desugared = true
  method satisfied = all_desugared

  method! datatype' = function
      (_, None) -> {< all_desugared = false >}
    | _ -> self

  method! row' = function
      (_, None) -> {< all_desugared = false >}
    | _ -> self

  method! type_arg' = function
      (_, None) -> {< all_desugared = false >}
    | _ -> self

  method! phrasenode = function
    | TableLit { tbl_type = (_, _, None); _ } ->
        {< all_desugared = false >}
    | p -> super#phrasenode p
end



let unpack_var_id = function
  | Types.Var (id, subkind, _) -> id, subkind
  | _ -> raise found_non_var_meta_var



module Desugar = struct

  let desugar_quantifiers  (sqs: SugarQuantifier.t list) :  Quantifier.t list =
    List.map SugarQuantifier.get_resolved_exn sqs

  let rec datatype (alias_env : Types.tycon_environment) t' =
    let datatype t' = datatype alias_env t' in
    match t' with
    | { node = t; pos } ->
      let open Datatype in
      (* let z = *)
      match t with
        | TypeVar stv ->
           let point = SugarTypeVar.get_resolved_type_exn stv in
           Meta point
        | QualifiedTypeApplication _ -> assert false (* will have been erased *)
        | Function (f, e, t) as _fn ->
            Types.Function ( Types.make_tuple_type (List.map datatype f)
                           , row alias_env e t'
                           , datatype t )
        | Lolli (f, e, t) ->
            Types.Lolli ( Types.make_tuple_type (List.map datatype f)
                        , row alias_env e t'
                        , datatype t )
        | Mu (stv, t) ->
           let mtv = SugarTypeVar.get_resolved_type_exn stv in
           let var, sk = unpack_var_id (Unionfind.find mtv) in
           let t = datatype t in

            (* Turn mtv into a proper recursive type *)
            Unionfind.change mtv (Types.Recursive (var, sk, t));
            Meta mtv
        | Forall (qs, t) ->
            let qs: Quantifier.t list = desugar_quantifiers qs in
            let t = datatype t in
              ForAll (qs, t)
        | Unit -> Types.unit_type
        | Tuple ks ->
            let labels = map string_of_int (Utility.fromTo 1 (1 + length ks)) in
            let unit = Types.make_empty_closed_row () in
            let present (s, x) = (s, Types.Present x)
            in
              Types.Record (fold_right2 (curry (Types.row_with -<- present)) labels (map datatype ks) unit)
        | Record r -> Types.Record (row alias_env r t')
        | Variant r -> Types.Variant (row alias_env r t')
        | Effect r -> Types.Effect (row alias_env r t')
        | Operation (f, t, b) -> Types.Operation ( Types.make_tuple_type (List.map datatype f)
                           , datatype t, b)
        | Table (tmp, r, w, n) -> Types.Table (tmp, datatype r, datatype w, datatype n)
        | List k -> Types.Application (Types.list, [(PrimaryKind.Type, datatype k)])
        | TypeApplication (tycon, ts) ->
            (* Matches kinds of the quantifiers against the type arguments.
             * Returns Types.type_args based on the given frontend type arguments. *)
            let match_quantifiers : type a. (a -> Kind.t) -> a list -> Types.type_arg list = fun proj qs ->
              let match_kinds i (q, t) =
                let primary_kind_of_type_arg : Datatype.type_arg -> PrimaryKind.t = function
                  | Type _ -> PrimaryKind.Type
                  | Row _ -> PrimaryKind.Row
                  | Presence _ -> PrimaryKind.Presence
                in
                let q_kind, _ = proj q in
                let t_kind = primary_kind_of_type_arg t in
                if q_kind <> t_kind then
                  raise
                    (type_application_kind_mismatch pos tycon i
                        (PrimaryKind.to_string q_kind)
                        (PrimaryKind.to_string t_kind))
                else t
              in
              let type_args qs ts =
                List.combine qs ts
                |> List.mapi
                     (fun i (q,t) ->
                       let  t = match_kinds i (q, t) in
                       type_arg alias_env t t')
              in

              let qn = List.length qs and tn = List.length ts in
              if qn = tn then
                type_args qs ts
              else
                raise (TypeApplicationArityMismatch { pos; name = tycon; expected = qn; provided = tn })
            in
            begin match SEnv.find_opt tycon alias_env with
              | None -> raise (unbound_tycon pos tycon)
              | Some (`Alias (k, qs, _dt)) ->
                  if k = pk_type then
                    let ts = match_quantifiers snd qs in
                    Instantiate.alias tycon ts alias_env
                  else
                    raise (type_application_global_kind_mismatch pos tycon
                        "Type" (PrimaryKind.to_string k))
              | Some (`Abstract abstype) ->
                  let ts = match_quantifiers identity (Abstype.arity abstype) in
                  Application (abstype, ts)
              | Some (`Mutual (qs, tygroup_ref)) ->
                  (* Check that the quantifiers / kinds match up, then generate
                   * a `RecursiveApplication. *)
                  let r_args = match_quantifiers snd qs in
                  let r_unwind args dual =
                    let _, body = StringMap.find tycon !tygroup_ref.type_map in
                    let body = Instantiate.recursive_application tycon qs args body in
                    if dual then dual_type body else body
                  in
                  let r_unique_name = tycon ^ string_of_int !tygroup_ref.id in
                  let r_linear () = StringMap.lookup tycon !tygroup_ref.linearity_map in
                  RecursiveApplication
                    { r_name = tycon;
                      r_dual = false;
                      r_unique_name;
                      r_quantifiers = List.map snd qs;
                      r_args; r_unwind; r_linear
                    }
            end
        | Primitive k -> Types.Primitive k
        | DB -> Types.Primitive Primitive.DB
        | (Input _ | Output _ | Select _ | Choice _ | Dual _ | End) as s ->
            session_type alias_env s t'

  and session_type alias_env st (node : 'a WithPos.t) =
    (* let lookup_type t = StringMap.find t var_env.tenv in -- used only in commented code *)
    (* HACKY *)
    let open Datatype in
    match st with
    | Input (t, s)  -> Types.Input  (datatype alias_env t, datatype alias_env s)
    | Output (t, s) -> Types.Output (datatype alias_env t, datatype alias_env s)
    | Select r      -> Types.Select (row alias_env r node)
    | Choice r      -> Types.Choice (row alias_env r node)
    | Dual s        -> Types.Dual (datatype alias_env s)
    | End           -> Types.End
    | _ -> assert false

  and fieldspec alias_env fs _ =
    match fs with
    | Datatype.Absent -> Types.Absent
    | Datatype.Present t -> Types.Present (datatype alias_env t)
    (* | Var stv when is_anon stv ->
     *    let (_name, sk, freedom) = SugarTypeVar.get_unresolved_exn stv in
     *    `Var (make_anon_point var_env pos sk freedom) *)
    | Datatype.Var spv ->
       let resolved_pv = SugarTypeVar.get_resolved_presence_exn spv in
       (* Debug.print ("presence var: " ^ Types.string_of_presence (Types.Meta resolved_pv)); *)
       Types.Meta resolved_pv

  and row alias_env (fields, rv) (node : 'a WithPos.t) =
    let seed =
      let open Datatype in
      match rv with
        | EffectApplication (name, ts) ->
            let match_quantifiers : type a. (a -> Kind.t) -> a list -> Types.type_arg list = fun proj qs ->
              let match_kinds i (q, t) =
                let primary_kind_of_type_arg : Datatype.type_arg -> PrimaryKind.t = function
                  | Type _ -> PrimaryKind.Type
                  | Row _ -> PrimaryKind.Row
                  | Presence _ -> PrimaryKind.Presence
                in
                let q_kind, _ = proj q in
                let t_kind = primary_kind_of_type_arg t in
                if q_kind <> t_kind then
                  raise
                    (type_application_kind_mismatch node.pos name i
                        (PrimaryKind.to_string q_kind)
                        (PrimaryKind.to_string t_kind))
                else t
              in
              let type_args qs ts =
                List.combine qs ts
                |> List.mapi
                     (fun i (q,t) ->
                       let  t = match_kinds i (q, t) in
                       type_arg alias_env t node)
              in
              let qn = List.length qs and tn = List.length ts in
              if qn = tn then
                type_args qs ts
              else
                raise (TypeApplicationArityMismatch { pos = node.pos; name = name; expected = qn; provided = tn })
            in
            begin match SEnv.find_opt name alias_env with
              | None -> raise (unbound_tycon node.pos name)
              | Some (`Alias (k, qs, _r)) ->
                  if k = pk_row then
                    let ts = match_quantifiers snd qs in
                    begin match Instantiate.alias name ts alias_env with
                      | Alias(PrimaryKind.Row, _, body) -> body
                      | _ -> raise (internal_error "Instantiation failed")
                    end
                  else
                    raise (type_application_global_kind_mismatch node.pos name
                        "Row" (PrimaryKind.to_string k))
              | Some (`Abstract abstype) ->
                  let ts = match_quantifiers identity (Abstype.arity abstype) in
                  Application (abstype, ts)
              | Some (`Mutual (qs, tygroup_ref)) ->
                  (* Check that the quantifiers / kinds match up, then generate
                   * a `RecursiveApplication. *)
                  let r_args = match_quantifiers snd qs in
                  let r_unwind args dual =
                    let _, body = StringMap.find name !tygroup_ref.type_map in
                    let body = Instantiate.recursive_application name qs args body in
                    if dual then dual_type body else body
                  in
                  let r_unique_name = name ^ string_of_int !tygroup_ref.id in
                  let r_linear () = StringMap.lookup name !tygroup_ref.linearity_map in
                  RecursiveApplication
                    { r_name = name;
                      r_dual = false;
                      r_unique_name;
                      r_quantifiers = List.map snd qs;
                      r_args; r_unwind; r_linear
                    }
            end
        | Closed -> Types.make_empty_closed_row ()
        | Open srv ->
           let rv = SugarTypeVar.get_resolved_row_exn srv in
           Types.Row (StringMap.empty, rv, false)
        | Recursive (stv, r) ->
           let mrv = SugarTypeVar.get_resolved_row_exn stv in

           let var, sk = unpack_var_id (Unionfind.find mrv) in
           let r = row alias_env r node in

           (* Turn mrv into a proper recursive row *)
           Unionfind.change mrv (Types.Recursive (var, sk, r));
           Types.Row (StringMap.empty, mrv, false)

    in
    let fields = List.map (fun (k, p) -> (k, fieldspec alias_env p node)) fields in
    fold_right Types.row_with fields seed


  and type_arg alias_env ta node =
    let open Datatype in
    let open PrimaryKind in
    match ta with
    | Type t     -> Type, datatype alias_env t
    | Row r      -> Row, row alias_env r node
    | Presence f -> Presence, fieldspec alias_env f node

  let datatype' alias_env ((dt, _) : datatype') =
    (dt, Some (datatype alias_env dt))

  let row' alias_env ((r, _) :row') =
    (r, Some (row alias_env r (WithPos.make (Datatype.Effect r)))) (* should we keep the pos ? have a real node ? *)

  let aliasbody alias_env = function
    | Typename dt' -> Typename (datatype' alias_env dt')
    | Effectname r' -> Effectname (row' alias_env r')

  let type_arg' alias_env ((ta, _) : type_arg') : type_arg' =
    let unlocated = WithPos.make Datatype.Unit in
    (ta, Some (type_arg alias_env ta unlocated))

  (* Desugar a foreign function declaration. Foreign declarations cannot use type variables from
     the context. Any type variables found are implicitly universally quantified at this point. *)
  let foreign alias_env dt =
    datatype' alias_env dt

  (* Desugar a table literal. No free variables are allowed here. We generate both read and write
     types by looking for readonly constraints *)
  let table_lit alias_env constraints dt =
    let read_type =
      match datatype' alias_env (dt, None) with
      | _, Some read_type -> read_type
      | _ -> assert false in
    let write_row, needed_row =
      match TypeUtils.concrete_type read_type with
      | Record (Row (fields, _, _)) ->
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
      | _ -> raise (internal_error "Table types must be record types") in
    (* We deliberately don't concretise the returned read_type in the hope of improving error
       messages during type inference. *)
    (read_type, Record write_row, Record needed_row)
end

(** Convert a syntactic type into a semantic type, using `map' to resolve free type variables *)
let desugar initial_alias_env =
object (self)
  inherit SugarTraversals.fold_map as super

  val alias_env = initial_alias_env

  method! datatype' node = (self, Desugar.datatype' alias_env node)

  method! row' node = (self, Desugar.row' alias_env node)

  method! type_arg' node = (self, Desugar.type_arg' alias_env node)

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
    | TableLit {
        tbl_name;
        tbl_type = (tmp, dt, _);
        tbl_field_constraints = cs;
        tbl_keys;
        tbl_temporal_fields; tbl_database } ->

        let read, write, needed = Desugar.table_lit alias_env cs dt in
        let o, tbl_name = self#phrase tbl_name in
        let o, tbl_keys = o#phrase tbl_keys in
        let o, tbl_database = o#phrase tbl_database in
        let lit = TableLit {
            tbl_name;
            tbl_type = (tmp, dt, Some (read, write, needed));
            tbl_field_constraints = cs;
            tbl_keys; tbl_temporal_fields; tbl_database }
        in
          o, lit
    (* Switch and receive type annotations are never filled in by
       this point, so we ignore them.  *)
    | p -> super#phrasenode p


  method! bindingnode = function
    | Aliases ts ->
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


        (* Add all type declarations in the group to the alias
         * environment, as mutuals. Quantifiers need to be desugared. *)
        let ((mutual_env : Types.tycon_environment), ts) =
          List.fold_left (fun (alias_env, ts) {node=(t, args, b); pos} ->
            let qs = Desugar.desugar_quantifiers args  in
            match b with
              | Typename (d,_) ->
                let alias_env = SEnv.bind t (`Mutual (qs, tygroup_ref)) alias_env in
                (alias_env, WithPos.make ~pos (t, args, Typename (d, None)) :: ts)
              | Effectname (r,_) ->
                let alias_env = SEnv.bind t (`Mutual (qs, tygroup_ref)) alias_env in
                (alias_env, WithPos.make ~pos (t, args, Effectname (r, None)) :: ts))
            (alias_env, []) ts in

        (* Desugar all DTs, given the temporary new alias environment. *)
        let desugared_mutuals =
          List.map (fun {node=(name, args, b); pos} ->
            (* Desugar the datatype *)
            (* Check if the datatype has actually been desugared *)
            let b' = match Desugar.aliasbody mutual_env b with
                | Typename     (_, Some _) as b' -> b'
                | Effectname   (_, Some _) as b' -> b'
                | _ -> raise (internal_error "Datatype not desugared")
            in
            WithPos.make ~pos (name, args, b')
          ) ts in

        (* Given the desugared datatypes, we now need to handle linearity.
           First, calculate linearity up to recursive application *)
        let (linearity_env, dep_graph) =
          List.fold_left (fun (lin_map, dep_graph) mutual   ->
            match SourceCode.WithPos.node mutual with
            | (name, _, Typename     (_, dt))
            | (name, _, Effectname   (_, dt)) ->
              let dt = OptionUtils.val_of dt in
              let lin_map = StringMap.add name (not @@ Unl.type_satisfies dt) lin_map in
              let deps = recursive_applications dt in
              let dep_graph = (name, deps) :: dep_graph in
              (lin_map, dep_graph)
          ) (StringMap.empty, []) desugared_mutuals in
        (* Next, use the toposorted dependency graph from above. We need to
           reverse since we propagate linearity information downwards from the
           SCCs which everything depends on, rather than upwards. *)
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
          List.fold_left (fun alias_env {node=(t, args, b); _} ->
            let semantic_qs = List.map SugarQuantifier.get_resolved_exn args in
            let dt, k = match b with
              | Typename     (_, dt') -> OptionUtils.val_of dt', pk_type
              | Effectname   (_, dt') -> OptionUtils.val_of dt', pk_row
            in
            let alias_env = SEnv.bind t (`Alias (k , semantic_qs, dt)) alias_env in
            tygroup_ref :=
              { !tygroup_ref with
                  type_map = (StringMap.add t (semantic_qs, dt) !tygroup_ref.type_map);
                  linearity_map };
            alias_env
        ) alias_env desugared_mutuals in

        ({< alias_env = alias_env >}, Aliases desugared_mutuals)

    | Foreign alien ->
       let binder, datatype = Alien.declaration alien in
       let _, binder = self#binder binder in
       let datatype = Desugar.foreign alias_env datatype in
       self, Foreign (Alien.modify ~declarations:[(binder, datatype)] alien)
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
  (desugar alias_env)#phrase p

let binding alias_env ({ node; pos } as b : binding) =
  match node with
  | Funs bnds ->
      let bnds =
        List.map
          (fun bnd ->
            (desugar alias_env)#recursive_function bnd
            |> snd )
          bnds
      in
      (alias_env, WithPos.make ~pos (Funs bnds))
  | _ ->
      let o, b = (desugar alias_env)#binding b in
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
  let dt = DesugarTypeVariables.standalone_signature dt in
  let dt = DesugarEffects.standalone_signature aliases dt in
  let _, ty = Generalise.generalise Env.String.empty (Desugar.datatype aliases dt) in
  ty

module Untyped = struct
  open Transform'.Untyped

  let name = "datatypes"

  let program state program' =
    (* Debug.print ("program': " ^ Sugartypes.show_program program'); *)
    let tyenv = Context.typing_environment (context state) in
    let program'' = program tyenv program' in
    (* Debug.print ("program'': " ^ Sugartypes.show_program program''); *)
    return state program''

  let sentence state sentence' =
    let tyenv = Context.typing_environment (context state) in
    let sentence'' = sentence tyenv sentence' in
    return state sentence''
end
