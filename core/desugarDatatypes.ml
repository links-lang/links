(** Desugar datatypes converts types from their syntactic representation
   (defined in {!Sugartypes}) to the semantic one defined in {!Types}. It also
   handles various forms of syntactic sugar within types, and translates them
   appropriately.

   The desugar pass transforms types in several passes:

   1. For every top-level node (either a binder or expression), we gather all
      free type variables, and determine their subkind (and kind if
      [infer_kinds] is enabled). From here, we build up a mapping of type
      variables to a corresponding {!meta_var}.

   This is done by {!tyvars} and {!Desugar.generate_var_mapping}.

   2. We then traverse the body of this node with these new type variables in
      scope. For each type signature, we apply several transformations:

      First ensure the effect type has the appropriate form, using
      {!Datatype.cleanup_effects}:

      1. Elaborate operators, converting the various short forms [{Op:Int}],
         [{Op:()->Int}] into the "correct" [{Op:() {}-> Int}] form.

      2. {!Datatype.remap_anon_effects}: Convert unnamed effect variables into
         the appropriate "$eff" (the shared effect variable) or the generic "$"
         one.

      While these passes {i could} potentially be done together with later ones,
      this initial transform makes it easier to do more complex analysis of effect
      rows.

      3. {!Datatype.gather_operations}: If the effect sugar is enabled, we now
         attempt to do a naive form of row kind inference for effect rows. This
         finds which operations are used on each effect row, and builds up a map
         of operation->presence variables for that row.

      4. {!Desugar.datatype}: We now finally convert the syntactic type to the
         semantic one. This goes largely as you would expect:
          - Named fresh variables are looked up in the environment.
          - Unnamed type variables ("$") are mapped to fresh ones.
          - The shared effect variable ("$eff") is mapped to the appropriate one.
          - When visiting an effect row, we insert presence variables according
            to the previously determined operation map. *)

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


type kinded_type_variable = Name.t * Sugartypes.kind * Freedom.t


let infer_kinds = DesugarTypeVariables.infer_kinds

let has_effect_sugar () = Settings.get Types.effect_sugar

(*
let typevar_primary_kind_mismatch pos var ~expected ~actual =
  Type_error
    ( pos,
      Printf.sprintf "Mismatch in kind for type variable `%s'.\n" var
      ^ Printf.sprintf "  This has kind `%s', but we require a `%s' here."
          (CommonTypes.PrimaryKind.to_string actual)
          (CommonTypes.PrimaryKind.to_string expected) )

*)

let typevar_mismatch pos (v1 : kinded_type_variable) (v2 : kinded_type_variable) =
  let var, _, _ = v1 in
  Type_error
    ( pos,
      Printf.sprintf "Mismatch in kind for type variable `%s'.\n" var
      ^ Printf.sprintf "  Declared as `%s' and `%s'."
          ("fixme")
          ("fixme") )

(*let duplicate_var pos var =
  Type_error (pos, Printf.sprintf "Multiple definitions of type variable `%s'." var)
*)

(** Construct an error for when a type alias has free variables within it. *)
let free_type_variable ?var pos =
  match var with
  | None -> Type_error (pos, "Unbound anonymous type variable.")
  | Some name -> Type_error (pos, Printf.sprintf "Unbound type variable `%s'." name)

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

  method! type_arg' = function
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

let is_anon stv =
  let (name, _, _) = SugarTypeVar.get_unresolved_exn stv in
  name.[0] = '$'

(** Ensure this variable has some kind, if {!infer_kinds} is disabled. *)
let ensure_kinded = function
  | name, (None, subkind), freedom when not (Settings.get infer_kinds) ->
      (name, (Some pk_type, subkind), freedom)
  | v -> v



(* Name used to indicate that a certain (originally anonymous) row variable
   should be replaced by a shared row variabled later on. *)
let sharable_effect_var_name = "$eff"



(* A map with SugarTypeVar as keys, use for associating the former
   with information about what

*)

module type ROW_VAR_MAP =
sig

  type key = SugarTypeVar.t
  type 'a t

  val empty : 'a t

  val add : key -> 'a -> 'a t -> 'a t
  val find_opt : key -> 'a t -> 'a option
  val map : ('a -> 'b) -> 'a t -> 'b t





  (* Predicate telling you if a given sugar variable should/can be
     handled by this map *)
  val is_relevant : SugarTypeVar.t -> bool

  (* like remove, but remove an entry by using the int id in the quantifier *)
  val remove_by_quantifier : SugarQuantifier.t -> 'a t -> 'a t
  val update_by_quantifier : SugarQuantifier.t -> ('a option -> 'a option) -> 'a t -> 'a t
  val find_opt_by_quantifier : SugarQuantifier.t -> 'a t -> 'a option

end

module RowVarMap : ROW_VAR_MAP =
struct

  type key = SugarTypeVar.t

  (* internal representation, hidden *)
  type 'a t = 'a IntMap.t

  let is_relevant : SugarTypeVar.t -> bool =
    let open SugarTypeVar in function
    | TUnresolved (name, _, _) when name <> sharable_effect_var_name -> false
    | _ -> true


  (* helpers *)
  let unpack_var_id = function
          | `Var (id, _, _) -> id
          | _ -> raise (internal_error "Every meta_*_var in a SugarTypeVar must be a `Var at this point")

  let get_var = let open SugarTypeVar in function
        | TUnresolved       (name, _, _) when name = sharable_effect_var_name ->
           (* magic number specially used for $eff *)
           -1
        | TUnresolved       (_, _, _) ->
           raise (internal_error ("must only use SugarTypeVarMap on resoled SugarTypeVars OR the special unresolved one
                                  named " ^ sharable_effect_var_name))
        | TResolvedType     mtv ->
           unpack_var_id (Unionfind.find mtv)
        | TResolvedRow      mrv ->
           unpack_var_id (Unionfind.find mrv)
        | TResolvedPresence mpv ->
           unpack_var_id (Unionfind.find mpv)

  let var_id_from_quantifier =
    let open SugarQuantifier in
    function
      | QResolved (var, _) -> var
      | QUnresolved (_, _, _) -> raise
          (internal_error "must not call *_by_quantifier functions on unresolved quantifiers")

  (* functions using SugarTypeVar.t as key *)
  let find_opt : key -> 'a t -> 'a option = fun k m ->
    let var = get_var k in
    IntMap.find_opt var m

  let add : key -> 'a -> 'a t -> 'a t = fun k v m ->
    let var = get_var k in
    IntMap.add var v m

  let empty = IntMap.empty

  let map : ('a -> 'b) -> 'a t -> 'b t = fun f m ->
    IntMap.map f m


  (* functions using SugarQuantifier.t as key *)
  let update_by_quantifier : SugarQuantifier.t -> ('a option -> 'a option) -> 'a t -> 'a t = fun q f m ->
    let var = var_id_from_quantifier q in
    IntMap.update var f m

  let find_opt_by_quantifier : SugarQuantifier.t -> 'a t -> 'a option = fun q m ->
    let var = var_id_from_quantifier q in
    IntMap.find_opt var m

  let remove_by_quantifier q map =
    let var = var_id_from_quantifier q in
    IntMap.remove var map


end



(** Find all unbound named type variables in a term *)
let typevars =
object (self)
  inherit SugarTraversals.fold as super

  val tyvar_list : Name.t list = []
  val tyvars : kinded_type_variable StringMap.t = StringMap.empty

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

  method! type_variable _x = self

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

  method quantified action (qs : SugarQuantifier.t list) =
    let o = List.fold_left (fun o sq ->
      let q =
         ensure_kinded (rigidify (SugarQuantifier.get_unresolved_exn sq)) in
      o#bind (rigidify q)) self qs
    in
    let o = action o in
    List.fold_left
      (fun o sq ->
        o#replace (SugarQuantifier.get_unresolved_name_exn sq) tyvars)
      o
      qs

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
    | TypeVar stv when (not (SugarTypeVar.is_resolved stv)) && is_anon stv -> self
    | TypeVar stv when (not (SugarTypeVar.is_resolved stv)) ->
       let (x, k, freedom) = SugarTypeVar.get_unresolved_exn stv in
       self#add (x, (Some pk_type, k), freedom)
    | Mu (v, t)       ->
       let sq = SugarQuantifier.mk_unresolved v (Some pk_type, None) `Rigid in
       self#quantified (fun o -> o#datatype t) [sq]
    | Forall (qs, t)  -> self#quantified (fun o -> o#datatype t) qs
    | dt -> super#datatypenode dt

  method! row_var = let open Datatype in function
    | Closed               -> self
    | Open stv when (not (SugarTypeVar.is_resolved stv)) && is_anon stv -> self
    | Open stv when (not (SugarTypeVar.is_resolved stv))->
       let (x, k, freedom) = SugarTypeVar.get_unresolved_exn stv in
       self#add (x, (Some pk_row, k), freedom)
    | Recursive (s, r)     ->
       let sq = SugarQuantifier.mk_unresolved s (Some pk_row, None) `Rigid in
       self#quantified (fun o -> o#row r) [sq]
    | Open _ -> self

  method! fieldspec = let open Datatype in function
    | Absent -> self
    | Present t -> self#datatype t
    | Var stv when (not (SugarTypeVar.is_resolved stv)) && is_anon stv -> self
    | Var stv when (not (SugarTypeVar.is_resolved stv))->
       let (x, k, freedom) = SugarTypeVar.get_unresolved_exn stv in
       self#add (x, (Some pk_presence, k), freedom)
    | Var _ -> self
end

type var_env = {
    tyvars : meta_var StringMap.t; (** The currently in-scope type variables. *)
    shared_effect : meta_row_var Lazy.t option; (** The active shared effect variable, if set. *)
    row_operations : meta_presence_var Lazy.t StringMap.t RowVarMap.t;
    (** Map of effect variables to all mentioned operations, and their
       corresponding effect variables. *)
    allow_fresh : bool; (** Whether to allow creating anonymous type variables. *)
  }

let empty_env allow_fresh = {
    tyvars = StringMap.empty;
    shared_effect = None;
    row_operations = RowVarMap.empty;
    allow_fresh
  }

let closed_env = empty_env false

(*
let kind_error pos var expected = function
  | Some d ->
      let actual =
        match d with
        | `Type _ -> PrimaryKind.Type
        | `Row _ -> PrimaryKind.Row
        | `Presence _ -> PrimaryKind.Presence
      in
      typevar_primary_kind_mismatch pos var ~expected ~actual
  | None -> free_type_variable ~var pos

*)

(*
let lookup_tvar pos t { tyvars; _ } =
  match StringMap.find_opt t tyvars with
  | Some (`Type v) -> v
  | x -> raise (kind_error pos t PrimaryKind.Type x)

let lookup_rvar pos t { tyvars; _ } =
  match StringMap.find_opt t tyvars with
  | Some (`Row v) -> v
  | x -> raise (kind_error pos t PrimaryKind.Row x)

let lookup_pvar pos t { tyvars; _ } =
  match StringMap.find_opt t tyvars with
  | Some (`Presence v) -> v
  | x -> raise (kind_error pos t PrimaryKind.Presence x)

*)

module Desugar = struct
   (** Whether this type may have an shared effect variable appear within it.

      We insert the shared effect variable at the right most candidate on any
      function. This will either be the arrow itself, or a type application to a
      type which accepts an effect in the last place. *)
   let may_have_shared_eff (alias_env : Types.tycon_environment) { node; _ } =
     let open Datatype in
     match node with
     | Function _ | Lolli _ -> true
     | TypeApplication (tycon, _) ->
        begin match SEnv.find_opt tycon alias_env with
        | Some (`Alias (qs, _) | (`Mutual (qs, _))) ->
           begin match ListUtils.last_opt qs with
           | Some (_, (PrimaryKind.Row, (_, Restriction.Effect))) -> true
           | _ -> false
           end
        | _ -> false
        end
     | _ -> false

  (** Perform some initial desugaring of effect rows, to make them more amenable
     to later analysis.

     - Elaborate operations in effect rows, converting from the various sugared
       forms to the canonical one.

     - Remap anonymous effect variables to the correct version.
      - If effect sugar is disabled, all anonymous effect variables become "$".

      - If we've an unnamed effect in a non-tail position (i.e. not the far
        right of an arrow/typename chain) then remap to "$". For instance,
        `(a) -> (b) -> c` becomes `(a) -$-> (b) -$-> c`.

      - If we're an anonymous variable in a row, remap to "$". (For instance,
        ` -_->` becomes `-$eff->`. *)
  let cleanup_effects alias_env =
    let has_effect_sugar = has_effect_sugar () in
    (object(self)
       inherit SugarTraversals.map as super

       method! datatypenode =
         let open Datatype in
         let do_fun a e r =
            let a = self#list (fun o -> o#datatype) a in
            let has_shared = may_have_shared_eff alias_env r in
            let e = self#effect_row ~allow_shared:(not has_shared) e in
            let r = self#datatype r in
            (a, e, r)
         in
         function
         | Function (a, e, r) -> let a, e, r = do_fun a e r in Function (a, e, r)
         | Lolli (a, e, r) -> let a, e, r = do_fun a e r in Lolli (a, e, r)
         | TypeApplication (name, ts) ->
            let tycon = SEnv.find_opt name alias_env in
            let rec go =
               (* We don't know if the arities match up yet (nor the final arities
                  of the definitions), so we handle mismatches, assuming spare rows
                  are effects. *)
              function
              | _, [] -> []
              | [], Row t :: ts -> Row (self#effect_row ~allow_shared:false t) :: go ([], ts)
              | (PrimaryKind.Row, (_, Restriction.Effect)) :: qs, Row t :: ts ->
                 Row (self#effect_row ~allow_shared:false t) :: go (qs, ts)
              | ([] as qs | _ :: qs), t :: ts -> self#type_arg t  :: go (qs, ts)
            in
            begin match tycon with
            | Some (`Alias (qs, _)) | Some (`Mutual (qs, _)) ->
               TypeApplication (name, go (List.map snd qs, ts))
            | Some (`Abstract ty) ->
               TypeApplication (name, go (Abstype.arity ty, ts))
            | _ -> TypeApplication (name, self#list (fun o -> o#type_arg) ts)
            end
         | t -> super#datatypenode t

       method! type_variable x = x

       method effect_row ~allow_shared (fields, var) =
         let open Datatype in
         let fields =
           List.map (function
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
               | name, Present node when not (TypeUtils.is_builtin_effect name) ->
                  (* Elaborates `Op : a' to `Op : () {}-> a' *)
                  name, Present (WithPos.make ~pos:node.pos (Function ([], ([], Closed), node)))
               | x -> x)
             fields
         in

         let gue = SugarTypeVar.get_unresolved_exn in
         let var = match var with
           | Datatype.Open stv
                when (not allow_shared || not has_effect_sugar)
                     && (not (SugarTypeVar.is_resolved stv))
                     && SugarTypeVar.get_unresolved_name_exn stv = "$eff" ->
              let (_, sk, fr) = gue stv in
              let stv' = SugarTypeVar.mk_unresolved "$" sk fr in
              Datatype.Open stv'
           | Datatype.Open stv when has_effect_sugar
                                    && (not (SugarTypeVar.is_resolved stv))
                                    && gue stv = ("$", None, `Rigid) ->
              let stv' = SugarTypeVar.mk_unresolved "$eff" None `Rigid in
              Datatype.Open stv'
           | _ -> var
         in
         self#row (fields, var)
     end)#datatype

  (** Desugars quantifiers into Quantifier.ts, returning the updated
     variable environment.

     This is used within the `typename` and `Forall` desugaring. *)
  let desugar_quantifiers (var_env: var_env) (sqs: SugarQuantifier.t list) _body _pos :
      (Quantifier.t list * var_env) =
    (* Bind all quantified variables, and then do a naive {!typevars} pass over this set to infer
       any unannotated kinds, and verify existing kinds/subkinds match up.

       Also verify no duplicate variables exist. *)
    (*let tvs, _ = List.fold_left
      (fun (o, names) sq ->
        let ((name, _, _) as v) = SugarQuantifier.get_unresolved_exn sq in
        if StringSet.mem name names then raise (duplicate_var pos name);
        (o#bind (ensure_kinded v), StringSet.add name names)) (typevars, StringSet.empty) qs in
    let tvs = (tvs#datatype body)#tyvars in
    let qs =
      List.map
        (fun sq -> StringMap.find (SugarQuantifier.get_unresolved_name_exn sq) tvs)
        qs
    in

    ListLabels.fold_right ~init:([], var_env) qs
      ~f:(fun (name, (primarykind, subkind), _freedom)
              (args, env) ->
            let var = Types.fresh_raw_variable () in
            let subkind = concrete_subkind subkind in
            let open PrimaryKind in
            let quant, def =
              match primarykind with
              | Some Type ->
                  let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
                  ((var, (Type, subkind)), `Type point)
              | Some Row ->
                  let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
                  ((var, (Row, subkind)), `Row point)
              | Some Presence ->
                  let point = Unionfind.fresh (`Var (var, subkind, `Rigid)) in
                  ((var, (Presence, subkind)), `Presence point)
              | None -> raise (internal_error "Undesugared kind")
            in
            (quant :: args,
             { env with
               tyvars = StringMap.add name def env.tyvars;
               row_operations = StringMap.remove name env.row_operations }) )
     *)
    let extract_resolved_quantifier sq (qs, var_env) =
      let q = SugarQuantifier.get_resolved_exn sq in
      let new_env =
      { var_env with
        row_operations = RowVarMap.remove_by_quantifier sq var_env.row_operations }
      in
      (q :: qs), new_env
    in
    List.fold_right extract_resolved_quantifier sqs ([], var_env)

   let make_anon_point var_env pos sk freedom =
     if not var_env.allow_fresh then raise (free_type_variable pos);
     let var = Types.fresh_raw_variable () in
     Unionfind.fresh (`Var (var, concrete_subkind sk, freedom))

   (** Gathers some information about type names, used for later analysis. *)
   let gather_mutual_info (alias_env : Types.tycon_environment) =
     (object(o)
        inherit SugarTraversals.fold as super

        val has_implicit = false
        val mutuals = StringSet.empty

        (** Determine if this type contains the shared effect variable ("$eff").

            In order for this to be accurate, it should be run after
           {!cleanup_effects} - otherwise we may have superfluous "$eff"s. *)
        method has_implicit = has_implicit

        (** Any mutually-defined types that this typename consumes. This is used
           to propagate implicit effectiness and linearity of definitions. *)
        method mutuals = mutuals

        method with_implicit = {<has_implicit = true>}
        method with_mutual ty = {<mutuals = StringSet.add ty mutuals>}

        method! type_variable _x = o

        method! datatypenode dt =
          let open Datatype in
          let self = super#datatypenode dt in
          match dt with
          | Function (_, (_, eff_var), _) | Lolli (_, (_, eff_var), _) ->
             begin match eff_var with
             | Datatype.Open stv
                  when SugarTypeVar.get_unresolved_exn stv = ("$eff", None, `Rigid) ->
                self#with_implicit
             | _ -> self
             end
          | TypeApplication (name, ts) ->
             let tycon = SEnv.find_opt name alias_env in
             let self =
               match tycon with
               | Some (`Alias (qs, _)) | Some (`Mutual (qs, _))
                    when List.length qs = List.length ts + 1 ->
                  begin match ListUtils.last qs with
                  | _, (PrimaryKind.Row, (_, Restriction.Effect)) -> self#with_implicit
                  | _ -> self
                  end
               | _ -> self
             in
             let self = match tycon with
               | Some (`Mutual _) -> self#with_mutual name
               | _ -> self
             in
             self
          | _ -> self
     end)#datatype

   (** Gather information about which operations are used with which row
      variables. *)
   let gather_operations (alias_env : Types.tycon_environment) var_env dt =
     let o =
       object(self)
         inherit SugarTraversals.fold as super

         val operations = RowVarMap.empty
         method operations = operations

         method replace quantifier map =
           let ubq = RowVarMap.update_by_quantifier in
           let fobq = RowVarMap.find_opt_by_quantifier in
           {<operations = ubq quantifier (fun _ -> fobq quantifier map) operations>}

         method quantified action (qs : SugarQuantifier.t list) =
           let mask_operations =
             List.fold_left
               (fun o sq -> RowVarMap.remove_by_quantifier sq o)
               operations
               qs
           in
           let o = action {<operations = mask_operations>} in
           List.fold_left
             (fun o sq -> o#replace sq operations)
             o
             qs

         method add (var : SugarTypeVar.t) op =
           if TypeUtils.is_builtin_effect op || not (RowVarMap.is_relevant var) then
             self
           else
             let ops =
               match RowVarMap.find_opt var operations with
               | None -> StringSet.singleton op
               | Some t -> StringSet.add op t
             in
             {<operations = RowVarMap.add var ops operations>}

         method! datatypenode =
           let open Datatype in
           function
           | Function (a, e, t) | Lolli (a, e, t) ->
              let o = self#list (fun o -> o#datatype) a in
              let o = o#effect_row e in
              let o = o#datatype t in
              o
           | TypeApplication (name, ts) ->
              let tycon = SEnv.find_opt name alias_env in
                 let rec go o =
                   (* We don't know if the arities match up yet, so we handle
                      mismatches, assuming spare rows are effects. *)

                   function
                   | _, [] -> o
                   | (PrimaryKind.Row, (_, Restriction.Effect)) :: qs, Row t :: ts ->
                      go (o#effect_row t) (qs, ts)
                   | ([] as qs | _ :: qs), t :: ts -> go (o#type_arg t) (qs, ts)
                 in
              begin match tycon with
              | Some (`Alias (qs, _)) | Some (`Mutual (qs, _)) ->
                 go self (List.map snd qs, ts)
              | Some (`Abstract ty) -> go self (Abstype.arity ty, ts)
              | _ -> self#list (fun o -> o#type_arg) ts
              end

           | Mu (v, t) ->
              let sq = SugarQuantifier.mk_unresolved v (Some pk_type, None) `Rigid in
              self#quantified (fun o -> o#datatype t) [sq]
           | Forall (qs, t) -> self#quantified (fun o -> o#datatype t) qs
           | dt -> super#datatypenode dt

         method! row_var =
           let open Datatype in
           function
           | Closed | Open _ -> self
           | Recursive (s, r) ->
              let sq = SugarQuantifier.mk_unresolved s (Some pk_row, None) `Rigid in
              self#quantified (fun o -> o#row r) [sq]

         method effect_row ((fields, var) : Datatype.row) =
           let self =
             match var with
             | Datatype.Open stv ->

                List.fold_left (fun o (op, _) -> o#add stv op) self fields
             | _ -> self
           in
           self#row (fields, var)
       end
     in
     if var_env.allow_fresh && has_effect_sugar () then
       (o#datatype dt)#operations
       |> RowVarMap.map (fun v ->
              StringSet.fold
                (fun op m ->
                  let point =
                    lazy begin
                        let var = Types.fresh_raw_variable () in
                        Unionfind.fresh (`Var (var, default_subkind, `Rigid))
                      end
                  in
                  StringMap.add op point m) v StringMap.empty)
     else
       RowVarMap.empty

  let rec datatype var_env (alias_env : Types.tycon_environment) t' =
    let datatype var_env t' = datatype var_env alias_env t' in
    match t' with
    | { node = t; pos } ->
      let open Datatype in
      match t with
        (*| TypeVar stv when is_anon stv ->
           let (_name, sk, freedom) = SugarTypeVar.get_unresolved_exn stv in
           `MetaTypeVar (make_anon_point var_env pos sk freedom)
        | TypeVar stv ->
           let s = SugarTypeVar.get_unresolved_name_exn stv in
           `MetaTypeVar (lookup_tvar pos s var_env) *)
        | TypeVar stv ->
           let point = SugarTypeVar.get_resolved_type_exn stv in
           `MetaTypeVar point
        | QualifiedTypeApplication _ -> assert false (* will have been erased *)
        | Function (f, e, t) ->
            `Function (Types.make_tuple_type (List.map (datatype var_env) f),
                      effect_row var_env alias_env e t',
                      datatype var_env t)
        | Lolli (f, e, t) ->
            `Lolli (Types.make_tuple_type (List.map (datatype var_env) f),
                    effect_row var_env alias_env e t',
                    datatype var_env t)
        | Mu (name, t) ->
            let var = Types.fresh_raw_variable () in
            (* FIXME: shouldn't we support other subkinds for recursive types? *)
            let point = Unionfind.fresh (`Var (var, default_subkind, `Flexible)) in
            let tyvars = StringMap.add name (`Type point) var_env.tyvars in
            (* TODO: this removal is superfluous *)
            (* let row_operations = IntMap.remove var var_env.row_operations in *)
            let _ = Unionfind.change point (`Recursive (var, datatype { var_env with tyvars} t)) in
              `MetaTypeVar point
        | Forall (qs, t) ->
            let (qs: Quantifier.t list), var_env = desugar_quantifiers var_env qs t pos in
            let t = datatype var_env t in
              `ForAll (qs, t)
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
                    (TypeApplicationKindMismatch
                       { pos;
                         name = tycon;
                         tyarg_number = i;
                         expected = PrimaryKind.to_string q_kind;
                         provided = PrimaryKind.to_string t_kind
                       })
                else (q, t)
              in
              let type_args var_env qs ts =
                List.combine qs ts
                |> List.mapi
                     (fun i (q,t) ->
                       let (q, t) = match_kinds i (q, t) in
                       let _, q_sk = proj q in
                       match t, q_sk with
                       | Row r, (_, Restriction.Effect) -> `Row (effect_row var_env alias_env r t')
                       | _ -> type_arg var_env alias_env t t')
              in

              let qn = List.length qs and tn = List.length ts in
              let arity_err () =
                raise (TypeApplicationArityMismatch { pos; name = tycon; expected = qn; provided = tn })
              in
              if qn = tn + 1 then
                let qs, q = ListUtils.unsnoc qs in
                match proj q , var_env.shared_effect with
                | (PrimaryKind.Row, (_, Restriction.Effect)), Some eff ->
                   (* If we've got a typename with an effect variable as the
                      last argument, and we're not applying it fully, then add
                      the implicit effect var. *)

                   (* Looking for this gives us the operations associcated with
                      the $eff var. The kind and freedom info are ignored *)
                   let eff_sugar_var = SugarTypeVar.mk_unresolved sharable_effect_var_name None `Rigid in
                   let fields = match RowVarMap.find_opt eff_sugar_var var_env.row_operations with
                     | None -> StringMap.empty
                     | Some ops -> StringMap.fold (fun op p -> StringMap.add op (`Var (Lazy.force p))) ops StringMap.empty
                   in
                   let eff = (fields, Lazy.force eff, false) in
                   type_args var_env qs ts @ [ `Row eff ]
                | _ -> arity_err ()
              else if qn = tn then
                type_args var_env qs ts
              else
                arity_err ()
            in
            begin match SEnv.find_opt tycon alias_env with
              | None -> raise (UnboundTyCon (pos, tycon))
              | Some (`Alias (qs, _dt)) ->
                  let ts = match_quantifiers snd qs in
                  Instantiate.alias tycon ts alias_env
              | Some (`Abstract abstype) ->
                  let ts = match_quantifiers identity (Abstype.arity abstype) in
                  `Application (abstype, ts)
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
                  `RecursiveApplication
                    { r_name = tycon;
                      r_dual = false;
                      r_unique_name;
                      r_quantifiers = List.map snd qs;
                      r_args; r_unwind; r_linear
                    }
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

  and fieldspec var_env alias_env fs _ =
    let open Datatype in
    match fs with
    | Absent -> `Absent
    | Present t -> `Present (datatype var_env alias_env t)
    (* | Var stv when is_anon stv ->
     *    let (_name, sk, freedom) = SugarTypeVar.get_unresolved_exn stv in
     *    `Var (make_anon_point var_env pos sk freedom) *)
    | Var spv ->
       let resolved_pv = SugarTypeVar.get_resolved_presence_exn spv in
       `Var resolved_pv

  and row var_env alias_env (fields, rv) (node : 'a WithPos.t) =
    let seed =
      let open Datatype in
      match rv with
        | Closed -> Types.make_empty_closed_row ()
        | Open stv when
               (not (SugarTypeVar.is_resolved stv))
               && SugarTypeVar.get_unresolved_name_exn stv = "$eff" ->
           let eff = match var_env.shared_effect with
             | None -> raise (internal_error "Needed shared effect, but not given one.")
             | Some s -> Lazy.force s
           in
           (StringMap.empty, eff, false)
        | Open stv when (not (SugarTypeVar.is_resolved stv)) && is_anon stv ->
           let (_name, sk, freedom) = SugarTypeVar.get_unresolved_exn stv in
           (StringMap.empty, make_anon_point var_env node.pos sk freedom, false)
        | Open srv ->
           let rv = SugarTypeVar.get_resolved_row_exn srv in
           (StringMap.empty, rv, false)
        | Recursive (name, r) ->
            let var = Types.fresh_raw_variable () in
            let point = Unionfind.fresh (`Var (var, default_subkind, `Flexible)) in
            let tyvars = StringMap.add name (`Row point) var_env.tyvars in
            (* let row_operations = StringMap.remove name var_env.row_operations in *)
            let _ = Unionfind.change point (`Recursive (var, row { var_env with tyvars} alias_env r node)) in
            (StringMap.empty, point, false)
    in
    let fields = List.map (fun (k, p) -> (k, fieldspec var_env alias_env p node)) fields in
    fold_right Types.row_with fields seed

  and effect_row var_env alias_env (fields, rv) node =
    let (fields, rho, dual) = row var_env alias_env (fields, rv) node in
    let fields =
      match rv with
      | Datatype.Open stv when RowVarMap.is_relevant stv ->
         begin match RowVarMap.find_opt stv var_env.row_operations with
         | Some ops ->
            let ops = StringMap.fold (fun k _ -> StringMap.remove k) fields ops in
            let fields = StringMap.fold (fun op p -> StringMap.add op (`Var (Lazy.force p))) ops fields
            in fields
         | None -> fields
         end
      | _ -> fields
    in
    (fields, rho, dual)

  and type_arg var_env alias_env ta node =
    let open Datatype in
    match ta with
    | Type t -> `Type (datatype var_env alias_env t)
    | Row r -> `Row (row var_env alias_env r node)
    | Presence f -> `Presence (fieldspec var_env alias_env f node)

  (* pre condition: all subkinds have been filled in *)
  let generate_var_mapping (vars : kinded_type_variable list) : Quantifier.t list * var_env =
    let addt x t envs = { envs with tyvars = StringMap.add x (`Type t) envs.tyvars } in
    let addr x r envs = { envs with tyvars = StringMap.add x (`Row r) envs.tyvars } in
    let addf x f envs = { envs with tyvars = StringMap.add x (`Presence f) envs.tyvars } in
    let vars, var_env =
      List.fold_left
        (fun (vars, envs) ->
           let var = Types.fresh_raw_variable () in
             fun (x, kind, freedom) ->
             let open PrimaryKind in
             match (kind, freedom) with
             | (Some Type, Some subkind), freedom ->
               let t = Unionfind.fresh (`Var (var, subkind, freedom)) in
                 (var, (Type, subkind))::vars, addt x t envs
             | (Some Row, Some subkind), freedom ->
               let r = Unionfind.fresh (`Var (var, subkind, freedom)) in
                 (var, (Row, subkind))::vars, addr x r envs
             | (Some Presence, Some subkind), freedom ->
               let f = Unionfind.fresh (`Var (var, subkind, freedom)) in
                 (var, (Presence, subkind))::vars, addf x f envs
             | (_, None), _ | (None, _), _ ->
               (* Shouldn't occur; we are assuming that all subkinds have been
                * filled in*)
               assert false)
        ([], empty_env true)
        vars
    in
      List.rev vars, var_env

  let datatype var_env alias_env dt =
    let dt = cleanup_effects alias_env dt in
    let var_env = { var_env with row_operations = gather_operations alias_env var_env dt } in
    let var_env = match var_env.shared_effect with
      | None when var_env.allow_fresh && has_effect_sugar () ->
        let point =
          lazy begin
              let var = Types.fresh_raw_variable () in
              Unionfind.fresh (`Var (var, (lin_unl, res_any), `Rigid))
            end
        in { var_env with shared_effect = Some point }
      | _ -> var_env
    in
    datatype var_env alias_env dt

  let datatype' map alias_env ((dt, _) : datatype') =
    (dt, Some (datatype map alias_env dt))

  let type_arg' map alias_env ((ta, _) : type_arg') : type_arg' =
    let unlocated = WithPos.make Datatype.Unit in
    (ta, Some (type_arg map alias_env ta unlocated))

  (* Desugar a foreign function declaration. Foreign declarations cannot use type variables from
     the context. Any type variables found are implicitly universally quantified at this point. *)
  let foreign alias_env dt =
    let tvars = (typevars#datatype' dt)#tyvar_list in
    datatype' (snd (generate_var_mapping tvars)) alias_env dt

  (* Desugar a table literal. No free variables are allowed here. We generate both read and write
     types by looking for readonly constraints *)
  let table_lit alias_env constraints dt =
    let read_type =
      match datatype' closed_env alias_env (dt, None) with
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

(** Convert a syntactic type into a semantic type, using `map' to resolve free type variables *)
let desugar initial_alias_env map =
object (self)
  inherit SugarTraversals.fold_map as super

  val alias_env = initial_alias_env

  method! datatype' node = (self, Desugar.datatype' map alias_env node)

  method! type_arg' node = (self, Desugar.type_arg' map alias_env node)

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
        let read, write, needed = Desugar.table_lit alias_env cs dt in
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
        let (mutual_env, venvs_map, ts) =
          List.fold_left (fun (alias_env, venvs_map, ts) {node=(t, args, (d, _)); pos} ->
            let args = List.map fst args in
            let qs, var_env =  Desugar.desugar_quantifiers closed_env args d pos in
            let alias_env = SEnv.bind t (`Mutual (qs, tygroup_ref)) alias_env in
            let venvs_map = StringMap.add t var_env venvs_map in
            let args = List.map2 (fun x y -> (x, Some y)) args qs in
            (alias_env, venvs_map, WithPos.make ~pos (t, args, (d, None)) :: ts))
            (alias_env, venvs_map, []) ts in

        (* First gather any types which require an implicit effect variable. *)
        let (implicits, dep_graph) =
          List.fold_left (fun (implicits, dep_graph) {node=(t, _, (d, _)); _} ->
              let d = Desugar.cleanup_effects mutual_env d in
              let eff = Desugar.gather_mutual_info mutual_env d in
              let has_imp = eff#has_implicit in
              let implicits = StringMap.add t has_imp implicits in
              let dep_graph = StringMap.add t (StringSet.elements eff#mutuals) dep_graph in
              implicits, dep_graph)
            (StringMap.empty, StringMap.empty) ts
        in
        (* We also gather a dependency graph of our mutually recursive types. Group this into SCCs
           and toposort it. We can then trivially propagate the implicit effect requirement - if
           anyone in our component has an implicit effect, or depends on a type which does, then we
           must too! *)
        let has_implicit implicits name =
          StringMap.find name implicits
          || List.exists (flip StringMap.find implicits) (StringMap.find name dep_graph)
        in
        let sorted_graph = Graph.topo_sort_sccs (StringMap.bindings dep_graph) in
        let implicits =
          List.fold_left (fun implicits scc ->
              let scc_imp = List.exists (has_implicit implicits) scc in
              List.fold_left (fun acc x -> StringMap.add x scc_imp acc) implicits scc)
            implicits sorted_graph
        in
        (* Now patch up the types to include this effect variable. *)
        let (mutual_env, venvs_map, ts) =
          List.fold_left (fun (alias_env, venvs_map, ts) ({node=(t, args, (d, _)); pos} as tn) ->
              if StringMap.find t implicits then
                let var = Types.fresh_raw_variable () in
                let q = (var, (PrimaryKind.Row, (lin_unl, res_effect))) in
                (* Add the new quantifier to the argument list and rebind. *)
                let qs = List.map (snd ->- OptionUtils.val_of) args @ [q] in
                let args = args @ [(SugarQuantifier.mk_unresolved "<implicit effect>" (None, None) `Rigid), Some q] in
                let alias_env = SEnv.bind t (`Mutual (qs, tygroup_ref)) alias_env in
                (* Also augment the variable environment with this shared effect. *)
                let var_env = StringMap.find t venvs_map in
                let var_env = {
                    var_env with
                    shared_effect = Some (lazy (Unionfind.fresh (`Var (var, (lin_unl, res_effect), `Rigid)))) }
                in
                let venvs_map = StringMap.add t var_env venvs_map in
                (alias_env, venvs_map, WithPos.make ~pos (t, args, (d, None)) :: ts)
              else
                (alias_env, venvs_map, tn :: ts)) (mutual_env, venvs_map, []) ts
        in

        (* Desugar all DTs, given the temporary new alias environment. *)
        let desugared_mutuals =
          List.map (fun {node=(name, args, dt); pos} ->
            (* Desugar the datatype *)
            let var_env = StringMap.find name venvs_map in
            let dt' = Desugar.datatype' var_env mutual_env dt in
            (* Check if the datatype has actually been desugared *)
            let (t, dt) =
              match dt' with
               | (t, Some dt) -> (t, dt)
               | _ -> assert false in
            WithPos.make ~pos (name, args, (t, Some dt))
          ) ts in

        (* Given the desugared datatypes, we now need to handle linearity.
           First, calculate linearity up to recursive application *)
        let linearity_env =
          List.fold_left (fun lin_map {node=(name, _, (_, dt)); _} ->
            let dt = OptionUtils.val_of dt in
            let lin_map = StringMap.add name (not @@ Unl.type_satisfies dt) lin_map in
            lin_map) StringMap.empty desugared_mutuals in
        (* Next, use the toposorted dependency graph from above. We need to
           reverse since we propagate linearity information downwards from the
           SCCs which everything depends on, rather than upwards. *)
        let sorted_graph = List.rev sorted_graph in
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
          List.fold_left (fun alias_env {node=(t, args, (_, dt')); _} ->
            let dt = OptionUtils.val_of dt' in
            let semantic_qs = List.map (snd ->- val_of) args in
            let alias_env =
              SEnv.bind t (`Alias (List.map (snd ->- val_of) args, dt)) alias_env in
            tygroup_ref :=
              { !tygroup_ref with
                  type_map = (StringMap.add t (semantic_qs, dt) !tygroup_ref.type_map);
                  linearity_map };
            alias_env
        ) alias_env desugared_mutuals in

        ({< alias_env = alias_env >}, Typenames desugared_mutuals)
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
  let tvars = (typevars#phrase p)#tyvar_list in
    (desugar alias_env (snd (Desugar.generate_var_mapping tvars)))#phrase p

let binding alias_env ({ node; pos } as b : binding) =
  match node with
  | Funs bnds ->
      let bnds =
        List.map
          (fun bnd ->
            let tvars = (typevars#bindingnode (Funs [ bnd ]))#tyvar_list in
            (desugar alias_env (snd (Desugar.generate_var_mapping tvars)))#recursive_function bnd
            |> snd )
          bnds
      in
      (alias_env, WithPos.make ~pos (Funs bnds))
  | _ ->
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
  let _, var_env = Desugar.generate_var_mapping (typevars#datatype dt)#tyvar_list in
  let dt = DesugarTypeVariables.datatype dt in
  let _, ty = Generalise.generalise Env.String.empty (Desugar.datatype var_env aliases dt) in
  ty

module Untyped = struct
  open Transform'.Untyped

  let name = "datatypes"

  let program state program' =
    let tyenv = Context.typing_environment (context state) in
    let program'' = program tyenv program' in
    return state program''

  let sentence state sentence' =
    let tyenv = Context.typing_environment (context state) in
    let sentence'' = sentence tyenv sentence' in
    return state sentence''
end
