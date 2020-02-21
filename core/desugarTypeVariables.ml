(*

  1. Annotaions on binders are lifted to signatures: fun f(x : a) {x}
   is equivalent to sig f : a {%e}~> %b fun (x : a) {x}


  2. The scope of variables bound in type annotations on binders is
   the same as the scope of the bound variable itself: sig f : a -> a
   fun (x) { a is bound here }

  3. Type variables on expressions are in scope up to the next
   enclosing generalisation point (let binding or type abstraction),
   independently from whether or not we actually generalise there

  4. Flexible type variables: same behavior as rigid variables for now

  5. recursive functions: mut { fun f(x : a) {g(x)}

        fun g(x : a) {f(x)}

     }

    due to the binder-to-sig rule (1.), the two a are not the same
   here, but the functions are both exhibiting polymorphic recursion



   Note: This leaves anonymous row variables un-resolved, because the
   effect sugar pass takes care of those.


*)

open CommonTypes
open Utility
open Sugartypes




type kinded_type_variable = Name.t * Sugartypes.kind * Freedom.t [@@deriving show]




type tyvar_map_entry =
  | TVUnkinded of Subkind.t option * Freedom.t
  | TVType     of Subkind.t option * Types.meta_type_var     [@printer fun fmt _p -> fprintf fmt "<TResolvedType>"]
  | TVRow      of Subkind.t option * Types.meta_row_var      [@printer fun fmt _p -> fprintf fmt "<TResolvedRow>"]
  | TVPresence of Subkind.t option * Types.meta_presence_var [@printer fun fmt _p -> fprintf fmt "<TResolvedPresence>"]

type tyvar_map = tyvar_map_entry StringMap.t


(* FIXME: must remove from desugarDatatypes *)
let infer_kinds
  = Settings.(flag "infer_kinds"
              |> convert parse_bool
              |> sync)

let internal_error message =
  Errors.internal_error ~filename:"desugarTypeVariables.ml" ~message


(* Errors *)

let typevar_mismatch pos (v1 : kinded_type_variable) (v2 : kinded_type_variable) =
  let var, _, _ = v1 in
  Errors.Type_error
    ( pos,
      Printf.sprintf "Mismatch in kind for type variable `%s'.\n" var
      ^ Printf.sprintf "  Declared as `%s' and `%s'."
          (show_kinded_type_variable v1)
          (show_kinded_type_variable v2))

let duplicate_var pos var =
  Errors.Type_error (pos, Printf.sprintf "Multiple definitions of type variable `%s'." var)


let concrete_subkind =
  function
  | Some subkind -> subkind
  | None         -> default_subkind

let free_type_variable ?var pos =
  let desc = match var with
    | None -> "anonymous type variable"
    | Some name -> Printf.sprintf "type variable `%s'"  name
  in Errors.Type_error
       (pos,
        "Unbound " ^ desc ^ " in position where
        no free type variables are allowed")



let get_entry_var_info (entry : tyvar_map_entry ) :  (int * Subkind.t  * Freedom.t) option =
  let extract_data  =
    function
    | `Var (var, sk, freedom) -> var, sk, freedom
    | _ -> raise (internal_error "A this stage, all meta_*_var things must be a `Var ")
  in
  match entry with
  | TVUnkinded (_,_)    -> None
  | TVType (_, mtv)     -> Some (extract_data (Unionfind.find mtv))
  | TVRow  (_, mrv)     -> Some (extract_data (Unionfind.find mrv))
  | TVPresence (_, mpv) -> Some (extract_data (Unionfind.find mpv))




let lookup_tyvar_exn name (map : tyvar_map) : Sugartypes.kind * Freedom.t * tyvar_map_entry =
  let extract_freedom =
    (* Consistency check: If the tyvar map contains subkind info,
       then it must coindice with the subkind in the Unionfind point *)
    function
    | None,        `Var (_, _subkind,  freedom)                       -> freedom
    | Some map_sk, `Var (_,  subkind,  freedom) when map_sk = subkind -> freedom
    | Some map_sk, `Var (_,  subkind, _freedom) when map_sk <> subkind ->
       raise (internal_error "kind information in map and point diverged")
    | _ ->
       raise (internal_error "A this stage, all meta_*_var things must be a `Var")
  in
  let entry = StringMap.find name map in
  match entry with
  | TVUnkinded (sk, fd) -> (None, sk), fd, entry
  | TVType (sk, mtv) ->
     let fd = extract_freedom (sk, (Unionfind.find mtv)) in
     (Some PrimaryKind.Type, sk), fd, entry
  | TVRow (sk, mrv) ->
     let fd = extract_freedom (sk, (Unionfind.find mrv)) in
     (Some PrimaryKind.Row, sk), fd, entry
  | TVPresence (sk, mpv) ->
     let fd = extract_freedom (sk, (Unionfind.find mpv)) in
     (Some PrimaryKind.Presence, sk), fd, entry


(* taken from DesugarDatatypes.desugar_quantifiers
note that we fill primary kind and subkind info.
However, this is not the same as setting defaults. Instead,
the info in the map takes precedence.

*)
let make_opt_kinded_var sk_opt freedom : [> `Var of (int * Subkind.t * Freedom.t)] =
  let var = Types.fresh_raw_variable () in
  let sk = concrete_subkind sk_opt in
  `Var (var, sk, freedom)

let get_var_info (info : [> `Var of (int * Subkind.t * Freedom.t)]) =
  match info with
  | `Var (var, sk, fd) -> (var, sk, fd)
  | _ -> raise (internal_error "at this stage, all meta_*_var things must be `Var")



let make_fresh_entry pk_opt sk_opt freedom : tyvar_map_entry =
  let open PrimaryKind in
  match pk_opt with
    | None -> TVUnkinded (sk_opt, freedom)
    | Some Type ->
       let point = Unionfind.fresh (make_opt_kinded_var sk_opt freedom) in
       TVType (sk_opt, point)
    | Some Row ->
       let point = Unionfind.fresh (make_opt_kinded_var sk_opt freedom) in
       TVRow (sk_opt, point)
    | Some Presence ->
       let point = Unionfind.fresh (make_opt_kinded_var sk_opt freedom) in
       TVPresence (sk_opt, point)





(* does not do all sanity checks *)
(* Note that this always reuses existing points! *)
let update_entry pk sk_opt freedom existing_entry : tyvar_map_entry =
  let con_sk = concrete_subkind sk_opt in
  let open PrimaryKind in
  match pk, existing_entry with
    | _, TVUnkinded _ ->
       make_fresh_entry (Some pk) sk_opt freedom
    | Type, TVType (_, point) ->
       let (var, prev_sk, _) = get_var_info (Unionfind.find point) in
       (if con_sk <> prev_sk then Unionfind.change point (`Var (var, con_sk, freedom)));
       TVType (sk_opt, point)
    | Row, TVRow (_, point) ->
       let (var, prev_sk, _) = get_var_info (Unionfind.find point) in
       (if con_sk <> prev_sk then Unionfind.change point (`Var (var, con_sk, freedom)));
       TVRow (sk_opt, point)
    | Presence, TVPresence (_, point) ->
       let (var, prev_sk, _) = get_var_info (Unionfind.find point) in
       (if con_sk <> prev_sk then Unionfind.change point (`Var (var, con_sk, freedom)));
       TVPresence (sk_opt, point)
    | _ -> raise (internal_error "inconsistent kind information")


let resolved_var_of_entry =
  let open Sugartypes.SugarTypeVar in
  function
    | TVUnkinded _->
       raise
         (internal_error "tried to resolve variable without known primary kind")
    | TVType (_, point) -> TResolvedType point
    | TVRow (_, point) -> TResolvedRow point
    | TVPresence (_, point) -> TResolvedPresence point




(* let make_sugartyvar name pk sk freedom =
 *   let open PrimaryKind in
 *   let open SugarTypeVar in
 *   match pk with
 *     | None          -> TUnresolved (name, sk, freedom)
 *     | Some Type     -> TResolvedType (make_opt_kinded_point sk freedom)
 *     | Some Row      -> TResolvedRow (make_opt_kinded_point sk freedom)
 *     | Some Presence -> TResolvedPresence (make_opt_kinded_point sk freedom) *)



(* let create_updated_variable_info
 *       (existing_entry  : tyvar_map_entry opion)
 *       name
 *       (pk : PrimaryKind.t option)
 *       sk
 *       freedom
 *     : tyvar_map_entry * SugarTypeVar.t =
 *   let make_map_entry () =
 *     let open CommonTypes.PrimaryKind in
 *     match pk with
 *     | Type ->
 *        let point = Unionfind.fresh (make_opt_kinded_var sk freedom) in
 *
 *
 *
 *
 *   match existing_entry, pk with
 *     | Some _, None -> raise (internal_error "Cannot decrease PK information"
 *     |
 *
 *     | Some var_info ->
 *
 *   Unionfind.change point *)


let default_kind : PrimaryKind.t = PrimaryKind.Type
let default_subkind : Subkind.t = (lin_unl, res_any)



let is_anonymous_name name =
  name.[0] = '$'

let is_anonymous stv =
  let (name, _, _) = SugarTypeVar.get_unresolved_exn stv in
  name.[0] = '$'


(* let make_anon_point subkind freedom =
 *   let var = Types.fresh_raw_variable () in
 *   Unionfind.fresh (`Var (var, concrete_subkind subkind, freedom)) *)

(** Ensure this variable has some kind, if {!infer_kinds} is disabled. *)
let ensure_kinded = function
  | name, (None, subkind), freedom when not (Settings.get infer_kinds) ->
      (name, (Some pk_type, subkind), freedom)
  | v -> v






let visitor initial_map =
object (o : 'self)
  inherit SugarTraversals.fold_map as super

  val tyvar_map : tyvar_map = initial_map


  (* Are named unbound type variables okay in the current context?.
     This only affects rigid variables.
   *)
  val new_named_vars_allowed = true

  (* Are anonynmous (hence also unbound) type variables okay in the current context?
     This only affects rigid variables.
   *)
  val new_anon_vars_allowed = true


  (* part of legacy compatibility, remove later *)
  val at_toplevel = true

  method reset_vars =
    (* just let unbind do its sanity check on all variables *)
    StringMap.iter (fun var _ -> ignore (o#unbind var o)) tyvar_map;
    {< tyvar_map = StringMap.empty >}

  method get_vars = tyvar_map

  method set_toplevelness at_toplevel = {< at_toplevel >}

  method set_new_named_vars_allowed new_named_vars_allowed = {< new_named_vars_allowed >}
  method set_new_anon_vars_allowed new_anon_vars_allowed = {< new_anon_vars_allowed >}


  method bind
           name
           (entry: tyvar_map_entry)  =
    {< tyvar_map = StringMap.add name entry tyvar_map >}



    (* Return all information obtained about the given variable,
       possibly filling in default kinding info.
       Restore the info stored about the variable in a previous object *)
  method unbind name (restore_from : 'self) : 'self * (Kind.t * Freedom.t) * tyvar_map_entry =
    let (pk_opt, sk_opt), fd, cur_entry = lookup_tyvar_exn name tyvar_map in

    let pk = from_option default_kind pk_opt in
    let sk = from_option default_subkind sk_opt in

    let old_map = restore_from#get_vars in
    let o = match StringMap.lookup name old_map with
      | Some old_entry ->
         o#bind name old_entry
      | None ->
         {< tyvar_map = StringMap.remove name tyvar_map >}
    in
    o, ((pk, sk), fd), cur_entry





  (* used for type/row/presence variables found along the way, including anonymous ones *)
  method add ?pos name (pk : PrimaryKind.t) (sk : Subkind.t option) freedom : 'self * SugarTypeVar.t =
    let anon = is_anonymous_name name in
    let pos = OptionUtils.from_option SourceCode.Position.dummy pos in
    if not anon && StringMap.mem name tyvar_map then
      begin
        let (pk', sk'), freedom', existing_entry  = lookup_tyvar_exn name tyvar_map in
        let union = function
          | Some x, None | None, Some x -> Some x, Some x
          | x, y -> x, y
        in
        (* monotonically increase subkinding information *)
        let (sk_union, sk_union') = union (sk, sk') in
        let (pk_union, pk_union') = union (Some pk, pk') in
        let tv = (name, (pk_union, sk_union), freedom) in
        let tv' = (name, (pk_union', sk_union'), freedom') in
        (* check that new info is consistent with existing knowledge *)
        if tv <> tv' then
          raise (typevar_mismatch SourceCode.Position.dummy tv tv');

        (* Check if we learned anything new at all.
           If so, update the entry to put in the tyvar map. This also updates
           the *existing* unionfind point carried inside the entry *)
        let entry =
          if pk' <> pk_union' || sk' <> sk_union' then
            update_entry pk sk freedom existing_entry
          else
            existing_entry
        in

        let resolved_var = resolved_var_of_entry entry in
        o#bind name entry, resolved_var
      end
    else
      begin
        (if        (anon &&  (not new_anon_vars_allowed)  && freedom = `Rigid)
           || ((not anon) && (not new_named_vars_allowed) && freedom = `Rigid) then
           let name_opt = if anon then None else Some name in
           raise (free_type_variable ?var:name_opt pos));



        (* We create a new entry for the tyvar map.
           Since at this point we know the primary kind,
           this will always result in the creation of a unionfind point.
         *)
        let entry = make_fresh_entry (Some pk) sk freedom in
        let o =
          if anon then o else o#bind name entry in
        let resolved_var = resolved_var_of_entry entry in
        o, resolved_var
      end



  (* In many instances we don't have accurate position information when raising
     type errors. In such a case, we re-throw the error using the position
     information of the closest surrounding datatype with such position info *)
  method! datatype ty =
    let open Errors in
    let open SourceCode in
    try
      super#datatype ty
    with
      Type_error (pos', msg) when pos' = Position.dummy ->
      raise (Type_error (WithPos.pos ty, msg))


  (* Used for Forall and Typenames *)
  method quantified : 'a. SugarQuantifier.t list -> ('self -> 'self * 'a) -> 'self * SugarQuantifier.t list * 'a =
    fun unresolved_qs action ->
    let original_o = o in
    let bind_quantifier (o, names) sq =
      let (name, _, _) as v =
        SugarQuantifier.get_unresolved_exn sq in
      let pos = SourceCode.Position.dummy in
      if StringSet.mem name names then raise (duplicate_var pos name);
      let (_, (pk, sk), freedom) = ensure_kinded v in
      (* let point = make_opt_kinded_point sk `Rigid in *)
      let entry = make_fresh_entry pk sk freedom in
      let o' = o#bind name entry in
      let names' = StringSet.add name names in
      (o', names')
    in
    let unbind_quantifier
          (sq : SugarQuantifier.t)
          (o, (rqs : SugarQuantifier.t list)) =
      let name = SugarQuantifier.get_unresolved_name_exn sq in
      let o, ((pk, sk), _freedom), entry = o#unbind name original_o in
      let var = match get_entry_var_info entry with
          | Some (var, _, _) -> var
          | None -> raise (internal_error "must have a Unionfind `Var at this time")
      in
      let q : Quantifier. t = var, (pk, sk) in
      let rq = SugarQuantifier.mk_resolved q in
      o, (rq :: rqs)
    in
    let o, _ = List.fold_left bind_quantifier (o, StringSet.empty) unresolved_qs in
    let o, action_result = action o in
    let o, resolved_qs = List.fold_right unbind_quantifier unresolved_qs (o, []) in
    o, resolved_qs, action_result



  method! datatypenode =
    let open Datatype in
    function
    (*| TypeVar utv when is_anonymous utv ->
       let (_, sk_opt, freedom) = SugarTypeVar.get_unresolved_exn utv in
       let point = make_anon_point sk_opt freedom in
       let rtv = SugarTypeVar.mk_resolved point in
       o, TypeVar rtv *)
    | TypeVar stv ->
       let (name, sk, freedom) = SugarTypeVar.get_unresolved_exn stv in
       let o, resolved_tv = o#add name pk_type sk freedom in
       (* let resolved_tv = resolved_var_of_entry entry in *)
       o, TypeVar resolved_tv
    | Forall (unresolved_qs, body) ->

       (* let t = WithPos.node wpt in *)
       let o = o#set_new_named_vars_allowed false in
       let o = o#set_new_anon_vars_allowed false in
       let o, resolved_qs, body = o#quantified unresolved_qs (fun o' -> o'#datatype body) in
       let o = o#set_new_named_vars_allowed new_named_vars_allowed in
       let o = o#set_new_anon_vars_allowed new_anon_vars_allowed in
       o, Forall (resolved_qs, body)


       (* let original_o = o in
        * let bind_quantifier (o, names) sq =
        *   let (name, ((_pk, sk)), _) as v =
        *     SugarQuantifier.get_unresolved_exn sq in
        *   let pos = SourceCode.Position.dummy in
        *   if StringSet.mem name names then raise (duplicate_var pos name);
        *   let (_, kind, _) = ensure_kinded v in
        *   let point = make_opt_kinded_point sk `Rigid in
        *   let o' = o#bind name kind point in
        *   let names' = StringSet.add name names in
        *   (o', names')
        * in
        * let unbind_quantifier
        *       (sq : SugarQuantifier.t)
        *       (o, (rqs : SugarQuantifier.t list)) =
        *   let name = SugarQuantifier.get_unresolved_name_exn sq in
        *   let (o, (var, (pk, sk), _freedom)) = o#unbind name original_o in
        *   let q : Quantifier. t = var, (pk, sk) in
        *   let rq = SugarQuantifier.mk_resolved q in
        *   o, (rq :: rqs)
        * in
        * let o, _ = List.fold_left bind_quantifier (o, StringSet.empty) unresolved_qs in
        * let o, t = o#datatypenode t in
        * let o, resolved_qs = List.fold_right unbind_quantifier unresolved_qs (o, []) in
        * o, Forall (resolved_qs, WithPos.make ~pos:tpos t) *)
    | Mu (stv, t) ->
       let original_o = o in
       (* let var = Types.fresh_raw_variable () in
        * let point = Unionfind.fresh (`Var (var, default_subkind, `Flexible)) in *)
       (* let point = make_opt_kinded_point (Some default_subkind) `Flexible in *)

       (* ignores subkind and freedom info of stv *)
       let name = SugarTypeVar.get_unresolved_name_exn stv in
       let entry = make_fresh_entry (Some PrimaryKind.Type) None `Rigid in
       let o = o#bind name entry in
       let o, t = o#datatype t in
       let o, _, _ = o#unbind name original_o in
       let resolved_tv = resolved_var_of_entry entry in

       (* At this point, the body of the Unionfind point inside of entry is a `Var.
          DesugarDatatypes changes that to `Recusive *)
       o, Mu (resolved_tv, t)
    | dt -> super#datatypenode dt


  method! row_var =
    let open Datatype in function
    | Closed -> o, Closed
    | Open srv as orig when is_anonymous srv ->
       let (name, sk, freedom) = SugarTypeVar.get_unresolved_exn srv in
       let _ = o#add name pk_row sk freedom in
       (* The call to o#add is only done to yield an error if we are not allowed
          to find anoynomous variables right now. Note that we discard the result.
          The row variable stays unresolved. *)
       o, orig
    | Open srv ->
       let (name, sk, freedom) = SugarTypeVar.get_unresolved_exn srv in
       let o, resolved_rv = o#add name pk_row sk freedom in
       o, Datatype.Open resolved_rv
    | Recursive (stv, r) ->
       let original_o = o in

       let name = SugarTypeVar.get_unresolved_name_exn stv in
       let entry = make_fresh_entry (Some PrimaryKind.Row) (Some default_subkind) `Rigid in
       let o = o#bind name entry in
       let o, t = o#row r in
       let o, _, _ = o#unbind name original_o in
       let resolved_rv = resolved_var_of_entry entry in

       (* At this point, the body of the Unionfind point inside of entry is a `Var.
          DesugarDatatypes changes that to `Recusive *)
       o, Recursive (resolved_rv, t)

  method! fieldspec = let open Datatype in function
    | Absent -> o, Absent
    | Present t ->
       let o, t = o#datatype t in
       o, Present t
    (*| Var utv when is_anonymous utv ->
       let (_, sk_opt, freedom) = SugarTypeVar.get_unresolved_exn utv in
       let point = make_anon_point sk_opt freedom in
       let rtv = SugarTypeVar.mk_resolved point in
       o, Var rtv *)
    | Var utv ->
       let (name, sk, freedom) = SugarTypeVar.get_unresolved_exn utv in
       let o, resolved_pv = o#add name pk_presence sk freedom in
       o, Var resolved_pv



  method! typenamenode (name, params, body) =
    let unresolved_qs = List.map fst params in

    (* Don't allow unbound named type variables in type definitions.
       We do allow unbound *anoynmous* variables, because those may be
       effect variables that the effect sugar handling will generalize the
       type binding over.
       Hence, we must re-check the free variables in the type definiton later on. *)

    let o = o#set_new_named_vars_allowed false in
    let o = o#set_new_anon_vars_allowed true in

    let o, resolved_qs, body = o#quantified unresolved_qs (fun o' -> o'#datatype' body) in

    let o = o#set_new_named_vars_allowed new_named_vars_allowed in
    let o = o#set_new_anon_vars_allowed new_anon_vars_allowed in
    let params = List.map2 (fun rq param -> (rq, snd param)) resolved_qs params in
    o, (name, params, body)


  method super_bindingnode = super#bindingnode



  method! bindingnode b =
    (* Debug.print (Sugartypes.show_bindingnode b); *)

   (* legacy type variables scoping hack *)
   let o = o#set_toplevelness false in
   let o, b = o#super_bindingnode b in
   let o = o#set_toplevelness at_toplevel in
   let o =
     if at_toplevel then
       o#reset_vars
     else
       o
   in
   o, b



end


let program p =
  let v = visitor StringMap.empty in
  snd (v#program p)


let sentence =

function
  | Definitions bs ->
     let v = visitor StringMap.empty in
     let _, bs = v#list (fun o b -> o#binding b) bs in
     Definitions bs
  | Expression  p  ->
     let v = visitor StringMap.empty in
     let _o, p = v#phrase p in
      Expression p
  | Directive   d  ->
     Directive d

let datatype t =
  let v = visitor StringMap.empty in
  snd (v#datatype t)



module Untyped = struct
  open Transform.Untyped

  let name = "type_variables"

  let program state program' =
    let _tyenv = Context.typing_environment (context state) in
    let program' = program program' in
    return state program'

  let sentence state sentence' =
    let _tyenv = Context.typing_environment (context state) in
    let sentence'' = sentence sentence' in
    return state sentence''
end
