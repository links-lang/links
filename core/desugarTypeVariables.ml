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




type kinded_type_variable = Name.t * Sugartypes.kind * Freedom.t

type tyvar_map_entry = Sugartypes.kind * (Types.type_var Unionfind.point)
type tyvar_map = tyvar_map_entry StringMap.t

(* FIXME: must remove from desugarDatatypes *)
let infer_kinds
  = Settings.(flag "infer_kinds"
              |> convert parse_bool
              |> sync)

let internal_error message =
  Errors.internal_error ~filename:"desugarTypeVariables.ml" ~message


(* Errors *)

let typevar_mismatch pos (v1 : kinded_type_variable) (_v2 : kinded_type_variable) =
  let var, _, _ = v1 in
  Errors.Type_error
    ( pos,
      Printf.sprintf "Mismatch in kind for type variable `%s'.\n" var
      ^ Printf.sprintf "  Declared as `%s' and `%s'."
          ("fixme")
          ("fixme") )

let duplicate_var pos var =
  Errors.Type_error (pos, Printf.sprintf "Multiple definitions of type variable `%s'." var)



let free_type_variable ?var pos =
  let desc = match var with
    | None -> "anonymous type variable"
    | Some name -> "type variable " ^ name
  in Errors.Type_error
       (pos,
        "Unbound " ^ desc ^ " in position where
        no free type variables are allowed")



let lookup_tyvar_exn name (map : tyvar_map)
    : Sugartypes.kind * Freedom.t * Types.type_var Unionfind.point =
  let kind, point = StringMap.find name map in
  let `Var (_, _, freedom) =  Unionfind.find point in
  kind, freedom, point


let default_kind : PrimaryKind.t = PrimaryKind.Type
let default_subkind : Subkind.t = (lin_unl, res_any)

let concrete_subkind =
  function
  | Some subkind -> subkind
  | None         -> default_subkind

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



(* taken from DesugarDatatypes.desugar_quantifiers
note that we fill primary kind and subkind info.
However, this is not the same as setting defaults. Instead,
the info in the map takes precedence.

*)
let make_opt_kinded_point sk freedom : Types.type_var Unionfind.point=
  let var = Types.fresh_raw_variable () in
  let con_sk = concrete_subkind sk in
  Unionfind.fresh (`Var (var, con_sk, freedom))



let visitor initial_map =
object (o : 'self)
  inherit SugarTraversals.fold_map as super

  val tyvar_map : tyvar_map = initial_map

  val new_vars_allowed = true


  (* part of legacy compatibility, remove later *)
  val at_toplevel = true

  method reset_vars =
    (* just let unbind do its sanity check on all variables *)
    StringMap.iter (fun var _ -> ignore (o#unbind var o)) tyvar_map;
    {< tyvar_map = StringMap.empty >}

  method get_vars = tyvar_map


  method bind
           name
           (kind : Sugartypes.kind)
           (point : Types.type_var Unionfind.point)  =
    {< tyvar_map = StringMap.add name (kind, point) tyvar_map >}



    (* Return all information obtained about the given variable,
       possibly filling in default kinding info.
       Restore the info stored about the variable in a previous object *)
  method unbind name (restore_from : 'self) : 'self * (int * Kind.t * Freedom.t) =
    let check_subkind actual_sk expected_sk =
      if actual_sk <> expected_sk then
        raise (internal_error "Map and point info about subkind diverged")
    in

    match StringMap.lookup name tyvar_map with
      | Some ((pk_opt, sk_opt), point) ->

         let `Var (var, point_sk, freedom) = Unionfind.find point in

         (* as a concisteny check, make sure that the kinding information
            stored in the point coincide with the info in the map *)
         OptionUtils.opt_iter (check_subkind point_sk) sk_opt;

         let pk = from_option default_kind pk_opt in
         let sk = from_option default_subkind sk_opt in

         let old_map = restore_from#get_vars in
         let o = match StringMap.lookup name old_map with
           | Some (old_kind, old_point) ->
              o#bind name old_kind old_point
           | None ->
              o
         in
         o, (var, (pk, sk), freedom)


      | None ->
         raise (internal_error "trying to unbind unknown variable")




  (* used for type/row/presence variables found along the way, including anonymous ones *)
  method add ?pos (name, (pk, sk), freedom) : 'self * Types.type_var Unionfind.point =
    let anon = is_anonymous_name name in
    let pos = OptionUtils.from_option SourceCode.Position.dummy pos in
    if not anon && StringMap.mem name tyvar_map then
      begin
        let (pk', sk'), freedom', point  = lookup_tyvar_exn name tyvar_map in
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
          raise (typevar_mismatch SourceCode.Position.dummy tv tv');

        (* Update the union-find point with potential new information *)
        let `Var (var_id, _point_sk, _) = Unionfind.find point in
        (match sk with
          | Some sk -> Unionfind.change point (`Var (var_id, sk, freedom))
          | None -> () );

        o#bind name (pk, sk) point, point
      end
    else
      begin
        (if not new_vars_allowed then
          let name_opt = if anon then None else Some name in
          raise (free_type_variable ?var:name_opt pos));

        (* We create a new union-find point for this variable.
           To do so, we need to give it a non-optional subkind.
           If we don't know the subkind, yet, we choose the default
           here. But the entry in the map takes precedence
         *)
        let point = make_opt_kinded_point sk freedom in
        let o =
          if anon then o else o#bind name (pk, sk) point in
        o, point
      end



  (* In many instances we don't have accurate position information when raising
     type errors. In such a case, we re-throw the error using the position
     information of the closest surrounding datatype with such position info *)
  method! datatype ty =
    let open Errors in
    let open SourceCode in
    try super#datatype ty
    with
      Type_error (pos', msg) when pos' = Position.dummy ->
      raise (Type_error (WithPos.pos ty, msg))


  (* Used for Forall and Typenames *)
  method quantified : 'a. SugarQuantifier.t list -> ('self -> 'self * 'a) -> 'self * SugarQuantifier.t list * 'a =
    fun unresolved_qs action ->
    let original_o = o in
    let bind_quantifier (o, names) sq =
      let (name, ((_pk, sk)), _) as v =
        SugarQuantifier.get_unresolved_exn sq in
      let pos = SourceCode.Position.dummy in
      if StringSet.mem name names then raise (duplicate_var pos name);
      let (_, kind, _) = ensure_kinded v in
      let point = make_opt_kinded_point sk `Rigid in
      let o' = o#bind name kind point in
      let names' = StringSet.add name names in
      (o', names')
    in
    let unbind_quantifier
          (sq : SugarQuantifier.t)
          (o, (rqs : SugarQuantifier.t list)) =
      let name = SugarQuantifier.get_unresolved_name_exn sq in
      let (o, (var, (pk, sk), _freedom)) = o#unbind name original_o in
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
       let (name, k, freedom) = SugarTypeVar.get_unresolved_exn stv in
       let o, point = o#add (name, (Some pk_type, k), freedom) in
       let resolved_tv = Sugartypes.SugarTypeVar.mk_resolved point in
       o, TypeVar resolved_tv
    | Forall (unresolved_qs, body) ->

       (* let t = WithPos.node wpt in *)
       let o, resolved_qs, body = o#quantified unresolved_qs (fun o' -> o'#datatype body) in
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
    | Mu (name, t) ->
       let original_o = o in
       (* let var = Types.fresh_raw_variable () in
        * let point = Unionfind.fresh (`Var (var, default_subkind, `Flexible)) in *)
       let point = make_opt_kinded_point (Some default_subkind) `Flexible in
       let o = o#bind name (Some pk_type, Some default_subkind) point in
       let o, t = o#datatype t in
       let o, _ = o#unbind name original_o in
       (* must change to Recursive in desugarDatatypes *)
       (* Unionfind.change point (`Recursive (var, t)); *)
       o, Mu (name, t)
    | dt -> super#datatypenode dt


  method! row_var =
    let open Datatype in function
    | Closed -> o, Closed
    | Open stv as orig when is_anonymous stv ->
       (* We leave these to be handled by desugarEffectSugar *)
       o, orig
    | Open srv ->
       let (name, k, freedom) = SugarTypeVar.get_unresolved_exn srv in
       let o, point = o#add (name, (Some pk_row, k), freedom) in
       let resolved_rv = Sugartypes.SugarTypeVar.mk_resolved point in
       o, Datatype.Open resolved_rv
    | Recursive (name, r) ->
       let original_o = o in
       let point = make_opt_kinded_point (Some default_subkind) `Flexible in
       let o = o#bind name (Some pk_row, Some default_subkind) point in
       let o, t = o#row r in
       let o, _ = o#unbind name original_o in
       (* must change to Recursive in desugarDatatypes *)
       (* Unionfind.change point (`Recursive (var, t)); *)
       o, Recursive (name, t)

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
       let (name, k, freedom) = SugarTypeVar.get_unresolved_exn utv in
       let o, point = o#add (name, (Some pk_type, k), freedom) in
       let resolved_pv = Sugartypes.SugarTypeVar.mk_resolved point in
       o, Var resolved_pv



  method typenamemnode (name, params, body) =
    let unresolved_qs = List.map fst params in
    let o, resolved_qs, body = o#quantified unresolved_qs (fun o' -> o'#datatype body) in
    let params = List.map2 (fun rq param -> (rq, snd param)) resolved_qs params in
    o, (name, params, body)


  method super_bindingnode = super#bindingnode

  method set_toplevelness at_toplevel = {< at_toplevel >}


  method! bindingnode b =
   (* legacy type variables scoping hack *)
   let o = o#set_toplevelness false in
   let o, b = o#super_bindingnode b in
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


let sentence = function
  | Definitions bs ->
     let v = visitor StringMap.empty in
     let _, bs = v#list (fun o b -> o#binding b) bs in
     Definitions bs
  | Expression  p  ->
     let v = visitor StringMap.empty in
     let _o, p = v#phrase p in
      Expression p
  | Directive   d  -> Directive d


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
