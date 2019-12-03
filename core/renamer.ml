(* Rename for type variables in terms *)

open Utility
open CommonTypes
open Sugartypes

let internal_error message =
  Errors.internal_error ~filename:"renamer.ml" ~message

type instantiation_maps =
  (Types.meta_type_var     IntMap.t *
   Types.meta_row_var      IntMap.t *
   Types.meta_presence_var IntMap.t)

let fresh_instantiation_maps : instantiation_maps =
  (IntMap.empty, IntMap.empty, IntMap.empty)

let add_type_var var t (ty_map, r_map, p_map) =
  (IntMap.add var t ty_map), r_map, p_map

let add_row_var var r (ty_map, r_map, p_map) =
  ty_map, (IntMap.add var r r_map), p_map

let add_presence_var var p (ty_map, r_map, p_map) =
  ty_map, r_map, (IntMap.add var p p_map)


let remove_type_var var (ty_map, r_map, p_map) =
  (IntMap.remove var ty_map), r_map, p_map

let remove_row_var var (ty_map, r_map, p_map) =
  ty_map, (IntMap.remove var r_map), p_map

let remove_presence_var var (ty_map, r_map, p_map) =
  ty_map, r_map, (IntMap.remove var p_map)


let unwrap_type_point = function
    | `MetaTypeVar point -> point
    | _ -> assert false

let unwrap_presence_point = function
    | `Var point -> point
    | _ -> assert false


(** Build instantiation maps between two lists of quantifiers *)
let build_instantiation_maps
     ?(instantiation_maps=fresh_instantiation_maps)
      (qs_old : Quantifier.t list)
      (qs_new : Quantifier.t list) : instantiation_maps =
  assert (List.length qs_old == List.length qs_new);
  List.fold_right2
    (fun q_old q_new imaps ->
      let (var_old, (pk_old, sk_old)) = q_old in
      let (var_new, (pk_new, sk_new)) = q_new in
      (if pk_old <> pk_new || sk_old <> sk_new then
        raise (internal_error "build_instantiation_maps: Kind missmatch"));
      let open CommonTypes.PrimaryKind in
      match pk_old with
      | Type ->
         let t = Types.make_rigid_type_variable var_new sk_new in
         add_type_var var_old (unwrap_type_point t) imaps
      | Row ->
         let r = Types.make_rigid_row_variable var_new sk_new in
         add_row_var var_old r imaps
      | Presence ->
         let f = Types.make_rigid_presence_variable var_new sk_new in
         add_presence_var var_old (unwrap_presence_point f) imaps)
    qs_old qs_new instantiation_maps

(** Remove quantifiers from instantiation_maps.  Required to handle variable
    shadowing. *)
let shadow_vars
      (maps : instantiation_maps)
      (qs : Quantifier.t list) : instantiation_maps =
  List.fold_right
    (fun (var, (pk, _)) imaps ->
      let open CommonTypes.PrimaryKind in
      match pk with
      | Type     -> remove_type_var     var imaps
      | Row      -> remove_row_var      var imaps
      | Presence -> remove_presence_var var imaps)
    qs maps

let renaming_type_visitor instantiation_map =
  let open Types in
  object ((o : 'self_type))
    inherit Transform.visitor as super

    val inst_maps : instantiation_maps = instantiation_map

    method set_maps new_inst_maps =
      {< inst_maps = new_inst_maps >}

    method! typ : datatype -> (datatype * 'self_type) = function
      | `ForAll (qs, t) ->
         let inst_maps = shadow_vars inst_maps qs in
         let t', _ = {< inst_maps >}#typ t in
         `ForAll (qs, t'), o
      | t -> super#typ t

    method! meta_type_var : meta_type_var -> (meta_type_var * 'self_type) =
      fun point ->
      match Unionfind.find point with
        | `Var (var, _, _) ->
           begin match IntMap.lookup var (fst3 inst_maps) with
             | Some subst_p -> subst_p, o
             | None -> point, o
           end
        | _ -> super#meta_type_var point

    method! meta_row_var : meta_row_var -> (meta_row_var * 'self_type) =
      fun point ->
      match Unionfind.find point with
        | `Var (var, _, _) ->
           begin match IntMap.lookup var (snd3 inst_maps) with
             | Some subst_p -> subst_p, o
             | None -> point, o
           end
        | _ -> super#meta_row_var point

    method! meta_presence_var :
            meta_presence_var -> (meta_presence_var * 'self_type) =
      fun point ->
      match Unionfind.find point with
      | `Var (id, _, _) ->
         begin match IntMap.lookup id (thd3 inst_maps) with
         | Some subst_p -> subst_p, o
         | None -> point, o
         end
      | _ -> super#meta_presence_var point

  end


let renamer qs_from qs_to =
  object(o : 'self_type)
    inherit SugarTraversals.fold_map as super

    val maps : instantiation_maps =
      build_instantiation_maps qs_from qs_to

    method set_maps new_inst_maps =
      {< maps = new_inst_maps >}

    method with_maps new_inst_maps =
      {< maps = new_inst_maps >}, maps

    method! typ = fun t ->
      o, fst ((renaming_type_visitor maps)#typ t)

    method! type_row = fun r ->
      o, fst ((renaming_type_visitor maps)#row r)

    method! type_field_spec = fun fs ->
      o, fst ((renaming_type_visitor maps)#field_spec fs)

    method! tyvar = fun q -> (o,q)

    method! bindingnode : bindingnode -> ('self_type * bindingnode) =
      function
      | Val (pat, (tyvars, phrase), loc, signature) ->
         (* Invariant: both signature and pat use quantifiers identical to
            tyvars.  If this invariant is ever to be changed then type variable
            shadowing needs to be done separately for pat and signature. *)
          let maps = shadow_vars maps tyvars in

          let o, old_maps   = o#with_maps maps in
          let o, pat'       = o#pattern pat in
          let o, phrase'    = o#phrase phrase in
          let o, loc'       = o#location loc in
          let o, signature' = o#option (fun o -> o#datatype') signature in
          let o             = o#set_maps old_maps in
          o, (Val ((pat', (tyvars, phrase'), loc', signature')))
      | other -> super#bindingnode other


   method! function_definition :
           function_definition -> 'self * function_definition
     = fun { fun_binder
           ; fun_linearity
           ; fun_definition = (tyvars, (pats, body))
           ; fun_location
           ; fun_signature
           ; fun_frozen
           ; fun_unsafe_signature } ->
     let o, (pats', tyvars', typ', _, signature', body') =
       o#handle_function pats tyvars (Binder.to_type fun_binder) None
                         fun_signature body in
     let function_definition' =
       { fun_binder = Binder.set_type fun_binder typ'
       ; fun_linearity
       ; fun_definition = (tyvars', (pats', body'))
       ; fun_location
       ; fun_signature = signature'
       ; fun_frozen
       ; fun_unsafe_signature } in
     o, function_definition'


     method! recursive_functionnode :
             recursive_functionnode -> 'self * recursive_functionnode
       = fun { rec_binder
             ; rec_linearity
             ; rec_definition = ((tyvars, ty), (pats, body))
             ; rec_location
             ; rec_signature
             ; rec_unsafe_signature
             ; rec_frozen } ->
       let o, (pats', tyvars', typ', ty', signature', body') =
         o#handle_function pats tyvars (Binder.to_type rec_binder) ty
                           rec_signature body in
       let recursive_definition' =
         { rec_binder = Binder.set_type rec_binder typ'
         ; rec_linearity
         ; rec_definition = ((tyvars', ty'), (pats', body'))
         ; rec_location
         ; rec_signature = signature'
         ; rec_unsafe_signature
         ; rec_frozen } in
       o, recursive_definition'


     method pattern_list (obj : 'self_type) (pats : Pattern.with_pos list)
            : 'self_type * Pattern.with_pos list =
       obj#list (fun obj' pat -> obj'#pattern pat) pats


     method handle_function = fun param_pats tyvars typ ty signature phrase ->
      (* Invariant: both signature, typ and param_pats use quantifiers identical
         to tyvars.  If this invariant is ever to be changed then type variable
         shadowing needs to be done separately for pat and signature. *)
      let maps = shadow_vars maps tyvars in
      let o, old_maps    = o#with_maps maps in
      let _, param_pats' = o#list o#pattern_list param_pats in
      let typ', _        = (renaming_type_visitor maps)#typ typ in
      let o, ty'         = o#option (fun o (ty, x) -> o, (snd (o#typ ty), x)) ty in
      let o, phrase'     = o#phrase phrase in
      let o, signature'  = o#option (fun o -> o#datatype') signature in
      let o              = o#set_maps old_maps in
      o, (param_pats', tyvars, typ', ty', signature', phrase')

     method forall = function
       | `ForAll (qs_from', t) ->
          assert (List.length qs_from' = List.length qs_to);
          let _, t' = o#typ t in
          `ForAll (qs_to, t')
       | t -> snd (o#typ t)

    end

let rename_function_definition : function_definition -> function_definition =
  fun { fun_binder
      ; fun_linearity
      ; fun_definition = (qs_from, (pats, body))
      ; fun_location
      ; fun_signature
      ; fun_frozen
      ; fun_unsafe_signature } ->
  let qs_to, _      = Instantiate.build_fresh_quantifiers qs_from in
  let o             = renamer qs_from qs_to in
  let typ'          = o#forall (Binder.to_type fun_binder) in
  let _, pats'      = List.split (List.map (o#pattern_list o) pats) in
  let _, body'      = o#phrase body in
  let _, signature' = o#option (fun o -> o#datatype') fun_signature in
  { fun_binder =  Binder.set_type fun_binder typ'
  ; fun_linearity
  ; fun_definition = (qs_to, (pats', body'))
  ; fun_location
  ; fun_signature = signature'
  ; fun_frozen
  ; fun_unsafe_signature }


let rename_recursive_functionnode :
      recursive_functionnode -> recursive_functionnode =
  fun { rec_binder
      ; rec_linearity
      ; rec_definition = ((qs_from, ty), (pats, body))
      ; rec_location
      ; rec_signature
      ; rec_frozen
      ; rec_unsafe_signature } ->
  let qs_to, _      = Instantiate.build_fresh_quantifiers qs_from in
  let o             = renamer qs_from qs_to in
  let typ'          = o#forall (Binder.to_type rec_binder) in
  let _, pats'      = List.split (List.map (o#pattern_list o) pats) in
  let _, ty'        = o#option (fun o (ty, x) -> o, (o#forall ty, x)) ty in
  let _, body'      = o#phrase body in
  let _, signature' = o#option (fun o -> o#datatype') rec_signature in
  { rec_binder =  Binder.set_type rec_binder typ'
  ; rec_linearity
  ; rec_definition = ((qs_to, ty'), (pats', body'))
  ; rec_location
  ; rec_signature = signature'
  ; rec_frozen
  ; rec_unsafe_signature }
