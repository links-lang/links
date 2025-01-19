(* Rename for type variables in terms *)

open Utility
open CommonTypes
open Sugartypes

let internal_error message =
  Errors.internal_error ~filename:"renamer.ml" ~message

type instantiation_map = Types.datatype IntMap.t

let fresh_instantiation_map : instantiation_map = IntMap.empty


(** Build instantiation maps between two lists of quantifiers *)
let build_instantiation_map
     ?(instantiation_map=fresh_instantiation_map)
      (qs_old : Quantifier.t list)
      (qs_new : Quantifier.t list) : instantiation_map =
  assert (List.length qs_old == List.length qs_new);
  List.fold_right2
    (fun q_old q_new imap ->
      let (var_old, (pk_old, sk_old)) = q_old in
      let (var_new, (pk_new, sk_new)) = q_new in
      (if pk_old <> pk_new || sk_old <> sk_new then
        raise (internal_error "build_instantiation_map: Kind missmatch"));
     let meta_var = Types.make_rigid_variable var_new (pk_old, sk_new) in
     IntMap.add var_old meta_var imap)
    qs_old qs_new instantiation_map

(** Remove quantifiers from instantiation_map.  Required to handle variable
    shadowing. *)
let shadow_vars
      (maps : instantiation_map)
      (qs : Quantifier.t list) : instantiation_map =
  List.fold_right
    (fun (var, _) imap -> IntMap.remove var imap)
    qs maps

let unwrap_meta = function
    | Types.Meta t -> t
    | _ -> raise (internal_error "expected Meta")


let renaming_type_visitor instantiation_map =
  let open Types in
  object ((o : 'self_type))
    inherit Transform.visitor as super

    val inst_map : instantiation_map = instantiation_map

    method set_maps new_inst_map =
      {< inst_map = new_inst_map >}

    method! typ : datatype -> ('self_type * datatype) = function
      | ForAll (qs, t) ->
         let _ , qs' = List.split (List.map {< inst_map >}#quantifier qs) in
         let _ , t' = {< inst_map >}#typ t in
         o, ForAll (qs', t')
      | t -> super#typ t

    method! quantifier : Quantifier.t -> ('self_type * Quantifier.t) =
      fun (var, kind) ->
      begin match IntMap.lookup var inst_map with
      | Some t -> begin match Unionfind.find (unwrap_meta t) with
                  | Var (var', _, _) -> o, (var', kind)
                  | _ -> assert false
                  end
      | None -> o, (var, kind)
      end

    method! meta_type_var : meta_type_var -> ('self_type * meta_type_var) =
      fun point ->
      match Unionfind.find point with
        | Var (var, _, _) ->
           begin match IntMap.lookup var inst_map with
             | Some t -> o, unwrap_meta t
             | None -> o, point
           end
        | _ -> super#meta_type_var point

    method! meta_row_var : meta_row_var -> ('self_type * meta_row_var) = o#meta_type_var

    method! meta_presence_var :
            meta_presence_var -> ('self_type * meta_presence_var) = o#meta_type_var

  end


let renamer qs_from qs_to =
  let open Types in
  object(o : 'self_type)
    inherit SugarTraversals.fold_map as super

    val maps : instantiation_map =
      build_instantiation_map qs_from qs_to

    method set_maps new_inst_map =
      {< maps = new_inst_map >}

    method with_maps new_inst_map =
      {< maps = new_inst_map >}, maps

    method! typ = fun t ->
      o, snd ((renaming_type_visitor maps)#typ t)

    method! type_row = fun r ->
      o, snd ((renaming_type_visitor maps)#row r)

    method! type_field_spec = fun fs ->
      o, snd ((renaming_type_visitor maps)#field_spec fs)

    method! quantifier = fun q -> (o,q)

    method! bindingnode : bindingnode -> ('self_type * bindingnode) =
      function
      | Val (pat, (tyvars, phrase), loc, signature) ->
         (* Invariant: both signature and pat use quantifiers identical to
            tyvars.  If this invariant is ever to be changed then type variable
            shadowing needs to be done separately for pat and signature. *)
          let quantifiers = List.map SugarQuantifier.get_resolved_exn tyvars in
          let maps = shadow_vars maps quantifiers in

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
           ; fun_definition = (tyvars, f)
           ; fun_location
           ; fun_signature
           ; fun_frozen
           ; fun_unsafe_signature } ->
     let o, (pats', tyvars', typ', _, signature', body') =
       match f with
        | NormalFunlit (pats, body) -> o#handle_function pats tyvars (Binder.to_type fun_binder) None fun_signature body
        | _ -> assert false in
     let function_definition' =
       { fun_binder = Binder.set_type fun_binder typ'
       ; fun_linearity
       ; fun_definition = (tyvars', NormalFunlit (pats', body'))
       ; fun_location
       ; fun_signature = signature'
       ; fun_frozen
       ; fun_unsafe_signature } in
     o, function_definition'


     method! recursive_functionnode :
             recursive_functionnode -> 'self * recursive_functionnode
       = fun { rec_binder
             ; rec_linearity
             ; rec_definition = ((tyvars, ty), f)
             ; rec_location
             ; rec_signature
             ; rec_unsafe_signature
             ; rec_frozen } ->
       let o, (pats', tyvars', typ', ty', signature', body') =
         match f with
          | NormalFunlit (pats, body) -> o#handle_function pats tyvars (Binder.to_type rec_binder) ty rec_signature body
          | _ -> assert false in
       let recursive_definition' =
         { rec_binder = Binder.set_type rec_binder typ'
         ; rec_linearity
         ; rec_definition = ((tyvars', ty'), NormalFunlit (pats', body'))
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
      let quantifiers = List.map SugarQuantifier.get_resolved_exn tyvars in
      let maps = shadow_vars maps quantifiers in
      let o, old_maps    = o#with_maps maps in
      let _, param_pats' = o#list o#pattern_list param_pats in
      let _, typ'        = (renaming_type_visitor maps)#typ typ in
      let o, ty'         = o#option (fun o (ty, x) -> o, (snd (o#typ ty), x)) ty in
      let o, phrase'     = o#phrase phrase in
      let o, signature'  = o#option (fun o -> o#datatype') signature in
      let o              = o#set_maps old_maps in
      o, (param_pats', tyvars, typ', ty', signature', phrase')

     method forall = function
       | ForAll (qs_from', t) ->
          assert (List.length qs_from' = List.length qs_to);
          let _, t' = o#typ t in
          ForAll (qs_to, t')
       | t -> snd (o#typ t)


     (* We do not rename inside the syntactic representation of types *)
     method! type_variable x = o, x

  end

let rename_function_definition : function_definition -> function_definition =
  fun { fun_binder
      ; fun_linearity
      ; fun_definition = (tyvars_from, f)
      ; fun_location
      ; fun_signature
      ; fun_frozen
      ; fun_unsafe_signature } ->
  let (pats, body) = Sugartypes.get_normal_funlit f in
  let qs_from = List.map SugarQuantifier.get_resolved_exn tyvars_from in
  let qs_to, _      = Instantiate.build_fresh_quantifiers qs_from in
  let tyvars_to     = List.map SugarQuantifier.mk_resolved qs_to in
  let o             = renamer qs_from qs_to in
  let typ'          = o#forall (Binder.to_type fun_binder) in
  let _, pats'      = List.split (List.map (o#pattern_list o) pats) in
  let _, body'      = o#phrase body in
  let _, signature' = o#option (fun o -> o#datatype') fun_signature in
  { fun_binder =  Binder.set_type fun_binder typ'
  ; fun_linearity
  ; fun_definition = (tyvars_to, NormalFunlit (pats', body'))
  ; fun_location
  ; fun_signature = signature'
  ; fun_frozen
  ; fun_unsafe_signature }


let rename_recursive_functionnode :
      recursive_functionnode -> recursive_functionnode =
  fun { rec_binder
      ; rec_linearity
      ; rec_definition = ((tyvars_from, ty), f)
      ; rec_location
      ; rec_signature
      ; rec_frozen
      ; rec_unsafe_signature } ->
  let (pats, body) = Sugartypes.get_normal_funlit f in
  let qs_from = List.map SugarQuantifier.get_resolved_exn tyvars_from in
  let qs_to, _      = Instantiate.build_fresh_quantifiers qs_from in
  let tyvars_to     = List.map SugarQuantifier.mk_resolved qs_to in
  let o             = renamer qs_from qs_to in
  let typ'          = o#forall (Binder.to_type rec_binder) in
  let _, pats'      = List.split (List.map (o#pattern_list o) pats) in
  let _, ty'        = o#option (fun o (ty, x) -> o, (o#forall ty, x)) ty in
  let _, body'      = o#phrase body in
  let _, signature' = o#option (fun o -> o#datatype') rec_signature in
  { rec_binder =  Binder.set_type rec_binder typ'
  ; rec_linearity
  ; rec_definition = ((tyvars_to, ty'), NormalFunlit (pats', body'))
  ; rec_location
  ; rec_signature = signature'
  ; rec_frozen
  ; rec_unsafe_signature }
