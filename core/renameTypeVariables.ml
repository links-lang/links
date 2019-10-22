
open Utility
open CommonTypes


let internal_error message =
  Errors.internal_error ~filename:"renameTypeVariables.ml" ~message

type instantiation_maps =
  (Types.meta_type_var IntMap.t *
   Types.meta_row_var IntMap.t *
   Types.meta_presence_var IntMap.t)

let add_type var t (ty_map, r_map, p_map) =
  (IntMap.add var t ty_map), r_map, p_map
let add_row var r (ty_map, r_map, p_map) =
  ty_map, (IntMap.add var r r_map), p_map
let add_presence var p (ty_map, r_map, p_map) =
  ty_map, r_map, (IntMap.add var p p_map)


let unwrap_type_point = function
    | `MetaTypeVar point -> point
    | _ -> assert false

let unwrap_row_point = snd3

let unwrap_presence_point = function
    | `Var point -> point
    | _ -> assert false

let freshen_quantifiers
      (qs : Quantifier.t list)
      (instantiation_maps : instantiation_maps)
    : (Quantifier.t list * instantiation_maps) =
  let open CommonTypes.PrimaryKind in
  List.fold_right
    (fun q (new_qs, imaps) ->
      match q with
      | (var, (Type, subkind)) ->
         let q, t = Types.fresh_type_quantifier subkind in
         (q :: new_qs), add_type var (unwrap_type_point t) imaps
      | (var, (Row, subkind)) ->
         let q, r = Types.fresh_row_quantifier subkind in
         (q :: new_qs), add_row var (unwrap_row_point r) imaps
      | (var, (Presence, subkind)) ->
         let q, f = Types.fresh_presence_quantifier subkind in
         (q :: new_qs), add_presence var (unwrap_presence_point f) imaps)
    qs ([], instantiation_maps)


(* Unused untill we integrate some syncing of tyvars and quantifiers *)
let _replace_quantifiers
  (qs_old : Quantifier.t list)
  (instantiation_maps : instantiation_maps)
  (qs_new : Quantifier.t list) : instantiation_maps =
  List.fold_right2
    (fun q_old q_new imaps ->
      let (var_old, (pk_old, sk_old)) = q_old in
      let (var_new, (pk_new, sk_new)) = q_new in
      (if pk_old <> pk_new || sk_old <> sk_new then
        raise (internal_error "replace_quantifiers: Kind missmatch"));
      let open CommonTypes.PrimaryKind in
      match pk_old with
      | Type ->
         let t = Types.make_rigid_type_variable var_new sk_new in
         add_type var_old (unwrap_type_point t) imaps
      | Row ->
         let r = Types.make_rigid_row_variable var_new sk_new in
         add_row var_old r imaps
      | Presence ->
         let f = Types.make_rigid_presence_variable var_new sk_new in
         add_presence var_old (unwrap_presence_point f) imaps)
    qs_old qs_new instantiation_maps


let renaming_type_visitor instantiation_map refresh_quantifiers =
  let open Types in
  object ((o : 'self_type))
    inherit Transform.visitor as super

    val inst_maps : instantiation_maps = instantiation_map

    method set_maps new_inst_maps =
      {< inst_maps = new_inst_maps >}

    method! typ : datatype -> (datatype * 'self_type) = function
      | `ForAll (qs, t) ->
         let qs', t' =
           if refresh_quantifiers then
             let (qs', inst_maps) = freshen_quantifiers qs inst_maps in
             let (substed_t, _) = {< inst_maps >}#typ t in
             qs' ,substed_t
           else
             qs, fst (o#typ t)
         in
         `ForAll (qs', t'), o
      | t -> super#typ t

    method! meta_type_var : meta_type_var -> (meta_type_var * 'self_type) = fun point ->
      match Unionfind.find point with
        | `Var (var, _, _) ->
           begin match IntMap.lookup var (fst3 inst_maps) with
             | Some subst_p -> subst_p, o
             | None -> point, o
           end
        | _ -> super#meta_type_var point

    method! meta_row_var : meta_row_var -> (meta_row_var * 'self_type) = fun point ->
      match Unionfind.find point with
        | `Var (var, _, _) ->
           begin match IntMap.lookup var (snd3 inst_maps) with
             | Some subst_p -> subst_p, o
             | None -> point, o
           end
        | _ -> super#meta_row_var point

    method! meta_presence_var :  meta_presence_var -> (meta_presence_var * 'self_type) =
      fun point ->
      match Unionfind.find point with
      | `Var (id, _, _) ->
         begin match IntMap.lookup id (thd3 inst_maps) with
         | Some subst_p -> subst_p, o
         | None -> point, o
         end
      | _ -> super#meta_presence_var point

  end


let refresher initial_maps refresh_quantifiers =
  let open Sugartypes in
  object(o : 'self_type)
    inherit SugarTraversals.fold_map as super

    val maps : instantiation_maps = initial_maps

    method set_maps new_inst_maps =
      {< maps = new_inst_maps >}

    method! typ = fun t ->
      let t', _ = (renaming_type_visitor maps refresh_quantifiers)#typ t in
      o, t'

    method! type_row = fun r ->
      let r', _ = (renaming_type_visitor maps refresh_quantifiers)#row r in
      o, r'

    method! type_field_spec = fun fs ->
      let fs', _ = (renaming_type_visitor maps refresh_quantifiers)#field_spec fs in
      o, fs'

    method! tyvar = fun q -> (o,q)

    method! unknown = fun _u ->
      raise (internal_error "Must implement all method stubs")


    method! bindingnode : Sugartypes.bindingnode -> ('self_type * Sugartypes.bindingnode) =
      function
      | Val (pat, (tyvars, phrase), loc, signature) ->
          let (o, pat') = o#pattern pat in

          let tyvars', new_maps =
            if refresh_quantifiers then
              freshen_quantifiers tyvars maps
            else
              tyvars, maps
          in
          let _, phrase' = (o#set_maps new_maps)#phrase phrase in

          let (o, loc') = o#location loc in
          let (o, signature') = o#option (fun o -> o#datatype') signature in
          (o, (Val ((pat', (tyvars', phrase'), loc', signature'))))
      | other -> super#bindingnode other


   method! function_definition : Sugartypes.function_definition -> 'self * Sugartypes.function_definition =
     fun { fun_binder;
           fun_linearity;
           fun_definition = (tyvars, lit);
           fun_location;
           fun_signature;
           fun_frozen;
           fun_unsafe_signature; } ->
     let (pats, body) = lit in
     let o, (pats', tyvars', typ', signature', phrase') =
       o#handle_function pats tyvars (Binder.to_type fun_binder) fun_signature body in
     let function_definition' =
       { fun_binder = Binder.set_type fun_binder typ';
         fun_linearity;
         fun_definition = (tyvars', (pats', phrase'));
         fun_location;
         fun_signature = signature';
         fun_frozen;
         fun_unsafe_signature; }
     in
     o,function_definition'


     method! recursive_functionnode : recursive_functionnode -> 'self * recursive_functionnode
         = fun { rec_binder;
              rec_linearity;
              rec_definition = ((tyvars, ty), lit);
              rec_location;
              rec_signature;
              rec_unsafe_signature;
              rec_frozen } ->
       let (pats, body) = lit in
       let o, (pats', tyvars', typ', signature', phrase') =
         o#handle_function pats tyvars (Binder.to_type rec_binder) rec_signature body in
       let recursive_definition' =
         { rec_binder = Binder.set_type rec_binder typ';
           rec_linearity;
           rec_definition = ((tyvars', ty), (pats', phrase'));
           rec_location;
           rec_signature = signature';
           rec_unsafe_signature;
           rec_frozen }
       in
       o, recursive_definition'


    method handle_function =
      fun param_pats tyvars typ signature phrase ->

      let handle_pattern_list
            (obj : 'self_type)
            (pats : Sugartypes.Pattern.with_pos list)
          : 'self_type * Sugartypes.Pattern.with_pos list =
        obj#list (fun obj' pat -> obj'#pattern pat) pats
      in

      let _, param_pats' =
        o#list (fun o plist -> handle_pattern_list o plist) param_pats
      in
      let typ', _ = (renaming_type_visitor maps refresh_quantifiers)#typ typ in

      let tyvars', new_maps =
        if refresh_quantifiers then
          freshen_quantifiers tyvars maps
        else
          tyvars, maps
      in

      let _, phrase' = (o#set_maps new_maps)#phrase phrase in

      (* For the time being, just visit the type in the signature *)
      let o, signature' = o#option (fun o -> o#datatype') signature in

      o, (param_pats', tyvars', typ', signature', phrase')

    end



let rename_phrase phrase inst_maps refresh_quantiifers =
  snd ((refresher inst_maps refresh_quantiifers)#phrase phrase)
