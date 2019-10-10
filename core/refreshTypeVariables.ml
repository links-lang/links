
open Utility

let internal_error message =
  Errors.internal_error ~filename:"refreshTypeVariables.ml" ~message

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
      (qs : Types.quantifier list)
      (instantiation_maps : instantiation_maps)
    : (Types.quantifier list * instantiation_maps) =
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



let replace_quantifiers
  (qs_old : Types.quantifier list)
  (instantiation_maps : instantiation_maps)
  (qs_new : Types.quantifier list) : instantiation_maps =
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

let refreshing_type_visitor instantiation_map =
  let open Types in
  object ((o : 'self_type))
    inherit Transform.visitor as super

    val inst_maps : instantiation_maps = instantiation_map

    method set_maps new_inst_maps =
      {< inst_maps = new_inst_maps >}


    method! typ : datatype -> (datatype * 'self_type) = function
      | `ForAll (qs, t) ->
         let (qs', inst_maps') = freshen_quantifiers qs inst_maps in
         let old_maps = inst_maps in
         let o_new_maps = o#set_maps inst_maps' in

         let (substed_t, substed_o) = o_new_maps#typ t in
         let o' = substed_o#set_maps old_maps in

         `ForAll (qs', substed_t), o'
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






let refresher program sync_quantifiers_tyvars =
  let open Sugartypes in
  object(o : 'self_type)
    inherit SugarTraversals.fold_map as super

    val maps : instantiation_maps =
      (IntMap.empty, IntMap.empty, IntMap.empty)

    method set_maps new_inst_maps =
      {< maps = new_inst_maps >}


    (* method! bindingnode : Sugartypes.bindingnode -> ('self_type * Sugartypes.bindingnode) =
     *   function
     *   | Val (pat, (tyvars, phrase), loc, typ) ->
     *       let (o, pat   ) = o#pattern pat in
     *
     *
     *
     *       let (o, _x_i2) = o#phrase _x_i2 in
     *       let (o, _x_i3) = o#location _x_i3 in
     *       let (o, _x_i4) = o#option (fun o -> o#datatype') _x_i4
     *       in (o, (Val ((_x, (_x_i1, _x_i2), _x_i3, _x_i4))))
     *   | other -> o#bindingnode other *)




   method! function_definition : Sugartypes.function_definition -> 'self * Sugartypes.function_definition =
     fun { fun_binder;
           fun_linearity;
           fun_definition = (tyvars, lit);
           fun_location;
           fun_signature;
           fun_frozen;
           fun_unsafe_signature; } ->
     let (pats, body) = lit in
     let (tyvars', typ', signature', phrase') =
       o#handle_function tyvars (Binder.to_type fun_binder) fun_signature body in
     o, { fun_binder = Binder.set_type fun_binder typ';
       fun_linearity;
       fun_definition = (tyvars', (pats, phrase'));
       fun_location;
       fun_signature = signature';
       fun_frozen;
       fun_unsafe_signature; }





    method handle_function =
      fun tyvars typ signature phrase ->
      (* let typ', _ = (refreshing_type_visitor maps)#typ typ in
       *
       * (\* In the body, replace the old tyvars with either a) the new quantifiers
       *    used in the type or b) fresh variables *\)
       * let tyvars', new_maps =
       *   if sync_quantifiers_tyvars then
       *     let tyvars_typ', _ = TypeUtils.split_quantified_type typ' in
       *     tyvars_typ', replace_quantifiers tyvars maps tyvars_typ'
       *   else
       *     freshen_quantifiers tyvars maps
       * in
       *
       * let o = o#set_maps new_maps in
       * let o , body' = o#phrase phrase in
       * let o = o#set_maps maps
       *
       *
       *
       *
       * (tyvars', typ', signature, phrase) *)

      failwith "implement me"





    end
