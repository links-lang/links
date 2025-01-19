open Utility
open CommonTypes
open Ir
open Var

let internal_error message =
  Errors.internal_error ~filename:"irTraversals.ml" ~message

(** Traversal with type reconstruction

    Essentially this is a map-fold operation over the IR datatypes
    that also constructs the type as it goes along (using type
    annotations on binders).
*)
module type IR_VISITOR = sig
  type environment = Types.datatype Env.Int.t

  class visitor : environment ->
  object ('self_type)
    val tyenv : environment

    method lookup_type : var -> Types.datatype
    method constant : Constant.t -> ('self_type * Constant.t * Types.datatype)
    method optionu :
      'a.
      ('self_type -> 'a -> ('self_type * 'a)) ->
      'a option -> 'self_type * 'a option
    method option :
      'a.
      ('self_type -> 'a -> ('self_type * 'a * Types.datatype)) ->
      'a option -> 'self_type * 'a option * Types.datatype option
    method list :
      'a.
      ('self_type -> 'a -> ('self_type * 'a * Types.datatype)) ->
      'a list -> 'self_type * 'a list * Types.datatype list
    method name_map :
      'a.
      ('self_type -> 'a -> ('self_type * 'a * Types.datatype)) ->
      'a name_map -> 'self_type * 'a name_map * Types.datatype name_map
    method var_map :
      'a.
      ('self_type -> 'a -> ('self_type * 'a * Types.datatype)) ->
      'a var_map -> 'self_type * 'a var_map * Types.datatype var_map
    method var : var -> ('self_type * var * Types.datatype)
    method temporal_update : temporal_update -> ('self_type * temporal_update)
    method temporal_deletion : temporal_deletion -> ('self_type * temporal_deletion)

    method value : value -> ('self_type * value * Types.datatype)

    method tail_computation :
      tail_computation -> ('self_type * tail_computation * Types.datatype)
    method special : special -> ('self_type * special * Types.datatype)
    method bindings : binding list -> ('self_type * binding list)
    method computation : computation -> ('self_type * computation * Types.datatype)
    method binding : binding -> ('self_type * binding)
    method binder : binder -> ('self_type * binder)

    method program : program -> ('self_type * program * Types.datatype)

    method get_type_environment : environment
  end
end

module Transform : IR_VISITOR =
struct
  open Types
  open TypeUtils

  type environment = datatype Env.Int.t

  let deconstruct f t = f t

  module Env = Env.Int

  class visitor (tyenv : environment) =
  object ((o : 'self_type))
    val tyenv = tyenv

    method lookup_type : var -> datatype = fun var ->
      Env.find var tyenv

    method constant : Constant.t -> ('self_type * Constant.t * datatype) = fun c ->
      match c with
        | Constant.Bool   _ -> o, c, bool_type
        | Constant.Int    _ -> o, c, int_type
        | Constant.Char   _ -> o, c, char_type
        | Constant.String _ -> o, c, string_type
        | Constant.Float  _ -> o, c, float_type
        | Constant.DateTime  _ -> o, c, datetime_type

    method optionu :
      'a.
      ('self_type -> 'a -> ('self_type * 'a)) ->
      'a option -> 'self_type * 'a option =
      fun f v ->
        match v with
          | None -> o, None
          | Some v ->
              let o, v = f o v in
                o, Some v

    method option :
      'a.
      ('self_type -> 'a -> ('self_type * 'a * datatype)) ->
      'a option -> 'self_type * 'a option * datatype option =
      fun f v ->
        match v with
          | None -> o, None, None
          | Some v ->
              let o, v, t = f o v in
                o, Some v, Some t

    method list :
      'a.
      ('self_type -> 'a -> ('self_type * 'a * datatype)) ->
      'a list -> 'self_type * 'a list * datatype list =
      fun f v ->
        let o, vs, ts =
          List.fold_left
            (fun (o, vs, ts) v ->
               let (o, v, t) = f o v in
                 o, v::vs, t::ts)
            (o, [], [])
            v
        in
          o, List.rev vs, List.rev ts

    method name_map :
      'a.
      ('self_type -> 'a -> ('self_type * 'a * datatype)) ->
      'a name_map -> 'self_type * 'a name_map * datatype name_map =
      fun f vmap ->
        StringMap.fold
          (fun name v (o, vmap, tmap) ->
             let (o, v, t) = f o v in
               (o, StringMap.add name v vmap,
                   StringMap.add name t tmap))
          vmap
          (o, StringMap.empty, StringMap.empty)

    method var_map :
      'a.
      ('self_type -> 'a -> ('self_type * 'a * datatype)) ->
      'a var_map -> 'self_type * 'a var_map * datatype var_map =
      fun f vmap ->
        IntMap.fold
          (fun name v (o, vmap, tmap) ->
             let (o, v, t) = f o v in
               (o, IntMap.add name v vmap,
                   IntMap.add name t tmap))
          vmap
          (o, IntMap.empty, IntMap.empty)

    method var : var -> ('self_type * var * datatype) =
      fun var -> (o, var, o#lookup_type var)

    method temporal_update : temporal_update -> ('self_type * temporal_update) =
      function
        | ValidTimeUpdate (NonsequencedUpdate { from_time; to_time }) ->
            let o, from_time, _ = o#option (fun o -> o#computation) from_time in
            let o, to_time  , _ = o#option (fun o -> o#computation) to_time in
            o, ValidTimeUpdate (NonsequencedUpdate { from_time; to_time })
        | ValidTimeUpdate (SequencedUpdate { validity_from; validity_to }) ->
            let o, validity_from, _ = o#value validity_from in
            let o, validity_to  , _ = o#value validity_to in
            o, ValidTimeUpdate (SequencedUpdate { validity_from; validity_to })
        | x -> o, x

    method temporal_deletion : temporal_deletion -> ('self_type * temporal_deletion) =
      function
        | ValidTimeDeletion (SequencedDeletion { validity_from; validity_to }) ->
            let o, validity_from, _ = o#value validity_from in
            let o, validity_to,   _ = o#value validity_to in
            o, ValidTimeDeletion (SequencedDeletion { validity_from; validity_to })
        | x -> o, x

    method value : value -> ('self_type * value * datatype) =
      function
        | Ir.Constant c -> let (o, c, t) = o#constant c in o, Ir.Constant c, t
        | Variable x -> let (o, x, t) = o#var x in o, Ir.Variable x, t
        | Extend (fields, base) ->
            let (o, fields, field_types) = o#name_map (fun o -> o#value) fields in
            let (o, base, base_type) = o#option (fun o -> o#value) base in

            let t =
              match base_type with
                | None -> make_record_type field_types
                | Some t ->
                    begin
                      match TypeUtils.concrete_type t with
                        | Types.Record row ->
                            Types.Record (extend_row field_types row)
                        | _ -> assert false
                    end
            in
              o, Extend (fields, base), t
        | Project (name, v) ->
            let (o, v, vt) = o#value v in
              o, Project (name, v), deconstruct (project_type name) vt
        | Erase (names, v) ->
            let (o, v, vt) = o#value v in
            let t = deconstruct (erase_type names) vt in
              o, Erase (names, v), t
        | Inject (name, v, t) ->
            let o, v, _vt = o#value v in
              o, Inject (name, v, t), t
        | TAbs (tyvars, v) ->
            let o, v, t = o#value v in
            let t = Types.for_all (tyvars, t) in
              o, TAbs (tyvars, v), t
        | TApp (v, ts) ->
            let o, v, t = o#value v in
              begin try
                let t = Instantiate.apply_type t ts in
                  o, TApp (v, ts), t
              with
                  Instantiate.ArityMismatch _ ->
                  raise (internal_error
                    (Printf.sprintf
                       "Arity mismatch in type application (Ir.Transform). Expression: %s\n type: %s\n args: %s\n"
                       (show_value (TApp (v, ts)))
                       (Types.string_of_datatype t)
                       (String.concat "," (List.map (fun t -> Types.string_of_type_arg t) ts))))
              end
        | XmlNode (tag, attributes, children) ->
            let (o, attributes, _) = o#name_map (fun o -> o#value) attributes in
            let (o, children  , _) = o#list (fun o -> o#value) children in

              (*
                let _ = assert (StringMap.for_all (fun t -> t=string_type) attribute_types) in
                let _ = assert (List.for_all (fun t -> t=xml_type) children_types) in
              *)
              o, XmlNode (tag, attributes, children), xml_type
        | ApplyPure (f, args) ->
            let (o, f, ft) = o#value f in
            let (o, args, _) = o#list (fun o -> o#value) args in
              (* TODO: check arg types match *)
              o, ApplyPure (f, args), deconstruct (return_type ~overstep_quantifiers:true) ft

        | Closure (f, tyargs, z) ->
            let (o, f, t) = o#var f in
            let t =
              match tyargs with
                | [] -> t
                | _ ->
                  let (remaining_type, instantiation_maps) = Instantiate.instantiation_maps_of_type_arguments false t tyargs in
                  Instantiate.datatype instantiation_maps remaining_type in
            let (o, z, _) = o#value z in
              (* TODO: check that closure environment types match expectations for f *)
              o, Closure (f, tyargs, z), t
        | Coerce (v, t) ->
            let o, v, _ = o#value v in
            (* TODO: check that vt <: t *)
              o, Coerce (v, t), t

    method tail_computation :
      tail_computation -> ('self_type * tail_computation * datatype) =
      function
          (* TODO: type checking *)
        | Return v ->
            let o, v, t = o#value v in
              o, Return v, t
        | Apply (f, args) ->
            let o, f, ft = o#value f in
            let o, args, _ = o#list (fun o -> o#value) args in
              (* TODO: check arg types match *)
              o, Apply (f, args), deconstruct (return_type ~overstep_quantifiers:true) ft

        | Special special ->
            let o, special, t = o#special special in
              o, Special special, t

        | Ir.Case (v, cases, default) ->
            let o, v, _ = o#value v in
            let o, cases, case_types =
              o#name_map
                (fun o (b, c) ->
                   let o, b = o#binder b in
                   let o, c, t = o#computation c in
                     o, (b, c), t) cases in
            let o, default, default_type =
              o#option (fun o (b, c) ->
                          let o, b = o#binder b in
                          let o, c, t = o#computation c in
                            o, (b, c), t) default in
            let t =
              if not (StringMap.is_empty case_types) then
                (StringMap.to_alist ->- List.hd ->- snd) case_types
              else
                val_of default_type
            in
              o, Ir.Case (v, cases, default), t
        | If (v, left, right) ->
            let o, v, _ = o#value v in
            let o, left, t = o#computation left in
            let o, right, _ = o#computation right in
              o, If (v, left, right), t

    method special : special -> ('self_type * special * datatype) =
      let open Ir in
      function
        | Wrong t -> o, Wrong t, t
        | Database v ->
            let o, v, _ = o#value v in
              o, Database v, Types.Primitive Primitive.DB
        | Table { database; table; keys; temporal_fields; table_type } ->
            let o, database, _ = o#value database in
            let o, keys, _ = o#value keys in
            let o, table, _ = o#value table in
              o, Table { database; table; keys; temporal_fields; table_type }, Types.Table table_type
        | Lens (table, rtype) ->
            let o, table, _ = o#value table in
              o, Lens (table, rtype), Types.Lens rtype
        | LensSerial {lens; columns; typ} ->
            let o, lens, _ = o#value lens in
              o, LensSerial {lens; columns; typ}, Types.Lens typ
        | LensDrop {lens; drop; key; default; typ} ->
            let o, lens, _ = o#value lens in
            let o, default, _ = o#value default in
              o, LensDrop {lens; drop; key; default; typ}, Types.Lens typ
        | LensSelect {lens; predicate; typ} ->
            let o, lens, _ = o#value lens in
            let o, predicate =
              (match predicate with
               | Dynamic predicate ->
                 let o, predicate, _ = o#value predicate in
                 o, Dynamic predicate
              | Static predicate -> o, Static predicate) in
              o, LensSelect {lens; predicate; typ}, Types.Lens typ
        | LensJoin {left; right; on; del_left; del_right; typ} ->
            let o, left, _ = o#value left in
            let o, right, _ = o#value right in
              o, LensJoin {left; right; on; del_left; del_right; typ}, Types.Lens typ
        | LensCheck (lens, t) ->
            let o, lens, _ = o#value lens in
              o, LensCheck (lens, t), Types.Lens t
        | LensGet (lens, rtype) ->
            let o, lens, _ = o#value lens in
              o, LensGet (lens, rtype), Types.make_list_type rtype
        | LensPut (lens, data, rtype) ->
            let o, lens, _ = o#value lens in
            let o, data, _ = o#value data in
            o, LensPut (lens, data, rtype), Types.make_tuple_type []
        | TemporalJoin (tmp, comp, _) ->
            let o, comp, t = o#computation comp in
            o, TemporalJoin (tmp, comp, t), t
        | Query (range, policy, e, _) ->
            let o, range =
              o#optionu
                (fun o (limit, offset) ->
                   let o, limit, _ = o#value limit in
                   let o, offset, _ = o#value offset in
                     o, (limit, offset))
                range in
            let o, e, t = o#computation e in
              o, Query (range, policy, e, t), t
        | InsertRows (tmp, source, rows) ->
            let o, source, _ = o#value source in
            let o, rows, _ = o#value rows in
              o, InsertRows (tmp, source, rows), Types.unit_type
        | InsertReturning (tmp, source, rows, returning) ->
            let o, source, _ = o#value source in
            let o, rows, _ = o#value rows in
            let o, returning, _ = o#value returning in
              o, InsertReturning (tmp, source, rows, returning), Types.unit_type
        | Update (upd, (x, source), where, body) ->
            let o, source, _ = o#value source in
            let o, x = o#binder x in
            let o, upd = o#optionu (fun o -> o#temporal_update) upd in
            let o, where, _ = o#option (fun o -> o#computation) where in
            let o, body, _ = o#computation body in
              o, Update (upd, (x, source), where, body), Types.unit_type
        | Delete (del, (x, source), where) ->
            let o, source, _ = o#value source in
            let o, x = o#binder x in
            let o, del = o#optionu (fun o -> o#temporal_deletion) del in
            let o, where, _ = o#option (fun o -> o#computation) where in
              o, Delete (del, (x, source), where), Types.unit_type
        | CallCC v ->
            let o, v, t = o#value v in
              o, CallCC v, deconstruct (return_type ~overstep_quantifiers:true) t
        | Select (l, v) ->
           let o, v, t = o#value v in
           o, Select (l, v), t
        | Choice (v, bs) ->
           let o, v, _ = o#value v in
           let o, bs, branch_types =
             o#name_map (fun o (b, c) ->
                         let o, b = o#binder b in
                         let o, c, t = o#computation c in
                         o, (b, c), t) bs in
           let t = (StringMap.to_alist ->- List.hd ->- snd) branch_types in
           o, Choice (v, bs), t
    | Handle ({ ih_comp; ih_cases; ih_return; ih_depth }) ->
       let (o, comp, _) = o#computation ih_comp in
           (* TODO FIXME traverse parameters *)
           let (o, depth) =
             match ih_depth with
             | Deep params ->
                let (o, bindings) =
                  List.fold_left
                    (fun (o, bvs) (b,v) ->
                      let (o, b) = o#binder b in
                      let (o, v, _) = o#value v in
                      (o, (b,v) :: bvs))
                    (o, []) params
                in
                o, Deep (List.rev bindings)
             | Shallow -> o, Shallow
           in
       let (o, cases, _branch_types) =
         o#name_map
               (fun o (x, resume, c) ->
                 let (o, x) = o#binder x in
         let (o, resume) = o#binder resume in
         let (o, c, t) = o#computation c in
         o, (x, resume, c), t)
           ih_cases
       in
           let (o, return, t) =
             let (o, b) = o#binder (fst ih_return) in
             let (o, comp, t) = o#computation (snd ih_return) in
             o, (b, comp), t
           in
       o, Handle { ih_comp = comp; ih_cases = cases; ih_return = return; ih_depth = depth}, t
    | DoOperation (name, vs, t) ->
       let (o, vs, _) = o#list (fun o -> o#value) vs in
       (o, DoOperation (name, vs, t), t)

   method bindings : binding list -> ('self_type * binding list) =
      fun bs ->
        let o, bs =
          List.fold_left
            (fun (o, bs) b ->
               let (o, b) = o#binding b in
                 (o, b::bs))
            (o, [])
            bs
        in
          o, List.rev bs

    method computation : computation -> ('self_type * computation * datatype) =
      fun (bs, tc) ->
        let o, bs = o#bindings bs in
        let o, tc, t = o#tail_computation tc in
          o, (bs, tc), t

    method binding : binding -> ('self_type * binding) =
      function
        | Let (x, (tyvars, tc)) ->
            let o, x = o#binder x in
            let o, tc, _ = o#tail_computation tc in
              o, Let (x, (tyvars, tc))
        | Fun fundef ->
            let {fn_binder = f; fn_tyvars; fn_params = xs; fn_body; fn_closure = z; fn_location; fn_unsafe} = fundef in
            let o, xs, body, z =
              let (o, z) = o#optionu (fun o -> o#binder) z in
              let (o, xs) =
                List.fold_right
                  (fun x (o, xs) ->
                     let o, x = o#binder x in
                       (o, x::xs))
                  xs
                  (o, []) in
              let o, body, _ = o#computation fn_body in
                o, xs, body, z in
            let o, f = o#binder f in
              (* TODO: check that xs and body match up with f *)
              let fundef = {fn_binder = f; fn_tyvars; fn_params = xs; fn_body = body; fn_closure = z;
                            fn_location; fn_unsafe}
              in
              o, Fun fundef
        | Rec defs ->
            (* it's important to traverse the function binders first in
               order to make sure they're in scope for all of the
               function bodies *)
            let o, defs =
              List.fold_right
                (fun fundef (o, fs) ->
                   let o, f = o#binder fundef.fn_binder in
                     (o, {fundef with fn_binder = f}::fs))
                defs
                (o, []) in

            let o, defs =
              List.fold_left
                (fun ((o : 'self_type), defs) fundef ->
                   let {fn_binder = f; fn_tyvars; fn_params = xs; fn_body; fn_closure = z;
                        fn_location; fn_unsafe} = fundef
                   in
                   let (o, z) = o#optionu (fun o -> o#binder) z in
                   let o, xs =
                     List.fold_right
                       (fun x (o, xs) ->
                          let (o, x) = o#binder x in
                            (o, x::xs))
                       xs
                       (o, []) in
                  let o, body, _ = o#computation fn_body in
                  let fundef = {fn_binder = f; fn_tyvars; fn_params = xs; fn_body = body; fn_closure = z;
                        fn_location; fn_unsafe}
                  in
                    o, fundef::defs)
                (o, [])
                defs in
            let defs = List.rev defs in
              o, Rec defs
        | Alien ({ binder; _ } as payload) ->
            let o, binder = o#binder binder in
            o, Alien { payload with binder}
        | Module (name, defs) ->
            let o, defs =
              match defs with
                | None -> o, None
                | Some defs ->
                    let o, defs = o#bindings defs
                    in
                      o, Some defs
            in
              o, Module (name, defs)

    method binder : binder -> ('self_type * binder) =
      fun b ->
      let var = Var.var_of_binder b in
      let t   = Var.type_of_binder b in
      let tyenv = Env.bind var t tyenv in
      {< tyenv=tyenv >}, b

    method program : program -> ('self_type * program * datatype) = o#computation

    method get_type_environment : environment = tyenv
  end
end




module Inline = struct
  let rec is_inlineable_value =
    function
      | v when is_atom v -> true
      | Project (_, v)
      | Erase (_, v)
      | Inject (_, v, _)
      | TAbs (_, v)
      | TApp (v, _) -> is_inlineable_value v
      | _ -> false

  let inliner tyenv env =
  object (o)
    inherit Transform.visitor(tyenv) as super

    val env = env

    method with_env env =
      {< env = env >}

    method! value =
      function
        | Variable var when IntMap.mem var env -> o, IntMap.find var env, o#lookup_type var
        | v -> super#value v

    method! bindings =
      function
        | b :: bs ->
            let o, b = o#binding b in
              begin
                match b with
                | Let (b, (tyvars, Return v)) when Var.(Scope.is_local (scope_of_binder b)) && is_inlineable_value v ->
                   let x = Var.var_of_binder b in
                   let v =
                     match tyvars with
                     | [] -> v
                     | tyvars -> TAbs (tyvars, v)
                   in
                   (o#with_env (IntMap.add x (snd3 (o#value v)) env))#bindings bs
                | _ ->
                   let o, bs = o#bindings bs in
                   o, b :: bs
              end
        | [] -> o, []
  end

  let name = "inline"

  let program state program =
    let open IrTransform in
    let tenv = Context.variable_environment (context state) in
    let _, program', _ = (inliner tenv IntMap.empty)#program program in
    return state program'
end

(*
  Eliminate dead functions and value bindings.

  Currently this is rather basic. It only does one pass, and it only
  eliminates variables in the following situations:

    - never used anywhere
    - only used recursively, but not mutually recursively
    - only used mutually recursively, and all the other mutually
    recursive bindings are only used mutually recursively

  If we partition mutually recursive bindings into strongly connected
  components beforehand then this will help eliminate more recursive
  bindings.

  A much more effective approach is to use one of Appel and Jim's
  algorithms described in `Shrinking lambda expressions in linear
  time'.

  They describe three algorithms. All of them eliminate all dead
  variables (as well as inlining linear variables, though that aspect
  is neither here nor there really).

  The naive algorithm gathers a census of variable counts, uses it to
  perform inlining, and is applied repeatedly until there are no dead
  variables left.

  The improved algorithm does the same, but updates the census as it
  goes along (e.g. whenever it deletes a function it passes over the
  body of the function and adjusts the census to take account of any
  uses of variables that have just been deleted).

  Both the naive algorithm and the improved algorithm are quadratic in
  the worst case, though the improved algorithm works quite well in
  practice. The improved algorithm is used in SML/NJ and MLton, and it
  used to be used in SML.NET. Appel and Jim suggest just bounding the
  number of times the improved algorithm is iterated rather than
  trying to run it exhaustively. In all but pathological cases this
  gets rid of most dead functions.

  The graphical algorithm depends on a graphical representation of
  terms (connecting definitions to uses of variables). It takes linear
  time and is the algorithm now used in SML.NET. It is extremely fast
  in practice and eliminates all dead variables in one
  pass. Unfortunately our terms are represented as trees, so we cannot
  use this algorithm here.
*)
module ElimDeadDefs = struct
  let show_rec_uses
    = Settings.(flag "show_rec_uses"
                |> convert parse_bool
                |> sync)

  let counter tyenv =
  object (o : 'self_type)
    inherit Transform.visitor(tyenv) as super

    val env = IntMap.empty
    val rec_env = IntMap.empty
    val mutrec_env = IntMap.empty

    method private with_env env =
      {< env = env >}

    method with_envs (env, rec_env, mutrec_env) =
      {< env = env; rec_env = rec_env; mutrec_env = mutrec_env >}

    method init b =
      let x = Var.var_of_binder b in
      o#with_env (IntMap.add x 0 env)

    method initrec b =
      let x = Var.var_of_binder b in
      o#with_envs (IntMap.add x 0 env, IntMap.add x (0, false) rec_env, IntMap.add x (0, true) mutrec_env)

    method set_rec_status f (r,m) =
      let (count, _) = IntMap.find f rec_env in
      let rec_env = IntMap.add f (count, r) rec_env in
      let (count, _) = IntMap.find f mutrec_env in
      let mutrec_env = IntMap.add f (count, m) mutrec_env in
        o#with_envs (env, rec_env, mutrec_env)

    method set_rec f =
      o#set_rec_status f (true, false)

    method set_mutrec f =
      o#set_rec_status f (false, true)

    method set_nonrec f =
      o#set_rec_status f (false, false)

    method set_nonrecs fs =
      IntSet.fold (fun f o -> o#set_nonrec f) fs o

    method inc x =
      if IntMap.mem x rec_env then
        let count = IntMap.find x env
        and rcount, ractive = IntMap.find x rec_env
        and mcount, mactive = IntMap.find x mutrec_env in
        let envs =
          match ractive, mactive with
            | false, false -> IntMap.add x (count+1) env, rec_env, mutrec_env
            | true, false -> env, IntMap.add x (rcount+1, ractive) rec_env, mutrec_env
            | false, true -> env, rec_env, IntMap.add x (mcount+1, mactive) mutrec_env
            | true, true -> assert false
        in
          o#with_envs envs
      else if IntMap.mem x env then
        o#with_env (IntMap.add x ((IntMap.find x env)+1) env)
      else
        o#with_env (IntMap.add x 1 env)

    method! var =
      fun x ->
        if IntMap.mem x env then
          o#inc x, x, o#lookup_type x
        else
          super#var x

    method! binding b =
      match b with
        | Let (x, (_, Return _)) ->
            let o, b = super#binding b in
              o#init x, b
        | Fun {fn_binder = f; _} ->
            let o, b = super#binding b in
              o#init f, b
        | Rec defs ->
            let o, fs =
              List.fold_right
                (fun {fn_binder = f; _} (o, fs) ->
                   let o, f = o#binder f in
                     (o#initrec f, IntSet.add (Var.var_of_binder f) fs))
                defs
                (o, IntSet.empty) in

            let o, defs =
              List.fold_left
                (fun ((o : 'self_type), defs) fundef ->
                  let {fn_binder = f; fn_tyvars; fn_params = xs; fn_body; fn_closure = z;
                        fn_location; fn_unsafe} = fundef
                   in
                   let o, z = o#optionu (fun o -> o#binder) z in
                   let o, xs =
                     List.fold_right
                       (fun x (o, xs) ->
                          let (o, x) = o#binder x in
                            (o, x::xs))
                       xs
                       (o, []) in
                   let o = o#set_rec (Var.var_of_binder f) in
                   let o, body, _ = o#computation fn_body in
                   let o = o#set_mutrec (Var.var_of_binder f) in
                   let fundef = {fn_binder = f; fn_tyvars; fn_params = xs; fn_body = body; fn_closure = z;
                        fn_location; fn_unsafe}
                   in
                     o, fundef::defs)
                (o, [])
                defs in
            let o = o#set_nonrecs fs in
            let defs = List.rev defs in
              o, Rec defs
        | _ ->
            super#binding b

    method get_envs () = (env, rec_env, mutrec_env)
  end

  let eliminator tyenv (env, rec_env, mutrec_env) =
  object (o)
    inherit Transform.visitor(tyenv)

    val env = env
    val rec_env = rec_env
    val mutrec_env = mutrec_env

    method is_dead x =
      IntMap.mem x env && (IntMap.find x env = 0)

    method is_dead_rec f =
      IntMap.mem f env && (IntMap.find f env = 0
          && (not (IntMap.mem f mutrec_env) || fst (IntMap.find f mutrec_env) = 0))

    method! bindings =
      function
        | b :: bs ->
            begin
              let o, b = o#binding b in
                match b with
                  | Let (b, (_tyvars, _)) when o#is_dead (Var.var_of_binder b) ->
                      o#bindings bs
                  | Fun {fn_binder = b; _} when o#is_dead (Var.var_of_binder b) ->
                      o#bindings bs
                  | Rec defs ->
                      Debug.if_set show_rec_uses (fun () -> "Rec block:");
                      let fs, defs =
                        List.fold_left
                          (fun (fs, defs) ({fn_binder = b; _} as def) ->
                            let f = Var.var_of_binder b in
                            let name = Var.name_of_binder b in
                             Debug.if_set show_rec_uses
                               (fun () ->
                                  "  (" ^ name ^ ") non-rec uses: "^string_of_int (IntMap.find f env)^
                                    ", rec uses: "^string_of_int (fst (IntMap.find f rec_env))^
                                    ", mut-rec uses: "^string_of_int (fst (IntMap.find f mutrec_env)));
                             if o#is_dead_rec f then fs, defs
                             else IntSet.add f fs, def :: defs)
                          (IntSet.empty, [])
                          defs
                      in

                      (*
                         If none of the mutually recursive bindings appear elsewhere
                         then we can delete them all.
                      *)
                      let defs =
                        if IntSet.for_all o#is_dead fs then []
                        else
                          List.rev defs
                      in
                        begin
                          match defs with
                            | [] -> o#bindings bs
                            | defs ->
                                let o, bs = o#bindings bs in
                                  o, Rec defs :: bs
                        end
                  | _ ->
                      let o, bs = o#bindings bs in
                        o, b :: bs
            end
        | [] -> o, []
  end

  let name = "elim_dead_defs"

  let program state program =
    let open IrTransform in
    let tenv = Context.variable_environment (context state) in
    let o, _, _ = (counter tenv)#program program in
    let envs = o#get_envs () in
    let _, program', _ = (eliminator tenv envs)#program program in
    return state program'
end

(** Applies a type visitor to all types occuring in an IR program
    Note that the "roots" of type objects found in the AST are types, quantifiers, or type arguments
**)
let ir_type_mod_visitor tyenv type_visitor =
  object
    inherit Transform.visitor(tyenv) as super
          method! value = function
            | Inject (name, value, datatype) ->
               let (_, datatype) = type_visitor#typ datatype in
               super#value (Inject (name, value, datatype))
            | TAbs (tyvars, value) ->
               let tyvars = List.map (fun arg -> snd (type_visitor#quantifier arg)) tyvars in
               super#value (TAbs (tyvars, value))
            | TApp (value, tyargs) ->
               let tyargs = List.map (fun arg -> snd (type_visitor#type_arg arg)) tyargs in
               super#value (TApp (value, tyargs))
            | Coerce (var, datatype) ->
               let (_, datatype) = type_visitor#typ datatype in
               super#value (Coerce (var, datatype))
            | Closure (var, tyargs, env) ->
              let tyargs = List.map (fun targ -> snd (type_visitor#type_arg targ)) tyargs in
              super#value (Closure (var, tyargs, env))
            | other -> super#value other

          method! special = function
            | Wrong datatype ->
               let (_, datatype) = type_visitor#typ datatype in
               super#special (Wrong datatype)
            | Table { database; table; keys; temporal_fields; table_type = (tmp, t1, t2, t3) } ->
               let (_, t1) = type_visitor#typ t1 in
               let (_, t2) = type_visitor#typ t2 in
               let (_, t3) = type_visitor#typ t3 in
               super#special (Table { database; table; keys; temporal_fields; table_type = (tmp, t1, t2, t3) })
            | Query (opt, policy, computation, datatype) ->
               let (_, datatype) = type_visitor#typ datatype in
               super#special (Query (opt, policy, computation, datatype))
            | DoOperation (name, vallist, datatype) ->
               let (_, datatype) = type_visitor#typ datatype in
               super#special (DoOperation (name, vallist, datatype))
            | other -> super#special other

          method! binder b =
            let (_, newtype) = type_visitor#typ (Var.type_of_binder b) in
            let b = Var.update_type newtype b in
            super#binder b


  end


(* Debugging traversal that checks if we have eliminated all cyclic recursive types *)
module CheckForCycles = struct

    let check_cycles =
      object (o: 'self_type)
         inherit Types.Transform.visitor as super
         val mu_vars = Utility.IntSet.empty (* Int Utility.IntSet*)

         val seen_types = []
         val seen_rows = []

         method typ_super = super#typ
         method row_super = super#row

         method! typ t =
           (* Debug.print ("Method typ, mu_vars is " ^ Utility.IntSet.show mu_vars); *)
           match List.assoc_opt t seen_types with
           | Some _ ->
               raise (
                Errors.internal_error
                  ~filename:"irTraversals.ml"
                  ~message:"descending into type cycle")
           | None ->
              let o' = {<seen_types =  (t,()) :: seen_types>} in
              let (_, t) = o'#typ_super t in
              (o, t)

         method! row r =
           match List.assoc_opt r seen_rows with
           | Some _ ->
               raise (Errors.internal_error
                 ~filename:"irTraversals.ml"
                 ~message:"descending into row cycle")
           | None ->
              let o' = {<seen_rows =  (r,()) :: seen_rows>} in
              let (_, r) = o'#row_super r in
              (o, r)

      end

    let name = "check_for_cycles"
    let program state program =
      let open IrTransform in
      let tenv = Context.variable_environment (context state) in
      let _,program', _ = (ir_type_mod_visitor tenv check_cycles)#program program in
      return state program'

  end


module ElimBodiesFromMetaTypeVars = struct

    let elim_bodies =
      object (o)
        inherit Types.Transform.visitor as super

        method! typ = function
          | Types.Meta point ->
            begin
              match Unionfind.find point with
                | Types.Recursive _
                | Types.Var _ -> o, Types.Meta point
                | t -> o#typ t
            end
          | other -> super#typ other
      end


    let name = "elim_bodies_from_meta_type_vars"

    let program state program =
      let open IrTransform in
      let tenv = Context.variable_environment (context state) in
      let _, program', _ = (ir_type_mod_visitor tenv elim_bodies)#program program in
      return state program'
end

module ElimTypeAliases = struct
    let elim_type_aliases =
      object (o)
        inherit Types.Transform.visitor as super

        method! typ = function
          | Types.Alias (_, _, typ) -> o#typ typ
          | other -> super#typ other
      end

    let name = "elim_type_aliases"
    let program state program =
      let open IrTransform in
      let tenv = Context.variable_environment (context state) in
      let _, program', _ = (ir_type_mod_visitor tenv elim_type_aliases)#program program in
      return state program'

end


(* Call Instantiate.datatype on all types occuring in a program *)
module InstantiateTypes = struct

    let instantiate instantiation_maps =
      object (o)
        inherit Types.Transform.visitor

        method! typ t =
          match t with
            | Types.Not_typed -> (o, t) (* instantiate.ml dies on `Not_typed *)
            | _ -> (o, Instantiate.datatype instantiation_maps t)

        method! row r = o, Instantiate.row instantiation_maps r

        method! field_spec p = o, Instantiate.presence instantiation_maps p

      end

    let computation tyenv instantiation_maps c  =
      let _, p, _ = (ir_type_mod_visitor tyenv (instantiate instantiation_maps))#computation c in
      p
end
