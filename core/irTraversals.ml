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
module type IR_VISITOR =
sig
  type environment = Types.datatype Env.Int.t

  class visitor : environment ->
  object ('self_type)
    val tyenv : environment

    method lookup_type : var -> Types.datatype
    method constant : Constant.t -> (Constant.t * Types.datatype * 'self_type)
    method optionu :
      'a.
      ('self_type -> 'a -> ('a * 'self_type)) ->
      'a option -> 'a option * 'self_type
    method option :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a option -> 'a option * Types.datatype option * 'self_type
    method list :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a list -> 'a list * Types.datatype list * 'self_type
    method name_map :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a name_map -> 'a name_map * Types.datatype name_map * 'self_type
    method var_map :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a var_map -> 'a var_map * Types.datatype var_map * 'self_type
    method var : var -> (var * Types.datatype * 'self_type)
    (* method closure_var : var -> (var * Types.datatype * 'self_type) *)
    method value : value -> (value * Types.datatype * 'self_type)

    method tail_computation :
      tail_computation -> (tail_computation * Types.datatype * 'self_type)
    method special : special -> (special * Types.datatype * 'self_type)
    method bindings : binding list -> (binding list * 'self_type)
    method computation : computation -> (computation * Types.datatype * 'self_type)
    method binding : binding -> (binding * 'self_type)
    method binder : binder -> (binder * 'self_type)
    (* method closure_binder : binder -> (binder * 'self_type) *)

    method program : program -> (program * Types.datatype * 'self_type)

    method get_type_environment : environment
  end
end

module type PROGTRANSFORM =
sig
   val program : Types.datatype Env.Int.t -> program -> program
   val bindings : Types.datatype Env.Int.t -> binding list -> binding list
end

module Transform : IR_VISITOR =
struct
  open Types
  open TypeUtils

  type environment = datatype Env.Int.t

  let info_type (t, _, _) = t

  let deconstruct f t = f t

  module Env = Env.Int

  class visitor (tyenv : environment) =
  object ((o : 'self_type))
    val tyenv = tyenv
    (* val cenv = Env.empty *)

    method lookup_type : var -> datatype = fun var ->
      Env.lookup tyenv var

    (* method private lookup_closure_type : var -> datatype = fun var -> *)
    (*   Env.lookup cenv var *)

    method constant : Constant.t -> (Constant.t * datatype * 'self_type) = fun c ->
      match c with
        | Constant.Bool   _ -> c, bool_type  , o
        | Constant.Int    _ -> c, int_type   , o
        | Constant.Char   _ -> c, char_type  , o
        | Constant.String _ -> c, string_type, o
        | Constant.Float  _ -> c, float_type , o

    method optionu :
      'a.
      ('self_type -> 'a -> ('a * 'self_type)) ->
      'a option -> 'a option * 'self_type =
      fun f v ->
        match v with
          | None -> None, o
          | Some v ->
              let v, o = f o v in
                Some v, o

    method option :
      'a.
      ('self_type -> 'a -> ('a * datatype * 'self_type)) ->
      'a option -> 'a option * datatype option * 'self_type =
      fun f v ->
        match v with
          | None -> None, None, o
          | Some v ->
              let v, t, o = f o v in
                Some v, Some t, o

    method list :
      'a.
      ('self_type -> 'a -> ('a * datatype * 'self_type)) ->
      'a list -> 'a list * datatype list * 'self_type =
      fun f v ->
        let vs, ts, o =
          List.fold_left
            (fun (vs, ts, o) v ->
               let (v, t, o) = f o v in
                 v::vs, t::ts, o)
            ([], [], o)
            v
        in
          List.rev vs, List.rev ts, o

    method name_map :
      'a.
      ('self_type -> 'a -> ('a * datatype * 'self_type)) ->
      'a name_map -> 'a name_map * datatype name_map * 'self_type =
      fun f vmap ->
        StringMap.fold
          (fun name v (vmap, tmap, o) ->
             let (v, t, o) = f o v in
               (StringMap.add name v vmap,
                StringMap.add name t tmap,
                o))
          vmap
          (StringMap.empty, StringMap.empty, o)

    method var_map :
      'a.
      ('self_type -> 'a -> ('a * datatype * 'self_type)) ->
      'a var_map -> 'a var_map * datatype var_map * 'self_type =
      fun f vmap ->
        IntMap.fold
          (fun name v (vmap, tmap, o) ->
             let (v, t, o) = f o v in
               (IntMap.add name v vmap,
                IntMap.add name t tmap,
                o))
          vmap
          (IntMap.empty, IntMap.empty, o)

    method var : var -> (var * datatype * 'self_type) =
      fun var -> (var, o#lookup_type var, o)

    (* method closure_var : var -> (var * datatype * 'self_type) = *)
    (*   fun var -> (var, o#lookup_closure_type var, o) *)

    method value : value -> (value * datatype * 'self_type) =
      function
        | Ir.Constant c -> let (c, t, o) = o#constant c in Ir.Constant c, t, o
        | Variable x -> let (x, t, o) = o#var x in Ir.Variable x, t, o
        (* | ClosureVar x -> let (x, t, o) = o#closure_var x in ClosureVar x, t, o *)
        | Extend (fields, base) ->
            let (fields, field_types, o) = o#name_map (fun o -> o#value) fields in
            let (base, base_type, o) = o#option (fun o -> o#value) base in

            let t =
              match base_type with
                | None -> make_record_type field_types
                | Some t ->
                    begin
                      match TypeUtils.concrete_type t with
                        | `Record row ->
                            `Record (extend_row field_types row)
                        | _ -> assert false
                    end
            in
              Extend (fields, base), t, o
        | Project (name, v) ->
            let (v, vt, o) = o#value v in
              Project (name, v), deconstruct (project_type name) vt, o
        | Erase (names, v) ->
            let (v, vt, o) = o#value v in
            let t = deconstruct (erase_type names) vt in
              Erase (names, v), t, o
        | Inject (name, v, t) ->
            let v, _vt, o = o#value v in
              Inject (name, v, t), t, o
        | TAbs (tyvars, v) ->
            let v, t, o = o#value v in
            let t = Types.for_all (tyvars, t) in
              TAbs (tyvars, v), t, o
        | TApp (v, ts) ->
            let v, t, o = o#value v in
              begin try
                let t = Instantiate.apply_type t ts in
                  TApp (v, ts), t, o
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
            let (attributes, _, o) = o#name_map (fun o -> o#value) attributes in
            let (children  , _, o) = o#list (fun o -> o#value) children in

              (*
                let _ = assert (StringMap.for_all (fun t -> t=string_type) attribute_types) in
                let _ = assert (List.for_all (fun t -> t=xml_type) children_types) in
              *)
              XmlNode (tag, attributes, children), xml_type, o
        | ApplyPure (f, args) ->
            let (f, ft, o) = o#value f in
            let (args, _, o) = o#list (fun o -> o#value) args in
              (* TODO: check arg types match *)
              ApplyPure (f, args), deconstruct (return_type ~overstep_quantifiers:true) ft, o

        | Closure (f, tyargs, z) ->
            let (f, t, o) = o#var f in
            let t =
              match tyargs with
                | [] -> t
                | _ ->
                  let (remaining_type, instantiation_maps) = Instantiate.instantiation_maps_of_type_arguments false t tyargs in
                  Instantiate.datatype instantiation_maps remaining_type in
            let (z, _, o) = o#value z in
              (* TODO: check that closure environment types match expectations for f *)
              Closure (f, tyargs, z), t, o
        | Coerce (v, t) ->
            let v, _, o = o#value v in
            (* TODO: check that vt <: t *)
              Coerce (v, t), t, o

    method tail_computation :
      tail_computation -> (tail_computation * datatype * 'self_type) =
      function
          (* TODO: type checking *)
        | Return v ->
            let v, t, o = o#value v in
              Return v, t, o
        | Apply (f, args) ->
            let f, ft, o = o#value f in
            let args, _, o = o#list (fun o -> o#value) args in
              (* TODO: check arg types match *)
              Apply (f, args), deconstruct (return_type ~overstep_quantifiers:true) ft, o
        (* | ApplyClosure (f, args) -> *)
        (*     let f, ft, o = o#value f in *)
        (*     let args, arg_types, o = o#list (fun o -> o#value) args in *)
        (*     (\* TODO: check arg types match *\) *)
        (*     (\* TOOD: add closure type *\) *)
        (*       ApplyClosure (f, args), deconstruct return_type ft, o *)
        | Special special ->
            let special, t, o = o#special special in
              Special special, t, o

        | Ir.Case (v, cases, default) ->
            let v, _, o = o#value v in
            let cases, case_types, o =
              o#name_map
                (fun o (b, c) ->
                   let b, o = o#binder b in
                   let c, t, o = o#computation c in
                     (b, c), t, o) cases in
            let default, default_type, o =
              o#option (fun o (b, c) ->
                          let b, o = o#binder b in
                          let c, t, o = o#computation c in
                            (b, c), t, o) default in
            let t =
              if not (StringMap.is_empty case_types) then
                (StringMap.to_alist ->- List.hd ->- snd) case_types
              else
                val_of default_type
            in
              Ir.Case (v, cases, default), t, o
        | If (v, left, right) ->
            let v, _, o = o#value v in
            let left, t, o = o#computation left in
            let right, _, o = o#computation right in
              If (v, left, right), t, o

    method special : special -> (special * datatype * 'self_type) =
      function
        | Wrong t -> Wrong t, t, o
        | Database v ->
            let v, _, o = o#value v in
              Database v, `Primitive Primitive.DB, o
        | Table (db, table_name, keys, tt) ->
            let db, _, o = o#value db in
            let keys, _, o = o#value keys in
            let table_name, _, o = o#value table_name in
              Table (db, table_name, keys, tt), `Table tt, o
        | Lens (table, rtype) ->
            let table, _, o = o#value table in
              Lens (table, rtype), `Lens rtype, o
        | LensDrop {lens; drop; key; default; typ} ->
            let lens, _, o = o#value lens in
            let default, _, o = o#value default in
              LensDrop {lens; drop; key; default; typ}, `Lens typ, o
        | LensSelect {lens; predicate; typ} ->
            let lens, _, o = o#value lens in
            let predicate, o =
              (match predicate with
               | Dynamic predicate ->
                 let predicate, _, o = o#value predicate in
                 Dynamic predicate, o
              | Static predicate -> Static predicate, o) in
              LensSelect {lens; predicate; typ}, `Lens typ, o
        | LensJoin {left; right; on; del_left; del_right; typ} ->
            let left, _, o = o#value left in
            let right, _, o = o#value right in
              LensJoin {left; right; on; del_left; del_right; typ}, `Lens typ, o
        | LensCheck (lens, t) ->
            let lens, _, o = o#value lens in
              LensCheck (lens, t), `Lens t, o
        | LensGet (lens, rtype) ->
            let lens, _, o = o#value lens in
              LensGet (lens, rtype), Types.make_list_type rtype, o
        | LensPut (lens, data, rtype) ->
            let lens, _, o = o#value lens in
            let data, _, o = o#value data in
            LensPut (lens, data, rtype), Types.make_tuple_type [], o
        | Query (range, e, _) ->
            let range, o =
              o#optionu
                (fun o (limit, offset) ->
                   let limit, _, o = o#value limit in
                   let offset, _, o = o#value offset in
                     (limit, offset), o)
                range in
            let e, t, o = o#computation e in
              Query (range, e, t), t, o
        | InsertRows (source, rows) ->
            let source, _, o = o#value source in
	    let rows, _, o = o#value rows in
              InsertRows(source, rows), Types.unit_type, o
        | InsertReturning (source, rows, returning) ->
            let source, _, o = o#value source in
	    let rows, _, o = o#value rows in
	    let returning, _, o = o#value returning in
              InsertReturning(source, rows, returning), Types.unit_type, o
        | Update ((x, source), where, body) ->
            let source, _, o = o#value source in
            let x, o = o#binder x in
            let where, _, o = o#option (fun o -> o#computation) where in
            let body, _, o = o#computation body in
              Update ((x, source), where, body), Types.unit_type, o
        | Delete ((x, source), where) ->
            let source, _, o = o#value source in
            let x, o = o#binder x in
            let where, _, o = o#option (fun o -> o#computation) where in
              Delete ((x, source), where), Types.unit_type, o
        | CallCC v ->
            let v, t, o = o#value v in
              CallCC v, deconstruct (return_type ~overstep_quantifiers:true) t, o
        | Select (l, v) ->
           let v, t, o = o#value v in
           Select (l, v), t, o
        | Choice (v, bs) ->
           let v, _, o = o#value v in
           let bs, branch_types, o =
             o#name_map (fun o (b, c) ->
                         let b, o = o#binder b in
                         let c, t, o = o#computation c in
                         (b, c), t, o) bs in
           let t = (StringMap.to_alist ->- List.hd ->- snd) branch_types in
           Choice (v, bs), t, o
    | Handle ({ ih_comp; ih_cases; ih_return; ih_depth }) ->
       let (comp, _, o) = o#computation ih_comp in
           (* TODO FIXME traverse parameters *)
           let (depth, o) =
             match ih_depth with
             | Deep params ->
                let (o, bindings) =
                  List.fold_left
                    (fun (o, bvs) (b,v) ->
                      let (b, o) = o#binder b in
                      let (v, _, o) = o#value v in
                      (o, (b,v) :: bvs))
                    (o, []) params
                in
                Deep (List.rev bindings), o
             | Shallow -> Shallow, o
           in
       let (cases, _branch_types, o) =
         o#name_map
               (fun o (x, resume, c) ->
                 let (x, o) = o#binder x in
         let (resume, o) = o#binder resume in
         let (c, t, o) = o#computation c in
         (x, resume, c), t, o)
           ih_cases
       in
           let (return, t, o) =
             let (b, o) = o#binder (fst ih_return) in
             let (comp, t, o) = o#computation (snd ih_return) in
             (b, comp), t, o
           in
       Handle { ih_comp = comp; ih_cases = cases; ih_return = return; ih_depth = depth}, t, o
    | DoOperation (name, vs, t) ->
       let (vs, _, o) = o#list (fun o -> o#value) vs in
       (DoOperation (name, vs, t), t, o)

   method bindings : binding list -> (binding list * 'self_type) =
      fun bs ->
        let bs, o =
          List.fold_left
            (fun (bs, o) b ->
               let (b, o) = o#binding b in
                 (b::bs, o))
            ([], o)
            bs
        in
          List.rev bs, o

    method computation : computation -> (computation * datatype * 'self_type) =
      fun (bs, tc) ->
        let bs, o = o#bindings bs in
        let tc, t, o = o#tail_computation tc in
          (bs, tc), t, o

    method binding : binding -> (binding * 'self_type) =
      function
        | Let (x, (tyvars, tc)) ->
            let x, o = o#binder x in
            let tc, _, o = o#tail_computation tc in
              Let (x, (tyvars, tc)), o
        | Fun (f, (tyvars, xs, body), z, location) ->
            let xs, body, z, o =
              let (z, o) = o#optionu (fun o -> o#binder) z in
              let (xs, o) =
                List.fold_right
                  (fun x (xs, o) ->
                     let x, o = o#binder x in
                       (x::xs, o))
                  xs
                  ([], o) in
              let body, _, o = o#computation body in
                xs, body, z, o in
            let f, o = o#binder f in
              (* TODO: check that xs and body match up with f *)
              Fun (f, (tyvars, xs, body), z, location), o
        | Rec defs ->
            (* it's important to traverse the function binders first in
               order to make sure they're in scope for all of the
               function bodies *)
            let defs, o =
              List.fold_right
                (fun (f, (tyvars, xs, body), z, location) (fs, o) ->
                   let f, o = o#binder f in
                     ((f, (tyvars, xs, body), z, location)::fs, o))
                defs
                ([], o) in

            let defs, o =
              List.fold_left
                (fun (defs, (o : 'self_type)) (f, (tyvars, xs, body), z, location) ->
                   let (z, o) = o#optionu (fun o -> o#binder) z in
                   let xs, o =
                     List.fold_right
                       (fun x (xs, o) ->
                          let (x, o) = o#binder x in
                            (x::xs, o))
                       xs
                       ([], o) in
                  let body, _, o = o#computation body in
                    (f, (tyvars, xs, body), z, location)::defs, o)
                ([], o)
                defs in
            let defs = List.rev defs in
              Rec defs, o
        | Alien (x, name, language) ->
            let x, o = o#binder x in
              Alien (x, name, language), o
        | Module (name, defs) ->
            let defs, o =
              match defs with
                | None -> None, o
                | Some defs ->
                    let defs, o = o#bindings defs
                    in
                      Some defs, o
            in
              Module (name, defs), o

    method binder : binder -> (binder * 'self_type) =
      fun (var, info) ->
        let tyenv = Env.bind tyenv (var, info_type info) in
          (var, info), {< tyenv=tyenv >}

    method program : program -> (program * datatype * 'self_type) = o#computation

    method get_type_environment : environment = tyenv
  end
end




module Inline =
struct
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
        | Variable var when IntMap.mem var env -> IntMap.find var env, o#lookup_type var, o
        | v -> super#value v

    method! bindings =
      function
        | b :: bs ->
            let b, o = o#binding b in
              begin
                match b with
                  | Let ((x, (_, _, Scope.Local)), (tyvars, Return v)) when is_inlineable_value v ->
                      let v =
                        match tyvars with
                          | [] -> v
                          | tyvars -> TAbs (tyvars, v)
                      in
                        (o#with_env (IntMap.add x (fst3 (o#value v)) env))#bindings bs
                  | _ ->
                      let bs, o = o#bindings bs in
                        b :: bs, o
              end
        | [] -> [], o
  end

  let program typing_env p =
    fst3 ((inliner typing_env IntMap.empty)#computation p)

  let bindings typing_env p =
    fst ((inliner typing_env IntMap.empty)#bindings p)
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
module ElimDeadDefs =
struct
  let show_rec_uses = Basicsettings.Ir.show_rec_uses

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

    method init (x, _) =
      o#with_env (IntMap.add x 0 env)

    method initrec (x, _) =
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
          x, o#lookup_type x, o#inc x
        else
          super#var x

    method! binding b =
      match b with
        | Let (x, (_, Return _)) ->
            let b, o = super#binding b in
              b, o#init x
        | Fun (f, _, _, _) ->
            let b, o = super#binding b in
              b, o#init f
        | Rec defs ->
            let fs, o =
              List.fold_right
                (fun (f, _, _, _) (fs, o) ->
                   let f, o = o#binder f in
                     (IntSet.add (Var.var_of_binder f) fs, o#initrec f))
                defs
                (IntSet.empty, o) in

            let defs, o =
              List.fold_left
                (fun (defs, (o : 'self_type)) (f, (tyvars, xs, body), z, location) ->
                   let z, o = o#optionu (fun o -> o#binder) z in
                   let xs, o =
                     List.fold_right
                       (fun x (xs, o) ->
                          let (x, o) = o#binder x in
                            (x::xs, o))
                       xs
                       ([], o) in
                   let o = o#set_rec (Var.var_of_binder f) in
                   let body, _, o = o#computation body in
                   let o = o#set_mutrec (Var.var_of_binder f) in
                     (f, (tyvars, xs, body), z, location)::defs, o)
                ([], o)
                defs in
            let o = o#set_nonrecs fs in
            let defs = List.rev defs in
              Rec defs, o
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
              let b, o = o#binding b in
                match b with
                  | Let ((x, _), (_tyvars, _)) when o#is_dead x ->
                      o#bindings bs
                  | Fun ((f, _), _, _, _) when o#is_dead f ->
                      o#bindings bs
                  | Rec defs ->
                      Debug.if_set show_rec_uses (fun () -> "Rec block:");
                      let fs, defs =
                        List.fold_left
                          (fun (fs, defs) (((f, (_, name, _)), _, _, _) as def) ->
                             Debug.if_set show_rec_uses
                               (fun () ->
                                  "  (" ^ name ^ ") non-rec uses: "^string_of_int (IntMap.find f env)^
                                    ", rec uses: "^string_of_int (fst (IntMap.find f rec_env))^
                                    ", mut-rec uses: "^string_of_int (fst (IntMap.find f mutrec_env)));
                             if o#is_dead_rec f then fs, defs
                             else
                               IntSet.add f fs, def :: defs)
                          (IntSet.empty, [])
                          defs in

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
                                let bs, o = o#bindings bs in
                                  Rec defs :: bs, o
                        end
                  | _ ->
                      let bs, o = o#bindings bs in
                        b :: bs, o
            end
        | [] -> [], o
  end

  let program tyenv p =
    let _, _, o = (counter tyenv)#computation p in
    let envs = o#get_envs () in
    let p, _, _ = (eliminator tyenv envs)#computation p in
      p

  let bindings tyenv bs =
    let _, o = (counter tyenv)#bindings bs in
    let envs = o#get_envs () in
    let bs, _ = (eliminator tyenv envs)#bindings bs in
      bs
end

(** Applies a type visitor to all types occuring in an IR program**)
let ir_type_mod_visitor tyenv type_visitor =
  object
    inherit Transform.visitor(tyenv) as super
          method! value = function
            | Inject (name, value, datatype) ->
               let (datatype, _) = type_visitor#typ datatype in
               super#value (Inject (name, value, datatype))
            | TAbs (tyvars, value) ->
               let tyvars = List.map (fun arg -> fst (type_visitor#quantifier arg)) tyvars in
               super#value (TAbs (tyvars, value))
            | TApp (value, tyargs) ->
               let tyargs = List.map (fun arg -> fst (type_visitor#type_arg arg)) tyargs in
               super#value (TApp (value, tyargs))
            | Coerce (var, datatype) ->
               let (datatype, _) = type_visitor#typ datatype in
               super#value (Coerce (var, datatype))
            | Closure (var, tyargs, env) ->
              let tyargs = List.map (fun targ -> fst (type_visitor#type_arg targ)) tyargs in
              super#value (Closure (var, tyargs, env))
            | other -> super#value other

          method! special = function
            | Wrong datatype ->
               let (datatype, _) = type_visitor#typ datatype in
               super#special (Wrong datatype)
            | Table (v1, v2, v3, (t1, t2, t3)) ->
               let (t1, _) = type_visitor#typ t1 in
               let (t2, _) = type_visitor#typ t2 in
               let (t3, _) = type_visitor#typ t3 in
               super#special (Table (v1, v2, v3, (t1, t2, t3)))
            | Query (opt, computation, datatype) ->
               let (datatype, _) = type_visitor#typ datatype in
               super#special (Query (opt, computation, datatype))
            | DoOperation (name, vallist, datatype) ->
               let (datatype, _) = type_visitor#typ datatype in
               super#special (DoOperation (name, vallist, datatype))
            | other -> super#special other

          method! binder b =
            let (newtype, _) = type_visitor#typ (Var.type_of_binder b) in
            let b = Var.update_type newtype b in
            super#binder b


  end


(* Debugging traversal that checks if we have eliminated all cyclic recursive types *)
module CheckForCycles =
  struct

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
              let (t, _) = o'#typ_super t in
              (t, o)

         method! row r =
           match List.assoc_opt r seen_rows with
           | Some _ ->
               raise (Errors.internal_error
                 ~filename:"irTraversals.ml"
                 ~message:"descending into row cycle")
           | None ->
              let o' = {<seen_rows =  (r,()) :: seen_rows>} in
              let (r,_) = o'#row_super r in
              (r, o)

      end


    let program tyenv p =
      let p, _, _ = (ir_type_mod_visitor tyenv check_cycles)#program p in
      p

    let bindings tyenv bs =
      let bs, _ = (ir_type_mod_visitor tyenv check_cycles)#bindings bs in
      bs

  end


module ElimBodiesFromMetaTypeVars =
  struct

    let elim_bodies =
      object (o)
        inherit Types.Transform.visitor as super

        method! typ = function
          | `MetaTypeVar point ->
          begin
            match Unionfind.find point with
              | `Body t ->
                  o#typ t
              | _ -> `MetaTypeVar point, o
          end
          | other -> super#typ other
      end


    let program tyenv p =
      let p, _, _ = (ir_type_mod_visitor tyenv elim_bodies)#program p in
      p

    let bindings tyenv bs =
      let bs, _ = (ir_type_mod_visitor tyenv elim_bodies)#bindings bs in
      bs

  end

module ElimTypeAliases =
  struct

    let elim_type_aliases =
      object (o)
        inherit Types.Transform.visitor as super

        method! typ = function
          | `Alias (_, typ) -> o#typ typ
          | other -> super#typ other
      end


    let program tyenv p =
      let p, _, _ = (ir_type_mod_visitor tyenv elim_type_aliases)#program p in
      p

    let bindings tyenv bs =
      let bs, _ = (ir_type_mod_visitor tyenv elim_type_aliases)#bindings bs in
      bs

  end


(* Call Instantiate.datatype on all types occuring in a program *)
module InstantiateTypes =
  struct

    let instantiate instantiation_maps =
      object (o)
        inherit Types.Transform.visitor

        method! typ t =
          match t with
            | `Not_typed -> (t, o) (* instantiate.ml dies on `Not_typed *)
            | _ -> (Instantiate.datatype instantiation_maps t, o)


      end

    let computation tyenv instantiation_maps c  =
      let p, _, _ = (ir_type_mod_visitor tyenv (instantiate instantiation_maps))#computation c in
      p

end
