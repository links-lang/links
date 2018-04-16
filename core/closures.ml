open Utility
open Ir

type fenv = (Ir.binder list) IntMap.t
    [@@deriving show]

module ClosureVars =
struct
  (* The object of this visitor is to compute the non-global free
     variables for each function so that we can subsequently perform
     closure conversion. These are accumulated in fenv, which maps
     each non-global function f to a list of its non-global free
     variables. *)
  class visitor tenv globals =
    object (o : 'self) inherit Transform.visitor(tenv) as super
      val globals = globals
      val bound_vars = IntSet.empty
      val free_vars = IntSet.empty

      val fenv : (Ir.binder list) IntMap.t = IntMap.empty

      method register_fun f (zs : binder list) =
        {< fenv = IntMap.add f zs fenv >}

      method global x =
        {< globals = IntSet.add x globals >}

      method bound x =
        {< bound_vars = IntSet.add x bound_vars >}

      (* recursively gather free variables required by inner closures *)
      method close x =
        if IntSet.mem x bound_vars then
          if IntMap.mem x fenv then
            let zs = IntMap.find x fenv in
            List.fold_left
              (fun o (z, _) -> o#close z)
              o
              zs
          else
            o
        else
          begin
            (* Debug.print ("free var: "^string_of_int x); *)
            {< free_vars = IntSet.add x free_vars >}
          end

      method register_var x =
        if IntSet.mem x globals then
          o
        else
          o#close x

      method private reset =
        {< bound_vars = IntSet.empty; free_vars = IntSet.empty >}
      method set bound_vars free_vars =
        {< bound_vars = bound_vars; free_vars = free_vars >}

      method get_bound_vars = bound_vars
      method get_free_vars = free_vars

      method get_fenv = fenv

      method! var =
        fun var ->
          let var, t, o = super#var var in
          var, t, o#register_var var

      method! binder ((_, (_, _, scope)) as b) =
        let b, o = super#binder b in
        match scope with
        | `Global -> b, o#global (Var.var_of_binder b)
        | `Local  -> b, o#bound (Var.var_of_binder b)

      method private super_binding = super#binding

      method super_binder = super#binder

      method! binding =
        function
        | (`Fun (f, (tyvars, xs, body), None, location)) as b when Ir.binding_scope b = `Local ->
          (* reset free and bound variables to be empty *)
          let o = o#reset in
          let (xs, o) =
            List.fold_right
              (fun x (xs, o) ->
                 let x, o = o#binder x in
                 (x::xs, o))
              xs
              ([], o) in
          (* Debug.print("Descending into: " ^ string_of_int (Var.var_of_binder f)); *)
          let body, _, o = o#computation body in
          (* Debug.print("Ascended from: " ^ string_of_int (Var.var_of_binder f)); *)

          let zs =
            List.rev
              (IntSet.fold
                 (fun x zs ->
                    (x, (o#lookup_type x, "fv_" ^ string_of_int x, `Local))::zs)
                 (o#get_free_vars)
                 []) in
          (* restore free and bound variables *)
          let o = o#set bound_vars free_vars in

          let f, o = o#binder f in
          let o = o#register_fun (Var.var_of_binder f) zs in
          `Fun (f, (tyvars, xs, body), None, location), o
        | (`Rec defs) as b when Ir.binding_scope b = `Local ->
          (* reset free and bound variables to be empty *)
          let o = o#reset in

          (* it's important to traverse the function binders first in
             order to make sure they're in scope for all of the
             function bodies *)
          let _, o =
            List.fold_right
              (fun (f, _, _, _) (fs, o) ->
                 let f, o = o#binder f in
                 (f::fs, o))
              defs
              ([], o) in

          (* We rely here on the invariant that variables have
             unique names in that we allow bound variables from
             earlier definitions in the `Rec to leak into
             subsequent ones *)
          let defs, o =
            List.fold_left
              (fun (defs, (o : 'self)) (f, (tyvars, xs, body), none, location) ->
                 assert (none = None);
                 let xs, o =
                   List.fold_right
                     (fun x (xs, o) ->
                        let (x, o) = o#binder x in
                        (x::xs, o))
                     xs
                     ([], o) in
                 let body, _, o = o#computation body in

                 (f, (tyvars, xs, body), None, location)::defs, o)
              ([], o)
              defs in

          let zs =
            List.rev
              (IntSet.fold
                 (fun x zs ->
                    (x, (o#lookup_type x, "fv_" ^ string_of_int x, `Local))::zs)
                 (o#get_free_vars)
                 []) in
          (* restore free and bound variables *)
          let o = o#set bound_vars free_vars in

          (* ensure functions are in scope for the continuation *)
          let _, o =
            List.fold_right
              (fun (f, _, _, _) (fs, o) ->
                 let f, o = o#binder f in
                 (f::fs, o))
              defs
              ([], o) in

          let o = List.fold_left
              (fun o (f, _, _, _) ->
                 o#register_fun (Var.var_of_binder f) zs) o defs in
          let defs = List.rev defs in
          `Rec defs, o
        | `Rec defs ->
          (* it's important to traverse the function binders first in
             order to make sure they're in scope for all of the
             function bodies *)
          (* HACK: invoking super_binder here ensures that f is
             treated like a free variable by any nested functions,
             which is necessary as they will all be hoisted out above
             this global mutually recursive definition. *)
          let _, o =
            List.fold_right
              (fun (f, _, _, _) (fs, o) ->
                 let f, o = o#super_binder f in
                 (f::fs, o))
              defs
              ([], o) in

          let defs, o =
              List.fold_left
                (fun (defs, (o : 'self_type)) (f, (tyvars, xs, body), none, location) ->
                   assert (none = None);
                   let xs, o =
                     List.fold_right
                       (fun x (xs, o) ->
                          let (x, o) = o#binder x in
                          (x::xs, o))
                       xs
                       ([], o) in
                   let body, _, o = o#computation body in
                   (f, (tyvars, xs, body), None, location)::defs, o)
                ([], o)
                defs
          in

          (* we traverse the function binders again in order to
               treat them as globals *)
          let _, o =
            List.fold_right
              (fun (f, _, _, _) (fs, o) ->
                 let f, o = o#binder f in
                 (f::fs, o))
              defs
              ([], o) in

          let defs = List.rev defs in
          `Rec defs, o
        | b -> super#binding b

      method! program =
        fun (bs, tc) ->
          let bs, o = o#bindings bs in
          let tc, t, o = o#tail_computation tc in
          (bs, tc), t, o
    end

  let bindings tyenv globals e =
    let _, o = (new visitor tyenv globals)#bindings e in
      o#get_fenv

  let program tyenv globals e =
    let _, _, o = (new visitor tyenv globals)#program e in
      o#get_fenv
end

(* mark top-level bindings as global *)
module Globalise =
struct
  let binder (x, (t, name, _)) = (x, (t, name, `Global))
  let fun_def (f, lam, z, location) = (binder f, lam, z, location)
  let binding = function
    | `Let (x, body) -> `Let (binder x, body)
    | `Fun def -> `Fun (fun_def def)
    | `Rec defs -> `Rec (List.map fun_def defs)
    | `Alien (x, n, language) -> `Alien (binder x, n, language)
    | `Module _ -> failwith "unimplemented"
  let bindings = List.map binding
  let computation (bs, tc) = (bindings bs, tc)
  let program : Ir.program -> Ir.program = computation
end

module ClosureConvert =
struct
  let close f zs =
    `Closure (f, `Extend (List.fold_right
                            (fun (zname, zv) fields ->
                               StringMap.add zname zv fields)
                            zs
                            StringMap.empty, None))

  class visitor tenv fenv =
    object (o : 'self) inherit Transform.visitor(tenv) as super
      (* currently active mutually recursive functions*)
      val parents : Ir.binder list = []
      (* currently active closure environment *)
      val parent_env = 0
      (* currently active closure variables *)
      val cvars = IntSet.empty

      val hoisted_bindings = []

      method push_binding b = {< hoisted_bindings = b :: hoisted_bindings >}
      method pop_hoisted_bindings = List.rev hoisted_bindings, {< hoisted_bindings = [] >}

      method! value =
        function
        | `Variable x ->
          let x, t, o = o#var x in

          let rec var_val x =
            if IntSet.mem x cvars then
              `Project (string_of_int x, `Variable parent_env)
            else if IntMap.mem x fenv then
              let zs = IntMap.find x fenv in
              match zs with
              | [] -> `Variable x
              | _ ->
                if List.mem_assoc x parents then
                  `Closure (x, `Variable parent_env)
                else
                  let zs =
                    List.map
                      (fun (z, _) ->
                         let v = var_val z in
                         (string_of_int z, v))
                      zs
                  in
                  close x zs
            else
              `Variable x
          in
          var_val x, t, o
        | v -> super#value v

      method set_context parents parent_env cvars =
        {< parents = parents; parent_env = parent_env; cvars = cvars >}

      method! bindings =
        function
        | [] -> [], o
        | b :: bs when Ir.binding_scope b = `Global ->
          let b, o = o#binding b in
          let bs', o = o#pop_hoisted_bindings in
          let bs, o = o#bindings bs in
          bs' @ (b :: bs), o
        | `Fun ((f, _) as fb, (tyvars, xs, body), None, location) :: bs ->
          assert (Var.scope_of_binder fb = `Local);
          let fb = Globalise.binder fb in
          let (xs, o) =
            List.fold_right
              (fun x (xs, o) ->
                 let x, o = o#binder x in
                 (x::xs, o))
              xs
              ([], o) in
          (* back up the previous context *)
          let parents', parent_env', cvars' = parents, parent_env, cvars in
          let zs = IntMap.find f fenv in
          let cvars = List.fold_left (fun cvars (z, _) -> IntSet.add z cvars) IntSet.empty zs in
          let zb, o =
            match zs with
            | [] -> None, o
            | _ ->
              let zt =
                Types.make_record_type
                  (List.fold_left
                     (fun fields (x, (xt, _, _)) ->
                        StringMap.add (string_of_int x) xt fields)
                     StringMap.empty
                     zs)
              in
              (* fresh variable for the closure environment *)
              let zb = Var.fresh_binder (zt, "env_" ^ string_of_int f, `Local) in
              let z = Var.var_of_binder zb in
              let o = o#set_context [fb] z cvars in
              Some zb, o in
          let body, _, o = o#computation body in
          let o = o#set_context parents' parent_env' cvars' in
          let fb, o = o#binder fb in
          let o = o#push_binding (`Fun (fb, (tyvars, xs, body), zb, location)) in
          let bs, o = o#bindings bs in
          bs, o
        | `Rec defs :: bs ->
            (* it's important to traverse the function binders first in
               order to make sure they're in scope for all of the
               function bodies *)
            let fbs, o =
              List.fold_right
                (fun (f, _, _, _) (fs, o) ->
                   let f, o = o#binder f in
                     (f::fs, o))
                defs
                ([], o) in
            let defs, o =
              List.fold_left
                (fun (defs, (o : 'self)) ((f, _) as fb, (tyvars, xs, body), none, location) ->
                   assert (none = None);
                   assert (Var.scope_of_binder fb = `Local);
                   let fb = Globalise.binder fb in
                   let xs, o =
                     List.fold_right
                       (fun x (xs, o) ->
                          let (x, o) = o#binder x in
                            (x::xs, o))
                       xs
                       ([], o) in

                   (* back up the previous context *)
                   let parents', parent_env', cvars' = parents, parent_env, cvars in
                   let zs = IntMap.find f fenv in
                   let cvars = List.fold_left (fun cvars (z, _) -> IntSet.add z cvars) IntSet.empty zs in
                   let zb, o =
                     match zs with
                     | [] -> None, o
                     | _ ->
                       let zt =
                         Types.make_record_type
                           (List.fold_left
                              (fun fields (x, (xt, _, _)) ->
                                 StringMap.add (string_of_int x) xt fields)
                              StringMap.empty
                              zs)
                       in
                       (* fresh variable for the closure environment *)
                       let zb = Var.fresh_binder (zt, "env_" ^ string_of_int f, `Local) in
                       let z = Var.var_of_binder zb in
                       Some zb, o#set_context fbs z cvars in
                   let body, _, o = o#computation body in
                   let o = o#set_context parents' parent_env' cvars' in
                    (fb, (tyvars, xs, body), zb, location)::defs, o)
                ([], o)
                defs in
            let defs = List.rev defs in
            let o = o#push_binding (`Rec defs) in
            let bs, o = o#bindings bs in
            bs, o
        | b :: bs ->
          let b, o = o#binding b in
          let bs, o = o#bindings bs in
          b :: bs, o

      method! program =
        fun (bs, tc) ->
          let bs, o = o#bindings bs in
          let tc, t, o = o#tail_computation tc in
          let bs', o = o#pop_hoisted_bindings in
          (bs @ bs', tc), t, o
    end

  let bindings tyenv fenv bs =
    let bs, _ = (new visitor tyenv fenv)#bindings bs in
    bs

  let program tyenv fenv e =
    let e, _, _ = (new visitor tyenv fenv)#program e in
    e
end

let program tyenv globals program =
  (* Debug.print ("Before closure conversion: " ^ Ir.show_program program); *)
  (* ensure that all top-level bindings are marked as global
     (desugaring can break this invariant) *)
  let program = Globalise.program program in
  let fenv = ClosureVars.program tyenv globals program in
  (* Debug.print ("fenv: " ^ Closures.show_fenv fenv); *)
  let program = ClosureConvert.program tyenv fenv program in
  (* Debug.print ("After closure conversion: " ^ Ir.show_program program); *)
  program

let bindings tyenv globals bs =
  let bs = Globalise.bindings bs in
  let fenv = ClosureVars.bindings tyenv globals bs in
  let bs = ClosureConvert.bindings tyenv fenv bs in
  bs
