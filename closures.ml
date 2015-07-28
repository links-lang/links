(*pp deriving *)
open Notfound
open Utility
open Ir

type fenv = (Ir.binder list) IntMap.t
    deriving (Show)

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

      val toplevel = true
      method private set_toplevel toplevel = {< toplevel = toplevel >}
      method private descend : 'a.('self -> 'a * 'self) -> 'a * 'self =
        fun f ->
          let l = toplevel in
          let o = o#set_toplevel false in
          let r, o = f o in
          let o = o#set_toplevel l in
          r, o

      val fenv : (Ir.binder list) IntMap.t = IntMap.empty

      method private register_fun f (zs : binder list) =
        {< fenv = IntMap.add f zs fenv >}

      method private global x =
        {< globals = IntSet.add x globals >}

      method private bound x =
        {< bound_vars = IntSet.add x bound_vars >}

      method private register_var x =
        if IntSet.mem x globals then
          o
        else if IntSet.mem x bound_vars then
          if IntMap.mem x fenv then
            let zs = IntMap.find x fenv in
            let free_vars =
              List.fold_left
                (fun free_vars (z, _) ->
                   if IntSet.mem z bound_vars then
                     free_vars
                   else
                     IntSet.add z free_vars)
                free_vars
                zs
            in
            {< free_vars = free_vars >}
          else
            o
        else
          {< free_vars = IntSet.add x free_vars >}

      method private reset =
        {< bound_vars = IntSet.empty; free_vars = IntSet.empty >}
      method private set bound_vars free_vars =
        {< bound_vars = bound_vars; free_vars = free_vars >}

      method private get_bound_vars = bound_vars
      method private get_free_vars = free_vars

      method get_fenv = fenv

      method var =
        fun var ->
          let var, t, o = super#var var in
          var, t, o#register_var var

      method binder ((_, (_, _, scope)) as b) =
        let b, o = super#binder b in
        if toplevel = true then
          b, o#global (Var.var_of_binder b)
        else
          b, o#bound (Var.var_of_binder b)
        (* match scope with *)
        (* | `Global -> b, o#global (Var.var_of_binder b) *)
        (* | `Local  -> b, o#bound (Var.var_of_binder b) *)

      method private super_binding = super#binding

      method binding =
        function
        | `Fun (f, (tyvars, xs, body), None, location) when toplevel = false ->
          (* reset free and bound variables to be empty *)
          let o = o#reset in
          let (xs, o) =
            List.fold_right
              (fun x (xs, o) ->
                 let x, o = o#binder x in
                 (x::xs, o))
              xs
              ([], o) in
          let body, o = o#descend (fun o -> let body, _, o = o#computation body in body, o) in
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
        | `Rec defs when toplevel = false ->
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
              (fun (defs, (o : 'self)) (f, (tyvars, xs, body), None, location) ->
                 let xs, o =
                   List.fold_right
                     (fun x (xs, o) ->
                        let (x, o) = o#binder x in
                        (x::xs, o))
                     xs
                     ([], o) in
                 let body, o = o#descend (fun o -> let body, _, o = o#computation body in body, o) in

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
        | b ->
          o#descend (fun o -> o#super_binding b)
    end

  let bindings tyenv globals e =
    let _, o = (new visitor tyenv globals)#bindings e in
      o#get_fenv

  let program tyenv globals e =
    let _, _, o = (new visitor tyenv globals)#program e in
      o#get_fenv
end

module ClosureConvert =
struct
  let globalise (f, (t, name, scope)) = (f, (t, name, `Global))

  let close f zs =
    `Closure (f, `Extend (List.fold_right
                            (fun (zname, zv) fields ->
                               StringMap.add zname zv fields)
                            zs
                            StringMap.empty, None))

  class visitor tenv globals fenv =
    object (o : 'self) inherit Transform.visitor(tenv) as super
      (* currently active mutually recursive functions*)
      val parents : Ir.binder list = []
      (* currently active closure environment *)
      val parent_env = 0
      (* currently active closure variables *)
      val cvars = IntSet.empty

      val hoisted_bindings = []

      val toplevel = true
      method private set_toplevel toplevel = {< toplevel = toplevel >}
      method private descend : 'a.('self -> 'a * 'self) -> 'a * 'self =
        fun f ->
          let l = toplevel in
          let o = o#set_toplevel false in
          let r, o = f o in
          let o = o#set_toplevel l in
          r, o

      method private push_binding b = {< hoisted_bindings = b :: hoisted_bindings >}
      method private pop_hoisted_bindings = List.rev hoisted_bindings, {< hoisted_bindings = [] >}

      method value =
        function
        | `Variable x ->
          let x, t, o = o#var x in
          let v =
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
                    List.map (fun (z, _) ->
                        let zname = string_of_int z in
                        if IntSet.mem z cvars then
                          (zname, `Project (zname,  `Variable parent_env))
                        else
                          (zname, `Variable z))
                      zs
                  in
                  close x zs
            else
              `Variable x
          in
          v, t, o
        | v -> super#value v

      method private set_context parents parent_env cvars =
        {< parents = parents; parent_env = parent_env; cvars = cvars >}

      method bindings =
        function
        | [] -> [], o
        | b :: bs when toplevel = true ->
          let b, o = o#descend (fun o -> o#binding b) in
          let bs', o = o#pop_hoisted_bindings in
          let bs, o = o#bindings bs in
          bs' @ (b :: bs), o
        | `Fun ((f, (t, _, _)) as fb, (tyvars, xs, body), None, location) :: bs ->
          assert (Var.scope_of_binder fb = `Local);
          let fb = globalise fb in
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
          let body, o = o#descend (fun o -> let body, _, o = o#computation body in body, o) in
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
                (fun (defs, (o : 'self)) ((f, (t, _, _)) as fb, (tyvars, xs, body), None, location) ->
                   assert (Var.scope_of_binder fb = `Local);
                   let fb = globalise fb in
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
                   let body, o = o#descend (fun o -> let body, _, o = o#computation body in body, o) in
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
    end

  let bindings tyenv globals fenv bs =
    let bs, _ = (new visitor tyenv globals fenv)#bindings bs in
      bs

  let program tyenv globals fenv e =
    let e, _, _ = (new visitor tyenv globals fenv)#program e in
      e
end
