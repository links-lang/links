open Notfound
open Utility
open Ir

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

      method private register_fun f (zs : binder list) =
        {< fenv = IntMap.add f zs fenv >}

      method private global x =
        {< globals = IntSet.add x globals >}

      method private bound x =
        {< bound_vars = IntSet.add x bound_vars >}

      method private free x =
        if IntSet.mem x globals || IntSet.mem x bound_vars then
          o
        else
          {< free_vars = IntSet.add x free_vars >}

      method private reset =
        {< bound_vars = IntSet.empty; free_vars = IntSet.empty >}
      method private set bound_vars free_vars =
        {< bound_vars = bound_vars; free_vars = free_vars >}

      method private get_bound_vars = bound_vars
      method private get_free_vars = free_vars

      method private wrap : 'a.(unit -> 'a * 'self) -> 'a * 'self =
        fun f ->
          let o = o#reset in
          let r, o = f() in
          let o = o#set bound_vars free_vars in
          r, o

      method get_fenv = fenv

      method var =
        fun var ->
          let var, t, o = super#var var in
          var, t, o#free var

      method binder ((_, (_, _, scope)) as b) =
        let b, o = super#binder b in
        match scope with
        | `Global -> b, o#global (Var.var_of_binder b)
        | `Local  -> b, o#bound (Var.var_of_binder b)

      method binding =
        function
        | b when Ir.binding_scope b = `Global -> super#binding b
        | `Fun (f, (tyvars, xs, body), None, location) ->
          let (xs, body, zs), o =
            o#wrap (fun () ->
                let (xs, o) =
                  List.fold_right
                    (fun x (xs, o) ->
                       let x, o = o#binder x in
                       (x::xs, o))
                    xs
                    ([], o) in
                let body, _, o = o#computation body in
                let zs =
                  List.rev
                    (IntSet.fold
                       (fun x zs ->
                          (x, (o#lookup_type x, "fv_" ^ string_of_int x, `Local))::zs)
                       (o#get_free_vars)
                       []) in
                (xs, body, zs), o) in
          let f, o = o#binder f in
          let o = o#register_fun (Var.var_of_binder f) zs in
          `Fun (f, (tyvars, xs, body), None, location), o
        | `Rec defs ->
          let (defs, zs), o =
            o#wrap (fun () ->
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
                let def, zs =
                  List.fold_left
                    (fun (defs, o) (f, (tyvars, xs, body), None, location) ->
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
                (defs, zs), o) in
          let o = List.fold_left
              (fun o (f, _, _, _) ->
                 o#register_fun (Var.var_of_binder f) zs) o defs in
          let defs = List.rev defs in
          `Rec defs, o
        | b -> super#binding b
    end

  let value tyenv globals v =
    let _, _, o = (new visitor tyenv globals)#value v in
      o#get_fenv

  let tail_computation tyenv globals e =
    let _, _, o = (new visitor tyenv globals)#tail_computation e in
      o#get_fenv

  let binding tyenv globals bs =
    let _, o = (new visitor tyenv globals)#binding bs in
      o#get_fenv

  let bindings tyenv globals bs =
    let _, o = (new visitor tyenv globals)#bindings bs in
      o#get_fenv

  let computation tyenv globals e =
    let _, _, o = (new visitor tyenv globals)#computation e in
      o#get_fenv

  let program = computation
end

module ClosureConvert =
struct
  let close f zs =
    `Closure (f, `Extend (List.fold_right
                            (fun zb fields ->
                               let z = Var.var_of_binder zb in
                               StringMap.add (string_of_int z) (`Variable z) fields)
                            zs
                            StringMap.empty, None))

  class visitor tenv globals fenv =
    object (o : 'self) inherit Transform.visitor(tenv) as super
      (* currently active mutually recursive functions*)
      val parents : Ir.binder list  = []
      (* currently active closure environment *)
      val parent_env = 0
      (* currently active closure variables *)
      val cvars = IntSet.empty

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
                  close x zs
            else
              `Variable x
          in
          v, t, o

      method private set_context parents parent_env cvars =
        {< parents = parents; parent_env = parent_env; cvars = cvars >}

      method binding =
        function
        | b when Ir.binding_scope b = `Global -> super#binding b
        | `Fun ((f, (t, _, _)) as fb, (tyvars, xs, body), None, location) ->
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
              (* fresh variable for the closure environment *)
              let zb = Var.fresh_binder (t, "env_" ^ string_of_int f, `Local) in
              let z = Var.var_of_binder zb in
              let o = o#set_context [fb] z cvars in
              Some zb, o in
          let body, _, o = o#computation body in
          let o = o#set_context parents' parent_env' cvars' in
          let fb, o = o#binder fb in
          `Fun (fb, (tyvars, xs, body), zb, location), o
        | `Rec defs ->
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
                (fun (defs, (o : 'self_type)) ((f, (t, _, _)) as fb, (tyvars, xs, body), None, location) ->
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
                       (* fresh variable for the closure environment *)
                       let zb = Var.fresh_binder (t, "env_" ^ string_of_int f, `Local) in
                       let z = Var.var_of_binder zb in
                       Some zb, o#set_context fbs z cvars in
                   let body, _, o = o#computation body in
                   let o = o#set_context parents' parent_env' cvars' in
                    (fb, (tyvars, xs, body), zb, location)::defs, o)
                ([], o)
                defs in
            let defs = List.rev defs in
              `Rec defs, o
    end
end



(* module ClosureConvert = *)
(* struct *)
(*   class visitor tenv fenv cenv = *)
(*     object (o : 'self) inherit Transform.visitor(tenv) as super *)

(*       method value = *)
(*         function *)
(*         | `Variable x -> *)
(*           let x, t, o = o#var x in *)
(*           if IntMap.mem x fenv then *)
(*             assert false *)
(*           else if IntMap.mem x cenv then *)
(*             `ClosureVar x, t, o *)
(*           `Variable x, t, o         *)

(*       method binding = *)
(*         function *)
(*         | `Fun (f, (tyvars, xs, body), [], location) -> *)
(*             let xs, body, o = *)
(*               let (xs, o) = *)
(*                 List.fold_right *)
(*                   (fun x (xs, o) -> *)
(*                      let x, o = o#binder x in *)
(*                        (x::xs, o)) *)
(*                   xs *)
(*                   ([], o) in *)
(*               let body, _, o = o#computation body in *)
(*               xs, body, o in *)
(*             let zs = IntMap.find f tenv in  *)
(*             let f, o = o#binder f in *)
(*               `Fun (f, (tyvars, xs, body), zs, location), o *)
(*         | `Rec _ -> assert false *)
(*         | b -> super#binding b *)

(*     end *)
(* end *)


(* module ClosureConvert = *)
(* struct *)
(*   class visitor tyenv bound_vars = *)
(*     object (o) *)
(*       inherit Transform.visitor(tyenv) as super *)

(*       val free_vars = IntSet.empty *)
(*       val bound_vars = bound_vars *)

(*       method bound x = *)
(*         {< bound_vars = IntSet.add x bound_vars >} *)

(*       method free x = *)
(*         if IntSet.mem x bound_vars then o *)
(*         else {< free_vars = IntSet.add x free_vars >} *)

(*       method reset_free = *)
(*         {< free_vars = IntSet.empty >} *)

(*       method extend_free free_vars' = *)
(*         {< free_vars = IntSet.union free_vars free_vars' >} *)


(*       method binder b = *)
(*         let b, o = super#binder b in *)
(*         b, o#bound (Var.var_of_binder b) *)

(*       method binding = *)
(*         function *)
(*         | `Let _ as b -> o#super b *)
(*         | `Fun (f, (tyvars, xs, body), [], scope) -> *)
(*           let free_vars' = free_vars in *)
(*           let o = o#reset_free in *)
(*           let xs, body, o = *)
(*             let (xs, o) = *)
(*               List.fold_right *)
(*                 (fun x (xs, o) -> *)
(*                    let x, o = o#binder x in *)
(*                    (x::xs, o)) *)
(*                 xs *)
(*                 ([], o) in *)
(*             let body, _, o = o#computation body in *)
(*             xs, body, o in *)

(*           let tyenv = get_type_environment in *)

(*           let zs = *)
(*             IntSet.fold *)
(*               (fun x zs -> *)
(*                  IntMap.find x *)
(*               ) *)
(*                  get_free_vars *)

(*           let o = o#extend_free *)
(*           let f, o = o#binder f in *)
(*           (\* TODO: check that xs and body match up with f *\) *)
(*           `Fun (f, (tyvars, xs, body), zs, location), o *)
(*     end            *)
(* end *)
