open Utility
open CommonTypes
open Ir
open Var

type freevars = {termvars: (Ir.binder list) ; typevars: Quantifier.t list} [@@deriving show]
type fenv = freevars IntMap.t [@@deriving show]

module ClosureVars =
struct
  (* The object of this visitor is to compute the non-global free
     variables for each function so that we can subsequently perform
     closure conversion. These are accumulated in fenv, which maps
     each non-global function f to a list of its non-global free type
     and term variables. *)
  class visitor tenv globals =
    object (o : 'self) inherit IrTraversals.Transform.visitor(tenv) as super
      val globals = globals
      val bound_term_vars = IntSet.empty

      (* We need to track the kinds here. For term variables, the visitor has a dedicated environment. *)
      val bound_type_vars = Types.TypeVarMap.empty

      val free_term_vars = IntSet.empty
      val free_type_vars = Types.TypeVarSet.empty

      (* each call of reset puts the active bound type vars on top of this stack, each call of restore pops an entry *)
      val bound_type_vars_stack = []

      (* Stores free type and term variables per local function. All types are _prior_ to any manipulations done by closure conversion *)
      val fenv : fenv = IntMap.empty

      method register_fun f (fv : freevars) =
        {< fenv = IntMap.add f fv fenv >}

      method global x =
        {< globals = IntSet.add x globals >}

      method bound_termvar x =
        {< bound_term_vars = IntSet.add x bound_term_vars >}

      method bound_typevar x kind =
        {< bound_type_vars = Types.TypeVarMap.add x kind bound_type_vars >}

      (* recursively gather free variables required by inner closures *)
      method close_term x =
        if IntSet.mem x bound_term_vars then
          if IntMap.mem x fenv then
            let freevars = IntMap.find x fenv in
            let zs = freevars.termvars in
            let typevars = freevars.typevars in
            let o =
              List.fold_left
                (fun o b ->
                  let z = Var.var_of_binder b in
                  o#close_term z)
                o
                zs
            in
            List.fold_left
              (fun o q ->
                let tv = Quantifier.to_var q in
                o#register_type_var tv)
              o
              typevars
          else
            o
        else
          begin
            (* Debug.print ("free var: "^string_of_int x); *)
            {< free_term_vars = IntSet.add x free_term_vars >}
          end

      method register_term_var x =
        if IntSet.mem x globals then
          o
        else
          o#close_term x

      method register_type_var tv =
        if Types.TypeVarMap.mem tv bound_type_vars then
          o
        else
          (* (Debug.print ("registering typevar: " ^ string_of_int tv);*)
          {< free_type_vars = Types.TypeVarSet.add tv free_type_vars >}

      method private reset =
        {< bound_term_vars = IntSet.empty; free_term_vars = IntSet.empty;
            bound_type_vars = Types.TypeVarMap.empty; free_type_vars = Types.TypeVarSet.empty;
             bound_type_vars_stack = bound_type_vars::bound_type_vars_stack >}
      method restore bound_term_vars free_term_vars bound_type_vars free_type_vars =
        {< bound_term_vars = bound_term_vars; free_term_vars = free_term_vars;
            bound_type_vars = bound_type_vars; free_type_vars = free_type_vars;
             bound_type_vars_stack = List.tl bound_type_vars_stack >}

      method get_bound_term_vars = bound_term_vars
      method get_free_term_vars = free_term_vars
      method get_bound_type_vars = bound_type_vars
      method get_free_type_vars = free_type_vars
      method get_bound_type_vars_stack = bound_type_vars_stack

      method get_fenv = fenv

      method! var =
        fun var ->
          let var, t, o = super#var var in
          let o = o#typ t in
          var, t, o#register_term_var var

      method! value = fun v -> match v with
        (* We need to find all types occuring in the given IR fragment *)
        | TApp (_, args) ->
           (* Debug.print ("args: " ^ (String.concat "," (List.map (fun t -> Types.string_of_type_arg t) args))); *)
          let o = List.fold_left (fun o arg -> o#type_arg arg) o args in
          o#super_value v
        | Closure (_, tyargs, _) ->
          let o = List.fold_left (fun o arg -> o#type_arg arg) o tyargs in
          o#super_value v
        | Inject (_, _, t) ->
          let o = o#typ t in
          o#super_value v
        | TAbs (quantifiers, v) ->
          let o = List.fold_left (fun o q -> o#quantifier q) o quantifiers in
          let (_, ti, o) = o#value v in
          let t = Types.ForAll (quantifiers, ti) in
          let o = List.fold_left (fun o q -> o#quantifier_remove q) o quantifiers in
          (v, t, o)
        | _ -> o#super_value v



      method! special = fun s ->
        (* We need to find all types occuring in the given IR fragment *)
        let o = match s with
          | Table (_, _, _, (t1, t2, t3)) ->
            let o1 = o#typ t1 in
            let o2 = o1#typ t2 in
            o2#typ t3
          | Query (_, _, _, t)
          | DoOperation (_, _, t) ->
            o#typ t
          | _ -> o in
        o#super_special s


      method typ t =
        let free_type_vars = Types.free_type_vars t in
        (*Debug.print ("free type vars:" ^ (IntSet.show free_type_vars));*)
        Types.TypeVarSet.fold (fun tvar o ->  o#register_type_var tvar) free_type_vars o

      method type_arg (_pk, t) =
        o#typ t

      method quantifier q =
        let var = Quantifier.to_var q in
        o#bound_typevar var q

      method quantifier_remove q =
        let var = Quantifier.to_var q in
        {< bound_type_vars = Types.TypeVarMap.remove var bound_type_vars >}



      method! binder b =
        let b, o = super#binder b in
        let t = Var.type_of_binder b in
        let o = o#typ t in
        match Var.scope_of_binder b with
        | Scope.Global -> b, o#global (Var.var_of_binder b)
        | Scope.Local  -> b, o#bound_termvar (Var.var_of_binder b)

      method super_binding = super#binding

      method super_binder = super#binder

      method super_value = super#value

      method super_special = super#special


      method create_fenv_entry =
        let rec query_boundvars_stack var remaining_stack =
          match remaining_stack with
            | m::ms ->
              begin match Types.TypeVarMap.find_opt var m with
                | Some quantifier -> Some quantifier
                | None -> query_boundvars_stack var ms
              end
            | [] -> None in
        let free_binders =
          List.rev
            (IntSet.fold
               (fun x zs ->
                 let info = Var.make_local_info (o#lookup_type x, "fv_" ^ string_of_int x) in
                 (Var.make_binder x info) :: zs)
               (o#get_free_term_vars)
               [])
        in
        (* We are only interested in free variables of the function that actually have a binder "above".
           This prevents breaking the value restriction. Since the currently bound type variables may be hidden
           begind multiple calls of o#reset, we access the stack collecting bound variable environments shadowed by a call of o#reset *)
        let free_typevars =
            Types.TypeVarSet.fold
                (fun tvar qlist -> match query_boundvars_stack tvar o#get_bound_type_vars_stack with
                  | Some quantifier -> quantifier :: qlist
                  | None -> qlist )
                (o#get_free_type_vars)
                [] in
        {termvars = free_binders ; typevars = free_typevars}


      method! binding =
        function
        | (Let (_, (quantifiers, _))) as b->
          let o = List.fold_left (fun o q -> o#quantifier q) o quantifiers in
          let (b, o) = o#super_binding b in
          let o = List.fold_left (fun o q -> o#quantifier_remove q) o quantifiers in
          (b, o)
        | (Fun (f, (tyvars, xs, body), None, location, unsafe)) as b
             when Scope.is_local (Ir.binding_scope b) ->
          (* reset free and bound variables to be empty *)
          let o = o#reset in


          (* We must process the binder f to check its type for free type variables.
             This must happen before adding the tyvars to o, as they are not bound in
             the annotation on f.
             Note that as a result, we call o#binder on f inside and outside of the
             reset/restore block *)
          let f, o = o#binder f in

          let o = List.fold_left (fun o q -> o#quantifier q) o tyvars in
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
          let o = List.fold_left (fun o q -> o#quantifier_remove q) o tyvars in


          (*Debug.print ("free type vars of " ^ (string_of_int (Var.var_of_binder f)) ^ " " ^ (IntSet.show o#get_free_type_vars));
          Debug.print ("bound type vars at  " ^ (string_of_int (Var.var_of_binder f)) ^  " " ^ (IntMap.show (Types.pp_kind) o#get_bound_type_vars));*)

          let fenv_entry = o#create_fenv_entry in
           (*Debug.print ("fventry of  " ^ (string_of_int (Var.var_of_binder f)) ^ " " ^ (show_freevars fenv_entry));*)

          (* restore free and bound variables *)
          let o = o#restore bound_term_vars free_term_vars bound_type_vars free_type_vars in
          let f, o = o#binder f in
          let o = o#register_fun (Var.var_of_binder f) fenv_entry in
          (*Debug.print ("fenv: " ^ show_fenv o#get_fenv);*)
          Fun (f, (tyvars, xs, body), None, location, unsafe), o

        | (Fun (_, (tyvars, _, _),_,_,_)) as b (* global *) ->
          let o = List.fold_left (fun o q -> o#quantifier q) o tyvars in
          let (b, o) = o#super_binding b in
          let o = List.fold_left (fun o q -> o#quantifier_remove q) o tyvars in
          (b, o)

        | (Rec defs) as b when Scope.is_local (Ir.binding_scope b) ->
          (* reset free and bound variables to be empty *)
          let o = o#reset in

          (* it's important to traverse the function binders first in
             order to make sure they're in scope for all of the
             function bodies.
             Further, this ensures that all free type variables in
             binder annotations are added to fenv_entry, before
             bringing the tyvars into scope (which are not bound
             in the function type itself) *)
          let _, o =
            List.fold_right
              (fun (f, _, _, _, _) (fs, o) ->
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
              (fun (defs, (o : 'self)) (f, (tyvars, xs, body), none, location, unsafe) ->
                 assert (none = None);
                 let o = List.fold_left (fun o q -> o#quantifier q) o tyvars in
                 let xs, o =
                   List.fold_right
                     (fun x (xs, o) ->
                        let (x, o) = o#binder x in
                        (x::xs, o))
                     xs
                     ([], o) in
                 let body, _, o = o#computation body in
                 let o = List.fold_left (fun o q -> o#quantifier_remove q) o tyvars in

                 (f, (tyvars, xs, body), None, location, unsafe)::defs, o)
              ([], o)
              defs in

          let fenv_entry = o#create_fenv_entry in
          (* restore free and bound variables *)
          let o = o#restore bound_term_vars free_term_vars bound_type_vars free_type_vars in

          (* ensure functions are in scope for the continuation *)
          let _, o =
            List.fold_right
              (fun (f, _, _, _, _) (fs, o) ->
                 let f, o = o#binder f in
                 (f::fs, o))
              defs
              ([], o) in

          let o = List.fold_left
              (fun o (f, _, _, _, _) ->
                 o#register_fun (Var.var_of_binder f) fenv_entry) o defs in
          let defs = List.rev defs in
          Rec defs, o
        | Rec defs (* global *) ->
          (* it's important to traverse the function binders first in
             order to make sure they're in scope for all of the
             function bodies *)
          (* HACK: invoking super_binder here ensures that f is
             treated like a free variable by any nested functions,
             which is necessary as they will all be hoisted out above
             this global mutually recursive definition. *)
          let _, o =
            List.fold_right
              (fun (f, _, _, _, _) (fs, o) ->
                 let f, o = o#super_binder f in
                 (f::fs, o))
              defs
              ([], o) in

          let defs, o =
              List.fold_left
                (fun (defs, (o : 'self_type)) (f, (tyvars, xs, body), none, location, unsafe) ->
                   assert (none = None);
                   let o = List.fold_left (fun o q -> o#quantifier q) o tyvars in
                   let xs, o =
                     List.fold_right
                       (fun x (xs, o) ->
                          let (x, o) = o#binder x in
                          (x::xs, o))
                       xs
                       ([], o) in
                   let body, _, o = o#computation body in
                   let o = List.fold_left (fun o q -> o#quantifier_remove q) o tyvars in
                   (f, (tyvars, xs, body), None, location, unsafe)::defs, o)
                ([], o)
                defs
          in

          (* we traverse the function binders again in order to
               treat them as globals *)
          let _, o =
            List.fold_right
              (fun (f, _, _, _, _) (fs, o) ->
                 let f, o = o#binder f in
                 (f::fs, o))
              defs
              ([], o) in

          let defs = List.rev defs in
          Rec defs, o
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
  let binder b = Var.globalise_binder b
  let fun_def (f, lam, z, location, unsafe) = (binder f, lam, z, location, unsafe)
  let binding = function
    | Let (x, body) -> Let (binder x, body)
    | Fun def -> Fun (fun_def def)
    | Rec defs -> Rec (List.map fun_def defs)
    | Alien { binder = x; object_name; language } ->
       Alien { binder = binder x; object_name; language }
    | Module _ ->
       raise (Errors.internal_error
                ~filename:"closures.ml"
                ~message:"Globalisation of modules unimplemented")
  let bindings = List.map binding
  let computation (bs, tc) = (bindings bs, tc)
  let program : Ir.program -> Ir.program = computation
end

module ClosureConvert =
struct

  let close f zs tyargs =
    Closure (f, tyargs, Extend (List.fold_right
                            (fun (zname, zv) fields ->
                               StringMap.add zname zv fields)
                            zs
                            StringMap.empty, None))

  class visitor tenv fenv =
    object (o : 'self) inherit IrTraversals.Transform.visitor(tenv) as super
      (* currently active mutually recursive functions*)
      val parents : (Ir.var * Ir.binder) list = []
      (* currently active closure environment *)
      val parent_env = 0
      (* currently active closure variables *)
      val cvars = IntSet.empty

      val hoisted_bindings = []

      method push_binding b = {< hoisted_bindings = b :: hoisted_bindings >}
      method pop_hoisted_bindings = List.rev hoisted_bindings, {< hoisted_bindings = [] >}

      method! value =
        function
        | Variable y ->
          let y, _, o = o#var y in

          let rec var_val x : (Ir.value * Types.datatype ) =
            let x_type = o#lookup_type x in
            if IntSet.mem x cvars then
              (* We cannot return t as the type of the result here. If x refers to a hoisted function that was generalised, then
                 t has additional quantifiers that are not present in the corresponding type of projecting x from parent_env *)
              let projected_t = TypeUtils.project_type (string_of_int x) (snd3 (o#var parent_env)) in
              Project (string_of_int x, Variable parent_env), projected_t
            else if IntMap.mem x fenv then
              let zs = (IntMap.find x fenv).termvars in
              let tyvars = (IntMap.find x fenv).typevars in

              match zs, tyvars with
              | [], [] -> Variable x, x_type
              | _ ->
                let tyargs = List.map Types.type_arg_of_quantifier tyvars in
                let (remaining_type, instantiation_maps) = Instantiate.instantiation_maps_of_type_arguments false x_type tyargs in
                let overall_type = Instantiate.datatype instantiation_maps remaining_type in
                if List.mem_assoc x parents then
                  Closure (x, tyargs,Variable parent_env), overall_type
                else
                  let zs =
                    List.map
                      (fun b ->
                        let z = Var.var_of_binder b in
                        let v = fst (var_val z) in
                        (string_of_int z, v))
                      zs
                  in
                  close x zs tyargs, overall_type
            else
              Variable x, x_type
          in
          let overall_val, overall_type = var_val y in
          overall_val, overall_type, o
        | v -> super#value v

      method set_context parents parent_env cvars =
        {< parents = parents; parent_env = parent_env; cvars = cvars >}


      method! bindings =
        function
        | [] -> [], o
        | b :: bs when Scope.is_global (Ir.binding_scope b) ->
          let b, o = o#binding b in
          let bs', o = o#pop_hoisted_bindings in
          let bs, o = o#bindings bs in
          bs' @ (b :: bs), o
        | Fun (fb, (tyvars, xs, body), None, location, unsafe) :: bs ->
          assert (Scope.is_local (Var.scope_of_binder fb));
          let f = Var.var_of_binder fb in
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
          let fenv_entry = IntMap.find f fenv in
          let zs = fenv_entry.termvars in
          let type_zs = fenv_entry.typevars in
          let cvars =
            List.fold_left
              (fun cvars b ->
                let z = Var.var_of_binder b in
                IntSet.add z cvars)
              IntSet.empty zs
          in

          (* HACK: this function and the type annotation (o : 'self)
             work around an as yet undiagnosed bug in OCaml 4.07.0 *)
          let binder_hack x = o#binder x in
          let zb, (o : 'self) =
            match zs, type_zs with
            | [], [] -> None, o
            | _ ->
              let zt =
                Types.make_record_type
                  (List.fold_left
                     (fun fields b ->
                       let x = Var.var_of_binder b in
                       let xt = Var.type_of_binder b in
                       StringMap.add (string_of_int x) xt fields)
                     StringMap.empty
                     zs)
              in
              (* fresh variable for the closure environment *)
              let zb = Var.(fresh_binder (make_local_info (zt, "env_" ^ string_of_int f))) in
              let z = Var.var_of_binder zb in
              (* HACK: the following line leads to a compiler error in
                 OCaml 4.07.0: Fatal error: exception Ctype.Unify(_)
                 *)
              (* let _, o = o#binder zb in *)
              let _, o = binder_hack zb in
              let o = o#set_context [(Var.var_of_binder fb, fb)] z cvars in
              Some zb, o in
          let body, _, o = o#computation body in
          let o = o#set_context parents' parent_env' cvars' in
          let fb, o = o#binder (o# generalise_function_type_for_hoisting fb) in
          let fundef = o#generalise_function_body_for_hoisting (fb, (tyvars, xs, body), zb, location, unsafe) in
          let o = o#push_binding (Fun  fundef) in
          let bs, o = o#bindings bs in
          bs, o
        | Rec defs :: bs ->
            (* it's important to traverse the function binders first in
               order to make sure they're in scope for all of the
               function bodies *)
            let fbs, defs, o =
              List.fold_right
                (fun (f, (tyvars, xs, body), zb, location, unsafe) (fs, defs,  o) ->
                   (* We have generalise the function's type here, but its body will only be generalised later on *)
                   let f, o = o#binder (o#generalise_function_type_for_hoisting f) in
                   let def = (f, (tyvars, xs, body), zb, location, unsafe) in
                     (f::fs, def::defs, o))
                defs
                ([], [], o) in

            let defs, o =
              List.fold_left
                (fun (defs, (o : 'self)) (fb, (tyvars, xs, body), none, location, unsafe) ->
                   assert (none = None);
                   assert (Scope.is_local (Var.scope_of_binder fb));
                   let f = Var.var_of_binder fb in
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
                   let fenv_entry = IntMap.find f fenv in
                   let zs = fenv_entry.termvars in
                   let type_zs = fenv_entry.typevars in
                   let cvars =
                     List.fold_left
                       (fun cvars b ->
                         IntSet.add (Var.var_of_binder b) cvars)
                       IntSet.empty zs
                   in
                   let zb, o =
                     match zs, type_zs with
                     | [], [] -> None, o
                     | _ ->
                       let zt =
                         Types.make_record_type
                           (List.fold_left
                              (fun fields b ->
                                let x = Var.var_of_binder b in
                                let xt = Var.type_of_binder b in
                                StringMap.add (string_of_int x) xt fields)
                              StringMap.empty
                              zs)
                       in
                       (* fresh variable for the closure environment *)
                       let zb = Var.(fresh_binder (make_local_info (zt, "env_" ^ string_of_int f))) in
                       let _, o = o#binder zb in
                       let z = Var.var_of_binder zb in
                       Some zb, o#set_context (List.map (fun fb -> Var.var_of_binder fb, fb) fbs) z cvars in
                   let body, _, o = o#computation body in
                   let o = o#set_context parents' parent_env' cvars' in
                   let fundef = o#generalise_function_body_for_hoisting (fb, (tyvars, xs, body), zb, location, unsafe) in
                   fundef::defs, o)
                ([], o)
                defs in
            let defs = List.rev defs in
            let o = o#push_binding (Rec defs) in
            let bs, o = o#bindings bs in
            bs, o
        | b :: bs ->
          let b, o = o#binding b in
          let bs, o = o#bindings bs in
          b :: bs, o


      (** Given a list of free variables, return a tuple containing the following:
        - a list of fresh quantifiers, each corresponding to one free variable
        - A map mapping the old free variables to fresh ones (to be used with Instantiate)  **)
      method create_substitutions_replacing_free_variables (free_type_vars : Quantifier.t list) =
        let open PrimaryKind in
        List.fold_right (fun oldq (qs, type_map) ->
          let typevar = Quantifier.to_var oldq in
          let primary_kind = Quantifier.to_primary_kind oldq in
          let subkind = Quantifier.to_subkind oldq in
          let newvar = Types.fresh_raw_variable () in
          let make_new_type_variable () = Unionfind.fresh (Types.Var (newvar, (primary_kind, subkind), `Rigid)) in
          let updated_maps = match primary_kind with
            | Type ->
              let new_type_variable = make_new_type_variable () in
              let t = Types.Meta new_type_variable in
              (IntMap.add typevar (Type, t) type_map)
            | Row ->
              let new_type_variable = make_new_type_variable () in
              let r = Types.Row (Types.empty_field_env, new_type_variable, false) in
              (IntMap.add typevar (Row, r) type_map)
            | Presence ->
              let new_type_variable = make_new_type_variable () in
              let p = Types.Meta new_type_variable in
              (IntMap.add typevar (Presence, p) type_map) in
          let new_quantifier = (newvar, (primary_kind, subkind)) in
          (new_quantifier :: qs, updated_maps)
        ) free_type_vars ([], IntMap.empty)


      method generalise_function_type_for_hoisting f_binder =
        let f_var = Var.var_of_binder f_binder in

        let free_type_vars = (IntMap.find f_var fenv).typevars in

        if free_type_vars = [] then
          f_binder
        else
          begin
            let outer_quantifiers, outer_maps = o#create_substitutions_replacing_free_variables free_type_vars in
            let f_type_generalised =
              let f_type = Var.type_of_binder f_binder in
              match TypeUtils.split_quantified_type f_type with
                | [], t  ->
                  let t' = Instantiate.datatype outer_maps t in
                  Types.ForAll (outer_quantifiers, t')
                | (f_quantifiers, t) ->
                  let t' = Instantiate.datatype outer_maps t in
                  Types.ForAll ((outer_quantifiers @ f_quantifiers), t') in
              Var.update_type f_type_generalised f_binder
            end


      method generalise_function_body_for_hoisting : Ir.fun_def ->  Ir.fun_def = fun fundef ->
        let (f, (tyvars, xs, body), z, location, unsafe) = fundef in
        let f_var = Var.var_of_binder f in
        let free_type_vars = (IntMap.find f_var fenv).typevars in

        (* We must have used generalise_function_type_for_hoisting on this function before and generalised the type in f (i.e., the binder)  already *)

        if free_type_vars = [] then
          fundef
        else
          begin
            let inner_quantifiers, inner_maps = o#create_substitutions_replacing_free_variables free_type_vars in
            let tyvars = inner_quantifiers @ tyvars in
            let (z, o) = match z with
              | Some zbinder ->
                let zbinder = Var.update_type (Instantiate.datatype inner_maps (Var.type_of_binder zbinder)) zbinder in
                (Some zbinder, snd (o#binder zbinder))
              | None -> None, o in
            let xs = List.fold_right (fun x xs ->
                let newtype = Instantiate.datatype inner_maps (Var.type_of_binder x) in
                (Var.update_type newtype x)::xs
              ) xs [] in
            (* Debug.print ("function currently being hoisted, before instantiation:\n" ^ Ir.string_of_binding (Fun (f, (tyvars, xs, body), z, location, unsafe))); *)
            let body = IrTraversals.InstantiateTypes.computation (o#get_type_environment) inner_maps body in
            (f, (tyvars, xs, body), z, location, unsafe)
          end



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

let name = "closure_conversion"

let program state program =
  let open IrTransform in
  let globals = state.primitive_vars in
  let tenv = Context.variable_environment (context state) in
  let program' = Globalise.program program in
  let fenv = ClosureVars.program tenv globals program' in
  let program'' = ClosureConvert.program tenv fenv program' in
  return state program''
