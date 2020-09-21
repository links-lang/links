open Utility
open Ir
open Var

module FunDefs =
struct
  type t = (Var.var, Ir.eval_fun_def) Hashtbl.t

  let make_eval_def : Ir.fun_def -> Ir.eval_fun_def =
    fun fdef ->
    let {fn_binder; fn_tyvars = _; fn_params; fn_body; fn_closure; fn_location; fn_unsafe = _} = fdef in
    let info = Var.info_of_binder fn_binder in
    info, (List.map Var.var_of_binder fn_params, fn_body), opt_map Var.var_of_binder fn_closure, fn_location

  let add fs def =
      let f = Var.var_of_binder (Ir.binder_of_fun_def def) in
      Hashtbl.add fs f (make_eval_def def)

  let binding fs : Ir.binding -> unit = function
    | Let _ -> ()
    | Fun def -> add fs def
    | Rec defs -> List.iter (add fs) defs
    | Alien _ -> ()
    | Module _ ->
        raise (Errors.internal_error
          ~filename:"buildTables.ml"
          ~message:"Module tables not implemented")

  let bindings fs = List.iter (binding fs)

  let computation fs =
    fun (bs, _) ->
      bindings fs bs

  let program = computation
end

module ScopesAndContDefs =
struct
  class visitor tyenv scopes cont_defs =
  object (_)
    inherit IrTraversals.Transform.visitor(tyenv) as super

    method bind_scope xb =
      Hashtbl.add scopes (Var.var_of_binder xb) (Var.scope_of_binder xb)

    method bind_cont (xb, e) =
      Hashtbl.add cont_defs (Var.var_of_binder xb) e

    method! binder =
      fun b ->
        let o, b = super#binder b in
        o#bind_scope b;
        o, b

    method! computation =
      fun e ->
        let o, (bs, main), t = super#computation e in
        let rec bind o =
          function
            | [] -> o
            | (Let (x, _))::bs ->
              o#bind_cont (x, (bs, main));
              bind o bs
            | _::bs -> bind o bs
        in
          bind o bs, (bs, main), t
  end

  let primitives scopes =
    IntSet.iter (fun x -> Hashtbl.add scopes x Scope.Global) Lib.primitive_vars

  let bindings tyenv scopes cont_defs bs =
    let _ = (new visitor tyenv scopes cont_defs)#bindings bs in ()

  let program tyenv scopes cont_defs e =
    let _ = (new visitor tyenv scopes cont_defs)#program e in ()
end

(** Compute free variables *)
module FreeVars =
struct
  class visitor tenv bound_vars =
  object (o)
    inherit IrTraversals.Transform.visitor(tenv) as super

    val free_vars = IntSet.empty
    val bound_vars = bound_vars

    method bound x =
      {< bound_vars = IntSet.add x bound_vars >}

    method free x =
      if IntSet.mem x bound_vars then o
      else {< free_vars = IntSet.add x free_vars >}

    method! binder =
      fun b ->
        let o, b = super#binder b in
          o#bound (Var.var_of_binder b), b

    method! var =
      fun x ->
        let o, x, t = super#var x in
          o#free x, x, t

    method get_free_vars = free_vars
  end

  let value tyenv bound_vars v =
    let o, _, _ = (new visitor tyenv bound_vars)#value v in
      o#get_free_vars

  let tail_computation tyenv bound_vars e =
    let o, _, _ = (new visitor tyenv bound_vars)#tail_computation e in
      o#get_free_vars

  let binding tyenv bound_vars bs =
    let o, _ = (new visitor tyenv bound_vars)#binding bs in
      o#get_free_vars

  let bindings tyenv bound_vars bs =
    let o, _ = (new visitor tyenv bound_vars)#bindings bs in
      o#get_free_vars

  let computation tyenv bound_vars e =
    let o, _, _ = (new visitor tyenv bound_vars)#computation e in
      o#get_free_vars

  let program = computation
end


(* module ClosureTable : *)
(* sig *)
(*   val value : Types.datatype Env.Int.t -> intset ->  value -> t *)
(*   val tail_computation : Types.datatype Env.Int.t -> intset -> tail_computation -> t *)
(*   val computation : Types.datatype Env.Int.t -> intset -> computation -> t *)
(*   val bindings : Types.datatype Env.Int.t -> intset -> binding list -> t *)
(*   val program : Types.datatype Env.Int.t -> intset -> program -> t *)
(* end *)


(* TODO:

   Computing the cont_vars table can probably be speeded up.

   Currently it can be quadratic because it can invoke FreeVars on the
   same term multiple times.

   One option would be to combine this computation with
   Closures.ClosureVars, which already computes the free variables of
   functions.
*)
(** Compute the closures in an IR expression

    This traversal computes the local free variables for each
    continuation in an IR expression.
*)
module ClosureTable =
struct
  class visitor tyenv bound_vars cont_vars =
  object (o : 'self_type)
    inherit IrTraversals.Transform.visitor(tyenv) as super

    val globals = bound_vars
    val cont_vars = cont_vars

    method close f vars =
      Hashtbl.add cont_vars (Var.var_of_binder f) vars
      (* {< relevant_vars = IntMap.add (Var.var_of_binder f) vars relevant_vars >} *)

    method global x =
      {< globals = IntSet.add x globals >}

    method! binder b =
      let o, b = super#binder b in
        if Scope.is_global (Var.scope_of_binder b) then
          o#global (Var.var_of_binder b), b
        else
          o, b

    (** Incrementally compute the free variables for every possible
       continuation arising from a computation.

       The first argument is the free variables in the tail. The
       second argument is the list of bindings in reverse order.

       The list of bindings is in reverse order in order to make
       things both easier to express and more efficient.
    *)
    method close_cont fvs : Ir.binding list -> 'self_type =
      function
        | [] -> o
        | Let (x, (_tyvars, body))::bs ->
            let fvs = IntSet.remove (Var.var_of_binder x) fvs in
            let fvs' = FreeVars.tail_computation o#get_type_environment globals body in
            (* we record the relevant free variables of the body *)
            o#close x fvs;
            o#close_cont (IntSet.union fvs fvs') bs
        | Fun fundef::bs ->
            let {fn_binder = f ; fn_params = xs; fn_body; fn_closure = z; _} = fundef in
            let fvs = IntSet.remove (Var.var_of_binder f) fvs in
            let xs = match z with None -> xs | Some z -> z :: xs in
            let bound_vars =
              List.fold_right
                (fun x bound_vars ->
                   IntSet.add (Var.var_of_binder x) bound_vars)
                xs
                globals in
            let fvs' = FreeVars.computation o#get_type_environment bound_vars fn_body in
            o#close_cont (IntSet.union fvs fvs') bs
        | Rec defs::bs ->
            let fvs, bound_vars =
              List.fold_right
                (fun fundef (fvs, bound_vars) ->
                   let {fn_binder = f; fn_params = xs; fn_closure = z; _} = fundef in
                   let f = Var.var_of_binder f in
                   let fvs = IntSet.remove f fvs in
                   let xs = match z with None -> xs | Some z -> z :: xs in
                   let bound_vars =
                     List.fold_right
                       (fun x bound_vars ->
                          IntSet.add (Var.var_of_binder x) bound_vars)
                       xs
                       (IntSet.add f bound_vars) in
                     fvs, bound_vars)
                defs
                (fvs, globals) in

            let fvs' =
              List.fold_left
                (fun fvs' {fn_body; _} ->
                   IntSet.union fvs' (FreeVars.computation o#get_type_environment bound_vars fn_body))
                (IntSet.empty)
                defs in

            o#close_cont (IntSet.union fvs fvs') bs
        | Alien { binder; _ } :: bs ->
           let f = Var.var_of_binder binder in
           let fvs = IntSet.remove f fvs in
           o#close_cont fvs bs
        | Module _::_ ->
            assert false

    method! computation : Ir.computation -> ('self_type * Ir.computation * Types.datatype) =
      fun (bs, tc) ->
        let o, bs = o#bindings bs in
        let o, tc, t = o#tail_computation tc in

        let free_vars = FreeVars.tail_computation o#get_type_environment globals tc in
        let o = o#close_cont free_vars (List.rev bs) in
          o, (bs, tc), t
  end

  let bindings tyenv bound_vars cont_vars bs =
    let o = new visitor tyenv bound_vars cont_vars in
    let _ = o#computation (bs, Return (Extend (StringMap.empty, None))) in ()

  let program tyenv bound_vars cont_vars e =
    let _ = (new visitor tyenv bound_vars cont_vars)#computation e in ()
end

let program state program =
  let open IrTransform in
  let tenv = Context.variable_environment (context state) in
  let globals = state.primitive_vars in
  FunDefs.program Tables.fun_defs program;
  ScopesAndContDefs.primitives Tables.scopes;
  ScopesAndContDefs.program tenv Tables.scopes Tables.cont_defs program;
  ClosureTable.program tenv globals Tables.cont_vars program
