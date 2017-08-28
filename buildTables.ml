open Utility

module FunDefs =
struct
  type t = (Var.var, Ir.eval_fun_def) Hashtbl.t

  let make_eval_def : Ir.fun_def -> Ir.eval_fun_def =
    fun ((_f, info), (_tyvars, xs, body), z, location) ->
      info, (List.map Var.var_of_binder xs, body), opt_map Var.var_of_binder z, location

  let add fs def =
      let f = Var.var_of_binder (Ir.binder_of_fun_def def) in
      Hashtbl.add fs f (make_eval_def def)

  let binding fs = function
    | `Let _ -> ()
    | `Fun def -> add fs def
    | `Rec defs -> List.iter (add fs) defs
    | `Alien _ -> ()
    | `Module _ -> failwith "Not implemented"

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
    inherit Ir.Transform.visitor(tyenv) as super

    method bind_scope xb =
      Hashtbl.add scopes (Var.var_of_binder xb) (Var.scope_of_binder xb)

    method bind_cont (xb, e) =
      Hashtbl.add cont_defs (Var.var_of_binder xb) e

    method! binder =
      fun b ->
        let b, o = super#binder b in
        o#bind_scope b;
        b, o

    method! computation =
      fun e ->
        let (bs, main), t, o = super#computation e in
        let rec bind o =
          function
            | [] -> o
            | (`Let (x, _))::bs ->
              o#bind_cont (x, (bs, main));
              bind o bs
            | _::bs -> bind o bs
        in
          (bs, main), t, bind o bs
  end

  let primitives scopes =
    IntSet.iter (fun x -> Hashtbl.add scopes x `Global) !Lib.primitive_vars

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
    inherit Ir.Transform.visitor(tenv) as super

    val free_vars = IntSet.empty
    val bound_vars = bound_vars

    method bound x =
      {< bound_vars = IntSet.add x bound_vars >}

    method free x =
      if IntSet.mem x bound_vars then o
      else {< free_vars = IntSet.add x free_vars >}

    method! binder =
      fun b ->
        let b, o = super#binder b in
          b, o#bound (Var.var_of_binder b)

    method! var =
      fun x ->
        let x, t, o = super#var x in
          x, t, o#free x

    method get_free_vars = free_vars
  end

  let value tyenv bound_vars v =
    let _, _, o = (new visitor tyenv bound_vars)#value v in
      o#get_free_vars

  let tail_computation tyenv bound_vars e =
    let _, _, o = (new visitor tyenv bound_vars)#tail_computation e in
      o#get_free_vars

  let binding tyenv bound_vars bs =
    let _, o = (new visitor tyenv bound_vars)#binding bs in
      o#get_free_vars

  let bindings tyenv bound_vars bs =
    let _, o = (new visitor tyenv bound_vars)#bindings bs in
      o#get_free_vars

  let computation tyenv bound_vars e =
    let _, _, o = (new visitor tyenv bound_vars)#computation e in
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
  object (o)
    inherit Ir.Transform.visitor(tyenv) as super

    val globals = bound_vars
    val cont_vars = cont_vars

    method close f vars =
      Hashtbl.add cont_vars (Var.var_of_binder f) vars
      (* {< relevant_vars = IntMap.add (Var.var_of_binder f) vars relevant_vars >} *)

    method global x =
      {< globals = IntSet.add x globals >}

    method! binder b =
      let b, o = super#binder b in
        if Var.scope_of_binder b = `Global then
          b, o#global (Var.var_of_binder b)
        else
          b, o

    (** Incrementally compute the free variables for every possible
       continuation arising from a computation.

       The first argument is the free variables in the tail. The
       second argument is the list of bindings in reverse order.

       The list of bindings is in reverse order in order to make
       things both easier to express and more efficient.
    *)
    method close_cont fvs =
      function
        | [] -> o
        | `Let (x, (_tyvars, body))::bs ->
            let fvs = IntSet.remove (Var.var_of_binder x) fvs in
            let fvs' = FreeVars.tail_computation o#get_type_environment globals body in
            (* we record the relevant free variables of the body *)
            o#close x fvs;
            o#close_cont (IntSet.union fvs fvs') bs
        | `Fun (f, (_tyvars, xs, body), z, _)::bs ->
            let fvs = IntSet.remove (Var.var_of_binder f) fvs in
            let xs = match z with None -> xs | Some z -> z :: xs in
            let bound_vars =
              List.fold_right
                (fun x bound_vars ->
                   IntSet.add (Var.var_of_binder x) bound_vars)
                xs
                globals in
            let fvs' = FreeVars.computation o#get_type_environment bound_vars body in
            o#close_cont (IntSet.union fvs fvs') bs
        | `Rec defs::bs ->
            let fvs, bound_vars =
              List.fold_right
                (fun (f, (_tyvars, xs, _body), z, _) (fvs, bound_vars) ->
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
                (fun fvs' (_f, (_tyvars, _xs, body), _zs, _location) ->
                   IntSet.union fvs' (FreeVars.computation o#get_type_environment bound_vars body))
                (IntSet.empty)
                defs in

            o#close_cont (IntSet.union fvs fvs') bs
        | `Alien (f, _language)::bs ->
            let fvs = IntSet.remove (Var.var_of_binder f) fvs in
              o#close_cont fvs bs
        | `Module _::_ ->
            assert false

    method! computation : Ir.computation -> (Ir.computation * Types.datatype * 'self_type) =
      fun (bs, tc) ->
        let bs, o = o#bindings bs in
        let tc, t, o = o#tail_computation tc in

        let free_vars = FreeVars.tail_computation o#get_type_environment globals tc in
        let o = o#close_cont free_vars (List.rev bs) in
          (bs, tc), t, o
  end

  let bindings tyenv bound_vars cont_vars bs =
    let o = new visitor tyenv bound_vars cont_vars in
    let _ = o#computation (bs, `Return (`Extend (StringMap.empty, None))) in ()

  let program tyenv bound_vars cont_vars e =
    let _ = (new visitor tyenv bound_vars cont_vars)#computation e in ()
end

let bindings : Ir.Transform.environment -> intset -> Ir.binding list -> unit =
  fun tyenv bound_vars bs ->
    FunDefs.bindings Tables.fun_defs bs;
    ScopesAndContDefs.primitives Tables.scopes;
    ScopesAndContDefs.bindings tyenv Tables.scopes Tables.cont_defs bs;
    ClosureTable.bindings tyenv bound_vars Tables.cont_vars bs

let program : Ir.Transform.environment -> intset -> Ir.program -> unit =
  fun tyenv bound_vars program ->
    FunDefs.program Tables.fun_defs program;
    ScopesAndContDefs.primitives Tables.scopes;
    ScopesAndContDefs.program tyenv Tables.scopes Tables.cont_defs program;
    ClosureTable.program tyenv bound_vars Tables.cont_vars program
