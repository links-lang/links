open Utility
open Ir

(* {0 Sugar To IR}

   This module implements a compiler from the syntactic sugar to the
   IR.

   It is based on the Compileir module which compiled the old IR to
   the new one.

   Both Compileir and this module use one-pass monadic transformations
   in order to perform administrative reductions on the fly. The basic
   idea is to use a non-standard evaluator which interprets source
   terms in a monadic semantics. The monadic semantics is chosen in
   such a way that we can reify semantic values as IR terms.

   We can view our implementation as an instance of normalisation by
   evaluation (where normalisation means reduction to monadic normal
   form).

   Unfortunately pattern-matching compilation screws up some of our
   abstractions. For instance, we have to pass in the environment to
   comp, letfun, letrec and switch; and we have to prematurely reify
   terms in comp and switch which is why we subsequently 'reflect'
   them. It would be nicer if we had a cleaner separation between the
   syntactic and the semantic worlds.
*)

(*
  TODO:

  - move `Section, `UnaryAppl, most of `InfixAppl, `RangeLit,
  `ListLit to frontend desugaring transformations
  - check that we're doing the right thing with tyvars
*)

(* It would be nice to use primitive functions to construct XML nodes
   and table handles. Then they could be moved to a frontend
   transformation.

   We should either use escape in the frontend and IR or call/cc in
   the frontend and IR. What we do at the moment (escape in the
   frontend and call/cc in the IR) is silly.
*)

let show_compiled_ir = Settings.add_bool ("show_compiled_ir", false, `User)

exception ASTSyntaxError = SourceCode.ASTSyntaxError

let dp = Sugartypes.dummy_position

type datatype = Types.datatype

module NEnv = Env.String
module TEnv = Env.Int

type nenv = var NEnv.t
type tenv = Types.datatype TEnv.t

type env = nenv * tenv * Types.row

let lookup_name_and_type name (nenv, tenv, _eff) =
  let var = NEnv.lookup nenv name in
    var, TEnv.lookup tenv var

let lookup_name name env = fst (lookup_name_and_type name env)
let lookup_type name env = snd (lookup_name_and_type name env)
let lookup_effects (_, _, eff)= eff

(* Hmm... shouldn't we need to use something like this? *)

(* let with_mailbox_type t (nenv, tenv) = *)
(*   let mb_var = NEnv.lookup nenv "_MAILBOX_" in *)
(*     (nenv, TEnv.bind tenv (mb_var, t)) *)

module type MONAD =
sig
  type 'a sem

  val lift : 'a -> 'a sem
  val bind : 'a sem -> ('a -> 'b sem) -> 'b sem
end

module type BINDINGMONAD =
sig
  include MONAD

  val reflect : (binding list * 'a) -> 'a sem

  val lift_binding : binding -> 'a -> 'a sem

  type 'a semt = ('a * datatype) sem
  val sem_type : 'a semt -> datatype
  val reify : tail_computation semt -> computation
end

module type INTERPRETATION =
sig
  type 'a sem

  val sem_type : 'a sem -> datatype
  val reify : tail_computation sem -> computation

  val value_of_comp : tail_computation sem -> value sem
  val comp_of_value : value sem -> tail_computation sem

  val constant : constant -> value sem
  val var : (var * datatype) -> value sem

  val escape : (var_info * Types.row * (var -> tail_computation sem)) -> tail_computation sem

  val tabstr : (Types.quantifier list * value sem) -> value sem
  val tappl : (value sem * Types.type_arg list) -> value sem

  val apply : (value sem * (value sem) list) -> tail_computation sem
  val apply_pure : (value sem * (value sem) list) -> value sem
  val condition : (value sem * tail_computation sem * tail_computation sem) -> tail_computation sem

  val comp : env -> (CompilePatterns.pattern * value sem * tail_computation sem) -> tail_computation sem
  val letvar : (var_info * tail_computation sem * (var -> tail_computation sem)) -> tail_computation sem

  val xml : value sem * string * (name * (value sem) list) list * (value sem) list -> value sem
  val record : (name * value sem) list * (value sem) option -> value sem

  val project : value sem * name -> value sem
  val update : value sem * (name * value sem) list -> value sem

  val coerce : value sem * datatype -> value sem

  val query : (value sem * value sem) option * tail_computation sem -> tail_computation sem

  val db_update : env -> (CompilePatterns.pattern * value sem * tail_computation sem option * tail_computation sem) -> tail_computation sem
  val db_delete : env -> (CompilePatterns.pattern * value sem * tail_computation sem option) -> tail_computation sem

  val switch : env -> (value sem * (CompilePatterns.pattern * (env -> tail_computation sem)) list * Types.datatype) -> tail_computation sem

  val inject : name * value sem * datatype -> value sem
  (* val case : *)
  (*   value sem * string * (var_info * (var -> tail_computation sem)) * *)
  (*   (var_info * (var -> tail_computation sem)) option -> *)
  (*   tail_computation sem *)
  val case_zero : value sem * datatype -> tail_computation sem

  val concat : (value sem * value sem * (value sem) list) -> value sem

  val database : value sem -> tail_computation sem

  val table_handle : value sem * value sem * value sem * (datatype * datatype * datatype) -> tail_computation sem

  val wrong : datatype -> tail_computation sem

  val letfun :
    env ->
    (var_info * (Types.quantifier list * (CompilePatterns.pattern list * tail_computation sem)) * location) ->
    (var -> tail_computation sem) ->
    tail_computation sem

  val letrec :
    env ->
    (var_info * (Types.quantifier list * (CompilePatterns.pattern list * (var list -> tail_computation sem))) * location) list ->
    (var list -> tail_computation sem) ->
    tail_computation sem

  val alien : var_info * language * (var -> tail_computation sem) -> tail_computation sem

  val select : name * value sem -> tail_computation sem

  val offer : env -> (value sem * (CompilePatterns.pattern * (env -> tail_computation sem)) list * Types.datatype) -> tail_computation sem
end

module BindingListMonad : BINDINGMONAD =
struct
  type 'a sem = binding list * 'a

  let lift a = ([], a)
  let bind (bs, a) f =
    let (bs', a') = f a in
      (bs @ bs', a')

  let reflect (bs, v) = (bs, v)

  let lift_binding b a =
    ([b], a)

  let rec lift_bindings v =
    function
      | [] -> lift v
      | b :: bs ->
          bind (lift_binding b ()) (fun () -> lift_bindings v bs)

  type 'a semt = ('a * datatype) sem
  let sem_type (_, (_, t)) = t
  let reify (bs, (e, t)) = (bs, e)
end

module BindingContinuationMonad : BINDINGMONAD =
struct
  type 'a sem = ('a -> computation * datatype) -> computation * datatype

  let lift a = fun k -> k a
  let bind c f =
    (fun k ->
       c (fun a -> f a k))

  let reflect (bs, v) =
  fun k ->
    let (bs', tc), t = k v in
      (bs @ bs', tc), t

  let lift_binding b a =
    (fun k ->
       let (bs, a'), t = k a
       in
         (b :: bs, a'), t)

  type 'a semt = ('a * datatype) sem
  let reify c =
    let (e, _) =
      c (fun (a, t) -> ([], a), t)
    in
      e

  let dummy_computation = `Special (`Wrong `Not_typed)
  let sem_type s =
    let (_, t) =
      s (fun (_, t) -> ([], dummy_computation), t)
    in
      t
end

module Interpretation(M : BINDINGMONAD) : INTERPRETATION =
struct
  type 'a sem = 'a M.semt

  let lift = M.lift
  let bind s f = M.bind s (fun (a, _) -> f a)

  let lift_binding = M.lift_binding

  let sem_type = M.sem_type
  let reify = M.reify
  let reflect = M.reflect

  module S :
  sig
    val lift_option : ('a sem) option -> ('a option) M.sem
    val lift_list : ('a sem) list -> ('a list) M.sem

    val lift_alist : ('a*'b sem) list -> (('a*'b) list) M.sem

    val comp_binding : var_info * tail_computation -> var M.sem

    val fun_binding :
      Var.var_info * (tyvar list * binder list * computation) * location ->
      Var.var M.sem
    val rec_binding :
      (Var.var_info * (tyvar list * binder list * (Var.var list -> computation))
       * location) list ->
      (Var.var list) M.sem

    val alien_binding : var_info * language -> var M.sem

    val value_of_untyped_var : var M.sem * datatype -> value sem
  end =
  struct
    let lift_option =
      function
        | None -> lift None
        | Some s ->
            bind s (fun v -> lift (Some v))

    let lift_list ss =
      List.fold_right
        (fun s s' ->
           bind s
             (fun v ->
                M.bind s'
                  (fun vs -> lift (v :: vs))))
        ss (lift [])

    let lift_alist ss =
      List.fold_right
        (fun (name, s) ss ->
           bind s
             (fun v ->
                M.bind ss
                  (fun vs -> lift ((name, v) :: vs))))
        ss (lift [])

    let comp_binding (x_info, e) =
      let xb, x = Var.fresh_var x_info in
        lift_binding (letm (xb, e)) x

    let fun_binding (f_info, (tyvars, xsb, body), location) =
      let fb, f = Var.fresh_var f_info in
        lift_binding (`Fun (fb, (tyvars, xsb, body), None, location)) f

    let rec_binding defs =
      let defs, fs =
        List.fold_right
          (fun (f_info, (tyvars, xsb, body), location) (defs, fs) ->
             let fb, f = Var.fresh_var f_info in
               ((fb, (tyvars, xsb, body), None, location) :: defs, f :: fs))
          defs ([], [])
      in
        lift_binding
          (`Rec
             (List.map
                (fun (fb, (tyvars, xsb, body), None, location) ->
                   (fb, (tyvars, xsb, body fs), None, location))
                defs))
          fs

    let alien_binding (x_info, language) =
      let xb, x = Var.fresh_var x_info in
        lift_binding (`Alien (xb, language)) x

    let value_of_untyped_var (s, t) =
      M.bind s (fun x -> lift (`Variable x, t))
  end
  open S

  let value_of_comp s =
    bind s
      (function
         | `Return v -> lift (v, sem_type s)
         | e ->
             let t = sem_type s in
               value_of_untyped_var (comp_binding (Var.info_of_type t, e), t))

  let comp_of_value s =
    bind s (fun v -> lift (`Return v, sem_type s))

  (* eval parameters *)
  let constant c = lift (`Constant c, Constant.constant_type c)
  let var (x, t) = lift (`Variable x, t)

  let apply (s, ss) =
    let ss = lift_list ss in
    let t = TypeUtils.return_type (sem_type s) in
      bind s
        (fun v ->
           M.bind ss
             (fun vs -> lift (`Apply (v, vs), t)))

  let apply_pure (s, ss) =
    let ss = lift_list ss in
    let t = TypeUtils.return_type (sem_type s) in
      bind s
        (fun v ->
           M.bind ss
             (fun vs -> lift (`ApplyPure (v, vs), t)))

  let condition (s, s1, s2) =
    bind s (fun v -> lift (`If (v, reify s1, reify s2), sem_type s1))

  let concat (nil, append, ss) =
    match ss with
      | [] -> nil
      | [s] -> s
      | s::ss ->
          List.fold_left (fun s s' -> apply_pure (append, [s; s'])) s ss

  let string_concat (string_append, ss) =
    match ss with
      | [] -> lift (`Constant (`String ""), Types.string_type)
      | [s] -> s
      | s::ss ->
          List.fold_left (fun s s' -> apply_pure (string_append, [s; s'])) s ss

  let xml (string_append, name, attrs, children) =
    let lift_attrs attrs =
      List.fold_right
        (fun (name, ss) attrs ->
           bind (string_concat (string_append, ss))
             (fun v ->
                M.bind attrs
                  (fun bs -> lift ((name, v) :: bs))))
        attrs (lift []) in
    let attrs = lift_attrs attrs in
    let children = lift_list children in
      M.bind attrs
        (fun attrs ->
           M.bind children
             (fun children ->
                let attrs = StringMap.from_alist attrs in
                  lift (`XmlNode (name, attrs, children), Types.xml_type)))

  let record (fields, r) =
    let field_types =
      List.fold_left
        (fun field_types (name, s) -> StringMap.add name (sem_type s) field_types)
        StringMap.empty
        fields in
    let s' = lift_alist fields in
      match r with
        | None ->
            let t = Types.make_record_type field_types in
              M.bind s'
                (fun fields ->
                   lift (`Extend (StringMap.from_alist fields, None), t))
        | Some s ->
            let t = `Record (Types.extend_row field_types (TypeUtils.extract_row (sem_type s))) in
              bind s
                (fun r ->
                   M.bind s'
                     (fun fields -> lift (`Extend (StringMap.from_alist fields, Some r), t)))

  let project (s, name) =
    let t = TypeUtils.project_type name (sem_type s) in
      bind s (fun v -> lift (`Project (name, v), t))

  let erase (s, names) =
    let t = TypeUtils.erase_type names (sem_type s) in
      bind s (fun v -> lift (`Erase (names, v), t))

  let coerce (s, t) =
    bind s (fun v -> lift (`Coerce (v, t), sem_type s))

  (*
      (r : (l1:A1, ... li:Ai | R) with (l1=v1, ..., li=vi))
    -->
      (l1=v1, ..., li=vi | r ((-l1, ..., -li | R) <-- (l1:A1, ... li:Ai | R)))
  *)
  let update (s, fields) =
    let names =
      List.fold_left
        (fun names (name, _) ->
           StringSet.add name names)
        StringSet.empty
        fields in
    let t = TypeUtils.record_without (sem_type s) names in
      record (fields, Some (erase (s, names)))

  let inject (name, s, t) =
      bind s (fun v -> lift (`Inject (name, v, t), t))

  (* this isn't used... *)
  (* let case (s, name, (cinfo, cbody), default) = *)
  (*   bind s (fun v -> *)
  (*             let cb, c = Var.fresh_var cinfo in *)
  (*             let cbody' = cbody c in *)
  (*             let t = sem_type cbody' in *)
  (*             let default = *)
  (*               opt_map (fun (dinfo, dbody) -> *)
  (*                          let db, d = Var.fresh_var dinfo in *)
  (*                            (db, reify (dbody d))) default *)
  (*             in *)
  (*               lift *)
  (*                 (`Case (v, *)
  (*                         StringMap.add name (cb, reify cbody') StringMap.empty, *)
  (*                         default), t)) *)

  let case_zero (s, t) =
    bind s (fun v ->
              lift (`Case (v, StringMap.empty, None), t))

  let database s =
    bind s (fun v -> lift (`Special (`Database v), `Primitive (`DB)))

  let table_handle (database, table, keys, (r, w, n)) =
    bind database
      (fun database ->
         bind table
           (fun table -> 
	     bind keys
		(fun keys ->  lift (`Special (`Table (database, table, keys, (r, w, n))),
                               `Table (r, w, n)))))

  let wrong t = lift (`Special (`Wrong t), t)

  let alien (x_info, language, rest) =
    M.bind (alien_binding (x_info, language)) rest

  let select (l, e) =
    let t = TypeUtils.select_type l (sem_type e) in
      bind e (fun v -> lift (`Special (`Select (l, v)), t))

  let offer env (v, cases, t) =
    let cases =
      List.map
        (fun (p, body) -> ([p], fun env -> reify (body env))) cases
    in
      bind v
        (fun e ->
           M.bind
             (comp_binding (Var.info_of_type (sem_type v), `Return e))
             (fun var ->
                let nenv, tenv, eff = env in
                let tenv = TEnv.bind tenv (var, sem_type v) in
                let (bs, tc) = CompilePatterns.compile_choices (nenv, tenv, eff) (t, var, cases) in
                  reflect (bs, (tc, t))))

  let db_update env (p, source, where, body) =
    let source_type = sem_type source in
    let xt = TypeUtils.table_read_type source_type in
    let xb, x = Var.fresh_var_of_type xt in
      bind source
        (fun source ->
           match where with
             | None ->
                 let body_type = sem_type body in
                 let body = CompilePatterns.let_pattern env p (`Variable x, xt) (reify body, body_type) in
                   lift (`Special (`Update ((xb, source), None, body)), Types.unit_type)
             | Some where ->
                 let body_type = sem_type body in
                 let wrap = CompilePatterns.let_pattern env p (`Variable x, xt) in
                 let where = wrap (reify where, Types.bool_type) in
                 let body = wrap (reify body, body_type) in
                   lift (`Special (`Update ((xb, source), Some where, body)), Types.unit_type))

  let db_delete env (p, source, where) =
    let source_type = sem_type source in
    let xt = TypeUtils.table_read_type source_type in
    let xb, x = Var.fresh_var_of_type xt in
      bind source
        (fun source ->
           match where with
             | None ->
                 lift (`Special (`Delete ((xb, source), None)), Types.unit_type)
             | Some where ->
                 let where = CompilePatterns.let_pattern env p (`Variable x, xt) (reify where, Types.bool_type) in
                   lift (`Special (`Delete ((xb, source), Some where)), Types.unit_type))

  let query (range, s) =
    let bs, e = reify s in
      match range with
        | None ->
            lift (`Special (`Query (None, (bs, e), sem_type s)), sem_type s)
        | Some (limit, offset) ->
            bind limit
              (fun limit ->
                 bind offset
                   (fun offset ->
                      lift (`Special (`Query (Some (limit, offset), (bs, e), sem_type s)), sem_type s)))

  let letvar (x_info, s, body) =
    bind s
      (fun e ->
         M.bind (comp_binding (x_info, e))
           (fun x -> body x))

  let comp env (p, s, body) =
    let vt = sem_type s in
      bind s
        (fun v ->
           let body_type = sem_type body in
           let (bs, tc) = CompilePatterns.let_pattern env p (v, vt) (reify body, body_type) in
             reflect (bs, (tc, body_type)))

  let escape ((kt, _, _) as k_info, eff, body) =
    let kb, k = Var.fresh_var k_info in
    let body = body k in
    let body_type = sem_type body in
    let body = reify body in
    let ft = `Function (Types.make_tuple_type [kt], eff, body_type) in
    let f_info = (ft, "", `Local) in
    let rest f : tail_computation sem = lift (`Special (`CallCC (`Variable f)),
                                              body_type) in
      M.bind (fun_binding (f_info, ([], [kb], body), `Unknown)) rest

  let letfun env ((ft, _, _) as f_info, (tyvars, (ps, body)), location) rest =
    let xsb : binder list =
      (* It is important to rename the quantifiers in the type to be
         those used in the body of the function. *)
      match Instantiate.replace_quantifiers ft tyvars with
        | `ForAll (_, t')
        | t' ->
            begin match TypeUtils.concrete_type t' with
              | `Function _ as ft' ->
                  let args = TypeUtils.arg_types ft' in
                    List.map (fun arg ->
                                Var.fresh_binder_of_type arg) args
              | `Lolli _ as ft' ->
                  let args = TypeUtils.arg_types ft' in
                    List.map (fun arg ->
                                Var.fresh_binder_of_type arg) args
              | _ -> assert false
            end in

    let body_type = sem_type body in
    let body =
      List.fold_left2
        (fun body p (xb : binder) ->
           let x = Var.var_of_binder xb in
           let xt = Var.type_of_binder xb in
             CompilePatterns.let_pattern env p (`Variable x, xt) (body, body_type))
        (reify body)
        ps
        xsb
    in
      M.bind (fun_binding (f_info, (tyvars, xsb, body), location)) rest
(*        fun_binding (f_info, (tyvars, (xs_info, ps, body)), location) rest *)

  let letrec env defs rest =
    let defs =
      List.map
        (fun ((ft, _, _) as f_info, (tyvars, (ps, body)), location) ->
           let xsb : binder list =
             (* It is important to rename the quantifiers in the type to be those used in
                the body of the function. *)
             match Instantiate.replace_quantifiers ft tyvars with
               | `ForAll (_, t')
               | t' ->
                   begin match TypeUtils.concrete_type t' with
                     | `Function _ as ft' ->
                         let args = TypeUtils.arg_types ft' in
                           List.map (Var.fresh_binder_of_type) args
                     | _ -> assert false
                   end in
           let body fs =
             let body = body fs in
             let body_type = sem_type body in
               List.fold_left2
                 (fun body p xb ->
                    let x = Var.var_of_binder xb in
                    let xt = Var.type_of_binder xb in
                      CompilePatterns.let_pattern env p (`Variable x, xt) (body, body_type))
                 (reify body)
                 ps
                 xsb
           in
             (f_info, (tyvars, xsb, body), location))
        defs
    in
      M.bind (rec_binding defs) rest

  let switch env (v, cases, t) =
    let cases =
      List.map
        (fun (p, body) -> ([p], fun env -> reify (body env))) cases
    in
      bind v
        (fun e ->
           M.bind
             (comp_binding (Var.info_of_type (sem_type v), `Return e))
             (fun var ->
                let nenv, tenv, eff = env in
                let tenv = TEnv.bind tenv (var, sem_type v) in
                let (bs, tc) = CompilePatterns.compile_cases (nenv, tenv, eff) (t, var, cases) in
                  reflect (bs, (tc, t))))

  let tabstr (tyvars, s) =
    let t = Types.for_all (tyvars, sem_type s) in
      bind s (fun v -> lift (`TAbs (tyvars, v), t))

  let tappl (s, tyargs) =
    let t = Instantiate.apply_type (sem_type s) tyargs in
      bind s (fun v -> lift (`TApp (v, tyargs), t))
end


module Eval(I : INTERPRETATION) =
struct
  let extend xs vs (nenv, tenv, eff) =
    List.fold_left2
      (fun (nenv, tenv, eff) x (v, t) ->
         (NEnv.bind nenv (x, v), TEnv.bind tenv (v, t), eff))
      (nenv, tenv, eff)
      xs
      vs

  let (++) (nenv, tenv, _) (nenv', tenv', eff') = (NEnv.extend nenv nenv', TEnv.extend tenv tenv', eff')

  let rec eval : env -> Sugartypes.phrase -> tail_computation I.sem =
    fun env (e, pos) ->
      let lookup_var name =
        let x, xt = lookup_name_and_type name env in
          I.var (x, xt) in
      let instantiate name tyargs =
        let x, xt = lookup_name_and_type name env in
          match tyargs with
            | [] -> I.var (x, xt)
            | _ ->
                try
                  I.tappl (I.var (x, xt), tyargs)
                with
                    Instantiate.ArityMismatch ->
                      prerr_endline ("Arity mismatch in instantiation (Sugartoir)");
                      prerr_endline ("name: "^name);
                      prerr_endline ("type: "^Types.string_of_datatype xt);
                      prerr_endline ("tyargs: "^String.concat "," (List.map Types.string_of_type_arg tyargs));
                      failwith "fatal internal error" in

      let rec is_pure_primitive (e, _) =
        match e with
          | `TAbstr (_, e)
          | `TAppl (e, _) -> is_pure_primitive e
          | `Var f when Lib.is_pure_primitive f -> true
          | _ -> false in

      let eff = lookup_effects env in
      let instantiate_mb name = instantiate name [`Row eff] in
      let cofv = I.comp_of_value in
      let ec = eval env in
      let ev = evalv env in
      let evs = List.map ev in
        match e with
          | `Constant c -> cofv (I.constant c)
          | `Var x -> cofv (I.var (lookup_name_and_type x env))
          | `RangeLit (low, high) ->
              I.apply (instantiate_mb "intRange", [ev low; ev high])
          | `ListLit ([], Some t) ->
              cofv (instantiate "Nil" [`Type t])
          | `ListLit (e::es, Some t) ->
              cofv (I.apply_pure(instantiate "Cons" [`Type t; `Row eff], [ev e; ev ((`ListLit (es, Some t)), pos)]))
          | `Escape ((k, Some kt, _), body) ->
              I.escape ((kt, k, `Local), eff, fun v -> eval (extend [k] [(v, kt)] env) body)
          | `Section (`Minus) -> cofv (lookup_var "-")
          | `Section (`FloatMinus) -> cofv (lookup_var "-.")
          | `Section (`Name name) -> cofv (lookup_var name)
          | `Conditional (p, e1, e2) ->
              I.condition (ev p, ec e1, ec e2)
          | `InfixAppl ((tyargs, `Name ((">" | ">=" | "==" | "<" | "<=" | "<>") as op)), e1, e2) ->
              cofv (I.apply_pure (instantiate op tyargs, [ev e1; ev e2]))
          | `InfixAppl ((tyargs, `Name "++"), e1, e2) ->
              cofv (I.apply_pure (instantiate "Concat" tyargs, [ev e1; ev e2]))
          | `InfixAppl ((tyargs, `Name "!"), e1, e2) ->
              I.apply (instantiate "Send" tyargs, [ev e1; ev e2])
          | `InfixAppl ((tyargs, `Name n), e1, e2) when Lib.is_pure_primitive n ->
              cofv (I.apply_pure (instantiate n tyargs, [ev e1; ev e2]))
          | `InfixAppl ((tyargs, `Name n), e1, e2) ->
              I.apply (instantiate n tyargs, [ev e1; ev e2])
          | `InfixAppl ((tyargs, `Cons), e1, e2) ->
              cofv (I.apply_pure (instantiate "Cons" tyargs, [ev e1; ev e2]))
          | `InfixAppl ((tyargs, `FloatMinus), e1, e2) ->
              cofv (I.apply_pure (instantiate "-." tyargs, [ev e1; ev e2]))
          | `InfixAppl ((tyargs, `Minus), e1, e2) ->
              cofv (I.apply_pure (instantiate "-" tyargs, [ev e1; ev e2]))
          | `InfixAppl ((_tyargs, `And), e1, e2) ->
              (* IMPORTANT: we compile boolean expressions to
                 conditionals in order to faithfully capture
                 short-circuit evaluation *)
              I.condition (ev e1, ec e2, cofv (I.constant (`Bool false)))
          | `InfixAppl ((_tyargs, `Or), e1, e2) ->
              I.condition (ev e1, cofv (I.constant (`Bool true)), ec e2)
          | `UnaryAppl ((_tyargs, `Minus), e) ->
              cofv (I.apply_pure(instantiate_mb "negate", [ev e]))
          | `UnaryAppl ((_tyargs, `FloatMinus), e) ->
              cofv (I.apply_pure(instantiate_mb "negatef", [ev e]))
          | `UnaryAppl ((tyargs, `Name n), e) when Lib.is_pure_primitive n ->
              cofv (I.apply_pure(instantiate n tyargs, [ev e]))
          | `UnaryAppl ((tyargs, `Name n), e) ->
              I.apply (instantiate n tyargs, [ev e])
          | `FnAppl ((`Var f, _), es) when Lib.is_pure_primitive f ->
              cofv (I.apply_pure (I.var (lookup_name_and_type f env), evs es))
          | `FnAppl ((`TAppl ((`Var f, _), tyargs), _), es) when Lib.is_pure_primitive f ->
              cofv (I.apply_pure (instantiate f tyargs, evs es))
          | `FnAppl (e, es) when is_pure_primitive e ->
              cofv (I.apply_pure (ev e, evs es))
          | `FnAppl (e, es) ->
              I.apply (ev e, evs es)
          | `TAbstr (tyvars, e) ->
              let v = ev e in
              let vt = I.sem_type v in
                cofv (I.tabstr (Types.unbox_quantifiers tyvars, v))
          | `TAppl (e, tyargs) ->
              let v = ev e in
              let vt = I.sem_type v in
                begin
                  try
                    cofv (I.tappl (v, tyargs))
                  with
                      Instantiate.ArityMismatch ->
                        prerr_endline ("Arity mismatch in type application (Sugartoir)");
                        prerr_endline ("expression: " ^ Sugartypes.Show_phrasenode.show (`TAppl (e, tyargs)));
                        prerr_endline ("type: "^Types.string_of_datatype vt);
                        prerr_endline ("tyargs: "^String.concat "," (List.map Types.string_of_type_arg tyargs));
                        failwith "fatal internal error"
                end
          | `TupleLit [e] ->
              (* It isn't entirely clear whether there should be any 1-tuples at this stage,
                 but if there are we should get rid of them.

                 The parser certainly doesn't disallow them.
              *)
              ec e
          | `TupleLit es ->
              let fields = mapIndex (fun e i -> (string_of_int (i+1), ev e)) es in
                cofv (I.record (fields, None))
          | `RecordLit (fields, rest) ->
              cofv
                (I.record
                   (List.map (fun (name, e) -> (name, ev e)) fields,
                    opt_map ev rest))
          | `Projection (e, name) ->
              cofv (I.project (ev e, name))
          | `With (e, fields) ->
              cofv (I.update
                      (ev e,
                       List.map (fun (name, e) -> (name, ev e)) fields))
          | `TypeAnnotation (e, _) ->
              (* we might consider getting rid of type annotations before here *)
              ec e
          | `Upcast (e, (_, Some t), _) ->
              cofv (I.coerce (ev e, t))
          | `ConstructorLit (name, None, Some t) ->
              cofv (I.inject (name, I.record ([], None), t))
          | `ConstructorLit (name, Some e, Some t) ->
              cofv (I.inject (name, ev e, t))

          | `Switch (e, cases, Some t) ->
              let cases =
                List.map
                  (fun (p, body) ->
                     let p, penv = CompilePatterns.desugar_pattern `Local p in
                       (p, fun env ->  eval (env ++ penv) body))
                  cases
              in
                I.switch env (ev e, cases, t)
          | `DatabaseLit (name, (None, args)) ->
              I.database (ev (`RecordLit ([("name", name)],
                                          Some (`FnAppl ((`Var "getDatabaseConfig", pos), []), pos)), pos))
          | `DatabaseLit (name, (Some driver, args)) ->
              let args =
                match args with
                  | None -> `Constant (`String ""), pos
                  | Some args -> args
              in
                I.database
                  (ev (`RecordLit ([("name", name); ("driver", driver); ("args", args)], None), pos))
          | `TableLit (name, (_, Some (readtype, writetype, neededtype)), constraints, keys, db) ->
              I.table_handle (ev db, ev name, ev keys, (readtype, writetype, neededtype))
          | `Xml (tag, attrs, attrexp, children) ->
              (* check for duplicates *)
              let () =
                let rec dup_check names =
                  function
                    | [] -> ()
                    | (name, _) :: attrs ->
                        if StringSet.mem name names then
                          raise (Errors.SugarError (pos,
                                                 "XML attribute '"^name^"' is defined more than once"))
                        else
                          dup_check (StringSet.add name names) attrs
                in
                  dup_check StringSet.empty attrs
              in
                if tag = "#" then
                  if List.length attrs != 0 || attrexp <> None then
                    raise (Errors.SugarError (pos,
                                           "XML forest literals cannot have attributes"))
                  else
                    cofv
                      (I.concat (instantiate "Nil" [`Type (`Primitive `XmlItem)],
                                 instantiate "Concat" [`Type (`Primitive `XmlItem); `Row eff],
                                 List.map ev children))
                else
                  let attrs = alistmap (List.map ev) attrs in
                  let children = List.map ev children in
                  let body =
                         I.xml (instantiate "^^" [`Row eff],
                                tag, attrs, children)
                  in
                    begin match attrexp with
                      | None -> cofv body
                      | Some e ->
                          cofv (I.apply_pure (instantiate_mb "addAttributes", [body; ev e]))
                    end
          | `TextNode name ->
              cofv
                (I.apply_pure
                   (instantiate_mb "stringToXml", [ev (`Constant (`String name), pos)]))
          | `Block (bs, e) -> eval_bindings `Local env bs e
          | `Query (range, e, _) ->
              I.query (opt_map (fun (limit, offset) -> (ev limit, ev offset)) range, ec e)

          | `DBUpdate (p, source, where, fields) ->
              let p, penv = CompilePatterns.desugar_pattern `Local p in
              let env' = env ++ penv in
              let source = ev source in
              let where =
                opt_map
                  (fun where -> eval env' where)
                  where in
              let body = eval env' (`RecordLit (fields, None), dp) in
                I.db_update env (p, source, where, body)
          | `DBDelete (p, source, where) ->
              let p, penv = CompilePatterns.desugar_pattern `Local p in
              let env' = env ++ penv in
              let source = ev source in
              let where =
                opt_map
                  (fun where -> eval env' where)
                  where
              in
                I.db_delete env (p, source, where)

          | `Select (l, e) ->
             I.select (l, ev e)
          | `Offer (e, cases, Some t) ->
              let cases =
                List.map
                  (fun (p, body) ->
                     let p, penv = CompilePatterns.desugar_pattern `Local p in
                       (p, fun env ->  eval (env ++ penv) body))
                  cases
              in
                I.offer env (ev e, cases, t)

                  (* These things should all have been desugared already *)
          | `Spawn _
          | `Receive _
          | `Section (`Project _)
          | `FunLit _
          | `Iteration _
          | `InfixAppl ((_, `RegexMatch _), _, _)
          | `DBInsert _
          | `Regex _
          | `Formlet _
          | `Page _
          | `FormletPlacement _
          | `PagePlacement _
          | `FormBinding _
          | `CP _ ->
              Debug.print ("oops: " ^ Sugartypes.Show_phrasenode.show e);
              assert false

  and eval_bindings scope env bs' e =
    let cofv = I.comp_of_value in
    let ec = eval env in
    let ev = evalv env in
      match bs' with
        | [] -> ec e
        | (b,bpos)::bs ->
            begin
              match b with
                | `Val (_, (`Variable (x, Some xt, _xpos), _), body, _, _) ->
                    let x_info = (xt, x, scope) in
                      I.letvar
                        (x_info,
                         ec body,
                         fun v ->
                           eval_bindings scope (extend [x] [(v, xt)] env) bs e)
                | `Val (_, p, body, _, _) ->
                    let p, penv = CompilePatterns.desugar_pattern scope p in
                    let env' = env ++ penv in
                    let s = ev body in
                    let ss = eval_bindings scope env' bs e in
                      I.comp env (p, s, ss)
                | `Fun ((f, Some ft, _), _, (tyvars, ([ps], body)), location, pos) ->
                    let ps, body_env =
                      List.fold_right
                        (fun p (ps, body_env) ->
                           let p, penv = CompilePatterns.desugar_pattern `Local p in
                             p::ps, body_env ++ penv)
                        ps
                        ([], env) in
                    let body = eval body_env body in
                      I.letfun
                        env
                        ((ft, f, scope), (tyvars, (ps, body)), location)
                        (fun v -> eval_bindings scope (extend [f] [(v, ft)] env) bs e)
                | `Exp e' ->
                    I.comp env (`Any, ev e', eval_bindings scope env bs e)
                | `Funs defs ->
                    let fs, inner_fts, outer_fts =
                      List.fold_right
                        (fun ((f, Some outer, _), _, ((_tyvars, Some (inner, _extras)), _), _, _, _) (fs, inner_fts, outer_fts) ->
                              (f::fs, inner::inner_fts, outer::outer_fts))
                        defs
                        ([], [], []) in
                    let defs =
                      List.map
                        (fun ((f, Some ft, _), _, ((tyvars, _), ([ps], body)), location, t, pos) ->
                           let ps, body_env =
                             List.fold_right
                               (fun p (ps, body_env) ->
                                  let p, penv = CompilePatterns.desugar_pattern `Local p in
                                    p::ps, body_env ++ penv)
                               ps
                               ([], env) in
                           let body = fun vs -> eval (extend fs (List.combine vs inner_fts) body_env) body in
                             ((ft, f, scope), (tyvars, (ps, body)), location))
                        defs
                    in
                      I.letrec env defs (fun vs -> eval_bindings scope (extend fs (List.combine vs outer_fts) env) bs e)
                | `Foreign ((x, Some xt, _), language, t) ->
                    I.alien ((xt, x, scope), language, fun v -> eval_bindings scope (extend [x] [(v, xt)] env) bs e)
                | `Type _
                | `Infix ->
                    (* Ignore type alias and infix declarations - they
                       shouldn't be needed in the IR *)
                    eval_bindings scope env bs e
                | `Import _ -> assert false
            end

  and evalv env e =
    I.value_of_comp (eval env e)

  (* Given a program, return a triple consisting of:

     - globals
     - the main computation (a list of locals and a tail computation)
     - an environment mapping source names of global bindings to IR names

     The globals list contains all top-level bindings on which global
     bindings may depend, i.e., all top-level bindings from the start
     of the file up to and including the last global binding.

     The locals list contains all top-level bindings on which global
     bindings may not depend, i.e., all top-level bindings after the
     last global binding. *)
  let partition_program : program -> binding list * computation * nenv =
    fun (bs, main) ->
    let rec partition (globals, locals, nenv) =
      function
        | [] -> List.rev globals, List.rev locals, nenv
        | b::bs ->
            begin
              match b with
                | `Let ((x, (_xt, x_name, `Global)), _) ->
                    partition (b::locals @ globals, [], Env.String.bind nenv (x_name, x)) bs
                | `Fun ((f, (_ft, f_name, `Global)), _, _, _) ->
                    partition (b::locals @ globals, [], Env.String.bind nenv (f_name, f)) bs
                | `Rec defs ->
                  (* we depend on the invariant that mutually
                     recursive definitions all have the same scope *)
                    let scope, nenv =
                      List.fold_left
                        (fun (scope, nenv) ((f, (_ft, f_name, f_scope)), _, _, _) ->
                           match f_scope with
                             | `Global -> `Global, Env.String.bind nenv (f_name, f)
                             | `Local -> scope, nenv)
                        (`Local, nenv) defs
                    in
                      begin
                        match scope with
                          | `Global ->
                              partition (b::locals @ globals, [], nenv) bs
                          | `Local ->
                              partition (globals, b::locals, nenv) bs
                      end
                | `Alien ((f, (_ft, f_name, `Global)), _) ->
                    partition (b::locals @ globals, [], Env.String.bind nenv (f_name, f)) bs
                | _ -> partition (globals, b::locals, nenv) bs
            end in
    let globals, locals, nenv = partition ([], [], Env.String.empty) bs in
      globals, (locals, main), nenv

  let compile env (bindings, body) =
    Debug.print ("compiling to IR");
(*     Debug.print (Sugartypes.Show_program.show (bindings, body)); *)
    let body =
      match body with
        | None -> (`RecordLit ([], None), dp)
        | Some body -> body in
      let s = eval_bindings `Global env bindings body in
        let r = (I.reify s) in
          Debug.print ("compiled IR");
          Debug.if_set show_compiled_ir (fun () -> Ir.Show_program.show r);
          r, I.sem_type s
end

module C = Eval(Interpretation(BindingListMonad))

let desugar_expression : env -> Sugartypes.phrase -> Ir.computation =
  fun env e ->
    let (bs, body), _ = C.compile env ([], Some e) in
      (bs, body)

let desugar_program : env -> Sugartypes.program -> Ir.binding list * Ir.computation * nenv =
  fun env p ->
    let (bs, body), _ = C.compile env p in
      C.partition_program (bs, body)

let desugar_definitions : env -> Sugartypes.binding list -> Ir.binding list * nenv =
  fun env bs ->
    let globals, _, nenv = desugar_program env (bs, None) in
      globals, nenv
