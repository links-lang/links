open Utility
open Ir

(* This module implements a compiler from the syntactic sugar to the
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
  - implement desugar_expression
  - compile record erasure to `Coerce and remove `Erase from the IR 
*)

(* If we implemented comparisons as primitive functions then their
   desugaring could be moved to a frontend transformation.

   Similarly, it would be nice to use primitive functions to construct
   XML nodes and table handles. Then they could also be moved to a
   frontend transformation.

   We should either use escape in the frontend and IR or call/cc in
   the frontend and IR. What we do at the moment (escape in the
   frontend and call/cc in the IR) is silly.
*)


exception ASTSyntaxError = SourceCode.ASTSyntaxError

let dp = Sugartypes.dummy_position

type datatype = Types.datatype

module NEnv = Env.String
module TEnv = Env.Int

type nenv = var NEnv.t
type tenv = Types.datatype TEnv.t

type env = nenv * tenv

let lookup_name_and_type name (nenv, tenv) =
  let var = NEnv.lookup nenv name in
    var, TEnv.lookup tenv var

let lookup_name name env = fst (lookup_name_and_type name env)
let lookup_type name env = snd (lookup_name_and_type name env)

let with_mailbox_type t (nenv, tenv) =
  let mb_var = NEnv.lookup nenv "_MAILBOX_" in
    (nenv, TEnv.bind tenv (mb_var, t))

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

  val abs : value sem -> value sem
  val app : value sem * value sem -> tail_computation sem

  val escape : (var_info * Types.datatype * (var -> tail_computation sem)) -> tail_computation sem

  val tappl : (value sem * Types.type_arg list) -> value sem

  val apply : (value sem * (value sem) list) -> tail_computation sem
  val apply_pure : (value sem * (value sem) list) -> value sem
  val condition : (value sem * tail_computation sem * tail_computation sem) -> tail_computation sem
    (* comparison? *)
  val comparison : (value sem * Syntaxutils.comparison * value sem) -> value sem

  val comp : env -> (CompilePatterns.pattern * value sem * tail_computation sem) -> tail_computation sem
  val seq : tail_computation sem * tail_computation sem -> tail_computation sem

  val xml : value sem * value sem * string * (name * (value sem) list) list * (value sem) list -> value sem
  val record : (name * value sem) list * (value sem) option -> value sem

  val project : value sem * name -> value sem
  val update : value sem * (name * value sem) list -> value sem
(* erase? *)
  val erase :  name * value sem * datatype -> value sem

  val coerce : value sem * datatype -> value sem

  val switch : env -> (value sem * (CompilePatterns.pattern * (env -> tail_computation sem)) list * Types.datatype) -> tail_computation sem

  val inject : name * value sem * datatype -> value sem
  val case :
    value sem * string * (var_info * (var -> tail_computation sem)) *
    (var_info * (var -> tail_computation sem)) option ->
    tail_computation sem
  val case_zero : value sem * datatype -> tail_computation sem

  val concat : (value sem * value sem * (value sem) list) -> value sem

  val database : value sem -> tail_computation sem

  val table_query : SqlQuery.sqlQuery * datatype -> tail_computation sem
  val table_handle : value sem * value sem * (datatype * datatype) -> tail_computation sem

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
      Var.var_info * (tyvar list * binder list * computation) * location -> Var.var M.sem
    val rec_binding :
      (Var.var_info * (tyvar list * binder list * (Var.var list -> computation)) * location) list ->
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
        lift_binding (`Fun (fb, (tyvars, xsb, body), location)) f


    let rec_binding defs =
      let defs, fs =
        List.fold_right
          (fun (f_info, (tyvars, xsb, body), location) (defs, fs) ->
             let fb, f = Var.fresh_var f_info in
               ((fb, (tyvars, xsb, body), location) :: defs, f :: fs))
          defs ([], [])
      in
        lift_binding
          (`Rec
             (List.map
                (fun (fb, (tyvars, xsb, body), location) ->
                   (fb, (tyvars, xsb, body fs), location))
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
  let abs s = bind s (fun v -> lift (`Abs v, sem_type s))
  let app (s1, s2) =
    let t = TypeUtils.return_type (sem_type s1) in
      bind s1
        (fun v1 ->
           bind s2
             (fun v2 ->
                lift (`Special (`App (v1, v2)), t)))
        
  let apply (s, ss) =
(*     Debug.print ("sem_type s: "^Types.string_of_datatype (sem_type s)); *)
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

  let rec concat (nil, cons, ss) =
    List.fold_right (fun s ss -> apply_pure (cons, [s; ss])) ss nil

  let xml (nil, cons, name, attrs, children) =
    let lift_attrs attrs =
      List.fold_right
        (fun (name, ss) attrs ->
           bind (concat (nil, cons, ss))
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
                  lift (`XmlNode (name, attrs, children), `Primitive `XmlItem)))

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
      record (fields, Some (coerce (s, t)))

  (* TODO:

     Get rid of `Erase and use `Coerce instead once we have got rid of Syntax.

     We will need to be a bit careful about compiling coercions though, because
     ignoring them breaks non-parametric operations such as the version of
     equality currently implemented in Links.
  *)
  let erase (name, s, t) =
    bind s (fun v -> lift (`Erase (name, v), t))


  let inject (name, s, t) =
      bind s (fun v -> lift (`Inject (name, v, t), t))
  let case (s, name, (cinfo, cbody), default) =
    bind s (fun v ->
              let cb, c = Var.fresh_var cinfo in
              let cbody' = cbody c in
              let t = sem_type cbody' in
              let default =
                opt_map (fun (dinfo, dbody) ->
                           let db, d = Var.fresh_var dinfo in
                             (db, reify (dbody d))) default
              in
                lift
                  (`Case (v,
                          StringMap.add name (cb, reify cbody') StringMap.empty,
                          default), t))

  let case_zero (s, t) =
    bind s (fun v ->
              lift (`Case (v, StringMap.empty, None), t))

  let comparison (s, c, s') =
    bind s (fun v ->
              bind s' (fun v' -> lift (`Comparison (v, c, v'), Types.bool_type)))

  let database s =
    bind s (fun v -> lift (`Special (`Database v), `Primitive (`DB)))

  let table_query (query, t) =
    lift (`Special (`Query query), t)
  let table_handle (database, table, (r, w)) =
    bind database
      (fun database ->
         bind table
           (fun table -> lift (`Special (`Table (database, table, (r, w))), `Table (r, w))))

  let wrong t = lift (`Special (`Wrong t), t)

  let alien (x_info, language, rest) =
    M.bind (alien_binding (x_info, language)) rest
   
  let comp env (p, s, body) =
    let vt = sem_type s in
      bind s
        (fun v ->
           let body_type = sem_type body in
           let (bs, tc) = CompilePatterns.let_pattern env p (v, vt) (reify body, body_type) in
             reflect (bs, (tc, body_type)))

  let escape ((kt, _, _) as k_info, mb, body) =
    let kb, k = Var.fresh_var k_info in
    let body = body k in
    let body_type = sem_type body in
    let body = reify body in
    let ft = `Function (kt, mb, body_type) in
    let f_info = (ft, "", `Local) in      
    let rest f : tail_computation sem = lift (`Special (`CallCC (`Variable f)), body_type) in        
      M.bind (fun_binding (f_info, ([], [kb], body), `Unknown)) rest
       
  let letfun env ((ft, _, _) as f_info, (tyvars, (ps, body)), location) rest =
    let xsb : binder list =
      (* It is important to rename the quantifiers in the type to be those used in 
         the body of the function. *)
      match Instantiate.replace_quantifiers ft tyvars with
        | `ForAll (_, t')
        | t' ->
            begin
              match TypeUtils.concrete_type t' with
                | `Function _ as ft' ->
                    let args = TypeUtils.arg_types ft' in
                      List.map (fun arg ->
(*                                   Debug.print ("arg: "^Types.string_of_datatype arg); *)
                                  Var.fresh_binder_of_type arg) args
                | _ -> assert false
            end in
      
    let body_type = sem_type body in
    let body =
      List.fold_left2
        (fun body p (xb : binder) ->
           let x = Var.var_of_binder xb in
           let xt = Var.type_of_binder xb in
(*              Debug.print ("ft: "^Types.string_of_datatype ft); *)
(*              Debug.print ("xt: "^Types.string_of_datatype xt); *)
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
                   begin
                     match TypeUtils.concrete_type t' with
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
                let nenv, tenv = env in
                let tenv = TEnv.bind tenv (var, sem_type v) in
                let (bs, tc) = CompilePatterns.compile_cases (nenv, tenv) (t, var, cases) in
                  reflect (bs, (tc, t))))

  let seq (s, s') =
    bind s (fun _ -> s')

  let tappl (s, tyargs) =
    let t = Instantiate.apply_type (sem_type s) tyargs in
      bind s (fun v -> lift (`TApp (v, tyargs), t))
end


module Eval(I : INTERPRETATION) =
struct
  let extend xs vs (nenv, tenv) =
    List.fold_left2
      (fun (nenv, tenv) x (v, t) ->
         (NEnv.bind nenv (x, v), TEnv.bind tenv (v, t)))
      (nenv, tenv)
      xs
      vs

  let (++) (nenv, tenv) (nenv', tenv') = (NEnv.extend nenv nenv', TEnv.extend tenv tenv')

  let rec eval : env -> Sugartypes.phrase -> tail_computation I.sem =
    fun env (e, pos) ->
      let lookup_var name =
        let x, xt = lookup_name_and_type name env in
          I.var (x, xt) in
      let instantiate name tyargs =
        let x, xt = lookup_name_and_type name env in
          match tyargs with
            | [] -> I.var (x, xt)
            | _ -> (* Debug.print ("name: "^name); *) I.tappl (I.var (x, xt), tyargs) in
      let mbt = lookup_type "_MAILBOX_" env in
      let instantiate_mb name = instantiate name [`Type mbt] in
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
              cofv (I.apply_pure(instantiate "Cons" [`Type t; `Type mbt], [ev e; ev ((`ListLit (es, Some t)), pos)]))
          | `Escape ((k, Some kt, _), body) ->
              I.escape ((kt, k, `Local), mbt, fun v -> eval (extend [k] [(v, kt)] env) body)
          | `Section (`Minus) -> cofv (lookup_var "-")
          | `Section (`FloatMinus) -> cofv (lookup_var "-.")
          | `Section (`Name name) -> cofv (lookup_var name)
          | `Conditional (p, e1, e2) ->
              I.condition (ev p, ec e1, ec e2)
          | `InfixAppl ((_tyargs, `Name ">"), e1, e2) -> cofv (I.comparison (ev e2, `Less, ev e1))
          | `InfixAppl ((_tyargs, `Name ">="), e1, e2) -> cofv (I.comparison (ev e2, `LessEq, ev e1))
          | `InfixAppl ((_tyargs, `Name "=="), e1, e2) -> cofv (I.comparison (ev e1, `Equal, ev e2))
          | `InfixAppl ((_tyargs, `Name "<"), e1, e2) -> cofv (I.comparison (ev e1, `Less, ev e2))
          | `InfixAppl ((_tyargs, `Name "<="), e1, e2) -> cofv (I.comparison (ev e1, `LessEq, ev e2))
          | `InfixAppl ((_tyargs, `Name "<>"), e1, e2) -> cofv (I.comparison (ev e1, `NotEq, ev e2))
          | `InfixAppl ((tyargs, `Name "++"), e1, e2) ->
              cofv (I.apply_pure (instantiate "Concat" tyargs, [ev e1; ev e2]))
          | `InfixAppl ((tyargs, `Name "!"), e1, e2) ->
              I.apply (instantiate "send" tyargs, [ev e1; ev e2])
          | `InfixAppl ((tyargs, `Name n), e1, e2) -> 
              I.apply (instantiate n tyargs, [ev e1; ev e2])
          | `InfixAppl ((tyargs, `Cons), e1, e2) ->
              cofv (I.apply_pure (instantiate "Cons" tyargs, [ev e1; ev e2]))
          | `InfixAppl ((tyargs, `FloatMinus), e1, e2) ->
              cofv (I.apply_pure (instantiate "-." tyargs, [ev e1; ev e2]))
          | `InfixAppl ((tyargs, `Minus), e1, e2) ->
              cofv (I.apply_pure (instantiate "-" tyargs, [ev e1; ev e2]))
          | `InfixAppl ((_tyargs, `And), e1, e2) ->
              I.condition (ev e1, ec e2, cofv (I.constant (`Bool false)))
          | `InfixAppl ((_tyargs, `Or), e1, e2) ->
              I.condition (ev e1, cofv (I.constant (`Bool true)), ec e2)
          | `InfixAppl ((_tyargs, `App), e1, e2) -> I.app (ev e1, ev e2)
          | `UnaryAppl ((_tyargs, `Minus), e) ->
              cofv (I.apply_pure(instantiate_mb "negate", [ev e]))
          | `UnaryAppl ((_tyargs, `FloatMinus), e) ->
              cofv (I.apply_pure(instantiate_mb "negatef", [ev e]))
          | `UnaryAppl ((tyargs, `Name n), e) ->
              cofv (I.apply_pure(instantiate n tyargs, [ev e]))
          | `UnaryAppl ((_tyargs, `Abs), e) -> cofv (I.abs (ev e))
          | `FnAppl (e, es) ->
(*              Debug.print ("fnappl: "^Sugartypes.Show_phrase.show e);*)
              I.apply (ev e, evs es)
          | `TAppl (e, tyargs) ->
(*              Debug.print ("tappl: "^Sugartypes.Show_phrasenode.show (`TAppl (e, tyargs)));*)
              cofv (I.tappl (ev e, tyargs))
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
(*              Debug.print ("projection: "^Sugartypes.Show_phrasenode.show (`Projection (e, name)));*)
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
          | `TableLit (name, (_, Some (readtype, writetype)), constraints, db) ->
              I.table_handle (ev db, ev name, (readtype, writetype))
          | `Xml (tag, attrs, attrexp, children) ->
              (* check for duplicates *)
              let () =
                let rec dup_check names =
                  function
                    | [] -> ()
                    | (name, _) :: attrs ->
                        if StringSet.mem name names then
                          raise (ASTSyntaxError (pos,
                                                 "XML attribute '"^name^"' is defined more than once"))
                        else
                          dup_check (StringSet.add name names) attrs
                in
                  dup_check StringSet.empty attrs
              in
                if tag = "#" then
                  if List.length attrs != 0 || attrexp <> None then
                    raise (ASTSyntaxError (pos,
                                           "XML forest literals cannot have attributes"))
                  else
                    cofv
                      (I.concat (instantiate "Nil" [`Type (`Primitive `XmlItem)],
                                 instantiate "Cons" [`Type (`Primitive `XmlItem); `Type mbt],
                                 List.map ev children))
                else
                  let attrs = alistmap (List.map ev) attrs in
                  let children = List.map ev children in
                  let body =
                    I.xml (instantiate "Nil" [`Type Types.string_type],
                           instantiate "Cons" [`Type Types.string_type; `Type mbt],
                           tag, attrs, children) in
                    begin
                      match attrexp with
                        | None -> cofv body
                        | Some e ->
                            cofv (I.apply_pure (instantiate_mb "addAttributes", [body; ev e]))
                    end
          | `TextNode name ->
              I.apply (instantiate_mb "stringToXml", [ev (`Constant (`String name), pos)])
          | `Block (bs, e) -> eval_bindings `Local env bs e
              (* These things should all have been desugared already *)
          | `Spawn _
          | `SpawnWait _
          | `Receive _
          | `Section (`Project _)
          | `FunLit _
          | `Iteration _
          | `InfixAppl ((_, `RegexMatch _), _, _)
          | `DBInsert _
          | `DBUpdate _
          | `DBDelete _
          | `Regex _
          | `Formlet _
          | `Page _
          | `FormletPlacement _
          | `PagePlacement _
          | `FormBinding _ ->
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
                | `Val (_, p, body, _, _) ->
                    let p, penv = CompilePatterns.desugar_pattern scope p in
                    let env' = env ++ penv in
                    let s = ev body in
                    let ss = eval_bindings scope env' bs e in
                      I.comp env (p, s, ss)
                | `Fun ((f, Some ft, _), (tyvars, ([ps], body)), location, pos) ->
                    let ps, body_env =
                      List.fold_right
                        (fun p (ps, body_env) ->
                           let p, penv = CompilePatterns.desugar_pattern scope p in
                             p::ps, body_env ++ penv)
                        ps
                        ([], env) in
                    let body = eval body_env body in
                      I.letfun
                        env
                        ((ft, f, scope), (tyvars, (ps, body)), location)
                        (fun v -> eval_bindings scope (extend [f] [(v, ft)] env) bs e)
                | `Exp e' ->
                    I.seq (ec e', eval_bindings scope env bs e)
                | `Funs defs ->
                    let fs, inner_fts, outer_fts =
                      List.fold_right
                        (fun ((f, Some outer, _), ((_tyvars, Some inner), _), _, _, _) (fs, inner_fts, outer_fts) ->
                              (f::fs, inner::inner_fts, outer::outer_fts))
                        defs 
                        ([], [], []) in
                    let defs =
                      List.map
                        (fun ((f, Some ft, _), ((tyvars, _), ([ps], body)), location, t, pos) ->
                           let ps, body_env =
                             List.fold_right
                               (fun p (ps, body_env) ->
                                  let p, penv = CompilePatterns.desugar_pattern scope p in
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
                | `Include _ -> assert false                   
            end

  and evalv env e =
    I.value_of_comp (eval env e)

  let rec get_global_names : binding list -> nenv =
    fun defs ->
      List.fold_left
        (fun nenv ->
           function
             | `Let ((x, (_xt, x_name, `Global)), _) ->
                 Env.String.bind nenv (x_name, x)
             | `Fun ((f, (_ft, f_name, `Global)), _, _) ->
                 Env.String.bind nenv (f_name, f)
             | `Rec defs ->
                 List.fold_left
                   (fun nenv ((f, (_ft, f_name, scope)), _, _) ->
                      match scope with
                        | `Global -> Env.String.bind nenv (f_name, f)
                        | `Local -> nenv)
                   nenv defs
             | `Alien ((f, (_ft, f_name, `Global)), _) ->
                 Env.String.bind nenv (f_name, f)
             | `Module (_, defs) ->
                 opt_app get_global_names nenv defs
             | _ -> nenv)
        NEnv.empty
        defs

  let compile env (bindings, body) =
    Debug.print ("compiling to IR");
(*    Debug.print (Sugartypes.Show_program.show (bindings, body));*)
    let body =
      match body with
        | None -> (`RecordLit ([], None), dp)
        | Some body -> body in
      let s = eval_bindings `Global env bindings body in
        let r = (I.reify s) in
          Debug.print ("compiled IR");
(*          Debug.print (Ir.Show_program.show r);*)
          r, I.sem_type s
end

module C = Eval(Interpretation(BindingListMonad))

let desugar_expression : env -> Sugartypes.phrase -> Ir.computation * nenv =
  fun env e ->
    let (bs, body), _ = C.compile env ([], Some e) in
      (bs, body), NEnv.empty

let desugar_program : env -> Sugartypes.program -> Ir.computation * nenv =
  fun env p ->
    let (bs, body), _ = C.compile env p in
      (bs, body), C.get_global_names bs

let desugar_definitions : env -> Sugartypes.binding list -> Ir.binding list * nenv =
  fun env bs ->
    let (bs, _), _ = C.compile env (bs, None) in
      bs, C.get_global_names bs
