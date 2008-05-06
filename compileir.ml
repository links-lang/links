open Utility
open Syntax
open Ir

let make_local_info = Var.make_local_info
let make_global_info = Var.make_global_info

type datatype = Types.datatype

module type MONAD =
sig
  type 'a sem

  val lift : 'a -> 'a sem
  val bind : 'a sem -> ('a -> 'b sem) -> 'b sem
end

module type BINDINGMONAD =
sig
  include MONAD

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

  val constant : (constant * datatype) -> value sem
  val var : (var * datatype) -> value sem
  val abs : value sem -> value sem
  val app : value sem * value sem * datatype -> tail_computation sem
  val apply : (value sem * (value sem) list * datatype) -> tail_computation sem
  val apply_pure : (value sem * (value sem) list * datatype) -> value sem
  val condition : (value sem * tail_computation sem * tail_computation sem) -> tail_computation sem
(* comparison? *)
  val comparison : (value sem * Syntaxutils.comparison * value sem) -> value sem

  val lam : datatype * var_info list * (var list -> tail_computation sem) * location -> value sem
  val comp : var_info * tail_computation sem * (var -> tail_computation sem) -> tail_computation sem
  val recursive : 
    (var_info * var_info list * (var list -> var list -> tail_computation sem) * location) list *
    (var list -> tail_computation sem) -> tail_computation sem

  val xmlnode : string * (value sem) StringMap.t * (value sem) list -> value sem
  val record : (value sem) StringMap.t * (value sem) option * datatype -> value sem

  val project : name * value sem * datatype -> value sem
(* erase? *)
  val erase :  name * value sem * datatype -> value sem

  val inject : name * value sem * datatype -> value sem
  val case :
    value sem * string * (var_info * (var -> tail_computation sem)) *
    (var_info * (var -> tail_computation sem)) option ->
    tail_computation sem
  val case_zero : value sem * datatype -> tail_computation sem
(*
  val nil : datatype -> value sem
  val listof : value sem * datatype -> value sem
  val concat : (value sem * value sem * datatype) -> value sem
  val comprehension : var_info * value sem * (var -> tail_computation sem) -> tail_computation sem
*)
  val database : value sem -> tail_computation sem
  val table_query : SqlQuery.sqlQuery * datatype -> tail_computation sem
  val table_handle : value sem * value sem * (datatype * datatype) * datatype -> tail_computation sem
(* sortby? *)
  val callcc : value sem * datatype -> tail_computation sem
  val wrong : datatype -> tail_computation sem

  val letfun :
    var_info * var_info list * (var list -> tail_computation sem) * location *
    (var -> tail_computation sem) ->
    tail_computation sem
  val alien : var_info * language * (var -> tail_computation sem) -> tail_computation sem

  val modul : string * tail_computation sem *
    (binding list -> unit -> tail_computation sem) -> tail_computation sem 
end

module BindingListMonad : BINDINGMONAD =
struct
  type 'a sem = binding list * 'a

  let lift a = ([], a)
  let bind (bs, a) f =
    let (bs', a') = f a in
      (bs @ bs', a')

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

  module S :
  sig
    val lift_list : ('a sem) list -> ('a list) M.sem
    val lift_stringmap : ('a sem) StringMap.t -> ('a StringMap.t) M.sem

    val comp_binding : var_info * tail_computation -> var M.sem
    val fun_binding :
      var_info * var_info list * (var list -> tail_computation sem) * location ->
      var M.sem
    val rec_binding :
      (var_info * var_info list * (var list -> var list -> tail_computation sem) * location) list ->
      (var list) M.sem
(*    val for_binding : var_info * value -> var M.sem*)

    val alien_binding : var_info * language -> var M.sem

    val module_binding : string * (binding list) option -> unit M.sem

    val value_of_untyped_var : var M.sem * datatype -> value sem
  end =
  struct
    let lift_list ss =
      List.fold_right
        (fun s s' ->
           bind s
             (fun v ->
                M.bind s'
                  (fun vs -> lift (v :: vs))))
        ss (lift [])

    (*
      WARNING:

      Using StringMaps instead of assoc lists affects the order of
      evaluation.

      e.g.
      (a=print("0"),b=print("1")) =/= (b=print("0"),a=print("1"))
      but
      (a=print("0"),b=print("1")) == (b=print("1"),a=print("0"))

      Of course this isn't an issue once we're in MNF as only values
      can occur inside the StringMaps, but it is a problem in our old
      IR. (One of the important features of A-NF/MNF/CPS is that it
      makes control flow explicit.)
    *)
    let lift_stringmap sm =
      StringMap.fold
        (fun name s s' ->
           bind s
             (fun v ->
                M.bind s'
                  (fun sm -> lift (StringMap.add name v sm))))
        sm (lift StringMap.empty)

    let comp_binding (x_info, e) =
      let xb, x = Var.fresh_var x_info in
        lift_binding (letm (xb, e)) x
          
    let fun_binding (f_info, xs_info, body, location) =
      let fb, f = Var.fresh_var f_info in
      let xsb, xs = List.split (List.map Var.fresh_var xs_info) in
        lift_binding (`Fun (([], fb), xsb, reify (body xs), location)) f
          
(*
    let for_binding (x_info, v) =
      let xb, x = Var.fresh_var x_info in
        lift_binding (`For (xb, v)) x
*)
        
    let rec_binding defs =
      let defs, fs =
        List.fold_right
          (fun (f_info, xs_info, body, location) (defs, fs) ->
             let fb, f = Var.fresh_var f_info in
             let xsb, xs = List.split (List.map Var.fresh_var xs_info) in
               ((([], fb), (xsb, xs), body, location) :: defs, f :: fs))
          defs ([], [])
      in
        lift_binding
          (`Rec
             (List.map 
                (fun (fb, (xsb, xs), body, location) ->
                   (fb, xsb, reify (body fs xs), location)) defs)) fs

    let alien_binding (x_info, language) =
      let xb, x = Var.fresh_var x_info in
        lift_binding (`Alien (([], xb), language)) x

    let module_binding (name, bindings) =
      lift_binding (`Module (name, bindings)) ()

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
  let constant (c, t) = lift (`Constant c, t)
  let var (x, t) = lift (`Variable x, t)
  let abs s = bind s (fun v -> lift (`Abs v, sem_type s))
  let app (s1, s2, t) =
    bind s1
      (fun v1 ->
         bind s2
           (fun v2 ->
              lift (`Special (`App (v1, v2)), t)))

  let apply (s, ss, t) =
    let ss = lift_list ss
    in
      bind s
        (fun v ->
           M.bind ss
             (fun vs -> lift (`Apply (v, vs), t)))

  let apply_pure (s, ss, t) =
    let ss = lift_list ss
    in
      bind s
        (fun v ->
           M.bind ss
             (fun vs -> lift (`ApplyPure (v, vs), t)))

  let condition (s, s1, s2) =
    bind s (fun v -> lift (`If (v, reify s1, reify s2), sem_type s1))

  let lam (t, xs_info, body, location) =
    value_of_untyped_var (fun_binding (Var.info_of_type t, xs_info, body, location), t)
  let comp (x_info, s, body) =
    bind s
      (fun e ->
         M.bind (comp_binding (x_info, e))
           (fun x -> body x))
  let recursive (defs, body) =
    M.bind (rec_binding defs) body
  let xmlnode (name, attributes, children) =
    let attributes = lift_stringmap attributes in
    let children = lift_list children in
      M.bind attributes
        (fun attributes ->
           M.bind children
             (fun children ->
                lift (`XmlNode (name, attributes, children), `Primitive `XmlItem)))
  let record (field_map, r, t) =
    let s' = lift_stringmap field_map
    in
      match r with
        | None ->
            M.bind s'
              (fun field_map -> lift (`Extend (field_map, None), t))
        | Some s ->
            bind s
              (fun r ->
                 M.bind s'
                   (fun field_map -> lift (`Extend (field_map, Some r), t)))

  let project (name, s, t) =
    bind s (fun v -> lift (`Project (name, v), t))

  let erase (name, s, t) =
    bind s (fun v -> lift (`Erase (name, v), t))


  let inject (name, s, t) =
    bind s (fun v -> lift (`Inject (name, v), t))
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

(*
  let nil t = lift (`Nil, t)
  let listof (s, t) =
    bind s (fun v -> lift (`Cons (v, `Nil), t))
  let concat (s, s', t) =
    bind s (fun v ->
              bind s' (fun v' -> lift (`Concat (v, v'), t)))
*)

(*
  let comprehension (x_info, s, body) =
    bind s
      (fun e ->
         M.bind (for_binding (x_info, e))
           (fun x -> body x))

*)

  let database s =
    bind s (fun v -> lift (`Special (`Database v), `Primitive (`DB)))
  let table_query (query, t) =
    lift (`Special (`Query query), t)
  let table_handle (database, table, ts, t) =
    bind database
      (fun database ->
         bind table
           (fun table -> lift (`Special (`Table (database, table, ts)), t)))

  let callcc (s, t) = bind s (fun v -> lift (`Special (`CallCC v), t))
  let wrong t = lift (`Special (`Wrong t), t)

  let letfun (f_info, xs_info, body, location, rest) =
     M.bind (fun_binding (f_info, xs_info, body, location)) rest

  let alien (x_info, language, rest) =
    M.bind (alien_binding (x_info, language)) rest

  let modul (name, body, rest) =
    let (bindings, _) = reify body in      
      M.bind (module_binding (name, Some bindings)) (rest bindings)
    
  let comp (x_info, s, body) =
    bind s
      (fun e ->
         M.bind (comp_binding (x_info, e))
           (fun x -> body x))
end


module VEnv = Env.String

type multi_env = var VEnv.t * datatype Env.Int.t

let builtins = ["+"; "+."; "-"; "-."; "*"; "*."; "/"; "/."]
let cps_prims = ["recv"; "sleep"]

module Eval(I : INTERPRETATION) =
struct
  let group_defs defs =
    let bothdefs l r = match l, r with
      | Define (_, Rec _, _, _), Define (_, Rec _, _, _) -> true
      | _ ->  false
    in
      groupBy bothdefs defs

  let extend env xs vs =
    List.fold_left2
      (fun env x v ->
         VEnv.bind env (x, v))
      env xs vs

  let rec add_globals_to_env : multi_env -> binding list -> multi_env = fun env ->
    List.fold_left
      (fun ((venv, tenv) as env) ->
         function
           | `Let ((_tyvars, (x, (xt, x_name, `Global))), _) ->
               extend venv [x_name] [x], Env.Int.bind tenv (x, xt)
           | `Fun ((_tyvars, (f, (ft, f_name, `Global))), _, _, _) ->
               extend venv [f_name] [f], Env.Int.bind tenv (f, ft)
           | `Rec defs ->
               List.fold_left
                 (fun((venv, tenv) as env) ((_tyvars, (f, (ft, f_name, scope))), _, _, _) ->
                    match scope with
                      | `Global -> extend venv [f_name] [f], Env.Int.bind tenv (f, ft)
                      | `Local -> env)
                 env defs
           | `Alien ((_tyvars, (f, (ft, f_name, `Global))), _) ->
               extend venv [f_name] [f], Env.Int.bind tenv (f, ft)
           | `Module (_, defs) ->
               opt_app (fun defs -> add_globals_to_env env defs) env defs
           | _ -> env)
      env


  let rec eval : var VEnv.t -> Syntax.expression -> tail_computation I.sem =
    fun env e ->

    let cofv = I.comp_of_value in
    let ec = eval env in
    let ev = evalv env in
    let evs = List.map ev in
    let t = node_datatype e in
      match e with
        | Constant (c, _) -> cofv (I.constant (c, t))
        | Variable (x, _) -> cofv (I.var (VEnv.lookup env x, t))
        | Abs (e, _) -> cofv (I.abs (ev e))
        | App (e1, e2, _) -> I.app (ev e1, ev e2, t)
        | Apply (Variable (f, _) as e, es, _)
            when (Library.is_pure_primitive f)
              -> cofv (I.apply_pure(I.var (VEnv.lookup env f, node_datatype e), evs es, t))
        | Apply (e', es, _) ->
(*             Debug.print ("apply: "^string_of_expression e); *)
            I.apply (ev e', evs es, t)
        | Condition (p, e1, e2, _) -> I.condition (ev p, ec e1, ec e2)
        | Comparison (e1, c, e2, _) ->
            cofv (I.comparison (ev e1, c, ev e2))
        | Abstr (xs, body, _) as e ->
(*             Debug.print ("xs: " ^ String.concat "," xs); *)
            let ft = node_datatype e in
            let xs_info = List.map2 (fun x xt -> make_local_info (xt, x)) xs (TypeUtils.arg_types ft) in
              cofv (I.lam (ft, xs_info, (fun vs -> eval (extend env xs vs) body), `Unknown))
        | Let (x, e1, e2, _) ->
            I.comp (make_local_info (node_datatype e1, x), ec e1, fun v -> eval (extend env [x] [v]) e2)
        | Rec (defs, e, _) ->
            let defs, fs =
              List.fold_right
                (fun (f, e, _) (defs, fs) ->
                   (f, e) :: defs, f :: fs)
                defs ([], []) in
              
            let defs =
              List.map
                (fun (f, e) ->
                   let ft = node_datatype e in
                   let f_info = make_local_info (ft, f) in
                   let xs_info, body = 
                     match e with
                       | Abstr (xs, body, _) ->
                           (List.map2 (fun x xt -> make_local_info (xt, x)) xs (TypeUtils.arg_types ft),
                            fun gs -> fun vs -> eval (extend env (fs @ xs) (gs @ vs)) body)
                       | _ -> assert false
                   in
                     (f_info, xs_info, body, `Unknown))
                defs
            in
              I.recursive (defs, fun gs -> eval (extend env fs gs) e)
        | Xml_node (name, attributes, children, _) ->
            let eval_attribute name e =
              if start_of name ~is:"l:" then
                let xt = `Application (Types.event, []) in
                  (* [HACK]
                     
                     x is global in order to ensure that it is assigned the name "event"
                     rather than "event_<number>" during JS generation
                  *)
                let x_info = make_global_info (xt, "event") in
                let ft = `Function (`Application (Types.event, []), Types.fresh_type_variable(), node_datatype e) in
                  I.lam (ft, [x_info], (fun [v] -> eval (extend env ["name"] [v]) e), `Unknown)
              else
                ev e in

            let eval_attribute name e = ev e in

            let attributes =
              List.fold_right
                (fun (name, e) attributes ->
                   StringMap.add name (eval_attribute name e) attributes)
                attributes StringMap.empty in
            let children =
              List.map ev children
            in
              cofv (I.xmlnode (name, attributes, children))
        | Record_intro (field_map, r, _) ->
            cofv
              (I.record (StringMap.fold
                           (fun name e field_map ->
                              StringMap.add name (ev e) field_map)
                           field_map StringMap.empty,
                         opt_map ev r,
                         node_datatype e))
        | Project (e, name, _) ->
            cofv (I.project (name, ev e, t))
        | Erase (e, name, _) ->
            cofv (I.erase (name, ev e, t))
(*            failwith "eval (Erase _) not implemented yet"*)
              (* coerce (forall rho\f . (f:A | rho) -> (rho)) *)
        | Variant_injection (name, e, _) ->
            cofv (I.inject (name, ev e, t))
        | Variant_selection (e, cname, cvar, cbody, dvar, dbody, _) ->
            let t = node_datatype e in
            let ctt, dt = TypeUtils.split_variant_type cname t in

            let ct =
              match ctt with
                | `Variant row ->
                    fst (TypeUtils.split_row cname row)
                | _ -> assert false in
(*
              Debug.print ("cname: "^cname^", cvar: "^cvar^
                             ", t: "^Types.string_of_datatype t^
                             ", ctt: "^Types.string_of_datatype ctt^
                             ", ct: "^Types.string_of_datatype ct);
*)
            let default =
              match dbody with
                | Variant_selection_empty _ -> None
                | _ -> Some (make_local_info (dt, dvar), fun v -> eval (extend env [dvar] [v]) dbody)
            in
              I.case (ev e,
                      cname,
                      (make_local_info (ct, cvar),
                       fun v -> eval (extend env [cvar] [v]) cbody),
                      default)
        | Variant_selection_empty (e', _) ->
            I.case_zero (ev e', t)
        | Nil _ ->
            cofv (I.var (VEnv.lookup env "Nil", t))
        | List_of (elem, _) ->
            let nil = I.var (VEnv.lookup env "Nil", t)
            and cons = VEnv.lookup env "Cons"
            and elem_type = TypeUtils.element_type t in
            let cons_type = `Function (Types.make_tuple_type [elem_type; t],
                                       Types.fresh_type_variable (),
                                       t) in
              cofv (I.apply_pure(I.var (cons, cons_type), [ev elem; nil], t))
        | Concat (left, right, _) ->
            let concat_type = `Function (Types.make_tuple_type [t; t],
                                         Types.fresh_type_variable (),
                                         t) in
              cofv (I.apply_pure(I.var (VEnv.lookup env "Concat", concat_type), [ev left; ev right], t))
        | For (body, x, e, _) ->
            (*
              compile comprehensions into map
            *)
            let xt = TypeUtils.element_type (node_datatype e) in
            let ft = `Function (xt, Types.fresh_type_variable(), t) in

            let map_type = `Function (Types.make_tuple_type [ft; Types.make_list_type xt],
                                      Types.fresh_type_variable (),
                                      Types.make_list_type t) in

            let x_info = make_local_info (xt, x) in
            let map = I.var (VEnv.lookup env "ConcatMap", map_type) in

            let f = I.lam (ft, [x_info], (fun [v] -> eval (extend env [x] [v]) body), `Unknown) in
              I.apply (map, [f; ev e], t)

(*
            I.comprehension (make_local_info (node_datatype e, x),
                             ev e,
                             fun v -> eval (extend env [x] [v]) body)
*)
        | Database (e, _) ->
            I.database (ev e)
        | TableHandle (database, table, ts, _) ->
            I.table_handle (ev database, ev table, ts, node_datatype e)
        | TableQuery (query, _) ->
            I.table_query (query, node_datatype e)
        | SortBy (expr, byExpr, _) ->
            failwith "eval (SortyBy _) not implemented yet"
        | Call_cc (e, _) ->
            I.callcc (ev e, t)
        | Wrong _ ->
            I.wrong (node_datatype e)
        | HasType (e, _, _) ->
            ec e
  and evalv env e =
    I.value_of_comp (eval env e)

  let rec eval_defs : var VEnv.t -> Syntax.definition list list -> Syntax.expression -> tail_computation I.sem =
    fun env defss rest ->
    match defss with
      | [] -> eval env rest
      | defs :: defss ->
          match List.hd defs with
            | Define (_, Rec _, _, _) ->
                let defs, fs, fs' =
                  List.fold_right
                    (fun (Define (f, Rec([f', e, _], _, _), location, _)) (defs, fs, fs') ->

                       (f, e, location) :: defs, f :: fs, f' :: fs')
                    defs ([], [], []) in
                  
                let defs =
                  List.map
                    (fun (f, e, location) ->
(*                         Debug.print("compiling rec fun: " ^ f); *)
                       let ft = node_datatype e in
                       let f_info = make_global_info (ft, f) in
                       let xs_info, body =
                         match e with
                           | Abstr (xs, body, _) ->
                               (List.map2 (fun x xt -> make_local_info (xt, x)) xs (TypeUtils.arg_types ft),
                                fun gs -> fun vs -> eval (extend env (fs @ fs' @ xs) (gs @ gs @ vs)) body)
                           | _ -> assert false
                       in
                         (f_info, xs_info, body, location))
                    defs
                in
                  I.recursive (defs, fun gs -> eval_defs (extend env fs gs) defss rest)
            | Define (f, (Abstr (xs, body, _) as e), location, _) -> 
(*                  Debug.print("compiling non-rec fun: " ^ f); *)
                let ft = node_datatype e in
                let xs_info = List.map2 (fun x xt -> make_local_info (xt, x)) xs (TypeUtils.arg_types ft) in
                  I.letfun (make_global_info (ft, f),
                            xs_info,
                            (fun vs -> eval (extend env xs vs) body),
                            location,
                            fun v -> eval_defs (extend env [f] [v]) defss rest)
            | Define (x, e, _, _) ->
(*                 Debug.print("compiling def: " ^ x); *)
                I.comp (make_global_info (node_datatype e, x),
                        eval env e,
                        fun v -> eval_defs (extend env [x] [v]) defss rest)
            | Alien (language, name, datatype, _) ->
                let x_info = make_global_info (datatype, name) in
                  I.alien (x_info, language, fun v -> eval_defs (extend env [name] [v]) defss rest)
            | Module (name, defs, _) ->
(*                Debug.print (if is_some defs then ("module with defs") else ("module without defs"));*)
                let () =
                  match name with
                    | None -> Debug.print ("anonymous module")
                    | Some name -> Debug.print ("opening module: " ^ name) in

                let defs = from_option [] defs in

                let v =
                  eval_program env
                    (Program (defs, Syntax.unit_expression Syntax.no_expr_data)) in

                let name =                  
                  match name with
                    | None -> ""
                    | Some name -> name
                in
                  I.modul (name,
                           v,
                           (fun bindings () ->
                              let env, _ = add_globals_to_env (env, Env.Int.empty) bindings in
                                eval_defs env defss rest))
                  
                  

(*                 let defss = *)
(*                   opt_app (fun defs -> group_defs defs @ defss) defss defs *)
(*                 in *)
(*                   eval_defs alias_env env defss rest *)

(*                 let (bindings, _) = eval_defs alias_env env defs (Syntax.unit_expression Syntax.no_expr_data) in *)
                  

(*                 I.modul (name, bindings, fun alias_env env -> eval_defs defss alias_env env rest) *)

  and eval_program env (Program (defs, body)) : tail_computation I.sem =
      eval_defs env (group_defs defs) body

  let compile (env, tenv) p =
    Debug.print ("compiling to IR");
    let s = eval_program env p in
      Debug.print ("compiled IR");
      let r = (Inline.program tenv (I.reify s)) in
        r, I.sem_type s      

  let invert_env env =
    VEnv.fold
      (fun name var env ->
         if IntMap.mem var env then
           failwith ("(invert_env) duplicate variable in environment")
         else
           IntMap.add var name env)
      env IntMap.empty

  let make_initial_env : Types.typing_environment -> multi_env = fun {Types.var_env = tenv} ->
    let venv, tenv = 
      Env.String.fold
        (fun name t (venv, tenv) ->
           let var = Var.fresh_raw_var () in
             extend venv [name] [var], Env.Int.bind tenv (var, t))
        tenv (VEnv.empty, Env.Int.empty)
    in
      venv, tenv
end

module C = Eval(Interpretation(BindingListMonad))

let compile_program : multi_env -> Syntax.program -> computation * datatype = C.compile

let make_initial_env : Types.typing_environment -> multi_env = C.make_initial_env
let invert_env : var VEnv.t -> string IntMap.t = C.invert_env
let add_globals_to_env : multi_env -> binding list -> multi_env = C.add_globals_to_env

