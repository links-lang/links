open Utility
open Syntax
open Ir

(* type stuff *)
let info_type (t, _, _) = t
let info_of_type t = (t, "", `Local)

let make_local_info (t, name) = (t, name, `Local)
let make_global_info (t, name) = (t, name, `Global)

type datatype = Types.datatype
type assumption = Types.assumption

(* to be moved into Types*)
let arg_types =
  Types.concrete_type ->-
    (function
       | `Function (ts, _, _) -> ts
       | t -> failwith ("Attempt to take arg types of non-function" ^ Types.string_of_datatype t))
        
let return_type =
  Types.concrete_type ->-
    (function
       | `Function (_, _, t) -> t
       | t -> failwith ("Attempt to take return type of non-function" ^ Types.string_of_datatype t))

(*
 [BUG]

  need to be a bit more principled here
  e.g. need to make sure name is actually present
  and take into account that the row may not have been flattened
*)
let split_variant_type name =
  function
    | `Variant (field_map, row_var) ->
        if StringMap.mem name field_map then
          (`Variant (StringMap.add name (StringMap.find name field_map) StringMap.empty, row_var),
           `Variant (StringMap.remove name field_map, row_var))
        else
          failwith "Attempt to split variant on absent constructor"
    | _ -> failwith "Attempt to split non-variant type"

(* Generation of fresh variables *)
let variable_counter = ref 0
let fresh_var : var_info -> binder * var =
  fun info ->
    incr variable_counter;
    let var = !variable_counter in
      (var, info), var

let fresh_raw_var : unit -> var =
  fun info ->
    incr variable_counter;
    !variable_counter

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
  val app : value sem * value sem -> tail_computation sem
  val apply : (value sem * (value sem) list) -> tail_computation sem
  val condition : (value sem * tail_computation sem * tail_computation sem) -> tail_computation sem
(* comparison? *)
  val comparison : (value sem * Syntax.comparison * value sem) -> value sem

  val lam : datatype * var_info list * (var list -> tail_computation sem) * location -> value sem
  val comp : var_info * tail_computation sem * (var -> tail_computation sem) -> tail_computation sem
  val recursive : 
    (var_info * var_info list * (var list -> var list -> tail_computation sem) * location) list *
    (var list -> tail_computation sem) -> tail_computation sem

  val xmlnode : string * (value sem) StringMap.t * (value sem) list -> value sem
  val record : (value sem) StringMap.t * (value sem) option * datatype -> value sem
(* record_selection? *)
  val project : name * value sem -> value sem
(* erase? *)
  val erase : name * value sem -> value sem

  val inject : name * value sem -> value sem
  val case :
    value sem * string * (var_info * (var -> tail_computation sem)) *
    (var_info * (var -> tail_computation sem)) option ->
    tail_computation sem
  val bottom : datatype * value sem -> tail_computation sem
  val nil : datatype -> value sem
  val listof : value sem -> value sem
  val concat : (value sem * value sem) -> value sem
  val comprehension : var_info * value sem * (var -> tail_computation sem) -> tail_computation sem
  val database : value sem -> tail_computation sem
  val table_query : (value sem) StringMap.t * Query.query * datatype -> tail_computation sem
  val table_handle : value sem * value sem * (datatype * datatype) * datatype -> tail_computation sem
(* sortby? *)
  val callcc : value sem * datatype -> tail_computation sem
  val wrong : datatype -> tail_computation sem

  val letfun :
    var_info * var_info list * (var list -> tail_computation sem) * location *
    (var -> tail_computation sem) ->
    tail_computation sem
  val alias : tyname * tyvar list * datatype * (unit -> tail_computation sem) -> tail_computation sem
  val alien : var_info * language * assumption * (var -> tail_computation sem) -> tail_computation sem
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

  let dummy_computation = `Special `Wrong
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
    val for_binding : var_info * value -> var M.sem

    val alias_binding : tyname * tyvar list * datatype -> unit M.sem
    val alien_binding : var_info * language * assumption -> var M.sem

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
      [WARNING]

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
      let xb, x = fresh_var x_info in
        lift_binding (`Let (xb, e)) x
          
    let fun_binding (f_info, xs_info, body, location) =
      let fb, f = fresh_var f_info in
      let xsb, xs = List.split (List.map fresh_var xs_info) in
        lift_binding (`Fun (fb, xsb, reify (body xs), location)) f
          
    let for_binding (x_info, v) =
      let xb, x = fresh_var x_info in
        lift_binding (`For (xb, v)) x
          
    let rec_binding defs =
      let defs, fs =
        List.fold_right
          (fun (f_info, xs_info, body, location) (defs, fs) ->
             let fb, f = fresh_var f_info in
             let xsb, xs = List.split (List.map fresh_var xs_info) in
               ((fb, (xsb, xs), body, location) :: defs, f :: fs))
          defs ([], [])
      in
        lift_binding
          (`Rec
             (List.map 
                (fun (fb, (xsb, xs), body, location) ->
                   (fb, xsb, reify (body fs xs), location)) defs)) fs

    let alias_binding (typename, quantifiers, datatype) =
      lift_binding (`Alias (typename, quantifiers, datatype)) ()

    let alien_binding (x_info, language, assumption) =
      let xb, x = fresh_var x_info in
        lift_binding (`Alien (xb, language, assumption)) x

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
               value_of_untyped_var (comp_binding (info_of_type t, e), t))
      
  let comp_of_value s =
    bind s (fun v -> lift (`Return v, sem_type s))

  (* eval parameters *)
  let constant (c, t) = lift (`Constant c, t)
  let var (x, t) = lift (`Variable x, t)
  let abs s = bind s (fun v -> lift (`Abs v, sem_type s))
  let app (s1, s2) =
    bind s1
      (fun v1 ->
         bind s2
           (fun v2 ->
              let t = return_type (sem_type s1) in
                lift (`Special (`App (v1, v2)), t)))
  let apply (s, ss) =
    let t = return_type (sem_type s) in
    let ss = lift_list ss
    in
      bind s
        (fun v ->
           M.bind ss
             (fun vs -> lift (`Apply (v, vs), t)))
  let condition (s, s1, s2) =
    bind s (fun v -> lift (`If (v, reify s1, reify s2), sem_type s1))

  let lam (t, xs_info, body, location) =
    value_of_untyped_var (fun_binding (info_of_type t, xs_info, body, location), t)
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

  (* [BUG] the type should be the projection of sem_type s at name

     The same bug is repeated in several places.
  *)
  let project (name, s) =
    bind s (fun v -> lift (`Project (name, v), sem_type s))

  let erase (name, s) =
    bind s (fun v -> lift (`Erase (name, v), sem_type s))


  let inject (name, s) =
    bind s (fun v -> lift (`Inject (name, v), sem_type s))
  let case (s, name, (cinfo, cbody), default) =
    bind s (fun v ->
              let cb, c = fresh_var cinfo in
              let cbody' = cbody c in
              let t = sem_type cbody' in
              let default =
                opt_map (fun (dinfo, dbody) ->
                           let db, d = fresh_var dinfo in
                             (db, reify (dbody d))) default
              in
                lift
                  (`Case (v,
                          StringMap.add name (cb, reify cbody') StringMap.empty,
                          default), t))

  let bottom (t, s) =
    bind s (fun v ->
              lift (`Case (v, StringMap.empty, None), t))

  let comparison (s, c, s') =
    bind s (fun v ->
              bind s' (fun v' -> lift (`Comparison (v, c, v'), sem_type s)))

  let nil t = lift (`Nil, t)
  let listof s =
    bind s (fun v -> lift (`Cons (v, `Nil), sem_type s))
  let concat (s, s') =
    bind s (fun v ->
              bind s' (fun v' -> lift (`Concat (v, v'), sem_type s)))

  let comprehension (x_info, s, body) =
    bind s
      (fun e ->
         M.bind (for_binding (x_info, e))
           (fun x -> body x))
  let database s =
    bind s (fun v -> lift (`Special (`Database v), `Primitive (`DB)))
  let table_query (tables, query, t) =
    let tables = lift_stringmap tables
    in
      M.bind tables
        (fun tables -> lift (`Special (`TableQuery (tables, query)), t))
  let table_handle (database, table, ts, t) =
    bind database
      (fun database ->
         bind table
           (fun table -> lift (`Special (`TableHandle (database, table, ts)), t)))

  let callcc (s, t) = bind s (fun v -> lift (`Special (`CallCC v), t))
  let wrong t = lift (`Special `Wrong, t)

  let lam (t, xs_info, body, location) =
    value_of_untyped_var (fun_binding (info_of_type t, xs_info, body, location), t)


  let letfun (f_info, xs_info, body, location, rest) =
     M.bind (fun_binding (f_info, xs_info, body, location)) rest
(*    M.bind (lam (t, xs_info, body, location)) rest *)

  let alien (x_info, language, assumption, rest) =
    M.bind (alien_binding (x_info, language, assumption)) rest

  let alias (typename, quantifiers, datatype, rest) =
    M.bind (alias_binding (typename, quantifiers, datatype)) rest

  let comp (x_info, s, body) =
    bind s
      (fun e ->
         M.bind (comp_binding (x_info, e))
           (fun x -> body x))
end


module Env :
sig
  type env = var StringMap.t

  val empty : env
  val extend : env -> string list -> var list -> env
  val lookup : env -> string -> var
end =
struct
  type env = var StringMap.t

  let empty = StringMap.empty
  let extend env xs vs =
    List.fold_right2 (fun x v env ->
                        StringMap.add x v env)
      xs vs env
  let lookup env x =
    if (StringMap.mem x env) then
      StringMap.find x env
    else
      failwith ("(compileir): variable " ^ x ^ " not in environment")
end

module Eval(I : INTERPRETATION) =
struct
  let rec eval : Env.env -> Syntax.expression -> tail_computation I.sem = fun env e ->
    let cofv = I.comp_of_value in
    let ec = eval env in
    let ev = evalv env in
    let evs = List.map ev in
    let t = node_datatype e in
      match e with
        | Constant (c, _) -> cofv (I.constant (c, t))
        | Variable (x, _) -> cofv (I.var (Env.lookup env x, t))
        | Abs (e, _) -> cofv (I.abs (ev e))
        | App (e1, e2, _) -> I.app (ev e1, ev e2)
        | Apply (e, es, _) -> I.apply (ev e, evs es)
        | Condition (p, e1, e2, _) -> I.condition (ev p, ec e1, ec e2)
        | Comparison (e1, c, e2, _) ->
            cofv (I.comparison (ev e1, c, ev e2))
        | Abstr (xs, body, _) as e ->
            let ft = node_datatype e in
            let xs_info = List.map (fun x -> make_local_info (arg_types ft, x)) xs in
              cofv (I.lam (ft, xs_info, (fun vs -> eval (Env.extend env xs vs) body), `Unknown))
        | Let (x, e1, e2, _) ->
            I.comp (make_local_info (node_datatype e, x), ec e1, fun v -> eval (Env.extend env [x] [v]) e2)
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
                           (List.map (fun x -> make_local_info (arg_types ft, x)) xs,
                            fun gs -> fun vs -> eval (Env.extend env (fs @ xs) (gs @ vs)) body)
                       | _ -> assert false
                   in
                     (f_info, xs_info, body, `Unknown))
                defs
            in
              I.recursive (defs, fun gs -> eval (Env.extend env fs gs) e)
        | Xml_node (name, attributes, children, _) ->
            let attributes =
              List.fold_right
                (fun (name, e) attributes ->
                   StringMap.add name (ev e) attributes)
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
        | Record_selection (label, x, y, e, body, _) ->
(*
            let t, r = split_record_type label (node_datatype e) in
            cofv
              (I.recordselection (
                 make_local_info (t, x),
                 make_local_info (r, y),
                 ec e,
                 fun (v, p) -> eval (Env.extend env [x; y] [v; p]) body))
*)
            failwith "eval (Record_selection _) not implemented yet"
        | Project (e, name, _) ->
            cofv (I.project (name, ev e))
        | Erase (e, name, _) ->
            cofv (I.erase (name, ev e))
(*            failwith "eval (Erase _) not implemented yet"*)
              (* coerce (forall rho\f . (f:A | rho) -> (rho)) *)
        | Variant_injection (name, e, _) ->
            cofv (I.inject (name, ev e))
        | Variant_selection (e, cname, cvar, cbody, dvar, dbody, _) ->
            let t = node_datatype e in
            let ct, dt = split_variant_type cname t in

            let default =
              match dbody with
                | Variant_selection_empty _ -> None
                | _ -> Some (make_local_info (dt, dvar), fun v -> eval (Env.extend env [dvar] [v]) dbody)
            in
              I.case (ev e,
                      cname,
                      (make_local_info (ct, cvar),
                       fun v -> eval (Env.extend env [cvar] [v]) cbody),
                      default)
        | Variant_selection_empty (e', _) ->
            I.bottom (node_datatype e, ev e')
        | Nil _ ->
            cofv (I.nil (node_datatype e))
        | List_of (elem, _) ->
            cofv (I.listof (ev elem))
        | Concat (left, right, _) ->
            cofv (I.concat (ev left, ev right))
        | For (e, x, body, _) ->
            I.comprehension (make_local_info (node_datatype e, x),
                             ev e,
                             fun v -> eval (Env.extend env [x] [v]) body)
        | Database (e, _) ->
            I.database (ev e)
        | TableHandle (database, table, ts, _) ->
            I.table_handle (ev database, ev table, ts, node_datatype e)
        | TableQuery (tables, query, _) ->
            I.table_query
              (List.fold_right
                 (fun (name, e) tables ->
                    StringMap.add name (ev e) tables)
                 tables StringMap.empty,
               query,
               node_datatype e)
        | SortBy (expr, byExpr, _) ->
            failwith "eval (SortyBy _) not implemented yet"
        | Call_cc (e, _) ->
            I.callcc (ev e, return_type (node_datatype e))
        | Wrong _ ->
            I.wrong (node_datatype e)
        | HasType (e, _, _) ->
            ec e
  and evalv env e =
    I.value_of_comp (eval env e)

  let rec eval_defs env defss rest =
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
                       let ft = node_datatype e in
                       let f_info = make_global_info (ft, f) in
                       let xs_info, body =
                         match e with
                           | Abstr (xs, body, _) ->
                               (List.map (fun x -> make_local_info (arg_types ft, x)) xs,
                                fun gs -> fun vs -> eval (Env.extend env (fs @ fs' @ xs) (gs @ gs @ vs)) body)
                           | _ -> assert false
                       in
                         (f_info, xs_info, body, location))
                    defs
                in
                  I.recursive (defs, fun gs -> eval_defs (Env.extend env fs gs) defss rest)
            | Define (f, (Abstr (xs, body, _) as e), location, _) -> 
                let ft = node_datatype e in
                let xs_info = List.map (fun x -> make_local_info (arg_types ft, x)) xs in
                  I.letfun (make_global_info (ft, f),
                            xs_info,
                            (fun vs -> eval (Env.extend env xs vs) body),
                            location,
                            fun v -> eval_defs (Env.extend env [f] [v]) defss rest)
            | Define (x, e, _, _) ->
                I.comp (make_global_info (node_datatype e, x),
                        eval env e,
                        fun v -> eval_defs (Env.extend env [x] [v]) defss rest)
            | Alias (typename, quantifiers, datatype, _) ->
                I.alias (typename, quantifiers, datatype,
                         fun () -> eval_defs env defss rest)
            | Alien (language, name, assumption, _) ->
                let x_info = make_global_info (snd assumption, name) in
                  I.alien (x_info, language, assumption,
                           fun v -> eval_defs (Env.extend env [name] [v]) defss rest)

  let eval_program env (Program (defs, body)) =
    let bothdefs l r = match l, r with
      | Define (_, Rec _, _, _), Define (_, Rec _, _, _) -> true
      | _ ->  false in
    let defss = groupBy bothdefs defs in
    let _ = Debug.print ("defs: "^String.concat "\n" (List.map string_of_definition defs)) in
      eval_defs env defss body

  let compile env p =
    let s = eval_program env p in
      I.reify s, I.sem_type s

  let add_globals_to_env : Env.env -> binding list -> Env.env = fun env ->
    List.fold_left
      (fun env ->
         function
           | `Let ((x, (_, x_name, `Global)), _) ->
               Env.extend env [x_name] [x]
           | `Fun ((f, (_, f_name, `Global)), _, _, _) ->
               Env.extend env [f_name] [f]
           | `Rec defs ->
               List.fold_left
                 (fun env ((f, (_, f_name, scope)), _, _, _) ->
                    match scope with
                      | `Global -> Env.extend env [f_name] [f]
                      | `Local -> env)
                 env defs
           | `Alien ((f, (_, f_name, `Global)), _, _) ->
               Env.extend env [f_name] [f]
           | _ -> env)
      env

  let make_initial_env : string list -> Env.env =
    List.fold_left
      (fun env f_name ->
         Env.extend env [f_name] [fresh_raw_var ()])
      Env.empty
end

module C = Eval(Interpretation(BindingListMonad))

let compile_program : Env.env -> Syntax.program -> computation * datatype = C.compile

let make_initial_env : string list -> Env.env = C.make_initial_env
let add_globals_to_env : Env.env -> binding list -> Env.env = C.add_globals_to_env

