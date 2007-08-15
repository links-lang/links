open Utility
open Syntax
open Ir

(* type stuff *)
let info_type (t, _) = t
let info_of_type t = (t, "")
let make_info info = info

type datatype = Types.datatype

(* to be moved into Types*)
let arg_types  =
  function
    | `Function (ts, _, _) -> ts
    | _ -> failwith "Attempt to take arg types of non-function"
        
let return_type =
  function
    | `Function (_, _, t) -> t
    | _ -> failwith "Attempt to take return type of non-function"

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
  val lam : datatype * var_info list * (var list -> tail_computation sem) -> value sem
  val comp : var_info * tail_computation sem * (var -> tail_computation sem) -> tail_computation sem
  val recursive : 
    (var_info * var_info list * (var list -> var list -> tail_computation sem)) list *
    (var list -> tail_computation sem) -> tail_computation sem
  val xmlnode : string * (value sem) StringMap.t * (value sem) list -> value sem
  val record : (value sem) StringMap.t * (value sem) option * datatype -> value sem
(* record_selection? *)
  val project : name * value sem -> value sem
(* erase? *)
  val inject : name * value sem -> value sem
  val case :
    value sem * string * (var_info * (var -> tail_computation sem)) *
    (var_info * (var -> tail_computation sem)) option ->
    tail_computation sem
  val bottom : datatype * value sem -> tail_computation sem
  val nil : datatype -> value sem
(* listof? *)
(* concat? *)
  val comprehension : var_info * value sem * (var -> tail_computation sem) -> tail_computation sem
  val database : value sem -> tail_computation sem
  val table_query : (value sem) StringMap.t * SqlQuery.sqlQuery * datatype -> tail_computation sem
  val table_handle : value sem * value sem * (datatype * datatype) * datatype -> tail_computation sem
(* sortby? *)
  val callcc : value sem * datatype -> tail_computation sem
  val wrong : datatype -> tail_computation sem
end

module BindingListMonad : BINDINGMONAD =
struct
  type 'a sem = binding list * 'a

  let lift a = ([], a)
  let bind (bs, a) f =
    let (bs', a') = f a in
      (bs @ bs, a')

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
    val abs_binding : var_info * var_info list * (var list -> tail_computation sem) -> var M.sem
    val rec_binding : (var_info * var_info list * (var list -> var list -> tail_computation sem)) list -> (var list) M.sem
    val for_binding : var_info * value -> var M.sem

    val value_of_untyped_var : var M.sem * datatype -> value sem
  end =
  struct
    let lift_list ss =
      List.fold_left
        (fun s' s ->
           bind s
             (fun v ->
                M.bind s'
                  (fun vs -> lift (v :: vs))))
        (lift []) ss

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
          
    let abs_binding (f_info, xs_info, body) =
      let fb, f = fresh_var f_info in
      let xsb, xs = List.split (List.map fresh_var xs_info) in
        lift_binding (`Abs (fb, xsb, reify (body xs))) f
          
    let for_binding (x_info, v) =
      let xb, x = fresh_var x_info in
        lift_binding (`For (xb, v)) x
          
    let rec_binding defs =
      let defs, fs =
        List.fold_right
          (fun (f_info, xs_info, body) (defs, fs) ->
             let fb, f = fresh_var f_info in
             let xsb, xs = List.split (List.map fresh_var xs_info) in
               ((fb, (xsb, xs), body) :: defs, f :: fs))
          defs ([], [])
      in
        lift_binding
          (`Rec
             (List.map 
                (fun (fb, (xsb, xs), body) ->
                   (fb, xsb, reify (body fs xs))) defs)) fs

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

  let lam (t, xs_info, body) =
    value_of_untyped_var (abs_binding (info_of_type t, xs_info, body), t)
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

  let project (name, s) =
    bind s (fun v -> lift (`Project (name, v), sem_type s))

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

  let nil t = lift (`Nil, t)
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

end

module Eval(I : INTERPRETATION) =
struct
  module Env :
  sig
    type env

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
        xs vs StringMap.empty
    let lookup env x = StringMap.find x env
  end

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
            failwith "eval (Comparison _) not implemented yet"
              (* cofv (comparison (ev e1, c, ev e2))*)
        | Abstr (xs, body, _) as e ->
            let ft = node_datatype e in
            let xs_info = List.map (fun x -> make_info (arg_types ft, x)) xs in
              cofv (I.lam (ft, xs_info, fun vs -> eval (Env.extend env xs vs) body))
        | Let (x, e1, e2, _) ->
            I.comp (make_info (node_datatype e, x), ec e1, fun v -> eval (Env.extend env [x] [v]) e2)
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
                   let f_info = make_info (ft, f) in
                   let xs_info, body = 
                     match e with
                       | Abstr (xs, body, _) ->
                           (List.map (fun x -> make_info (arg_types ft, x)) xs,
                            fun gs -> fun vs -> eval (Env.extend env (fs @ xs) (gs @ vs)) body)
                       | _ -> assert false
                   in
                     (f_info, xs_info, body))
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
        | Record_selection (label, label_variable, variable, e, body, _) ->
            failwith "eval (Record_selection _) not implemented yet"
        | Project (e, name, _) ->
            cofv (I.project (name, ev e))
        | Erase (e, name, _) ->
            failwith "eval (Erase _) not implemented yet"
              (* coerce (forall rho\f . (f:A | rho) -> (rho)) *)
        | Variant_injection (name, e, _) ->
            cofv (I.inject (name, ev e))
        | Variant_selection (e, cname, cvar, cbody, dvar, dbody, _) ->
            let t = node_datatype e in
            let ct, dt = split_variant_type cname t in

            let default =
              match dbody with
                | Variant_selection_empty _ -> None
                | _ -> Some (make_info (dt, dvar), fun v -> eval (Env.extend env [dvar] [v]) dbody)
            in
              I.case (ev e,
                      cname,
                      (make_info (ct, cvar),
                       fun v -> eval (Env.extend env [cvar] [v]) cbody),
                      default)
        | Variant_selection_empty (e', _) ->
            I.bottom (node_datatype e, ev e')
        | Nil _ ->
            cofv (I.nil (node_datatype e))
        | List_of (elem, _) ->
            failwith "eval (List_of _) not implemented yet"
        | Concat (left, right, _) ->
            failwith "eval (Concat _) not implemented yet"
        | For (e, x, body, _) ->
            I.comprehension (make_info (node_datatype e, x),
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

  let compile e =
    let s = eval Env.empty e
    in
      I.reify s, I.sem_type s
end

module C = Eval(Interpretation(BindingListMonad))

let compile_expression : expression -> computation * datatype = C.compile
