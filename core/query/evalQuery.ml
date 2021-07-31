open Utility
open CommonTypes
module Q = Query
module QL = QueryLang

(* Introducing ordering indexes in order to support a list
   semantics. *)
module Order =
struct
  type gen = Var.var * QL.t
  type context = gen list

(* TODO:

      - add a setting for selecting unordered queries
      - more refined generation of ordered queries
        - make use of unique keys to
          cut down on the number of order indexes
        - share order indexes when possible
        - remove duplicate fields from the output
  *)

  (* The following abstraction should allow us to customise the
     concrete choice of order indexes.

     In particular, we might use primary keys for generators.

     Including tail generators gives:

     table t

     a deterministic semantics, in the sense that asList (table t)
     will return the same list of rows throughout a query.

     In general this is unlikely to be necessary, as the programmer
     can supply an orderby clause when needed.

     If we ignore tail generators, then this corresponds to
     interpretting asList (table t) non-deterministically, that is,
     each invocation of asList (table t) may return a different
     permutation of the rows. (In practice, it's not clear whether
     real databases actually take advantage of the freedom to
     reorganise rows within a single query, but I believe it is
     allowed.)

     Given the non-deterministic interpration of asList, the
     normalisation procedure becomes technically unsound as it may
     duplicate instances of asList. If we wanted to restore soundness
     then we could do so by keeping track of which tables get
     duplicated and ensuring that we order by all occurences of them -
     e.g. by converting tail generators to non-tail generators. *)

  type order_index =
    | Val        of QL.t
    | Gen        of gen
    | TailGen    of gen
    | DefVal     of Primitive.t
    | DefGen     of gen
    | DefTailGen of gen
    | Branch     of int

  (* TODO:

     We should probably represent 'defaultness' using a boolean flag
     rather than wiring it into a single polymorphic variant type. *)

  (* We might implement an optimisation to remove duplicate
     expressions from the output - in particular, the expressions we
     order by will often already be present in the output. Perhaps,
     though, it would make sense to apply such an optimisation more
     generally on any query. For instance:

     for (x <-- t) [(x.a, x.a)]

     might be translated as:

     select x.a from t as a

     followed by a post-processing phase that creates two copies of
     x.a.

     Another optimisation would be to remove duplicate expressions
     from the output order by clause. More ambitiously, this could be
     further generalised to handle examples such as the following:

     for (x <-- t) orderby (x.a, -x.a) [x]

     which is equivalent to:

     for (x <-- t) orderby (x.a) [x] *)

  type orders = order_index list

  type query_tree =
    | Node of orders * (int * query_tree) list
    | Leaf of (context * QL.t) * orders

  type path = int list

  type preclause = (path * (context * QL.t)) * query_tree
  type clause = context * QL.t * orders

  let gen : (Var.var * QL.t) -> QL.t list =
    function
      | (x, QL.Table t) ->
        let field_types = QL.table_field_types t in
        let tyx = Types.make_record_type field_types in
        List.rev
            (StringMap.fold
               (fun name _t es ->
                 QL.Project (QL.Var (x, tyx), name) :: es
               ) field_types [])
      | _ -> assert false

  let base_type_of_expression t =
    match QL.type_of_expression t with
      | Types.Primitive p -> p
      | _ -> assert false

  let default_of_base_value = QL.default_of_base_type -<- base_type_of_expression

  (* convert orders to a list of expressions

     - represent generators by projecting all fields
     - ignore tail generators
  *)
  let long_orders : orders -> QL.t list =
    let long =
      function
        | Val t        -> [t]
        | Gen g        -> gen g
        | TailGen _    -> []
        | DefVal t     -> [QL.default_of_base_type t]
        | DefGen g     -> List.map default_of_base_value (gen g)
        | DefTailGen _ -> []
        | Branch i     -> [QL.Constant (Constant.Int i)]
    in
      concat_map long

  let lift_vals = List.map (fun o -> Val o)
  let lift_gens = List.map (fun g -> Gen g)
  let lift_tail_gens = List.map (fun g -> TailGen g)

  let rec query : context -> QL.t -> QL.t -> query_tree =
    fun gs cond ->
      let open QL in
      function
        | Concat vs ->
          let cs = queries gs cond vs in
            Node ([], cs)
        | If (cond', v, Concat []) ->
          query gs (Q.reduce_and (cond, cond')) v
        | For (_, gs', os, Concat vs) ->
          let os' = lift_vals os @ lift_gens gs' in
          let cs = queries (gs @ gs') cond vs in
            Node (os', cs)
        | For (_, gs', os, body) ->
          Leaf ((gs @ gs',
                  Q.reduce_where_then (cond, body)),
                 lift_vals os @ lift_gens gs @ lift_tail_gens gs')
        | Singleton r ->
          Leaf ((gs, Q.reduce_where_then (cond, Singleton r)), [])
        | _ -> assert false
  and queries : context -> QL.t -> QL.t list -> (int * query_tree) list =
    fun gs cond vs ->
      let _, cs =
        List.fold_left
          (fun (i, cs) v ->
            let c = query gs cond v in
              (i+1, (i, c)::cs))
          (1, [])
          vs
      in
        List.rev cs

  (* convert all order indexes to default values *)
  let rec mask : query_tree -> query_tree =
    let dv =
      List.map
        (function
          | Val t -> DefVal (base_type_of_expression t)
          | Gen g -> DefGen g
          | TailGen g -> DefTailGen g
          | _ -> assert false)
    in
      function
        | Node (os, cs) -> Node (dv os, mask_children cs)
        | Leaf (x, os)  -> Leaf (x, dv os)
  and mask_children : (int * query_tree) list -> (int * query_tree) list =
    fun cs ->
      List.map (fun (branch, tree) -> (branch, mask tree)) cs

  (* decompose a query tree into a list of preclauses
     (path, query, tree) *)
  let rec decompose : query_tree -> preclause list =
    function
      | Leaf (q, os) -> [(([], q), Leaf (q, os))]
      | Node (os, cs) ->
        List.map
          (fun ((path, q), cs) ->
            ((path, q), Node (os, cs)))
          (decompose_children [] cs)
  and decompose_children prefix : (int * query_tree) list
      -> ((int list * (context * QL.t)) * (int * query_tree) list) list =
    function
      | [] -> []
      | (branch, tree) :: cs ->
        let xs = decompose tree in
        let m = mask tree in
        let ms = mask_children cs in
          List.map
            (fun ((path, q), tree) ->
              ((branch :: path, q), prefix @ (branch, tree) :: ms))
            xs
          @ decompose_children (prefix @ [(branch, m)]) cs

  (* compute the order indexes for the specified query tree along a
     path *)
  let rec flatten_at path active : query_tree -> orders =
    function
        | Leaf (_, os) -> os
        | Node (os, cs) ->
          if active then
            let branch = List.hd path in
            let path   = List.tl path in
              os @ Branch branch :: flatten_at_children branch path active cs
          else
            os @ Branch 0 :: flatten_at_children 0 [] active cs
  and flatten_at_children branch path active =
    function
      | [] -> []
      | ((branch', tree) :: cs) ->
        if active then
          if branch == branch' then
            (flatten_at path true tree) @ (flatten_at_children branch path false cs)
          else
            (flatten_at path false tree) @ (flatten_at_children branch path true cs)
        else
          (flatten_at path false tree) @ (flatten_at_children branch path false cs)

  (* flatten a query tree as a list of subqueries *)
  let flatten_tree q =
    List.map
      (fun ((path, (gs, body)), tree) ->
        (gs, body, flatten_at path true tree))
      (decompose q)

  let query : QL.t -> clause list =
    fun v ->
      let q = query [] (QL.Constant (Constant.Bool true)) v in
      let ss = flatten_tree q in
        ss

  (* FIXME:

     Be more careful about ensuring that the order index field names
     do not clash with existing field names *)
  let query_of_clause pick_orders (gs, body, os) =
    let orders = pick_orders os in
    let rec add_indexes fields i =
      function
        | []      -> fields
        | o :: os ->
          add_indexes
            (StringMap.add ("order_" ^ string_of_int i) o fields)
            (i+1)
            os in
    let rec order =
      function
        | QL.Singleton (QL.Record fields) ->
          QL.Singleton (QL.Record (add_indexes fields 1 orders))
        | QL.If (c, body, QL.Concat []) ->
          QL.If (c, order body, QL.Concat [])
        | _ -> assert false in
    let body' = order body in
      match gs with
        | [] -> body'
        | _  -> QL.For (None, gs, [], body')

  let index_length : (orders -> QL.t list) -> clause list -> int =
    fun pick_orders ->
      function
        | (_, _, os) :: _ -> List.length (pick_orders os)
        | [] -> 0

  let ordered_query v =
    let ss = query v in
    let n = index_length long_orders ss in
    let vs = List.map (query_of_clause long_orders) ss in
      vs, n
end

(* WR: the following is logically part of Sql, but... *)
let ordered_query v =
  (* Debug.print ("v: "^Q.string_of_t v); *)
  Sql.reset_dummy_counter ();
  let vs, n = Order.ordered_query v in
  (* Debug.print ("concat vs: "^Q.string_of_t (`Concat vs)); *)
  Sql.Union (Sql.All, List.map QL.sql_of_query vs, n)

let compile : Value.env -> (int * int) option * Ir.computation -> (Value.database * Sql.query * Types.datatype) option =
  fun env (range, e) ->
    (* Debug.print ("e: "^Ir.show_computation e); *)
    let v = Q.Eval.eval QueryPolicy.Flat env e in
      (* Debug.print ("v: "^Q.string_of_t v); *)
      match QL.used_database v with
        | None -> None
        | Some db ->
            let t = Types.unwrap_list_type (QL.type_of_expression v) in
            let q = ordered_query v in
              Debug.print ("Generated query: "^(db#string_of_query ~range q));
              Some (db, q, t)
