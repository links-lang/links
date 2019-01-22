open Utility
module Q = Query


(* Introducing ordering indexes in order to support a list
   semantics. *)
module Order =
struct
  type gen = Var.var * Q.t
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

  type order_index = [ `Val of Q.t | `Gen of gen | `TailGen of gen
                     | `DefVal of Types.primitive | `DefGen of gen | `DefTailGen of gen
                     | `Branch of int ]

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

  type query_tree = [ `Node of orders * (int * query_tree) list
                    | `Leaf of (context * Q.t) * orders ]

  type path = int list

  type preclause = (path * (context * Q.t)) * query_tree
  type clause = context * Q.t * orders

  let gen : (Var.var * Q.t) -> Q.t list =
    function
      | (x, `Table t) ->
        let field_types = Q.table_field_types t in
          List.rev
            (StringMap.fold
               (fun name _t es ->
                 `Project (`Var (x, field_types), name) :: es
               ) field_types [])
      | _ -> assert false

  let base_type_of_expression t =
    match Q.type_of_expression t with
      | `Primitive p -> p
      | _ -> assert false

  let default_of_base_value = Q.default_of_base_type -<- base_type_of_expression

  (* convert orders to a list of expressions

     - represent generators by projecting all fields
     - ignore tail generators
  *)
  let long_orders : orders -> Q.t list =
    let long =
      function
        | `Val t        -> [t]
        | `Gen g        -> gen g
        | `TailGen _    -> []
        | `DefVal t     -> [Q.default_of_base_type t]
        | `DefGen g     -> List.map default_of_base_value (gen g)
        | `DefTailGen _ -> []
        | `Branch i     -> [`Constant (`Int i)]
    in
      concat_map long

  let lift_vals = List.map (fun o -> `Val o)
  let lift_gens = List.map (fun g -> `Gen g)
  let lift_tail_gens = List.map (fun g -> `TailGen g)

  let rec query : context -> Q.t -> Q.t -> query_tree =
    fun gs cond ->
      function
        | `Concat vs ->
          let cs = queries gs cond vs in
            `Node ([], cs)
        | `If (cond', v, `Concat []) ->
          query gs (Q.Eval.reduce_and (cond, cond')) v
        | `For (_, gs', os, `Concat vs) ->
          let os' = lift_vals os @ lift_gens gs' in
          let cs = queries (gs @ gs') cond vs in
            `Node (os', cs)
        | `For (_, gs', os, body) ->
          `Leaf ((gs @ gs',
                  Q.Eval.reduce_where_then (cond, body)),
                 lift_vals os @ lift_gens gs @ lift_tail_gens gs')
        | `Singleton r ->
          `Leaf ((gs, Q.Eval.reduce_where_then (cond, `Singleton r)), [])
        | _ -> assert false
  and queries : context -> Q.t -> Q.t list -> (int * query_tree) list =
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
          | `Val t -> `DefVal (base_type_of_expression t)
          | `Gen g -> `DefGen g
          | `TailGen g -> `DefTailGen g
          | _ -> assert false)
    in
      function
        | `Node (os, cs) -> `Node (dv os, mask_children cs)
        | `Leaf (x, os)  -> `Leaf (x, dv os)
  and mask_children : (int * query_tree) list -> (int * query_tree) list =
    fun cs ->
      List.map (fun (branch, tree) -> (branch, mask tree)) cs

  (* decompose a query tree into a list of preclauses
     (path, query, tree) *)
  let rec decompose : query_tree -> preclause list =
    function
      | `Leaf (q, os) -> [(([], q), `Leaf (q, os))]
      | `Node (os, cs) ->
        List.map
          (fun ((path, q), cs) ->
            ((path, q), `Node (os, cs)))
          (decompose_children [] cs)
  and decompose_children prefix : (int * query_tree) list
      -> ((int list * (context * Q.t)) * (int * query_tree) list) list =
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
        | `Leaf (_, os) -> os
        | `Node (os, cs) ->
          if active then
            let branch = List.hd path in
            let path   = List.tl path in
              os @ `Branch branch :: flatten_at_children branch path active cs
          else
            os @ `Branch 0 :: flatten_at_children 0 [] active cs
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

  let query : Q.t -> clause list =
    fun v ->
      let q = query [] (`Constant (`Bool true)) v in
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
        | `Singleton (`Record fields) ->
          `Singleton (`Record (add_indexes fields 1 orders))
        | `If (c, body, `Concat []) ->
          `If (c, order body, `Concat [])
        | _ -> assert false in
    let body' = order body in
      match gs with
        | [] -> body'
        | _  -> `For (None, gs, [], body')

  let index_length : (orders -> Q.t list) -> clause list -> int =
    fun pick_orders ->
      function
        | (_, _, os) :: _ -> List.length (pick_orders os)
        | [] -> assert false

  let ordered_query v =
    let ss = query v in
    let n = index_length long_orders ss in
    let vs = List.map (query_of_clause long_orders) ss in
      vs, n
end

    (* TODO: Unify this with Queryshredding.ShreddedSql *)

module Sql =
struct
  type query =
    [ `UnionAll of query list * int
    | `Select of (base * string) list * (string * Var.var) list * base * base list ]
  and base =
    [ `Case of (base * base * base)
    | `Constant of Constant.constant
    | `Project of Var.var * string
    | `Apply of string * base list
    | `Empty of query
    | `Length of query ]

  (* Table variables that are actually used are always bound in a for
     comprehension. In this case the IR variable from the for
     comprehension is used to generate the table variable.

     e.g. if the IR variable is 1485 then the table variable is t1485
  *)
  let fresh_table_var : unit -> Var.var = Var.fresh_raw_var
  let string_of_table_var var = "t" ^ string_of_int var

  (* Because of limitations of SQL we sometimes need to generate dummy
     table variables. These have the prefix "dummy" and have their own
     name source. *)
  let dummy_counter = ref 0
  let reset_dummy_counter () = dummy_counter := 0
  let fresh_dummy_var () =
    incr dummy_counter;
    "dummy" ^ string_of_int (!dummy_counter)

  module Arithmetic :
  sig
    val is : string -> bool
    val gen : (string * string * string) -> string
  end =
  struct
    let builtin_ops =
      StringMap.from_alist
        [ "+",   Some "+"  ;
          "+.",  Some "+"  ;
          "-",   Some "-"  ;
          "-.",  Some "-"  ;
          "*",   Some "*"  ;
          "*.",  Some "*"  ;
          "/",   None      ;
          "^",   None      ;
          "^.",  None      ;
          "/.",  Some "/"  ;
          "mod", Some "%"  ;
	  (* FIXME: The SQL99 || operator is supported in PostgreSQL and
	     SQLite but not in MySQL, where it denotes the logical or
	     operator *)
	  "^^",  Some "||" ]

    let is x = StringMap.mem x builtin_ops
    let sql_name op = val_of (StringMap.find op builtin_ops)
    let gen (l, op, r) =
      match op with
        | "/" -> "floor("^l^"/"^r^")"
        | "^" -> "floor(pow("^l^","^r^"))"
        | "^." -> "pow("^l^","^r^")"
        | _ -> "("^l^sql_name op^r^")"
  end

  module SqlFuns :
  sig
    val is : string -> bool
    val name : string -> string
  end =
  struct
    let funs =
      StringMap.from_alist
        [ "toUpper",  "upper";
          "toLower",  "lower";
          "ord",      "ord";
          "chr",      "char";
          "random",   "rand" ]

    let is f = StringMap.mem f funs
    let name f = StringMap.find f funs
  end

  let order_by_clause n =
    if n == 0 then
      ""
    else
      let rec order i n =
        if i > n then
          []
        else
          ("order_" ^ string_of_int i) :: order (i+1) n
      in
        " order by " ^ String.concat "," (order 1 n)

  (* For `Empty and `Length we don't care about the actual data
     returned. This allows these operators to take lists that have any
     element type at all. *)

  let rec string_of_query db ignore_fields q =
    let sq = string_of_query db ignore_fields in
    let sb = string_of_base db false in
    let string_of_fields fields =
      if ignore_fields then
        "0 as dummy" (* SQL doesn't support empty records! *)
      else
        match fields with
          | [] -> "0 as dummy" (* SQL doesn't support empty records! *)
          | fields ->
            mapstrcat ","
              (fun (b, l) ->
                "(" ^ sb b ^ ") as "^ db#quote_field l) (* string_of_label l) *)
              fields
    in
      match q with
        | `UnionAll ([], _) -> assert false
        | `UnionAll ([q], n) -> sq q ^ order_by_clause n
        | `UnionAll (qs, n) ->
          mapstrcat " union all " (fun q -> "(" ^ sq q ^ ")") qs ^ order_by_clause n
        | `Select (fields, [], `Constant (`Bool true), _os) ->
            let fields = string_of_fields fields in
              "select " ^ fields
        | `Select (fields, [], condition, _os) ->
            let fields = string_of_fields fields in
              "select * from (select " ^ fields ^ ") as " ^ fresh_dummy_var () ^ " where " ^ sb condition
        | `Select (fields, tables, condition, os) ->
            let tables = mapstrcat "," (fun (t, x) -> db#quote_field t ^ " as " ^ (string_of_table_var x)) tables in
            let fields = string_of_fields fields in
            let orderby =
              match os with
                | [] -> ""
                | _ -> " order by " ^ mapstrcat "," sb os in
            let where =
              match condition with
                | `Constant (`Bool true) -> ""
                | _ ->  " where " ^ sb condition
            in
              "select " ^ fields ^ " from " ^ tables ^ where ^ orderby
  and string_of_base db one_table b =
    let sb = string_of_base db one_table in
      match b with
        | `Case (c, t, e) ->
            "case when " ^ sb c ^ " then " ^sb t ^ " else "^ sb e ^ " end"
        | `Constant c -> Constant.string_of_constant c
        | `Project (var, label) ->
            if one_table then
              db#quote_field label
            else
              string_of_table_var var ^ "." ^ (db#quote_field label)
        | `Apply (op, [l; r]) when Arithmetic.is op
            -> Arithmetic.gen (sb l, op, sb r)
        | `Apply (("intToString" | "stringToInt" | "intToFloat" | "floatToString"
                  | "stringToFloat"), [v]) -> sb v
        | `Apply ("floatToInt", [v]) -> "floor("^sb v^")"

        (* optimisation *)
        | `Apply ("not", [`Empty q]) -> "exists (" ^ string_of_query db true q ^ ")"

        | `Apply ("not", [v]) -> "not (" ^ sb v ^ ")"
        | `Apply (("negate" | "negatef"), [v]) -> "-(" ^ sb v ^ ")"
        | `Apply ("&&", [v; w]) -> "(" ^ sb v ^ ")" ^ " and " ^ "(" ^ sb w ^ ")"
        | `Apply ("||", [v; w]) -> "(" ^ sb v ^ ")" ^ " or " ^ "(" ^ sb w ^ ")"
        | `Apply ("==", [v; w]) -> "(" ^ sb v ^ ")" ^ " = " ^ "(" ^ sb w ^ ")"
        | `Apply ("<>", [v; w]) -> "(" ^ sb v ^ ")" ^ " <> " ^ "(" ^ sb w ^ ")"
        | `Apply ("<", [v; w]) -> "(" ^ sb v ^ ")" ^ " < " ^ "(" ^ sb w ^ ")"
        | `Apply (">", [v; w]) -> "(" ^ sb v ^ ")" ^ " > " ^ "(" ^ sb w ^ ")"
        | `Apply ("<=", [v; w]) -> "(" ^ sb v ^ ")" ^ " <= " ^ "(" ^ sb w ^ ")"
        | `Apply (">=", [v; w]) -> "(" ^ sb v ^ ")" ^ " >= " ^ "(" ^ sb w ^ ")"
        | `Apply ("RLIKE", [v; w]) -> "(" ^ sb v ^ ")" ^ " RLIKE " ^ "(" ^ sb w ^ ")"
        | `Apply ("LIKE", [v; w]) -> "(" ^ sb v ^ ")" ^ " LIKE " ^ "(" ^ sb w ^ ")"
        | `Apply (f, args) when SqlFuns.is f -> SqlFuns.name f ^ "(" ^ String.concat "," (List.map sb args) ^ ")"
        | `Apply (f, args) -> f ^ "(" ^ String.concat "," (List.map sb args) ^ ")"
        | `Empty q -> "not exists (" ^ string_of_query db true q ^ ")"
        | `Length q -> "select count(*) from (" ^ string_of_query db true q ^ ") as " ^ fresh_dummy_var ()

  let string_of_query db range q =
    let range =
      match range with
        | None -> ""
        | Some (limit, offset) -> " limit " ^ string_of_int limit^" offset "^ string_of_int offset
    in
      string_of_query db false q ^ range

  let prepare_clauses : Q.t -> Q.t list =
    function
      | `Concat vs -> vs
      | v -> [v]

  let rec clause : Value.database -> Q.t -> query = fun db v ->
(*    Debug.print ("clause: "^string_of_t v); *)
    match v with
      | `Concat _ -> assert false
      | `For (_, [],  _, body) ->
          clause db body
      | `For (_, (x, `Table (_db, table, _, _row))::gs, os, body) ->
          let body = clause db (`For (None, gs, [], body)) in
          let os = List.map (base db) os in
            begin
              match body with
                | `Select (fields, tables, condition, []) ->
                    `Select (fields, (table, x)::tables, condition, os)
                | _ -> assert false
            end
      | `If (c, body, `Concat []) ->
        (* Turn conditionals into where clauses. We might want to do
           this earlier on.  *)
        let c = base db c in
        let body = clause db body in
          begin
            match body with
              | `Select (fields, tables, c', os) ->
                let c =
                  match c, c' with
                    (* optimisations *)
                    | `Constant (`Bool true), c
                    | c, `Constant (`Bool true) -> c
                    | `Constant (`Bool false), _
                    | _, `Constant (`Bool false) -> `Constant (`Bool false)
                    (* default case *)
                    | c, c' -> `Apply ("&&", [c; c'])
                in
                  `Select (fields, tables, c, os)
              | _ -> assert false
          end
      | `Table (_db, table, _keys, (fields, _, _)) ->
        (* eta expand tables. We might want to do this earlier on.  *)
        (* In fact this should never be necessary as it is impossible
           to produce non-eta expanded tables. *)
        let var = fresh_table_var () in
        let fields =
          List.rev
            (StringMap.fold
               (fun name _ fields ->
                 (`Project (var, name), name)::fields)
               fields
               [])
        in
          `Select (fields, [(table, var)], `Constant (`Bool true), [])
      | `Singleton (`Record fields) ->
        let fields =
          List.rev
            (StringMap.fold
               (fun name v fields ->
                 (base db v, name)::fields)
               fields
               [])
        in
          `Select (fields, [], `Constant (`Bool true), [])

      | `Singleton _ ->
        (* If we're inside an `Empty or a `Length it's safe to
           ignore any fields here. Otherwise this line should be
           unreachable. *)
        `Select ([], [], `Constant (`Bool true), [])
      | _ -> assert false
  and base : Value.database -> Q.t -> base = fun db ->
    function
      | `If (c, t, e) ->
        `Case (base db c, base db t, base db e)
      | `Apply ("tilde", [s; r]) ->
        begin
          match likeify r with
            | Some r ->
              `Apply ("LIKE", [base db s; `Constant (`String r)])
            | None ->
              let r =
                    (* HACK:

                       this only works if the regexp doesn't include any variables bound by the query
                    *)
                    `Constant (`String (Regex.string_of_regex (Linksregex.Regex.ofLinks (Q.value_of_expression r))))
                  in
                    `Apply ("RLIKE", [base db s; r])
          end
      | `Apply ("Empty", [v]) ->
          `Empty (outer_query db v)
      | `Apply ("length", [v]) ->
          `Length (outer_query db v)
      | `Apply (f, vs) ->
          `Apply (f, List.map (base db) vs)
      | `Project (`Var (x, _field_types), name) ->
          `Project (x, name)
      | `Constant c -> `Constant c
      | v -> failwith ("Bad base value: " ^ Q.string_of_t v)

  (* convert a regexp to a like if possible *)
  and likeify v =
    let quote = Str.global_replace (Str.regexp_string "%") "\\%" in
      match v with
        | `Variant ("Repeat", pair) ->
            begin
              match Q.unbox_pair pair with
                | `Variant ("Star", _), `Variant ("Any", _) -> Some ("%")
                | _ -> None
            end
        | `Variant ("Simply", `Constant (`String s)) -> Some (quote s)
        | `Variant ("Quote", `Variant ("Simply", v)) ->
            (* TODO:

               detect variables and convert to a concatenation operation
               (this needs to happen in RLIKE compilation as well)
            *)
           let rec string =
              function
                | `Constant (`String s) -> Some s
                | `Singleton (`Constant (`Char c)) -> Some (string_of_char c)
                | `Concat vs ->
                    let rec concat =
                      function
                        | [] -> Some ""
                        | v::vs ->
                            begin
                              match string v with
                                | None -> None
                                | Some s ->
                                    begin
                                      match concat vs with
                                        | None -> None
                                        | Some s' -> Some (s ^ s')
                                    end
                            end
                    in
                      concat vs
                | _ -> None
            in
              opt_map quote (string v)
        | `Variant ("Seq", rs) ->
            let rec seq =
              function
                | [] -> Some ""
                | r::rs ->
                    begin
                      match likeify r with
                        | None -> None
                        | Some s ->
                            begin
                              match seq rs with
                                | None -> None
                                | Some s' -> Some (s^s')
                            end
                    end
            in
              seq (Q.unbox_list rs)
        | `Variant ("StartAnchor", _) -> Some ""
        | `Variant ("EndAnchor", _) -> Some ""
        | _ -> assert false
  and outer_query db v =
    `UnionAll (List.map (clause db) (prepare_clauses v), 0)

  let ordered_query db range v =
    (* Debug.print ("v: "^Q.string_of_t v); *)
    reset_dummy_counter ();
    let vs, n = Order.ordered_query v in
    (* Debug.print ("concat vs: "^Q.string_of_t (`Concat vs)); *)
    let q = `UnionAll (List.map (clause db) vs, n) in
      string_of_query db range q

  let update db ((_, table), where, body) =
    reset_dummy_counter ();
    let base = (base db) ->- (string_of_base db true) in
    let where =
      match where with
        | None -> ""
        | Some where ->
            " where (" ^ base where ^ ")" in
    let fields =
      match body with
        | `Record fields ->
            String.concat ","
              (List.map
                 (fun (label, v) -> db#quote_field label ^ " = " ^ base v)
                 (StringMap.to_alist fields))
        | _ -> assert false
    in
      "update "^table^" set "^fields^where

  let delete db ((_, table), where) =
    reset_dummy_counter ();
    let base = base db ->- (string_of_base db true) in
    let where =
      match where with
        | None -> ""
        | Some where ->
            " where (" ^ base where ^ ")"
    in
      "delete from "^table^where
end

let compile : Value.env -> (int * int) option * Ir.computation -> (Value.database * string * Types.datatype) option =
  fun env (range, e) ->
    (* Debug.print ("e: "^Ir.show_computation e); *)
    let v = Q.Eval.eval env e in
      (* Debug.print ("v: "^Q.string_of_t v); *)
      match Q.used_database v with
        | None -> None
        | Some db ->
            let t = Q.type_of_expression v in
            let q = Sql.ordered_query db range v in
              Debug.print ("Generated query: "^q);
              Some (db, q, t)

let compile_update : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option * Ir.computation) -> string =
  fun db env ((x, table, field_types), where, body) ->
    let env = Q.Eval.bind (Q.Eval.env_of_value_env env) (x, `Var (x, field_types)) in
(*      let () = opt_iter (fun where ->  Debug.print ("where: "^Ir.show_computation where)) where in*)
    let where = opt_map (Q.Eval.computation env) where in
(*       Debug.print ("body: "^Ir.show_computation body); *)
    let body = Q.Eval.computation env body in
    let q = Sql.update db ((x, table), where, body) in
      Debug.print ("Generated update query: "^q);
      q

let compile_delete : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option) -> string =
  fun db env ((x, table, field_types), where) ->
    let env = Q.Eval.bind (Q.Eval.env_of_value_env env) (x, `Var (x, field_types)) in
    let where = opt_map (Q.Eval.computation env) where in
    let q = Sql.delete db ((x, table), where) in
      Debug.print ("Generated update query: "^q);
      q
