open Utility
module Q = Query

type query =
  [ `UnionAll of query list * int
  | `Select of (base * string) list * (string * Var.var) list * base * base list
  | `With of Var.var * query * Var.var * query ]
and base =
  [ `Case of (base * base * base)
  | `Constant of Constant.constant
  | `Project of Var.var * string
  | `Apply of string * base list
  | `Empty of query
  | `Length of query
  | `RowNumber of (Var.var * string) list]
    [@@deriving show]

(* Table variables that are actually used are always bound in a for
   comprehension. In this case the IR variable from the for
   comprehension is used to generate the table variable.

   e.g. if the IR variable is 1485 then the table variable is t1485
*)
let fresh_table_var : unit -> Var.var = Var.fresh_raw_var
let string_of_table_var var = "t" ^ string_of_int var
let string_of_subquery_var var = "q" ^ string_of_int var

(* Because of limitations of SQL we sometimes need to generate dummy
   table variables. These have the prefix "dummy" and have their own
   name source. *)
let dummy_counter = ref 0
let reset_dummy_counter () = dummy_counter := 0
let fresh_dummy_var () =
  incr dummy_counter;
  "dummy" ^ string_of_int (!dummy_counter)

let string_of_label label =
  if Str.string_match (Str.regexp "[0-9]+") label 0 then
    "\"" ^ label ^ "\""     (* The SQL-standard way to quote an identifier;
                               works in MySQL and PostgreSQL *)
  else
    label

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
  let string_of_select fields tables condition os =
    let tables = String.concat "," tables in
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
          (* using quote_field assumes tables contains table names (not nested queries) *)
          let tables = List.map (fun (t, x) -> db#quote_field t ^ " as " ^ (string_of_table_var x)) tables
          in string_of_select fields tables condition os
      | `With (_, q, z, q') ->
          match q' with
          | `Select (fields, tables, condition, os) ->
              (* Inline the query *)
              let tables = List.map (fun (t, x) -> db#quote_field t ^ " as " ^ (string_of_table_var x)) tables in
              let q = "(" ^ sq q ^ ") as " ^ string_of_table_var z in
              string_of_select fields (q::tables) condition os
          | _ -> assert false

and string_of_base db one_table b =
  let sb = string_of_base db one_table in
    match b with
      | `Case (c, t, e) ->
          "case when " ^ sb c ^ " then " ^sb t ^ " else "^ sb e ^ " end"
      | `Constant c -> Constant.string_of_constant c
      | `Project (_var, _label as p) -> string_of_projection db one_table p
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
      | `RowNumber [] -> "1"
      | `RowNumber ps ->
        "row_number() over (order by " ^ String.concat "," (List.map (string_of_projection db one_table) ps) ^ ")"
and string_of_projection db one_table (var, label) =
  if one_table then
    db#quote_field label
  else
    string_of_table_var var ^ "." ^ (db#quote_field label)

let string_of_query db range q =
  let range =
    match range with
      | None -> ""
      | Some (limit, offset) -> " limit " ^string_of_int limit^" offset "^string_of_int offset
  in
    string_of_query db false q ^ range

let prepare_clauses : Q.t -> Q.t list =
  function
    | `Concat vs -> vs
    | v -> [v]

type index = (Var.var * string) list


let gens_index gs  =
  let all_fields t =
    let field_types = Q.table_field_types t in
    Q.labels_of_field_types field_types
  in
 (* Use keys if available *)
  let key_fields t =
    match t with
      (_, _, (ks::_), _) -> StringSet.from_list ks
    |	_ -> all_fields t
  in
  let table_index get_fields (x, source) =
    let t = match source with `Table t -> t | _ -> assert false in
    let labels = get_fields t in
      List.rev
        (StringSet.fold
           (fun name ps -> (x, name) :: ps)
           labels
           [])
  in
  if Settings.get_value Basicsettings.use_keys_in_shredding
  then concat_map (table_index key_fields) gs
  else concat_map (table_index all_fields) gs

let outer_index gs_out = gens_index gs_out
let inner_index z gs_in =
  (* it's just a dynamic index! *)
  (z, "2") :: gens_index gs_in

let extract_gens =
  function
    | `For (_, gs, _, _) -> gs
    | _ -> assert false

type let_clause = Var.var * Q.t * Var.var * Q.t
type let_query = let_clause list

let rec let_clause : Value.database -> let_clause -> query =
  fun db (q, outer, z, inner) ->
    let gs_out = extract_gens outer in
    let gs_in = extract_gens inner in
      `With (q,
             clause db (outer_index gs_out) false outer,
             z,
             clause db (inner_index z gs_in) false inner)
and clause : Value.database -> index -> bool -> Q.t -> query = fun db index unit_query v ->
  (*  Debug.print ("clause: "^string_of_t v); *)
  match v with
    | `Concat _ -> assert false
    | `For (_, [], _, body) ->
        clause db index unit_query body
    | `For (_, (x, `Table (_db, table, _keys, _row))::gs, os, body) ->
        let body = clause db index unit_query (`For (None, gs, [], body)) in
        let os = List.map (base db index) os in
          begin
            match body with
              | `Select (fields, tables, condition, []) ->
                  `Select (fields, (table, x)::tables, condition, os)
              | _ -> assert false
          end
    | `If (c, body, `Concat []) ->
      (* Turn conditionals into where clauses. We might want to do
         this earlier on.  *)
      let c = base db index c in
      let body = clause db index unit_query body in
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
    | `Singleton _ when unit_query ->
      (* If we're inside an `Empty or a `Length it's safe to ignore
         any fields here. *)
      (* We currently detect this earlier, so the unit_query stuff here
         is redundant. *)
      `Select ([], [], `Constant (`Bool true), [])
    | `Singleton (`Record fields) ->
      let fields =
        List.rev
          (StringMap.fold
             (fun name v fields ->
               (base db index v, name)::fields)
             fields
             [])
      in
        `Select (fields, [], `Constant (`Bool true), [])
    | _ -> assert false
and base : Value.database -> index -> Q.t -> base = fun db index ->
  function
    | `If (c, t, e) ->
      `Case (base db index c, base db index t, base db index e)
    | `Apply ("tilde", [s; r]) ->
      begin
        match likeify r with
          | Some r ->
            `Apply ("LIKE", [base db index s; `Constant (`String r)])
          | None ->
            let r =
                  (* HACK:

                     this only works if the regexp doesn't include any variables bound by the query
                  *)
                  `Constant (`String (Regex.string_of_regex (Linksregex.Regex.ofLinks (Q.value_of_expression r))))
                in
                  `Apply ("RLIKE", [base db index s; r])
        end
    | `Apply ("Empty", [v]) ->
        `Empty (unit_query db v)
    | `Apply ("length", [v]) ->
        `Length (unit_query db v)
    | `Apply (f, vs) ->
        `Apply (f, List.map (base db index) vs)
    | `Project (`Var (x, _field_types), name) ->
        `Project (x, name)
    | `Constant c -> `Constant c
    | `Primitive "index" -> `RowNumber index
    | e ->
      Debug.print ("Not a base expression: " ^ Q.show e);
      assert false

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
and unit_query db v =
  (* queries passed to Empty and Length
     (where we don't care about what data they return)
  *)
  `UnionAll (List.map (clause db [] true) (prepare_clauses v), 0)

and query : Value.database -> let_query -> query =
  fun db cs ->
    `UnionAll (List.map (let_clause db) cs, 0)

let update db ((_, table), where, body) =
  reset_dummy_counter ();
  let base = (base db []) ->- (string_of_base db true) in
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
  let base = base db [] ->- (string_of_base db true) in
  let where =
    match where with
      | None -> ""
      | Some where ->
          " where (" ^ base where ^ ")"
  in
    "delete from "^table^where

