open Utility
open CommonTypes

type index = (Var.var * string) list

type query =
  | UnionAll  of query list * int
  | Select    of select_clause
  | With      of Var.var * query * Var.var * query
and select_clause = (base * string) list * (string * Var.var) list * base * base list
and base =
  | Case      of base * base * base
  | Constant  of Constant.t
  | Project   of Var.var * string
  | Apply     of string * base list
  | Empty     of query
  | Length    of query
  | RowNumber of (Var.var * string) list
    [@@deriving show]

(* optimizing smart constructor for && *)
let smart_and c c' =
  let open Constant in
  match c, c' with
  (* optimisations *)
  | Constant (Bool true), c
  | c, Constant (Bool true) -> c
  | Constant (Bool false), _
  | _, Constant (Bool false) ->
    Constant (Bool false)
  (* default case *)
  | c, c' -> Apply ("&&", [c; c'])

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

(* For Empty and Length we don't care about the actual data
   returned. This allows these operators to take lists that have any
   element type at all. *)

let rec string_of_query db ignore_fields q =
  let sq = string_of_query db ignore_fields in
  let sb = string_of_base db false in
  let string_of_fields fields =
    if ignore_fields then
      "0 as \"@unit@\"" (* SQL doesn't support empty records! *)
    else
      match fields with
        | [] -> "0 as \"@unit@\"" (* SQL doesn't support empty records! *)
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
        | Constant (Constant.Bool true) -> ""
        | _ ->  " where " ^ sb condition
    in
      "select " ^ fields ^ " from " ^ tables ^ where ^ orderby
  in
    match q with
      | UnionAll ([], _) -> "select 42 as \"@unit@\" where false"
      | UnionAll ([q], n) -> sq q ^ order_by_clause n
      | UnionAll (qs, n) ->
        mapstrcat " union all " (fun q -> "(" ^ sq q ^ ")") qs ^ order_by_clause n
      | Select (fields, [], Constant (Constant.Bool true), _os) ->
          let fields = string_of_fields fields in
            "select " ^ fields
      | Select (fields, [], condition, _os) ->
          let fields = string_of_fields fields in
            "select * from (select " ^ fields ^ ") as " ^ fresh_dummy_var () ^ " where " ^ sb condition
      | Select (fields, tables, condition, os) ->
          (* using quote_field assumes tables contains table names (not nested queries) *)
          let tables = List.map (fun (t, x) -> db#quote_field t ^ " as " ^ (string_of_table_var x)) tables
          in string_of_select fields tables condition os
      | With (_, q, z, q') ->
          match q' with
          | Select (fields, tables, condition, os) ->
              (* Inline the query *)
              let tables = List.map (fun (t, x) -> db#quote_field t ^ " as " ^ (string_of_table_var x)) tables in
              let q = "(" ^ sq q ^ ") as " ^ string_of_table_var z in
              string_of_select fields (q::tables) condition os
          | _ -> assert false

and string_of_base db one_table b =
  let sb = string_of_base db one_table in
    match b with
      | Case (c, t, e) ->
          "case when " ^ sb c ^ " then " ^sb t ^ " else "^ sb e ^ " end"
      | Constant c -> Constant.to_string c
      | Project (var, label) -> string_of_projection db one_table (var, label)
      | Apply (op, [l; r]) when Arithmetic.is op
          -> Arithmetic.gen (sb l, op, sb r)
      | Apply (("intToString" | "stringToInt" | "intToFloat" | "floatToString"
                | "stringToFloat"), [v]) -> sb v
      | Apply ("floatToInt", [v]) -> "floor("^sb v^")"

      (* optimisation *)
      | Apply ("not", [Empty q]) -> "exists (" ^ string_of_query db true q ^ ")"

      | Apply ("not", [v]) -> "not (" ^ sb v ^ ")"
      | Apply (("negate" | "negatef"), [v]) -> "-(" ^ sb v ^ ")"
      | Apply ("&&", [v; w]) -> "(" ^ sb v ^ ")" ^ " and " ^ "(" ^ sb w ^ ")"
      | Apply ("||", [v; w]) -> "(" ^ sb v ^ ")" ^ " or " ^ "(" ^ sb w ^ ")"
      | Apply ("==", [v; w]) -> "(" ^ sb v ^ ")" ^ " = " ^ "(" ^ sb w ^ ")"
      | Apply ("<>", [v; w]) -> "(" ^ sb v ^ ")" ^ " <> " ^ "(" ^ sb w ^ ")"
      | Apply ("<", [v; w]) -> "(" ^ sb v ^ ")" ^ " < " ^ "(" ^ sb w ^ ")"
      | Apply (">", [v; w]) -> "(" ^ sb v ^ ")" ^ " > " ^ "(" ^ sb w ^ ")"
      | Apply ("<=", [v; w]) -> "(" ^ sb v ^ ")" ^ " <= " ^ "(" ^ sb w ^ ")"
      | Apply (">=", [v; w]) -> "(" ^ sb v ^ ")" ^ " >= " ^ "(" ^ sb w ^ ")"
      | Apply ("RLIKE", [v; w]) -> "(" ^ sb v ^ ")" ^ " RLIKE " ^ "(" ^ sb w ^ ")"
      | Apply ("LIKE", [v; w]) -> "(" ^ sb v ^ ")" ^ " LIKE " ^ "(" ^ sb w ^ ")"
      | Apply (f, args) when SqlFuns.is f -> SqlFuns.name f ^ "(" ^ String.concat "," (List.map sb args) ^ ")"
      | Apply (f, args) -> f ^ "(" ^ String.concat "," (List.map sb args) ^ ")"
      | Empty q -> "not exists (" ^ string_of_query db true q ^ ")"
      | Length q -> "select count(*) from (" ^ string_of_query db true q ^ ") as " ^ fresh_dummy_var ()
      | RowNumber [] -> "1"
      | RowNumber ps ->
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
