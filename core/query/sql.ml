open Utility
open CommonTypes

type index = (Var.var * string) list
type range = int * int

type table_name = string (* FIXME: allow variables? *)
    [@@deriving show]

type query =
  | UnionAll  of query list * int
  | Select    of select_clause
  | Insert    of {
      ins_table: table_name;
      ins_fields: string list;
      ins_records: base list list
    }
  | Update    of {
      upd_table: table_name;
      upd_fields: (string * base) list;
      upd_where: base option
    }
  | Delete    of { del_table: table_name; del_where: base option }
  | With      of table_name * query * query
and select_clause = (base * string) list * from_clause list * base * base list
and from_clause =
  | TableRef of table_name * Var.var
  | Subquery of query * Var.var
and sql_like =
  (* Used to implement 'LIKE' in SQL. Monoidal
   * (with unit `LikeString ""`) . *)
  | LikeString of string
  | LikeProject of Var.var * string
  | LikeAppend of (sql_like * sql_like)
and base =
  | Case      of base * base * base
  | Constant  of Constant.t
  | Like      of sql_like
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
  val sql_name : string -> string
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

let pp_comma ppf () = Format.pp_print_string ppf ","
let pp_comma_separated pp_item = Format.pp_print_list ~pp_sep:pp_comma pp_item

let rec pr_query quote ignore_fields ppf q =
  let pp_quote ppf q = Format.pp_print_string ppf (quote q) in

  let pr_q = pr_query quote ignore_fields in
  let pr_b = pr_base quote false in
  let pr_b_ignore_fields = pr_base quote true in

  let gen_pp_pair ppf fmt_str fl fr (l, r) = Format.fprintf ppf fmt_str fl l fr r in
  let gen_pp_option ppf fmt_str f option = OptionUtils.opt_iter (Format.fprintf ppf fmt_str f) option in

  let pr_fields ppf fields =
    let pp_field ppf (b, l) = gen_pp_pair ppf "(%a) as %a" pr_b pp_quote (b, l) in
    if ignore_fields then
      Format.pp_print_string ppf "0 as \"@unit@\"" (* SQL doesn't support empty records! *)
    else
      match fields with
        | [] -> Format.pp_print_string ppf "0 as \"@unit@\"" (* SQL doesn't support empty records! *)
        | fields -> (pp_comma_separated pp_field) ppf fields
  in

  let pr_select ppf fields tables condition os =
    let pp_os_condition ppf a = Format.fprintf ppf "%a" pr_b a in
    let pp_orderby ppf os =
      match os with
        | [] -> ()
        | _ -> Format.fprintf ppf "\norder by %a" (pp_comma_separated pp_os_condition) os in
    let pp_from_clause ppf fc =
      match fc with
        | TableRef (t, x) -> Format.fprintf ppf "%a as %s" pp_quote t (string_of_table_var x)
        | Subquery (q, x) -> Format.fprintf ppf "(%a) as %s" pr_q q (string_of_table_var x) in
    let pp_where ppf condition =
      match condition with
        | Constant (Constant.Bool true) -> ()
        | _ -> Format.fprintf ppf "\nwhere %a" pp_os_condition condition in
    Format.fprintf ppf "select %a\nfrom %a%a%a"
      pr_fields fields
      (pp_comma_separated pp_from_clause) tables
      pp_where condition
      pp_orderby os
  in
  let pp_opt_where ppf where = gen_pp_option ppf "where (%a)" pr_b_ignore_fields where in
  let pr_delete ppf table where =
    Format.fprintf ppf "delete from %a %a"
      Format.pp_print_string table
      pp_opt_where where
  in
  let pr_update ppf table fields where =
    let pp_field ppf (k, v) = gen_pp_pair ppf "%a = %a" pp_quote pr_b_ignore_fields (k, v) in
    Format.fprintf ppf "update %a\nset %a %a"
      Format.pp_print_string table
      (pp_comma_separated pp_field) fields
      pp_opt_where where
  in
  let pr_insert ppf table fields values =
    let pp_value ppf x =
      Format.fprintf ppf "(%a)"
        (pp_comma_separated pr_b_ignore_fields) x in
    Format.fprintf ppf "insert into %a (%a)\nvalues %a"
      Format.pp_print_string table
      (pp_comma_separated Format.pp_print_string) fields
      (pp_comma_separated pp_value) values
  in
  match q with
    | UnionAll ([], _) -> Format.pp_print_string ppf "select 42 as \"@unit@\" from (select 42) x where 1=0"
    | UnionAll ([q], n) ->
        Format.fprintf ppf "%a%a"
          pr_q q
          Format.pp_print_string (order_by_clause n)
    | UnionAll (qs, n) ->
      let pp_sep_union ppf () = Format.fprintf ppf "\nunion all\n" in
      let pp_value ppf x = Format.fprintf ppf "(%a)" pr_q x in
      Format.fprintf ppf "%a%a"
        (Format.pp_print_list ~pp_sep:pp_sep_union pp_value) qs
        Format.pp_print_string (order_by_clause n)
    | Select (fields, [], Constant (Constant.Bool true), _os) ->
      Format.fprintf ppf "select %a" pr_fields fields
    | Select (fields, [], condition, _os) ->
      Format.fprintf ppf "select * from (select %a) as %a where %a"
        pr_fields fields
        Format.pp_print_string (fresh_dummy_var ())
        pr_b condition
    | Select (fields, tables, condition, os) ->
        pr_select ppf fields tables condition os
    | Delete { del_table; del_where } ->
        pr_delete ppf del_table del_where
    | Update { upd_table; upd_fields; upd_where } ->
        pr_update ppf upd_table upd_fields upd_where
    | Insert { ins_table; ins_fields; ins_records } ->
        pr_insert ppf ins_table ins_fields ins_records
    | With (z, q, q') ->
        Format.fprintf ppf "with %s as (@[<v>%a@])\n%a"
          z
          pr_q q
          pr_q q'

and pr_base quote one_table ppf b =
  let pp_projection quote one_table ppf (var, label) =
  if one_table then
    Format.pp_print_string ppf (quote label)
  else
    Format.fprintf ppf "%s.%s" (string_of_table_var var) (quote label)
  in
  let pr_b_one_table = pr_base quote one_table in
  let pr_q_true = pr_query quote true in
  let unary_ops =
    StringSet.of_list ["intToString"; "stringToInt"; "intToFloat";
                       "floatToString"; "stringToFloat"; "floatToInt";
                       "not"; "negate"; "negatef"] in
  let binary_ops =
    StringSet.of_list ["&&"; "||"; "=="; "<>"; "<"; ">"; "<="; ">="; "RLIKE"; "LIKE"] in
  let binary_map op =
    match op with
      | "&&" -> "and"
      | "||" -> "or"
      | "==" -> "="
      | _    -> op
  in
  let unary_map op =
    match op with
      | "floatToInt"    -> "floor"
      | "not"           -> "not "
      | "negate"
      | "negatef"       -> "-"
      | "intToString"
      | "stringToInt"
      | "intToFloat"
      | "floatToString"
      | "stringToFloat" -> ""
      | _               -> assert false
  in
  let pp_sql_arithmetic ppf (l, op, r) =
    match op with
      | "/" -> Format.fprintf ppf "floor(%a/%a)"
            pr_b_one_table l
            pr_b_one_table r
      | "^" -> Format.fprintf ppf "floor(pow(%a,%a))"
            pr_b_one_table l
            pr_b_one_table r
      | "^." -> Format.fprintf ppf "pow(%a,%a)"
            pr_b_one_table l
            pr_b_one_table r
      | _ -> Format.fprintf ppf "(%a%s%a)"
            pr_b_one_table l
            (Arithmetic.sql_name op)
            pr_b_one_table r
  in
  let rec pp_sql_like ppf = function
    | LikeString s ->
        Format.pp_print_string ppf
          ("'" ^ CommonTypes.Constant.escape_string s ^ "'")
    | LikeProject (v, f) ->
        Format.fprintf ppf "%a.%a"
          Format.pp_print_string (quote (string_of_table_var v))
          Format.pp_print_string f
    (* Special case appends with the empty string *)
    | LikeAppend (LikeString "", l) ->
        pp_sql_like ppf l
    | LikeAppend (l, LikeString "") ->
        pp_sql_like ppf l
    | LikeAppend (l1, l2) ->
        Format.fprintf ppf "%a || %a"
          pp_sql_like l1
          pp_sql_like l2
  in
    match b with
      | Case (c, t, e) ->
          Format.fprintf ppf "case when %a then %a else %a end"
            pr_b_one_table c
            pr_b_one_table t
            pr_b_one_table e
      | Like sl ->
          pp_sql_like ppf sl
      | Constant c ->
          Format.pp_print_string ppf (Constant.to_string c)
      | Project (var, label) ->
          pp_projection quote one_table ppf (var, label)
      | Apply (op, [l; r]) when Arithmetic.is op ->
          pp_sql_arithmetic ppf (l, op, r)
            (* special case: not empty is translated to exists *)
      | Apply ("not", [Empty q]) ->
          Format.fprintf ppf "exists (%a)"
            pr_q_true q
      | Apply (uop, [v]) when StringSet.mem uop unary_ops ->
          Format.fprintf ppf "%s(%a)"
            (unary_map uop)
            pr_b_one_table v
      | Apply (op, [v; w]) when StringSet.mem op binary_ops ->
          Format.fprintf ppf "(%a) %s (%a)"
            pr_b_one_table v
            (binary_map op)
            pr_b_one_table w
      | Apply (f, args) when SqlFuns.is f ->
          Format.fprintf ppf "%a(%a)"
            Format.pp_print_string (SqlFuns.name f)
            (pp_comma_separated pr_b_one_table) args
      | Apply (f, args) ->
          Format.fprintf ppf "%a(%a)"
            Format.pp_print_string f
            (pp_comma_separated pr_b_one_table) args
      | Empty q ->
          Format.fprintf ppf "not exists (%a)"
            pr_q_true q
      | Length q ->
          Format.fprintf ppf "select count(*) from (%a) as %a"
            pr_q_true q
            Format.pp_print_string (fresh_dummy_var ())
      | RowNumber [] ->
          Format.fprintf ppf "%a" Format.pp_print_string "1"
      | RowNumber ps ->
        Format.fprintf ppf "row_number() over (order by %a)"
            (pp_comma_separated (pp_projection quote one_table)) ps


(* NOTE: Inlines a WITH common table expression if it is the toplevel
 * constructor of a union of queries and is used immediately in a SELECT query.
 * This handles the common case where WITH is generated solely as an
 * abbreviation by shredding.  This is a very limited case of inlining WITH bindings. *)

let rec inline_outer_with q =
  let replace_subquery z q = function
    | TableRef(y,x) when y = z -> Subquery(q,x)
    | fromclause -> fromclause
  in
  match q with
    | With (z, q, Select (fields, tables, condition, os)) ->
        Select(fields, List.map (replace_subquery z q) tables, condition, os)
    | UnionAll(qs,n) -> UnionAll(List.map inline_outer_with qs,n)
    | q -> q


let string_of_base quote one_table b =
  Format.asprintf "%a" (pr_base quote one_table) b

let string_of_query ?(range=None) quote q =
  let pr_range ppf range =
    match range with
      | None -> ()
      | Some (limit, offset) -> Format.fprintf ppf " limit %i offset %i" limit offset
  in
  Format.asprintf "%a%a" (pr_query quote false) q pr_range range
