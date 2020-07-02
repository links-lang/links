open Utility
open CommonTypes

type index = (Var.var * string) list
type range = int * int

type query =
  | UnionAll  of query list * int
  | Select    of select_clause
  | Insert    of {
      ins_table: string;
      ins_fields: string list;
      ins_records: base list list
    }
  | Update    of {
      upd_table: string;
      upd_fields: (string * base) list;
      upd_where: base option
    }
  | Delete    of { del_table: string; del_where: base option }
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

(* concatenation implement with Buffer module*)
let buffer_concat xs =
  let buf = Buffer.create 0 in (* maybe a better heuristic init size? *)
  List.iter (Buffer.add_string buf) xs;
  Buffer.contents buf

let rec buf_mapstrcat buf list f sep =
  match list with
    | [] -> ()
    | [x] -> f x
    | x :: xs ->
      f x; Buffer.add_string buf sep; buf_mapstrcat buf xs f sep

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

let rec pr_query ppf buf quote ignore_fields q =
  let buf_add = Buffer.add_string buf in
  let pp_constant ppf s = Format.fprintf ppf "%s" s in
  let pp_comma ppf () = Format.fprintf ppf "," in
  let pp_quote ppf q = Format.fprintf ppf "%s" (quote q) in

  let pr_q ppf q = pr_query ppf buf quote ignore_fields q in
  let pr_b ppf q = pr_base ppf buf quote false q in
  let pr_b_ignore_fields ppf q = pr_base ppf buf quote true q in

  (*TODO: maybe add a template pair/option/list printer func? *)
  (*TODO: or single term with/without bracket template printer func *)
  let pr_fields ppf fields =
    let pp_field ppf (b, l) = Format.fprintf ppf "(%a) as %a" pr_b b pp_quote l in (*MARK: can be template-pair *)
    if ignore_fields then
      Format.fprintf ppf "%a" pp_constant "0 as \"@unit@\"" (* SQL doesn't support empty records! *)
    else
      match fields with
        | [] -> Format.fprintf ppf "%a" pp_constant "0 as \"@unit@\"" (* SQL doesn't support empty records! *)
        | fields -> Format.fprintf ppf "%a" (Format.pp_print_list ~pp_sep:pp_comma pp_field) fields
  in

  let pr_select ppf fields tables condition os =
    let pp_tables ppf tables = Format.fprintf ppf "%a" (Format.pp_print_list ~pp_sep:pp_comma Format.pp_print_string) tables in (*MARK: can be template-list *)
    let pp_os_condition ppf a = Format.fprintf ppf "%a" pr_b a in
    let pp_orderby ppf os = 
      match os with
        | [] -> ()
        | _ -> Format.fprintf ppf " order by %a" (Format.pp_print_list ~pp_sep:pp_comma pp_os_condition) os in
    let pp_where ppf condition = 
      match condition with
        | Constant (Constant.Bool true) -> ()
        | _ -> Format.fprintf ppf " where %a" pp_os_condition condition in

    Format.fprintf ppf "select %a from %a%a%a" 
      pr_fields fields
      pp_tables tables
      pp_where condition
      pp_orderby os
  in

  let pr_delete ppf table where =
    let pp_where ppf where = 
      match where with
        | None -> ()
        | Some x -> Format.fprintf ppf "where (%a)" pr_b_ignore_fields x in (*MARK: can be template-option *)

    Format.fprintf ppf "delete from %a%a"
      pp_constant table
      pp_where where
  in
  let pr_update ppf table fields where =
    let pp_field ppf (k, v) = Format.fprintf ppf "%a = %a" pp_quote k pr_b_ignore_fields v in (*MARK: can be template-pair *)
    let pp_fields ppf fields = Format.fprintf ppf "%a" (Format.pp_print_list ~pp_sep:pp_comma pp_field) fields in (*MARK: can be template-list *)
    let pp_where ppf where = 
      match where with
        | None -> ()
        | Some x -> Format.fprintf ppf "where (%a)" pr_b_ignore_fields x in (*MARK: can be template-option *)
    
    Format.fprintf ppf "update %a set %a %a"
      pp_constant table
      pp_fields fields
      pp_where where
  in
  let pr_insert ppf table fields values =
    let pp_fields ppf fields = Format.fprintf ppf "%a" (Format.pp_print_list ~pp_sep:pp_comma Format.pp_print_string) fields in (*MARK: can be template-list *)
    let pp_value ppf x = Format.fprintf ppf "(%a)" (Format.pp_print_list ~pp_sep:pp_comma pr_b_ignore_fields) x in
    let pp_values ppf values = Format.fprintf ppf "%a" (Format.pp_print_list ~pp_sep:pp_comma pp_value) values in (*MARK: can be template-list *)
    
    Format.fprintf ppf "insert into %a (%a) values %a"
      pp_constant table
      pp_fields fields
      pp_values values
  in
  match q with
    | UnionAll ([], _) -> Format.fprintf ppf "%a" pp_constant "select 42 as \"@unit@\" where false"
    | UnionAll ([q], n) -> Format.fprintf ppf "%a%a" pr_q q pp_constant (order_by_clause n)
    | UnionAll (qs, n) ->
      let pp_sep_union ppf () = Format.fprintf ppf " union all " in
      let pp_value ppf x = Format.fprintf ppf "(%a)" pr_q x in
      let pp_values ppf values = Format.fprintf ppf "%a" (Format.pp_print_list ~pp_sep:pp_sep_union pp_value) values in (*MARK: can be template-list *)
      Format.fprintf ppf "%a%a" 
        pp_values qs 
        pp_constant (order_by_clause n)
    | Select (fields, [], Constant (Constant.Bool true), _os) ->
      Format.fprintf ppf "select %a" pr_fields fields
    | Select (fields, [], condition, _os) ->
      Format.fprintf ppf "select * from (select %a) as %a where %a"
        pr_fields fields
        pp_constant (fresh_dummy_var ())
        pr_b condition
    | Select (fields, tables, condition, os) ->
        (* using quote_field assumes tables contains table names (not nested queries) *)
        let tables = List.map (fun (t, x) -> buffer_concat [quote t; " as "; (string_of_table_var x)]) tables in 
        pr_select ppf fields tables condition os
    | Delete { del_table; del_where } ->
        pr_delete ppf del_table del_where
    | Update { upd_table; upd_fields; upd_where } ->
        pr_update ppf upd_table upd_fields upd_where
    | Insert { ins_table; ins_fields; ins_records } ->
        pr_insert ppf ins_table ins_fields ins_records
    | With (_, q, z, q') ->
        match q' with
        | Select (fields, tables, condition, os) ->
            (* Inline the query *)
            let tables = List.map (fun (t, x) -> buffer_concat [quote t; " as "; (string_of_table_var x)]) tables in
            let buf2 = Buffer.create 0 in
            let ppf2 = Format.formatter_of_buffer buf2 in
            let q2 = pr_query ppf2 buf2 quote ignore_fields q; Format.pp_print_flush ppf2 (); Buffer.contents buf2 in
            let q = buffer_concat ["("; q2; ") as "; string_of_table_var z] in
            pr_select ppf fields (q::tables) condition os
        | _ -> assert false

and pr_base ppf buf quote one_table b =
  let pp_constant ppf s = Format.fprintf ppf "%s" s in
  let pp_comma ppf () = Format.fprintf ppf "," in
  let pp_quote ppf q = Format.fprintf ppf "%s" (quote q) in
  let string_of_projection quote one_table (var, label) =
  if one_table then
    quote label
  else
    buffer_concat [string_of_table_var var; "."; (quote label)]
  in
  let buf_add = Buffer.add_string buf in
  let pr_b ppf b = pr_base ppf buf quote one_table b in
  let pr_q ppf q = pr_query ppf buf quote true q in
    match b with
      | Case (c, t, e) ->
        Format.fprintf ppf "case when %a then %a else %a end"
          pr_b c
          pr_b t
          pr_b e
      | Constant c -> Format.fprintf ppf "%a" pp_constant (Constant.to_string c)
      | Project (var, label) -> Format.fprintf ppf "%a" pp_constant (string_of_projection quote one_table (var, label))
      | Apply (op, [l; r]) when Arithmetic.is op ->
        let buf2 = Buffer.create 0 in
        let ppf2 = Format.formatter_of_buffer buf2 in
        let buf3 = Buffer.create 0 in
        let ppf3 = Format.formatter_of_buffer buf3 in
        let l2 = pr_base ppf2 buf2 quote one_table l; Format.pp_print_flush ppf2 (); Buffer.contents buf2 in
        let r2 = pr_base ppf3 buf3 quote one_table r; Format.pp_print_flush ppf3 (); Buffer.contents buf3 in
        Format.fprintf ppf "%a" pp_constant (Arithmetic.gen (l2, op, r2))
      | Apply (("intToString" | "stringToInt" | "intToFloat" | "floatToString"
                | "stringToFloat"), [v]) -> Format.fprintf ppf "%a" pr_b v
      | Apply ("floatToInt", [v]) -> Format.fprintf ppf "floor(%a)" pr_b v

      (* optimisation *)
      | Apply ("not", [Empty q]) -> Format.fprintf ppf "exists (%a)" pr_q q
      | Apply ("not", [v]) -> Format.fprintf ppf "not (%a)" pr_b v
      | Apply (("negate" | "negatef"), [v]) -> Format.fprintf ppf "-(%a)" pr_b v
      | Apply ("&&", [v; w]) -> Format.fprintf ppf "(%a) and (%a)" pr_b v pr_b w
      | Apply ("||", [v; w]) -> Format.fprintf ppf "(%a) or (%a)" pr_b v pr_b w
      | Apply ("==", [v; w]) -> Format.fprintf ppf "(%a) = (%a)" pr_b v pr_b w
      | Apply ("<>", [v; w]) -> Format.fprintf ppf "(%a) <> (%a)" pr_b v pr_b w
      | Apply ("<", [v; w]) -> Format.fprintf ppf "(%a) < (%a)" pr_b v pr_b w
      | Apply (">", [v; w]) -> Format.fprintf ppf "(%a) > (%a)" pr_b v pr_b w
      | Apply ("<=", [v; w]) -> Format.fprintf ppf "(%a) <= (%a)" pr_b v pr_b w
      | Apply (">=", [v; w]) -> Format.fprintf ppf "(%a) >= (%a)" pr_b v pr_b w
      | Apply ("RLIKE", [v; w]) -> Format.fprintf ppf "(%a) RLIKE (%a)" pr_b v pr_b w
      | Apply ("LIKE", [v; w]) -> Format.fprintf ppf "(%a) LIKE (%a)" pr_b v pr_b w
      | Apply (f, args) when SqlFuns.is f -> 
        let pp_value ppf x = Format.fprintf ppf "%a" pr_b x in
        let pp_values ppf values = Format.fprintf ppf "%a" (Format.pp_print_list ~pp_sep:pp_comma pp_value) values in
        Format.fprintf ppf "%a(%a)" pp_constant (SqlFuns.name f) pp_values args
      | Apply (f, args) -> 
        let pp_value ppf x = Format.fprintf ppf "%a" pr_b x in
        let pp_values ppf values = Format.fprintf ppf "%a" (Format.pp_print_list ~pp_sep:pp_comma pp_value) values in
        Format.fprintf ppf "%a(%a)" pp_constant f pp_values args
      | Empty q -> Format.fprintf ppf "not exists (%a)" pr_q q
      | Length q -> Format.fprintf ppf "select count(*) from (%a) as %a" pr_q q pp_constant (fresh_dummy_var ())
      | RowNumber [] -> Format.fprintf ppf "%a" pp_constant "1"
      | RowNumber ps -> Format.fprintf ppf "row_number() over (order by %a)" pp_constant (String.concat "," (List.map (string_of_projection quote one_table) ps))

let string_of_base quote one_table b =
  let buf = Buffer.create 0 in
  let ppf = Format.formatter_of_buffer buf in
  pr_base ppf buf quote one_table b; Format.pp_print_flush ppf (); Buffer.contents buf

let string_of_query ?(range=None) quote q =
  let pp_constant ppf s = Format.fprintf ppf "%s" s in
  let buf = Buffer.create 0 in
  let ppf = Format.formatter_of_buffer buf in
  let buf_add = Buffer.add_string buf in
  let range =
    match range with
      | None -> ""
      | Some (limit, offset) -> buffer_concat [" limit "; string_of_int limit; " offset "; string_of_int offset]
  in
  let print_query ppf q = pr_query ppf buf quote false q in
    Format.fprintf ppf "%a%a" print_query q pp_constant range;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
    (* pr_query buf quote false q; buf_add range; Buffer.contents buf *)
