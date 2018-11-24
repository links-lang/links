open Types
open Lens_operators

module Constant = Lens_constant

type t = Value.database

class dummy_database = object(_self)
  inherit Value.database
  method driver_name () = "dummy"
  method exec _query : Value.dbvalue =
    failwith "Dummy database exec not supported."
  method escape_string str =
    "'" ^ Str.global_replace (Str.regexp "'") "''" str ^ "'"
  method quote_field f =
    "`" ^ Str.global_replace (Str.regexp "`") "``" f ^ "`"
  method! make_insert_returning_query : (string * string list * string list list * string) -> string list =
    failwith "Dummy database make_insert_returning_query not supported"
end

let fmt_comma_seperated v =
  let pp_sep f () = Format.fprintf f ", " in
  Format.pp_print_list ~pp_sep v

let fmt_col ~db f col =
  Format.fprintf f "%s.%s AS %s" (db#quote_field col.table) (db#quote_field col.name) (db#quote_field col.alias)

let fmt_table ~db f (table, alias) =
  Format.fprintf f "%s AS %s" (db#quote_field table) (db#quote_field alias)

let fmt_tables ~db f tables =
  Format.fprintf f "%a" (fmt_table ~db |> fmt_comma_seperated) tables

let fmt_cols ~db f cols =
  Format.fprintf f "%a" (fmt_col ~db |> fmt_comma_seperated) cols

let rec fmt_phrase ~db ~map f expr =
  let pp_sep f () = Format.fprintf f ", " in
  let fmt = fmt_phrase ~db ~map in
  match expr with
  | Constant c -> Format.fprintf f "%a" Constant.fmt c
  | Var v -> Format.fprintf f "%s" (map v)
  | InfixAppl (op, a1, a2) -> Format.fprintf f "%a %a %a" fmt a1 Binary.fmt op fmt a2
  | TupleLit l -> Format.fprintf f "(%a)" (Format.pp_print_list ~pp_sep fmt) l
  | UnaryAppl (op, a) ->
    let op = match op with
      | Unary.Not -> "NOT"
      | _ -> Unary.to_string op in
    Format.fprintf f "%s (%a)" op fmt a
  | In (names, vals) ->
    let fmt_name f v = Format.fprintf f "%s" (map v) in
    let fmt_val f v = Format.fprintf f "(%a)" (Format.pp_print_list ~pp_sep Constant.fmt) v in
    Format.fprintf f "(%a) IN (%a)"
      (Format.pp_print_list ~pp_sep fmt_name) names
      (Format.pp_print_list ~pp_sep fmt_val) vals
  | Case (inp, cases, otherwise) ->
    if cases = [] then
      Format.fprintf f "%a" fmt otherwise
    else
      let f =
        match inp with
        | None -> Format.fprintf f "CASE %a ELSE %a END"
        | Some inp -> Format.fprintf f "CASE (%a) %a ELSE %a END" fmt inp in
      let pp_sep f () = Format.fprintf f " " in
      let fmt_case f (k,v) = Format.fprintf f "WHEN %a THEN %a" fmt k fmt v in
      f (Format.pp_print_list ~pp_sep fmt_case) cases fmt otherwise

let fmt_phrase_dummy f expr =
  let db = (new dummy_database) in
  let map = fun a -> db#quote_field a in
  fmt_phrase ~db ~map f expr

let to_string_dummy expr =
  Format.asprintf "%a" fmt_phrase_dummy expr

module Select = struct
  type db = t

  type t = {
      tables : (string * string) list;
      cols : lens_col list;
      predicate: lens_phrase option;
      db : db;
  }

  let of_sort t ~sort =
    let predicate = Lens_sort.predicate sort in
    let cols = Lens_sort.cols sort in
    let tables =
      List.map Lens_column.table cols
      |> List.sort_uniq String.compare
      |> List.map (fun c -> (c,c)) in
    {
      predicate;
      cols;
      tables;
      db = t;
    }

  let fmt f v =
    let db = v.db in
    let map = fun a ->
      let col = List.find (fun c -> c.alias = a) v.cols in
      Format.sprintf "%s.%s" (db#quote_field col.table) (db#quote_field col.name) in
    match v.predicate with
    | None -> Format.fprintf f "SELECT %a FROM %a" (fmt_cols ~db) v.cols (fmt_tables ~db) v.tables
    | Some pred ->
      Format.fprintf f "SELECT %a FROM %a WHERE %a"
        (fmt_cols ~db) v.cols
        (fmt_tables ~db) v.tables
        (fmt_phrase ~db ~map) pred
end
