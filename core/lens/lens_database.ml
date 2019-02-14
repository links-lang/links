open Types
open Utility
open Lens_operators
open Lens_utility

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
    (match vals with
     | [] -> Format.fprintf f "FALSE"
     | _ ->
       Format.fprintf f "(%a) IN (%a)"
         (Format.pp_print_list ~pp_sep fmt_name) names
         (Format.pp_print_list ~pp_sep fmt_val) vals)
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
    let cols = v.cols |> Lens_column.List.present in
    match v.predicate with
    | None -> Format.fprintf f "SELECT %a FROM %a" (fmt_cols ~db) cols (fmt_tables ~db) v.tables
    | Some pred ->
      Format.fprintf f "SELECT %a FROM %a WHERE %a"
        (fmt_cols ~db) cols
        (fmt_tables ~db) v.tables
        (fmt_phrase ~db ~map) pred

  let execute query ~(database:db) ~field_types =
    let query = Format.asprintf "%a" fmt query in
    let result,rs = Database.execute_select_result field_types query database in
    Database.build_result (result,rs)

  let query_exists query ~(database:db) =
    let sql = Format.asprintf "SELECT EXISTS (%a) AS t" fmt query in
    let mappings = ["t", `Primitive `Bool] in
    let res = Database.execute_select_result mappings sql database in
    let res = Database.build_result res in
    let (_,v) = Value.unbox_record (List.hd (Value.unbox_list res)) |> List.hd in
    Value.unbox_bool v
end

module Delete = struct
  type db = t

  type t = {
    table : string;
    predicate: lens_phrase option;
    db : db;
  }

  let fmt_table ~(db:db) f v =
    Format.fprintf f "%s" @@ db#quote_field v

  let fmt f v =
    let db = v.db in
    let map = fun col ->
      Format.sprintf "%s" (db#quote_field col) in
    match v.predicate with
    | None -> Format.fprintf f "DELETE FROM %a" (fmt_table ~db) v.table
    | Some pred ->
      Format.fprintf f "DELETE FROM %a WHERE %a"
        (fmt_table ~db) v.table
        (fmt_phrase ~db ~map) pred
end

module Update = struct
  type db = t

  type t = {
    table : string;
    predicate : lens_phrase option;
    set : (string * Value.t) list;
    db : db;
  }

  let fmt_set_value ~(db:db) f (key, value) =
    Format.fprintf f "%s = %a" (db#quote_field key) Constant.fmt (Constant.of_value value)

  let fmt_set_values ~(db:db) f vs =
    Format.pp_comma_list (fmt_set_value ~db) f vs

  let fmt_table ~(db:db) f v =
    Format.fprintf f "%s" @@ db#quote_field v

  let fmt f v =
    let db = v.db in
    let map = fun col ->
      Format.sprintf "%s" (db#quote_field col) in
    match v.predicate with
    | None -> Format.fprintf f "UPDATE %a SET %a" (fmt_table ~db) v.table (fmt_set_values ~db) v.set
    | Some pred ->
      Format.fprintf f "UPDATE %a SET %a WHERE %a"
        (fmt_table ~db) v.table
        (fmt_set_values ~db) v.set
        (fmt_phrase ~db ~map) pred
end

module Insert = struct
  type db = t

  type t = {
    table : string;
    columns : string list;
    values : Value.t list;
    db : db;
  }

  let fmt_table ~(db:db) f v =
    Format.fprintf f "%s" @@ db#quote_field v

  let fmt_col ~(db:db) f v =
    Format.pp_print_string f (db#quote_field v)

  let fmt_val ~db:_ f v =
    Constant.fmt f (Constant.of_value v)

  let fmt f v =
    let db = v.db in
    Format.fprintf f "INSERT INTO %a (%a) VALUES (%a)"
      (fmt_table ~db) v.table
      (fmt_col ~db |> Format.pp_comma_list) v.columns
      (fmt_val ~db |> Format.pp_comma_list) v.values

end
