open Lens_operators
open Lens_utility
module LPV = Lens_phrase_value
module Phrase = Lens_phrase
module Column = Lens_column
module Sort = Lens_sort

type t =
  { driver_name: unit -> string
  ; escape_string: string -> string
  ; quote_field: string -> string
  ; execute: string -> unit
  ; execute_select:
         string
      -> field_types:(string * Lens_phrase_type.t) list
      -> Lens_phrase_value.t list }

module Table = struct
  type t = {name: string; keys: string list list}
end

let dummy_database =
  let driver_name () = "dummy" in
  let escape_string str =
    "'" ^ Str.global_replace (Str.regexp "'") "''" str ^ "'"
  in
  let quote_field f = "`" ^ Str.global_replace (Str.regexp "`") "``" f ^ "`" in
  let execute _ = failwith "Dummy database exec not supported." in
  let execute_select _ ~field_types:_ =
    failwith "Dummy database exec not supported."
  in
  {driver_name; escape_string; quote_field; execute; execute_select}

let fmt_comma_seperated v =
  let pp_sep f () = Format.fprintf f ", " in
  Format.pp_print_list ~pp_sep v

let fmt_col ~db f col =
  Format.fprintf f "%s.%s AS %s"
    (Column.table col |> db.quote_field)
    (Column.name col |> db.quote_field)
    (Column.alias col |> db.quote_field)

let fmt_table ~db f (table, alias) =
  Format.fprintf f "%s AS %s" (db.quote_field table) (db.quote_field alias)

let fmt_tables ~db f tables =
  Format.fprintf f "%a" (fmt_table ~db |> fmt_comma_seperated) tables

let fmt_cols ~db f cols =
  Format.fprintf f "%a" (fmt_col ~db |> fmt_comma_seperated) cols

let fmt_phrase_value ~db f v =
  Format.fprintf f "%s"
    ( match v with
    | LPV.Bool b -> string_of_bool b
    | LPV.Int v -> string_of_int v
    | LPV.String v -> db.escape_string v
    | LPV.Char c -> db.escape_string (String.make 1 c)
    | LPV.Float v ->
        let s = string_of_float v in
        if s.[String.length s - 1] = '.' then s ^ "0" else s
    | _ -> failwith "Unexpected phrase value." )

let rec fmt_phrase ~db ~map f expr =
  let pp_sep f () = Format.fprintf f ", " in
  let fmt = fmt_phrase ~db ~map in
  match expr with
  | Phrase.Constant c -> fmt_phrase_value ~db f c
  | Phrase.Var v -> Format.fprintf f "%s" (map v)
  | Phrase.InfixAppl (op, a1, a2) ->
      Format.fprintf f "%a %a %a" fmt a1 Binary.fmt op fmt a2
  | Phrase.TupleLit l ->
      Format.fprintf f "(%a)" (Format.pp_print_list ~pp_sep fmt) l
  | Phrase.UnaryAppl (op, a) ->
      let op = match op with Unary.Not -> "NOT" | _ -> Unary.to_string op in
      Format.fprintf f "%s (%a)" op fmt a
  | Phrase.In (names, vals) -> (
      let fmt_name f v = Format.fprintf f "%s" (map v) in
      let fmt_val f v =
        Format.fprintf f "(%a)"
          (Format.pp_print_list ~pp_sep (fmt_phrase_value ~db))
          v
      in
      match vals with
      | [] -> Format.fprintf f "FALSE"
      | _ ->
          Format.fprintf f "(%a) IN (%a)"
            (Format.pp_print_list ~pp_sep fmt_name)
            names
            (Format.pp_print_list ~pp_sep fmt_val)
            vals )
  | Phrase.Case (inp, cases, otherwise) ->
      if cases = [] then Format.fprintf f "%a" fmt otherwise
      else
        let f =
          match inp with
          | None -> Format.fprintf f "CASE %a ELSE %a END"
          | Some inp -> Format.fprintf f "CASE (%a) %a ELSE %a END" fmt inp
        in
        let pp_sep f () = Format.fprintf f " " in
        let fmt_case f (k, v) =
          Format.fprintf f "WHEN %a THEN %a" fmt k fmt v
        in
        f (Format.pp_print_list ~pp_sep fmt_case) cases fmt otherwise

let fmt_phrase_dummy f expr =
  let db = dummy_database in
  let map a = db.quote_field a in
  fmt_phrase ~db ~map f expr

let to_string_dummy expr = Format.asprintf "%a" fmt_phrase_dummy expr

module Select = struct
  type db = t

  type t =
    { tables: (string * string) list
    ; cols: Lens_column.t list
    ; predicate: Lens_phrase.t option
    ; db: db }

  let of_sort t ~sort =
    let predicate = Lens_sort.predicate sort in
    let cols = Lens_sort.cols sort in
    let tables =
      List.map ~f:Lens_column.table cols
      |> List.sort_uniq String.compare
      |> List.map ~f:(fun c -> (c, c))
    in
    {predicate; cols; tables; db= t}

  let fmt f v =
    let db = v.db in
    let map a =
      let col = List.find (fun c -> Lens_column.alias c = a) v.cols in
      Format.sprintf "%s.%s"
        (Lens_column.table col |> db.quote_field)
        (Lens_column.name col |> db.quote_field)
    in
    let cols = v.cols |> Lens_column.List.present in
    match v.predicate with
    | None ->
        Format.fprintf f "SELECT %a FROM %a" (fmt_cols ~db) cols
          (fmt_tables ~db) v.tables
    | Some pred ->
        Format.fprintf f "SELECT %a FROM %a WHERE %a" (fmt_cols ~db) cols
          (fmt_tables ~db) v.tables (fmt_phrase ~db ~map) pred

  let execute query ~database ~field_types =
    let query = Format.asprintf "%a" fmt query in
    database.execute_select query ~field_types

  let query_exists query ~database =
    let sql = Format.asprintf "SELECT EXISTS (%a) AS t" fmt query in
    let field_types = [("t", Lens_phrase_type.Bool)] in
    let res = database.execute_select sql ~field_types in
    match res with
    | [Lens_phrase_value.Record [(_, Lens_phrase_value.Bool b)]] -> b
    | _ -> failwith "Expected singleton value."
end

module Delete = struct
  type db = t

  type t = {table: string; predicate: Lens_phrase.t option; db: db}

  let fmt_table ~(db : db) f v = Format.fprintf f "%s" @@ db.quote_field v

  let fmt f v =
    let db = v.db in
    let map col = Format.sprintf "%s" (db.quote_field col) in
    match v.predicate with
    | None -> Format.fprintf f "DELETE FROM %a" (fmt_table ~db) v.table
    | Some pred ->
        Format.fprintf f "DELETE FROM %a WHERE %a" (fmt_table ~db) v.table
          (fmt_phrase ~db ~map) pred
end

module Update = struct
  type db = t

  type t =
    { table: string
    ; predicate: Lens_phrase.t option
    ; set: (string * Lens_phrase_value.t) list
    ; db: db }

  let fmt_set_value ~db f (key, value) =
    Format.fprintf f "%s = %a" (db.quote_field key) (fmt_phrase_value ~db)
      value

  let fmt_set_values ~db f vs = Format.pp_comma_list (fmt_set_value ~db) f vs

  let fmt_table ~db f v = Format.fprintf f "%s" @@ db.quote_field v

  let fmt f v =
    let db = v.db in
    let map col = Format.sprintf "%s" (db.quote_field col) in
    match v.predicate with
    | None ->
        Format.fprintf f "UPDATE %a SET %a" (fmt_table ~db) v.table
          (fmt_set_values ~db) v.set
    | Some pred ->
        Format.fprintf f "UPDATE %a SET %a WHERE %a" (fmt_table ~db) v.table
          (fmt_set_values ~db) v.set (fmt_phrase ~db ~map) pred
end

module Insert = struct
  type db = t

  type t =
    { table: string
    ; columns: string list
    ; values: Lens_phrase_value.t list
    ; db: db }

  let fmt_table ~db f v = Format.fprintf f "%s" @@ db.quote_field v

  let fmt_col ~db f v = Format.pp_print_string f (db.quote_field v)

  let fmt f v =
    let db = v.db in
    Format.fprintf f "INSERT INTO %a (%a) VALUES (%a)" (fmt_table ~db) v.table
      (fmt_col ~db |> Format.pp_comma_list)
      v.columns
      (fmt_phrase_value ~db |> Format.pp_comma_list)
      v.values
end
