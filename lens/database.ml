open Operators
open Lens_utility
module LPV = Phrase_value

type t =
  { driver_name: unit -> string
  ; escape_string: string -> string
  ; quote_field: string -> string
  ; execute: string -> unit
  ; execute_select:
         string
      -> field_types:(string * Phrase_type.t) list
      -> Phrase_value.t list }

module Table = struct
  type t = {name: string; keys: string list list}

  let name t = t.name
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
    | _ -> Format.asprintf "Unexpected phrase value %a." LPV.pp v |> failwith
    )

module Precedence = struct
  type t = Or | And | Not | Add | Sub | Mult | Divide | Cmp

  let ordering = [Or; And; Cmp; Not; Add; Sub; Mult; Divide]

  let first = List.hd ordering

  let order t =
    List.findi ordering ~f:(fun v -> v = t)
    |> Option.map ~f:fst
    |> fun v -> Option.value_exn v

  let pp_eq pr npr fmt f v =
    if order npr < order pr then Format.fprintf f "(%a)" fmt v
    else Format.fprintf f "%a" fmt v

  let pp_gr pr npr fmt f v =
    if order npr <= order pr then Format.fprintf f "(%a)" fmt v
    else Format.fprintf f "%a" fmt v
end

let rec fmt_phrase_ex ?(precedence = Precedence.And) ~db ~map f expr =
  let module Pr = Precedence in
  let pp_sep f () = Format.fprintf f ", " in
  let fmt i = fmt_phrase_ex ~precedence:i ~db ~map in
  let pp_eq = Pr.pp_eq precedence in
  let pp_gr = Pr.pp_gr precedence in
  match expr with
  | Phrase.Constant c -> fmt_phrase_value ~db f c
  | Phrase.Var v -> Format.fprintf f "%s" (map v)
  | Phrase.InfixAppl (op, a1, a2) ->
      let pr =
        match op with
        | Binary.LogicalAnd -> Pr.And
        | Binary.LogicalOr -> Pr.Or
        | Binary.Plus -> Pr.Add
        | Binary.Minus -> Pr.Sub
        | Binary.Equal
         |Binary.Greater
         |Binary.GreaterEqual
         |Binary.Less
         |Binary.LessEqual ->
            Pr.Cmp
        | Binary.Multiply -> Pr.Mult
        | Binary.Divide -> Pr.Divide
      in
      let pp f () =
        Format.fprintf f "%a %a %a" (fmt pr) a1 Binary.fmt op (fmt pr) a2
      in
      Format.fprintf f "%a" (pp |> pp_eq pr) ()
  | Phrase.TupleLit l ->
      Format.fprintf f "(%a)" (Format.pp_print_list ~pp_sep (fmt Pr.first)) l
  | Phrase.UnaryAppl (op, a) ->
      let op =
        match op with
        | Unary.Not -> "NOT"
        | _ -> Unary.to_string op
      in
      let pp f () = Format.fprintf f "%s %a" op (fmt Pr.Not) a in
      Format.fprintf f "%a" (pp |> pp_gr Pr.Not) ()
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
      if cases = [] then Format.fprintf f "%a" (fmt precedence) otherwise
      else
        let f =
          match inp with
          | None -> Format.fprintf f "CASE %a ELSE %a END"
          | Some inp ->
              Format.fprintf f "CASE (%a) %a ELSE %a END" (fmt Pr.first) inp
        in
        let pp_sep f () = Format.fprintf f " " in
        let fmt_case f (k, v) =
          Format.fprintf f "WHEN %a THEN %a" (fmt Pr.first) k (fmt Pr.first) v
        in
        f
          (Format.pp_print_list ~pp_sep fmt_case)
          cases (fmt Pr.first) otherwise

let fmt_phrase ~db ~map f expr = fmt_phrase_ex ~db ~map f expr

let fmt_phrase_dummy f expr =
  let db = dummy_database in
  let map a = db.quote_field a in
  fmt_phrase ~db ~map f expr

let to_string_dummy expr = Format.asprintf "%a" fmt_phrase_dummy expr

module Select = struct
  type db = t

  type t =
    { tables: (string * string) list
    ; cols: Column.t list
    ; predicate: Phrase.t option
    ; db: db }

  let select t ~predicate =
    let predicate = Phrase.Option.combine_and t.predicate predicate in
    {t with predicate}

  let of_sort t ~sort =
    let predicate = Sort.query sort in
    let cols = Sort.cols sort in
    let tables =
      List.map ~f:Column.table cols
      |> List.sort_uniq String.compare
      |> List.map ~f:(fun c -> (c, c))
    in
    {predicate; cols; tables; db= t}

  let fmt f v =
    let db = v.db in
    let map a =
      let col = List.find_exn ~f:(fun c -> Column.alias c = a) v.cols in
      Format.sprintf "%s.%s"
        (Column.table col |> db.quote_field)
        (Column.name col |> db.quote_field)
    in
    let cols = v.cols |> Column.List.present in
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
    let field_types = [("t", Phrase_type.Bool)] in
    let res = database.execute_select sql ~field_types in
    match res with
    | [Phrase_value.Record [(_, Phrase_value.Bool b)]] -> b
    | _ -> failwith "Expected singleton value."
end

module Delete = struct
  type db = t

  type t = {table: string; predicate: Phrase.t option; db: db}

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
    ; predicate: Phrase.t option
    ; set: (string * Phrase_value.t) list
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
    ; values: Phrase_value.t list list
    ; db: db }

  let fmt_table ~db f v = Format.fprintf f "%s" @@ db.quote_field v

  let fmt_col ~db f v = Format.pp_print_string f (db.quote_field v)

  let fmt f v =
    let db = v.db in
    let fmt_vals f v =
      Format.fprintf f "(%a)" (fmt_phrase_value ~db |> Format.pp_comma_list) v
    in
    Format.fprintf f "INSERT INTO %a (%a) VALUES %a" (fmt_table ~db) v.table
      (fmt_col ~db |> Format.pp_comma_list)
      v.columns
      (fmt_vals |> Format.pp_comma_list)
      v.values
end
