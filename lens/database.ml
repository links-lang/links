open Operators
open Lens_utility
module LPV = Phrase_value

type t = {
  serialize : unit -> string;
  driver_name : unit -> string;
  escape_string : string -> string;
  quote_field : string -> string;
  execute : string -> unit;
  execute_select :
    string -> field_types:(string * Phrase_type.t) list -> Phrase_value.t list;
}

module E = struct
  type t =
    | Unsupported_for_driver of { driver : string; fn : string }
    | Unsupported_phrase_value of { value : Phrase_value.t }

  let pp f v =
    match v with
    | Unsupported_for_driver { driver; fn } ->
        Format.fprintf f
          "The function '%s' is not supported for the lens database driver \
           '%s'."
          driver fn
    | Unsupported_phrase_value { value } ->
        Format.fprintf f
          "The value '%a' is not supported by the lens database driver."
          Phrase_value.pp value

  let show v = Format.asprintf "%a" pp v

  exception E of t

  let raise v = raise (E v)

  let () =
    let print v =
      match v with
      | E e -> Some (show e)
      | _ -> None
    in
    Printexc.register_printer print
end

let show _ = "<db_driver>"
let pp f v = Format.fprintf f "%s" (show v)

module Table = struct
  type t = { name : string; keys : string list list } [@@deriving sexp]

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
  let serialize _ = "<dummy driver>" in
  {
    serialize;
    driver_name;
    escape_string;
    quote_field;
    execute;
    execute_select;
  }

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
    (match v with
    | LPV.Bool b -> (
        match b with
        | true -> "(1=1)"
        | false -> "(0=1)")
    | LPV.Int v -> string_of_int v
    | LPV.String v -> db.escape_string v
    | LPV.Char c -> db.escape_string (String.make 1 c)
    | LPV.Float v ->
        let s = string_of_float v in
        if s.[String.length s - 1] = '.' then s ^ "0" else s
    | LPV.Serial (`Key k) ->
        string_of_int k (* only support converting known keys. *)
    | LPV.Serial (`NewKeyMapped k) ->
        string_of_int k (* only support converting known keys. *)
    | _ -> E.Unsupported_phrase_value { value = v } |> E.raise)

module Precedence = struct
  type t = Or | And | Not | Add | Sub | Mult | Divide | Cmp

  let ordering = [ Or; And; Cmp; Not; Add; Sub; Mult; Divide ]

  let first = List.hd ordering

  let order t =
    List.findi ordering ~f:(fun v -> v = t) |> Option.map ~f:fst |> fun v ->
    Option.value_exn v

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
            vals)
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
        f (Format.pp_print_list ~pp_sep fmt_case) cases (fmt Pr.first) otherwise

let fmt_phrase ~db ~map f expr = fmt_phrase_ex ~db ~map f expr

let fmt_phrase_dummy f expr =
  let db = dummy_database in
  let map a = db.quote_field a in
  fmt_phrase ~db ~map f expr

let to_string_dummy expr = Format.asprintf "%a" fmt_phrase_dummy expr

module Select = struct
  type db = t

  type t = {
    tables : (string * string) list;
    cols : Column.t list;
    predicate : Phrase.t option;
  }

  let select t ~predicate =
    let predicate = Phrase.Option.combine_and t.predicate predicate in
    { t with predicate }

  let of_sort ~sort =
    let predicate = Sort.query sort in
    let cols = Sort.cols sort in
    let tables =
      List.map ~f:Column.table cols
      |> List.sort_uniq String.compare
      |> List.map ~f:(fun c -> (c, c))
    in
    { predicate; cols; tables }

  let fmt ~db f v =
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

  let execute query ~db ~field_types =
    let query = Format.asprintf "%a" (fmt ~db) query in
    Statistics.time_query (fun () -> db.execute_select query ~field_types)

  let query_exists query ~db =
    let sql = Format.asprintf "SELECT EXISTS (%a) AS t" (fmt ~db) query in
    let field_types = [ ("t", Phrase_type.Bool) ] in
    let res =
      Statistics.time_query (fun () -> db.execute_select sql ~field_types)
    in
    match res with
    | [ Phrase_value.Record [ (_, Phrase_value.Bool b) ] ] -> b
    | _ -> failwith "Expected singleton value."
end

module Delete = struct
  type db = t

  type t = { table : string; predicate : Phrase.t option }

  let fmt_table ~(db : db) f v = Format.fprintf f "%s" @@ db.quote_field v

  let fmt ~db f v =
    let map col = Format.sprintf "%s" (db.quote_field col) in
    match v.predicate with
    | None -> Format.fprintf f "DELETE FROM %a" (fmt_table ~db) v.table
    | Some pred ->
        Format.fprintf f "DELETE FROM %a WHERE %a" (fmt_table ~db) v.table
          (fmt_phrase ~db ~map) pred
end

module Update = struct
  type db = t

  type t = {
    table : string;
    predicate : Phrase.t option;
    set : (string * Phrase_value.t) list;
  }

  let fmt_set_value ~db f (key, value) =
    Format.fprintf f "%s = %a" (db.quote_field key) (fmt_phrase_value ~db) value

  let fmt_set_values ~db f vs = Format.pp_comma_list (fmt_set_value ~db) f vs

  let fmt_table ~db f v = Format.fprintf f "%s" @@ db.quote_field v

  let fmt ~db f v =
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

  type t = {
    table : string;
    columns : string list;
    values : Phrase_value.t list list;
    returning : string list;
  }

  let fmt_table ~db f v = Format.fprintf f "%s" @@ db.quote_field v

  let fmt_col ~db f v = Format.pp_print_string f (db.quote_field v)

  let fmt ~db f v =
    let fmt_vals f v =
      Format.fprintf f "(%a)" (fmt_phrase_value ~db |> Format.pp_comma_list) v
    in
    let fmt_returning f () =
      match v.returning with
      | [] -> ()
      | _ ->
          Format.fprintf f " RETURNING %a"
            (fmt_col ~db |> Format.pp_comma_list)
            v.returning
    in
    Format.fprintf f "INSERT INTO %a (%a) VALUES %a%a" (fmt_table ~db) v.table
      (fmt_col ~db |> Format.pp_comma_list)
      v.columns
      (fmt_vals |> Format.pp_comma_list)
      v.values fmt_returning ()

  let split v =
    let { table; columns; values; returning } = v in
    let f v =
      let values = [ v ] in
      { table; columns; values; returning }
    in
    List.map ~f values

  let no_returning v = { v with returning = [] }

  let exec_insert_returning_hack ~db data =
    let last_id_fun =
      let driver = db.driver_name () in
      match driver with
      | "mysql8" -> "last_insert_id()"
      | "sqlite3" -> "last_insert_rowid()"
      | _ ->
          let fn = "exec_insert_returning_hack" in
          E.Unsupported_for_driver { driver; fn } |> E.raise
    in
    let getid = Format.asprintf "SELECT %s as id" last_id_fun in
    let exec_insert v =
      let ins = Format.asprintf "%a" (fmt ~db) v in
      Debug.print ins;
      Statistics.time_query (fun () -> db.execute ins);
      Debug.print getid;
      Statistics.time_query (fun () ->
          db.execute_select getid ~field_types:[ ("id", Phrase_type.Serial) ])
    in
    no_returning data |> split |> List.map ~f:exec_insert |> List.concat

  let exec_insert_returning ~db ~field_types data =
    match db.driver_name () with
    | "sqlite3"
     |"mysql8" ->
        exec_insert_returning_hack ~db data
    | _ ->
        let cmd = Format.asprintf "%a" (fmt ~db) data in
        Debug.print cmd;
        Statistics.time_query (fun () -> db.execute_select ~field_types cmd)

  (* optionally add a returning statement if required *)
end

module Change = struct
  type db = t

  type t = Insert of Insert.t | Update of Update.t | Delete of Delete.t

  let fmt ~db f v =
    match v with
    | Insert v -> Insert.fmt ~db f v
    | Update v -> Update.fmt ~db f v
    | Delete v -> Delete.fmt ~db f v

  let exec ~db cmd =
    Debug.print cmd;
    Statistics.time_query (fun () -> db.execute cmd)

  let exec_multi_slow ~db data =
    let exec_cmd cmd =
      let cmd = Format.asprintf "%a" (fmt ~db) cmd in
      exec ~db cmd
    in
    List.iter ~f:exec_cmd data

  let exec_multi ~db data =
    match db.driver_name () with
    | "mysql8" -> exec_multi_slow ~db data
    | _ ->
        let fmt_cmd_sep f () = Format.pp_print_string f ";\n" in
        let fmt_cmd_list = Format.pp_print_list ~pp_sep:fmt_cmd_sep (fmt ~db) in
        let cmds = Format.asprintf "%a" fmt_cmd_list data in
        if String.equal "" cmds |> not then exec ~db cmds
end
