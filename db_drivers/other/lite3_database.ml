open Utility
open Sqlite3

(* TODO: Better type/error handling *)

let error_as_string = function
| Rc.OK -> "ok"
| Rc.ERROR -> "error"
| Rc.INTERNAL -> "internal"
| Rc.PERM -> "perm"
| Rc.ABORT -> "abort"
| Rc.BUSY -> "busy"
| Rc.LOCKED -> "locked"
| Rc.NOMEM -> "nomem"
| Rc.READONLY -> "readonly"
| Rc.INTERRUPT -> "interrupt"
| Rc.IOERR -> "ioerr"
| Rc.CORRUPT -> "corrupt"
| Rc.NOTFOUND -> "notfound"
| Rc.FULL -> "full"
| Rc.CANTOPEN -> "cantopen"
| Rc.PROTOCOL -> "protocol"
| Rc.EMPTY -> "empty"
| Rc.SCHEMA -> "schema"
| Rc.TOOBIG -> "toobig"
| Rc.CONSTRAINT -> "constraint"
| Rc.MISMATCH -> "mismatch"
| Rc.MISUSE -> "misuse"
| Rc.NOFLS -> "nofls"
| Rc.AUTH -> "auth"
| Rc.FORMAT -> "format"
| Rc.RANGE -> "range"
| Rc.NOTADB -> "notadb"
| Rc.ROW -> "row"
| Rc.DONE -> "done"
| Rc.UNKNOWN e -> "unknown: "^ string_of_int (Rc.int_of_unknown e)

let data_to_string data =
  match data with
    Data.NONE -> ""
  | Data.NULL -> ""
  | Data.INT i -> Int64.to_string i
  | Data.FLOAT f -> string_of_float' f
  | Data.TEXT s | Data.BLOB s -> s
;;

class lite3_result (stmt: stmt) = object
  inherit Value.dbvalue
  val result_list_and_status =
    let rec get_results (results,status) =
      match status with
        `QueryOk -> (
          match step stmt with
            Rc.OK|Rc.ROW ->
            let data = Array.to_list (row_data stmt) in
            let row = List.map data_to_string data in
            get_results (row::results,`QueryOk )
          | Rc.DONE ->
            results,`QueryOk
          | e -> results, `QueryError (error_as_string e)
        )
      | _ -> (results,status)
    in
    get_results ([],`QueryOk)

  method status : Value.db_status = snd(result_list_and_status)
  method nfields : int =  column_count stmt
  method ntuples : int = List.length (fst result_list_and_status)
  method fname n : string = column_name stmt n
  method get_all_lst : string list list = fst(result_list_and_status)
  method getvalue : int -> int -> string = fun n i ->
    List.nth(List.nth (fst(result_list_and_status)) n) i
  method gettuple : int -> string array = fun n ->
    Array.of_list(List.nth (fst(result_list_and_status)) n)
  method error : string =
    match snd(result_list_and_status) with
      `QueryError(msg) -> msg
    | `QueryOk -> "OK"
end


class lite3_database file = object(self)
  inherit Value.database
  val connection = db_open file
  method exec query : Value.dbvalue =
    let stmt = prepare connection query in
      new lite3_result stmt
  (* See http://www.sqlite.org/lang_expr.html *)
  method escape_string = Str.global_replace (Str.regexp_string "'") "''"
  method quote_field = self#escape_string
  method driver_name () = "sqlite3"
end

let driver_name = "sqlite3"
let _ = Value.register_driver (driver_name,
			       fun args ->
			         new lite3_database args,
			         Value.reconstruct_db_string (driver_name, args))
