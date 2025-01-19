open Links_core
open Utility
module S3 = Sqlite3
module Rc = Sqlite3.Rc
module Data = Sqlite3.Data

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


(* TODO: Better NULL handling *)
let data_to_string data =
  match data with
    Data.NONE -> ""
  | Data.NULL -> ""
  | Data.INT i -> Int64.to_string i
  | Data.FLOAT f -> string_of_float' f
  | Data.TEXT s | Data.BLOB s -> s
;;



class lite3_result (stmt: S3.stmt) = object
  inherit Value.dbvalue

  val result_buf_and_status =
    let result_buf = PolyBuffer.init 1 1024 [] in
    let rec get_results (status) =
      match status with
        `QueryOk -> (
          match S3.step stmt with
            Rc.OK|Rc.ROW ->
            let data = Array.to_list (S3.row_data stmt) in
            let row = List.map data_to_string data in
            PolyBuffer.append result_buf row;
            get_results `QueryOk
          | Rc.DONE ->
            `QueryOk
          | e -> `QueryError (error_as_string e)
        )
      | _ -> (status)
    in
    (result_buf,get_results (`QueryOk))

  method status : Value.db_status = snd(result_buf_and_status)
  method nfields : int = S3.column_count stmt
  method ntuples : int = PolyBuffer.length (fst result_buf_and_status)
  method fname n : string = S3.column_name stmt n
  method getvalue : int -> int -> string = fun n i ->
    List.nth(PolyBuffer.get (fst result_buf_and_status) n) i
  method gettuple : int -> string array = fun n ->
    Array.of_list(PolyBuffer.get (fst result_buf_and_status) n)
  method error : string =
    match (snd result_buf_and_status) with
      `QueryError(msg) -> msg
    | `QueryOk -> "OK"
end

let lite3_escape_string = Str.global_replace (Str.regexp_string "'") "''"
let lite3_quote s = "\"" ^ lite3_escape_string s ^ "\""

class lite3_database file = object(self)
  inherit Value.database (Sql.default_printer lite3_quote)
  val mutable _supports_shredding : bool option = None
  val connection = S3.db_open file
  method exec query : Value.dbvalue =
    Debug.print query;
    let stmt = S3.prepare connection query in
    (* Fully prepare statement to deal correctly with multi-statement execution *)
    let rec last_res r =
      match S3.prepare_tail r with
      | None -> r
      | Some rnew ->
        let _ = new lite3_result r in
        last_res rnew in
    let stmt = last_res stmt in
      new lite3_result stmt
  (* See http://www.sqlite.org/lang_expr.html *)
  method escape_string = lite3_escape_string
  method driver_name () = "sqlite3"
  method supports_shredding () =
    match _supports_shredding with
    | Some result -> result
    | None ->
       let stmt = S3.prepare connection "SELECT sqlite_version()" in
       let result = new lite3_result stmt in
       match result#status with
       | `QueryOk ->
          let version = result#getvalue 0 0 in
          let is_number str =
            Str.string_match (Str.regexp "^[0-9]+$") str 0
          in
          begin match Str.split (Str.regexp "[0-9]\\.") version with
          | major :: minor :: _ when is_number major && is_number minor ->
             let nmajor = int_of_string major in
             let nminor = int_of_string minor in
             if nmajor >= 3 && nminor >= 25 then
               (_supports_shredding <- Some true; true)
             else
               (_supports_shredding <- Some false; false)
          | _ -> _supports_shredding <- Some false; false
          end
       | _ -> false
  method! make_insert_returning_query : string -> Sql.query -> string list =
    fun returning q ->
      match q with
        Sql.Insert ins ->
          [self#string_of_query q;
           Printf.sprintf "select %s from %s where rowid = last_insert_rowid()" returning ins.ins_table]
      | _ -> assert false

end

let driver_name = "sqlite3"
let _ = Value.register_driver (driver_name,
			       fun args ->
			         new lite3_database args,
			         Value.reconstruct_db_string (driver_name, args))
