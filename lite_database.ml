open Sqlite

let error_as_string = function
| RC_ok -> "ok"
| RC_error -> "error"
| RC_internal -> "internal"
| RC_perm -> "perm"
| RC_abort -> "abort"
| RC_busy -> "busy"
| RC_locked -> "locked"
| RC_nomem -> "nomem"
| RC_readonly -> "readonly"
| RC_interrupt -> "interrupt"
| RC_ioerr -> "ioerr"
| RC_corrupt -> "corrupt"
| RC_notfound -> "notfound"
| RC_full -> "full"
| RC_cantopen -> "cantopen"
| RC_protocol -> "protocol"
| RC_empty -> "empty"
| RC_schema -> "schema"
| RC_toobig -> "toobig"
| RC_constraint -> "constraint"
| RC_mismatch -> "mismatch"
| RC_misuse -> "misuse"
| RC_nofls -> "nofls"
| RC_auth -> "auth"
| RC_format -> "format"

class lite_result (vm: vm) = object
  inherit Value.dbvalue
  val result_list =
    (* step_simple must be called before any of the methods, otherwise
       column_names, column_types, etc. will fail *)
    let rec collect_results results =
      try
        collect_results (Array.to_list (step_simple vm) :: results) 
      with Sqlite_done -> results
    in List.rev (collect_results [])
  method status : Value.db_status = 
    match vm_rc vm with
      | RC_ok -> `QueryOk
      | e     -> `QueryError (error_as_string e)
  method nfields : int = 
    match result_list with 
      | [] -> -1
      | _  -> List.length (List.hd result_list)
  method fname  n : string = 
    Array.get (column_names vm) n
  method get_all_lst : string list list =
    result_list
  method error : string = 
    error_as_string (vm_rc vm)
end


class lite_database file = object(self)
  inherit Value.database
  val connection = db_open file
  method driver_name () = "sqlite"
  method exec query : Value.dbvalue =
    let vm, _, _ = compile connection query 0 true in
      new lite_result vm
  (* See http://www.sqlite.org/lang_expr.html *)
  method escape_string = Str.global_replace (Str.regexp_string "'") "''"
  method quote_field = self#escape_string
end

let driver_name = "sqlite"
let _ = Value.register_driver (driver_name, fun args -> new lite_database args, Value.reconstruct_db_string (driver_name, args))
