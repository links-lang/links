module Mysql = Mysql8
open Mysql
open Links_core
open Utility

let string_of_error_code = function
| Aborting_connection              -> "Aborting connection"
| Access_denied_error              -> "Access denied error"
| Alter_info                       -> "Alter info"
| Bad_db_error                     -> "Bad db error"
| Bad_field_error                  -> "Bad field error"
| Bad_host_error                   -> "Bad host error"
| Bad_null_error                   -> "Bad null error"
| Bad_table_error                  -> "Bad table error"
| Blob_cant_have_default           -> "Blob can't have default"
| Blob_key_without_length          -> "Blob key without length"
| Blob_used_as_key                 -> "Blob used as key"
| Blobs_and_no_terminated          -> "Blobs and no terminated"
| Cant_create_db                   -> "Can't create db"
| Cant_create_file                 -> "Can't create file"
| Cant_create_table                -> "Can't create table"
| Cant_create_thread               -> "Can't create thread"
| Cant_delete_file                 -> "Can't delete file"
| Cant_drop_field_or_key           -> "Can't drop field or key"
| Cant_find_dl_entry               -> "Can't find dl entry"
| Cant_find_system_rec             -> "Can't find system rec"
| Cant_find_udf                    -> "Can't find udf"
| Cant_get_stat                    -> "Can't get stat"
| Cant_get_wd                      -> "Can't get wd"
| Cant_initialize_udf              -> "Can't initialize udf"
| Cant_lock                        -> "Can't lock"
| Cant_open_file                   -> "Can't open file"
| Cant_open_library                -> "Can't open library"
| Cant_read_charset                -> "Can't read charset"
| Cant_read_dir                    -> "Can't read dir"
| Cant_remove_all_fields           -> "Can't remove all fields"
| Cant_reopen_table                -> "Can't reopen table"
| Cant_set_wd                      -> "Can't set wd"
| Checkread                        -> "Checkread"
| Columnaccess_denied_error        -> "Columnaccess denied error"
| Commands_out_of_sync             -> "Commands out of sync"
| Con_count_error                  -> "Con count error"
| Conn_host_error                  -> "Conn host error"
| Connection_error                 -> "Connection error"
| Db_create_exists                 -> "Db create exists"
| Db_drop_delete                   -> "Db drop delete"
| Db_drop_exists                   -> "Db drop exists"
| Db_drop_rmdir                    -> "Db drop rmdir"
| Dbaccess_denied_error            -> "Dbaccess denied error"
| Delayed_cant_change_lock         -> "Delayed can't change lock"
| Delayed_insert_table_locked      -> "Delayed insert table locked"
| Disk_full                        -> "Disk full"
| Dup_entry                        -> "Dup entry"
| Dup_fieldname                    -> "Dup fieldname"
| Dup_key                          -> "Dup key"
| Dup_keyname                      -> "Dup keyname"
| Dup_unique                       -> "Dup unique"
| Empty_query                      -> "Empty query"
| Error_on_close                   -> "Error on close"
| Error_on_read                    -> "Error on read"
| Error_on_rename                  -> "Error on rename"
| Error_on_write                   -> "Error on write"
| Field_specified_twice            -> "Field specified twice"
| File_exists_error                -> "File exists error"
| File_not_found                   -> "File not found"
| File_used                        -> "File used"
| Filsort_abort                    -> "Filsort abort"
| Forcing_close                    -> "Forcing close"
| Form_not_found                   -> "Form not found"
| Function_not_defined             -> "Function not defined"
| Get_errno                        -> "Get errno"
| Got_signal                       -> "Got signal"
| Grant_wrong_host_or_user         -> "Grant wrong host or user"
| Handshake_error                  -> "Handshake error"
| Hashchk                          -> "Hashchk"
| Host_is_blocked                  -> "Host is blocked"
| Host_not_privileged              -> "Host not privileged"
| Illegal_grant_for_table          -> "Illegal grant for table"
| Illegal_ha                       -> "Illegal ha"
| Insert_info                      -> "Insert info"
| Insert_table_used                -> "Insert table used"
| Invalid_default                  -> "Invalid default"
| Invalid_group_func_use           -> "Invalid group func use"
| Invalid_use_of_null              -> "Invalid use of null"
| Ipsock_error                     -> "Ipsock error"
| Key_column_does_not_exits        -> "Key column does not exits"
| Key_not_found                    -> "Key not found"
| Kill_denied_error                -> "Kill denied error"
| Load_info                        -> "Load info"
| Localhost_connection             -> "Localhost connection"
| Mix_of_group_func_and_fields     -> "Mix of group func and fields"
| Multiple_pri_key                 -> "Multiple pri key"
| Namedpipe_connection             -> "Namedpipe connection"
| Namedpipeopen_error              -> "Namedpipeopen error"
| Namedpipesetstate_error          -> "Namedpipesetstate error"
| Namedpipewait_error              -> "Namedpipewait error"
| Net_error_on_write               -> "Net error on write"
| Net_fcntl_error                  -> "Net fcntl error"
| Net_packet_too_large             -> "Net packet too large"
| Net_packets_out_of_order         -> "Net packets out of order"
| Net_read_error                   -> "Net read error"
| Net_read_error_from_pipe         -> "Net read error from pipe"
| Net_read_interrupted             -> "Net read interrupted"
| Net_uncompress_error             -> "Net uncompress error"
| Net_write_interrupted            -> "Net write interrupted"
| Nisamchk                         -> "Nisamchk"
| No                               -> "No"
| No_db_error                      -> "No db error"
| No_raid_compiled                 -> "No raid compiled"
| No_such_index                    -> "No such index"
| No_such_table                    -> "No such table"
| No_such_thread                   -> "No such thread"
| No_tables_used                   -> "No tables used"
| No_unique_logfile                -> "No unique logfile"
| Non_uniq_error                   -> "Non uniq error"
| Nonexisting_grant                -> "Nonexisting grant"
| Nonexisting_table_grant          -> "Nonexisting table grant"
| Nonuniq_table                    -> "Nonuniq table"
| Normal_shutdown                  -> "Normal shutdown"
| Not_allowed_command              -> "Not allowed command"
| Not_form_file                    -> "Not form file"
| Not_keyfile                      -> "Not keyfile"
| Null_column_in_index             -> "Null column in index"
| Old_keyfile                      -> "Old keyfile"
| Open_as_readonly                 -> "Open as readonly"
| Out_of_memory                    -> "Out of memory"
| Out_of_resources                 -> "Out of resources"
| Out_of_sortmemory                -> "Out of sortmemory"
| Outofmemory                      -> "Outofmemory"
| Parse_error                      -> "Parse error"
| Password_anonymous_user          -> "Password anonymous user"
| Password_no_match                -> "Password no match"
| Password_not_allowed             -> "Password not allowed"
| Primary_cant_have_null           -> "Primary can't have null"
| Ready                            -> "Ready"
| Record_file_full                 -> "Record file full"
| Regexp_error                     -> "Regexp error"
| Requires_primary_key             -> "Requires primary key"
| Server_gone_error                -> "Server gone error"
| Server_handshake_err             -> "Server handshake err"
| Server_lost                      -> "Server lost"
| Server_shutdown                  -> "Server shutdown"
| Shutdown_complete                -> "Shutdown complete"
| Socket_create_error              -> "Socket create error"
| Stack_overrun                    -> "Stack overrun"
| Syntax_error                     -> "Syntax error"
| Table_cant_handle_auto_increment -> "Table can't handle auto increment"
| Table_cant_handle_blob           -> "Table can't handle blob"
| Table_exists_error               -> "Table exists error"
| Table_must_have_columns          -> "Table must have columns"
| Table_not_locked                 -> "Table not locked"
| Table_not_locked_for_write       -> "Table not locked for write"
| Tableaccess_denied_error         -> "Tableaccess denied error"
| Tcp_connection                   -> "Tcp connection"
| Textfile_not_readable            -> "Textfile not readable"
| Too_big_fieldlength              -> "Too big fieldlength"
| Too_big_rowsize                  -> "Too big rowsize"
| Too_big_select                   -> "Too big select"
| Too_big_set                      -> "Too big set"
| Too_long_ident                   -> "Too long ident"
| Too_long_key                     -> "Too long key"
| Too_long_string                  -> "Too long string"
| Too_many_delayed_threads         -> "Too many delayed threads"
| Too_many_fields                  -> "Too many fields"
| Too_many_key_parts               -> "Too many key parts"
| Too_many_keys                    -> "Too many keys"
| Too_many_rows                    -> "Too many rows"
| Too_many_tables                  -> "Too many tables"
| Udf_exists                       -> "Udf exists"
| Udf_no_paths                     -> "Udf no paths"
| Unexpected_eof                   -> "Unexpected eof"
| Unknown_character_set            -> "Unknown character set"
| Unknown_com_error                -> "Unknown com error"
| Unknown_error                    -> "Unknown error"
| Unknown_host                     -> "Unknown host"
| Unknown_procedure                -> "Unknown procedure"
| Unknown_table                    -> "Unknown table"
| Unsupported_extension            -> "Unsupported extension"
| Update_info                      -> "Update info"
| Update_without_key_in_safe_mode  -> "Update without key in safe mode"
| Version_error                    -> "Version error"
| Wrong_auto_key                   -> "Wrong auto key"
| Wrong_column_name                -> "Wrong column name"
| Wrong_db_name                    -> "Wrong db name"
| Wrong_field_spec                 -> "Wrong field spec"
| Wrong_field_terminators          -> "Wrong field terminators"
| Wrong_field_with_group           -> "Wrong field with group"
| Wrong_group_field                -> "Wrong group field"
| Wrong_host_info                  -> "Wrong host info"
| Wrong_key_column                 -> "Wrong key column"
| Wrong_mrg_table                  -> "Wrong mrg table"
| Wrong_outer_join                 -> "Wrong outer join"
| Wrong_paramcount_to_procedure    -> "Wrong paramcount to procedure"
| Wrong_parameters_to_procedure    -> "Wrong parameters to procedure"
| Wrong_sub_key                    -> "Wrong sub key"
| Wrong_sum_select                 -> "Wrong sum select"
| Wrong_table_name                 -> "Wrong table name"
| Wrong_value_count                -> "Wrong value count"
| Wrong_value_count_on_row         -> "Wrong value count on row"
| Yes                              -> "Yes"

class otherfield (thing : Mysql.dbty) : Value.otherfield =
object
  method show = pretty_type thing
end

let iterUntilNone (fn : unit -> 'b option) (g : 'b -> unit) : unit =
  let rec iterate () =
    match fn () with
      | None -> ()
      | Some value -> g value; iterate()
  in
    iterate ()

class mysql_result (result : result) db = object
  inherit Value.dbvalue
  val result_buf =
    if size result > Int64.of_int(0)
    then let buf = PolyBuffer.init 1 1024 (Array.init 0 (fun _ -> None)) in
         iterUntilNone (fun () -> fetch result) (PolyBuffer.append buf);
         buf
    else  PolyBuffer.init 0 1 (Array.init 0 (fun _ -> None))
  method status : Value.db_status =
    match status db with
      | StatusOK | StatusEmpty -> `QueryOk
      | StatusError c          -> `QueryError (string_of_error_code c)
  method nfields : int =
    fields result
  method ntuples : int =
    Int64.to_int(size result)
  method fname  n : string =
    (Utility.val_of (fetch_field_dir result n)).name
  method getvalue : int -> int -> string = fun n f ->
    let row = PolyBuffer.get result_buf n in
(* TODO: Handle nulls better *)
    Utility.from_option "" (row.(f))
  method gettuple : int -> string array = fun n ->
    let row = PolyBuffer.get result_buf n in
    Array.map (Utility.from_option "") row
  method error : string =
    Utility.val_of (errmsg db)
end

let mysql_printer = object (self)
  inherit Sql.printer as super

  method quote_field x =
    "`" ^ Str.global_replace (Str.regexp "`") "``" x ^ "`"

  (* Infix concatenation is not supported in MySQL; ensure that Links ^^
   * is translated to MySQL concat(-, -) *)
  method! pp_sql_arithmetic ppf one_table (l, op, r) =
    let pr_b_one_table = self#pp_base one_table in
    match op with
      | "^^" ->
          Format.fprintf ppf "concat(%a, %a)"
            pr_b_one_table l
            pr_b_one_table r
      | x -> super#pp_sql_arithmetic ppf one_table (l, x, r)
end

class mysql_database spec = object(self)
  inherit Value.database mysql_printer
  val connection = connect spec
  method driver_name () = "mysql8"
  method exec query : Value.dbvalue =
    try
      new mysql_result (exec connection query) connection
    with
        Mysql.Error msg ->
          failwith("Mysql returned error: " ^ msg)

  method escape_string = Mysql.escape

  method! make_insert_returning_query : string -> Sql.query -> string list =
    fun returning q ->
      match q with
        Sql.Insert ins ->
          [self#string_of_query q;
           Printf.sprintf "select %s from %s where _rowid = last_insert_id()" returning ins.ins_table]
      | _ -> assert false

  method supports_shredding () = false
end

let parse_args (args : string) : db =
  match Utility.split_string args ':' with
    | (name::host::port::user::pass::_) ->
       (* If "user" field was left empty then get the name of user running the
          process.  This has to be done by acquiring UID, finding corresponding
          entry in passwd table and reading user's login name. *)
	let user = if user = ""
                   then let open Unix in (getpwuid (getuid ())).pw_name
                   else user in
        (try
          {
            name = Some name;
            host = Some host;
            port = Some (int_of_string port);
            user = Some user;
            pwd  = Some pass;
            socket = None;
          }
         with Failure msg ->
           failwith ("[" ^ msg ^ "] Couldn't parse mysql port number : " ^ port))
    | _ -> failwith "Insufficient arguments when establishing mysql connection"

let driver_name = "mysql8"
let _ = Value.register_driver (driver_name, fun args -> new mysql_database (parse_args args), Value.reconstruct_db_string (driver_name, args))
