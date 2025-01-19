open Postgresql
open Links_core
open Utility

(* Pg_database
   Implements the Value.database interface
   for Postgresql back ends *)

class otherfield (thing : Postgresql.ftype) : Value.otherfield =
object (_self)
  method show = match thing with
    | BOOL -> "bool"
    | BYTEA -> "bytea"
    | CHAR -> "char"
    | NAME -> "name"
    | INT8 -> "int8"
    | INT2 -> "int2"
    | INT2VECTOR -> "int2vector"
    | INT4 -> "int4"
    | REGPROC -> "regproc"
    | TEXT -> "text"
    | OID -> "oid"
    | TID -> "tid"
    | XID -> "xid"
    | CID -> "cid"
    | OIDVECTOR -> "oidvector"
    | POINT -> "point"
    | LSEG -> "lseg"
    | PATH -> "path"
    | BOX -> "box"
    | POLYGON -> "polygon"
    | LINE -> "line"
    | FLOAT4 -> "float4"
    | FLOAT8 -> "float8"
    | ABSTIME -> "abstime"
    | RELTIME -> "reltime"
    | TINTERVAL -> "tinterval"
    | UNKNOWN -> "unknown"
    | CIRCLE -> "circle"
    | CASH -> "cash"
    | MACADDR -> "macaddr"
    | INET -> "inet"
    | CIDR -> "cidr"
    | ACLITEM -> "aclitem"
    | BPCHAR -> "bpchar"
    | VARCHAR -> "varchar"
    | DATE -> "date"
    | TIME -> "time"
    | TIMESTAMP -> "timestamp"
    | TIMESTAMPTZ -> "timestamptz"
    | INTERVAL -> "interval"
    | TIMETZ -> "timetz"
    | BIT -> "bit"
    | VARBIT -> "varbit"
    | NUMERIC -> "numeric"
    | REFCURSOR -> "refcursor"
    | REGPROCEDURE -> "regprocedure"
    | REGOPER -> "regoper"
    | REGOPERATOR -> "regoperator"
    | REGCLASS -> "regclass"
    | REGTYPE -> "regtype"
    | RECORD -> "record"
    | CSTRING -> "cstring"
    | ANY -> "any"
    | ANYARRAY -> "anyarray"
    | VOID -> "void"
    | TRIGGER -> "trigger"
    | LANGUAGE_HANDLER -> "language_handler"
    | INTERNAL -> "internal"
    | OPAQUE -> "opaque"
    | ANYELEMENT -> "anyelement"
    | JSON -> "json"
    | JSONB -> "jsonb"
end

class pg_dbresult (pgresult:Postgresql.result) = object
  inherit Value.dbvalue
  val original = pgresult
  method status : Value.db_status = match original#status with
      Command_ok
    | Tuples_ok -> `QueryOk
    | Single_tuple    -> `QueryError ("Single tuple repsonse not handled")
    | Empty_query     -> `QueryError ("String sent to the backend was empty")
    | Copy_out        -> `QueryError ("Copy Out (from server) data transfer started")
    | Copy_in         -> `QueryError ("Copy In (to server) data transfer started")
    | Copy_both       -> `QueryError ("Copy Both data transfer started")
    | Bad_response    -> `QueryError ("Bad_response : The server's response was not understood")
    | Nonfatal_error  -> `QueryError ("Nonfatal_error : The server's response was not understood")
    | Fatal_error     -> `QueryError ("Fatal_error : The server's response was not understood (" ^ original#error ^ ")")

  method nfields : int = original#nfields
  method ntuples : int = original#ntuples
  method fname : int -> string = original#fname
  (*TODO: better handling of NULLs *)
  method getvalue : int -> int -> string = pgresult#getvalue
  method gettuple : int -> string array = pgresult#get_tuple
  method error : string = original#error
end

let pg_printer = object(self)
  inherit Sql.printer as super

  method quote_field x =
    "\"" ^ Str.global_replace (Str.regexp "\"") "\"\"" x ^ "\""

  method! pp_insert ppf table_name field_names body =
    let open Sql in
    let quoted_field_names = (List.map self#quote_field field_names) in
    match body with
        | Values vss ->
            let vss = List.map (List.map (self#string_of_base false)) vss in
            let body =
              match field_names, vss with
                | _, [] -> failwith("We should not even generate code for empty inserts.")
                | [],    [_] ->
                    (* HACK:

                       PostgreSQL doesn't allow an empty tuple of columns to
                       be specified for an insert. *)
                    " default values"
                | [],    _::_::_ ->
                    (* In order to handle this case we need support for the
                       standard mult-row insert syntax (Postgres version 8.2
                       and later), and we will need access to the type of the
                       table. In fact, we only really need the name of one of
                       the columns, c. Then we can do:

                       insert into table(c) values (c),...,(c)
                    *)
                    failwith("Unable to translate a multi-row insert with empty rows to PostgreSQL")
                | _::_,  _ ->
                   let values =
                     String.concat "), (" (List.map (String.concat ",") vss)
                   in "(" ^ String.concat "," quoted_field_names ^") VALUES (" ^ values ^ ")"
            in
            Format.fprintf ppf
              "insert into %s %s" table_name body
        | TableQuery var ->
            let open Format in
            fprintf ppf
              "insert into %s (%a) (select * from %s)"
              table_name
              (pp_print_list
                ~pp_sep:(fun ppf () -> pp_print_string ppf ",") pp_print_string) quoted_field_names
              (string_of_table_var var)

    method! pp_base one_table ppf = function
      | Sql.Constant (CommonTypes.Constant.DateTime (CommonTypes.Timestamp.Infinity)) ->
          Format.pp_print_string ppf "'infinity' :: timestamp with time zone"
      | Sql.Constant (CommonTypes.Constant.DateTime (CommonTypes.Timestamp.MinusInfinity)) ->
          Format.pp_print_string ppf "'-infinity' :: timestamp with time zone"
      | b -> super#pp_base one_table ppf b

end

class pg_database host port dbname user password = object(self)
  inherit Value.database pg_printer

  val connection =
    try
      new connection ~host:host ~port:port ~dbname:dbname
                     ~user:user ~password:password ()
    with
        Postgresql.Error msg ->
          failwith("PostgreSQL returned error: " ^Postgresql.string_of_error msg)
  method driver_name () = "postgresql"
  method exec : string -> Value.dbvalue = fun query ->
    Debug.debug_time "db#exec" (fun () ->
      try
      let raw_result = connection#exec query in
	new pg_dbresult raw_result
    with
        Postgresql.Error msg ->
          failwith("PostgreSQL returned error: " ^Postgresql.string_of_error msg)
	    )
  method escape_string s =
    connection#escape_string s

  method! make_insert_returning_query
      : string -> Sql.query -> string list =
    fun returning q ->
      assert (match q with | Sql.Insert _ -> true | _ -> false);
      [Printf.sprintf "%s returning %s" (self#string_of_query q) returning]

  method supports_shredding () = true
end

let driver_name = "postgresql"

let get_pg_database_by_string args =
  match Utility.split_string args ':' with
    | (name::host::port::user::pass::_) ->
       (* If "user" field was left empty then get the name of user running the
          process.  This has to be done by acquiring UID, finding corresponding
          entry in passwd table and reading user's login name. *)
       let user = if user = ""
                  then let open Unix in (getpwuid (getuid ())).pw_name
                  else user in
        (new pg_database host port name user pass,
         Value.reconstruct_db_string (driver_name, args))
    | _ ->
        failwith "Insufficient arguments when establishing postgresql connection"

let _ = Value.register_driver (driver_name, get_pg_database_by_string)

