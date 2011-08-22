open Postgresql
open Utility

(* Pg_database 
   Implements the Value.database interface
   for Postgresql back ends *)

class otherfield (thing : Postgresql.ftype) : Value.otherfield =
object (self)
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
end

class pg_dbresult (pgresult:Postgresql.result) = object
  inherit Value.dbvalue
  val original = pgresult
  method status : Value.db_status = match original#status with
      Command_ok 
    | Tuples_ok -> `QueryOk
        
    | Empty_query     -> `QueryError ("String sent to the backend was empty"); 
    | Command_ok      -> `QueryError ("Successful completion of a command returning no data"); 
    | Tuples_ok       -> `QueryError ("The query successfully executed")
    | Copy_out        -> `QueryError ("Copy Out (from server) data transfer started")
    | Copy_in         -> `QueryError ("Copy In (to server) data transfer started")
    | Bad_response    -> `QueryError ("Bad_response : The server's response was not understood")
    | Nonfatal_error  -> `QueryError ("Nonfatal_error : The server's response was not understood")
    | Fatal_error     -> `QueryError ("Fatal_error : The server's response was not understood (" ^ original#error ^ ")")
        
  method nfields : int = original#nfields
  method fname : int -> string = original#fname
  method get_all_lst : string list list = pgresult#get_all_lst
  method error : string = original#error
end

class pg_database host port dbname user password = object(self)
  inherit Value.database

  val connection =
    try
      new connection ~host:host ~port:port ~dbname:dbname
                     ~user:user ~password:password ()
    with
        Postgresql.Error msg ->
          failwith("PostgreSQL returned error: " ^Postgresql.string_of_error msg)
  method driver_name () = "postgresql"
  method exec : string -> Value.dbvalue = fun query ->
    try
      let raw_result = connection#exec query in
        new pg_dbresult raw_result
    with
        Postgresql.Error msg ->
          failwith("PostgreSQL returned error: " ^Postgresql.string_of_error msg)
  method escape_string s = connection#escape_string s

(* jcheney: Added quoting to avoid problems with mysql keywords. *)
  method make_insert_query (table_name, field_names, vss) =
    let insert_table = "insert into " ^ table_name in
    let quoted_field_names = (List.map (fun x -> "\"" ^ x ^ "\"") field_names) in
    let body, vss =
      match field_names with
        | [],    [_] ->
            (* HACK:
               
               PostgreSQL doesn't allow an empty tuple of columns to
               be specified for an insert. *)
            " default values"
        | [],    _::_::_ ->
            failwith("Unable to translate a multi-row insert with empty rows to PostgreSQL")
        | _::_,  _ ->
            (* HACK:
               
               This translation is compatible with PostgreSQL versions
               prior to 8.2, but perhaps we should now switch to the
               more idiomatic multi-row insert supported by version
               8.2 and later. *)
            "("^String.concat "," quoted_field_names ^") "^
              String.concat " union all " (List.map (fun vs -> "select " ^ 
                                                       String.concat "," vs) vss)
        | 
    in
      insert_table ^ body

  method make_insert_returning_query 
      : (string * string list * string list list * string) -> string list =
    fun (table_name, field_names, vss, returning) ->
      [self#make_insert_query(table_name,
                              field_names,
                              vss) ^ " returning " ^ "\"" ^ returning ^ "\""]
end

let driver_name = "postgresql"

let get_pg_database_by_string args =
  match Utility.split_string args ':' with
    | (name::host::port::user::pass::_) ->
        (new pg_database host port name user pass, 
         Value.reconstruct_db_string (driver_name, args))
    | _ ->
        failwith "Insufficient arguments when establishing postgresql connection"

let _ = Value.register_driver (driver_name, get_pg_database_by_string)
