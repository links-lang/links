open Postgresql
open Sl_utility
open Sl_result

(* Pg_database 
   Implements the Sl_result.database interface
   for Postgresql back ends *)

class otherfield (thing : Postgresql.ftype) : Sl_result.otherfield =
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
  inherit dbresult
  val original = pgresult
  method status : db_status = match original#status with
                                Command_ok 
    | Tuples_ok -> QueryOk

  | Empty_query     -> debug ("String sent to the backend was empty"); QueryError
  | Command_ok      -> debug ("Successful completion of a command returning no data"); QueryError
  | Tuples_ok       -> debug ("The query successfully executed"); QueryError
  | Copy_out        -> debug ("Copy Out (from server) data transfer started"); QueryError
  | Copy_in         -> debug ("Copy In (to server) data transfer started"); QueryError
  | Bad_response    -> debug ("Bad_response : The server's response was not understood"); QueryError
  | Nonfatal_error    -> debug ("Nonfatal_error : The server's response was not understood"); QueryError
  | Fatal_error    -> debug ("Fatal_error : The server's response was not understood (" ^ original#error ^ ")"); QueryError


  method nfields : int = original#nfields
  method fname : int -> string = original#fname
  method ftype : int -> db_field_type = 
    fun n -> match original#ftype n with
    | BOOL -> BoolField
    | BYTEA | CHAR | TEXT | NAME | VARCHAR -> TextField
    | INT8 | INT2 | INT4 -> IntField
    | NUMERIC | FLOAT4 | FLOAT8 -> FloatField
(*    | fieldtype -> SpecialField (new otherfield fieldtype)*)
    | _ -> TextField
  method get_all_lst : string list list = pgresult#get_all_lst
  method error : string = original#error
end

class pg_database host port dbname user password = object(self)
  inherit database
  val connection = new connection ~host:host ~port:port ~dbname:dbname ~user:user ~password:password ()
  method exec : string -> dbresult = fun query ->
    let raw_result = connection#exec query in
    new pg_dbresult raw_result
end
let driver_name = "postgresql"
    
let get_pg_database_by_string args =
  match Sl_utility.split_string args ':' with
    | (name::host::port::user::pass::others) ->
        (new pg_database host port name user pass, reconstruct_db_string (driver_name, args))
    | _ -> failwith "Insufficient arguments when establishing postgresql connection"

let _ = register_driver (driver_name, get_pg_database_by_string)
