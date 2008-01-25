(** A generic interface for SQL-style databases. Vendor-specific implementations are elsewhere *)

class virtual db_args : string -> object 
  val strval : string
  method virtual from_string : string -> unit
end

val execute_command : (string -> Result.database -> Result.result)

(** [execute_select \[row1; row2; ...\] sql db] runs the query [sql]
    on the database [db] and interprets the results according to the
    field types of the [row]i. This should really take an alist of
    fieldname -> fieldtype. *)
val execute_select : ((string * Types.datatype) list -> string -> Result.database -> 
                        Result.result)

val execute_insert : (string * string list * string list list) ->  Result.database -> Result.result

val execute_insert_returning : (string * string list * string list list * string) ->  Result.database -> Result.result 
