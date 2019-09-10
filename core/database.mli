(** A generic interface for SQL-style databases. Vendor-specific implementations are elsewhere *)

val connection_info : string option Settings.setting
val relax_query_type_constraint : bool Settings.setting
val shredding : bool Settings.setting

class virtual db_args : string -> object
  val strval : string
  method virtual from_string : string -> unit
end

val value_of_db_string : string -> Types.datatype -> Value.t

val execute_command : (string -> Value.database -> Value.t)

(* Builds raw dbvalue with mapping from names to types and positions *)
val execute_select_result : (string * Types.datatype) list -> string -> Value.database -> Value.dbvalue * (string * (Types.datatype * int)) list

val build_result : Value.dbvalue * (string * (Types.datatype * int)) list -> Value.t

(** [execute_select \[row1; row2; ...\] sql db] runs the query [sql]
    on the database [db] and interprets the results according to the
    field types of the [row]i. This should really take an alist of
    fieldname -> fieldtype. *)
val execute_select : (string * Types.datatype) list -> string -> Value.database -> Value.t

val execute_untyped_select : string -> Value.database -> Value.t

val execute_insert : (string * string list * string list list) ->  Value.database -> Value.t

val execute_insert_returning : (string * string list * string list list * string) ->  Value.database -> Value.t

