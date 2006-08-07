(** A generic interface for SQL-style databases. Vendor-specific implementations are elsewhere *)

class virtual db_args : string -> object 
  val strval : string
  method virtual from_string : string -> unit
end

val execute_select : (Types.datatype -> string -> Result.database -> Result.result)
