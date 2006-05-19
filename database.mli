class virtual db_args : string -> object 
  val strval : string
  method virtual from_string : string -> unit
end

val execute_select : (Types.kind -> string -> Result.database -> Result.result)
