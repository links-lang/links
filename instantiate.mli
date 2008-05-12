val show_recursion : bool Settings.setting

val var : Types.environment -> string -> (Types.type_arg list * Types.datatype)
val typ : Types.datatype -> (Types.type_arg list * Types.datatype)
val datatype : Types.datatype Utility.IntMap.t * Types.row_var Utility.IntMap.t -> Types.datatype -> Types.datatype
val alias : string -> Types.datatype list -> Types.tycon_environment -> Types.datatype
