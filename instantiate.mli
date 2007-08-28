val show_recursion : bool Settings.setting

val var : Types.environment -> string -> Types.datatype
val datatype : Types.datatype Utility.IntMap.t * Types.row_var Utility.IntMap.t -> Types.datatype -> Types.datatype
val alias : Types.assumption -> Types.datatype list -> Types.datatype
