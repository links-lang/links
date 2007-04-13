val show_recursion : bool Settings.setting

val instantiate_datatype : Types.datatype Utility.IntMap.t * Types.row_var Utility.IntMap.t -> Types.datatype -> Types.datatype
val instantiate_alias : Types.assumption -> Types.datatype list -> Types.datatype

