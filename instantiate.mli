exception ArityMismatch

val show_recursion : bool Settings.setting
val quantified_instantiation : bool Settings.setting

val var : Types.environment -> string -> (Types.type_arg list * Types.datatype)
val rigid : Types.environment -> string -> (Types.type_arg list * Types.datatype)
val typ : Types.datatype -> (Types.type_arg list * Types.datatype)
val typ_rigid : Types.datatype -> (Types.type_arg list * Types.datatype)
val datatype :
  Types.datatype Utility.IntMap.t * Types.row Utility.IntMap.t * Types.field_spec Utility.IntMap.t ->
  Types.datatype -> Types.datatype
val alias : string -> Types.type_arg list -> Types.tycon_environment -> Types.datatype

val apply_type : Types.datatype -> Types.type_arg list -> Types.datatype
val freshen_quantifiers : Types.datatype -> Types.datatype
val replace_quantifiers : Types.datatype -> Types.quantifier list -> Types.datatype
