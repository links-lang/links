val generalise : Types.environment -> Types.datatype -> ((Types.quantifier list * Types.type_arg list) * Types.datatype)
val generalise_rigid : Types.environment -> Types.datatype -> ((Types.quantifier list * Types.type_arg list) * Types.datatype)
val get_type_variables : Types.environment -> Types.datatype -> Types.type_variable list
val rigidify_quantifier : Types.quantifier -> unit
