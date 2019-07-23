val generalise : ?unwrap:bool -> Types.environment -> Types.datatype -> ((Types.quantifier list * Types.type_arg list) * Types.datatype)
val generalise_rigid : ?unwrap:bool -> Types.environment -> Types.datatype -> ((Types.quantifier list * Types.type_arg list) * Types.datatype)
val get_quantifiers_rigid : Types.environment -> Types.datatype -> Types.quantifier list
val rigidify_type_arg : Types.type_arg -> unit
