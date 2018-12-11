val generalise : FrontendTypeEnv.var_environment -> Types.datatype -> ((Types.quantifier list * Types.type_arg list) * Types.datatype)
val generalise_rigid : FrontendTypeEnv.var_environment -> Types.datatype -> ((Types.quantifier list * Types.type_arg list) * Types.datatype)
val get_quantifiers : FrontendTypeEnv.var_environment -> Types.datatype -> Types.quantifier list
val extract_quantifiers : Types.quantifier list -> Types.quantifier list
val rigidify_quantifier : Types.quantifier -> unit
