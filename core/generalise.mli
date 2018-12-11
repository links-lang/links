val generalise : FrontendTypeEnv.qual_var_environment -> Types.datatype -> ((Types.quantifier list * Types.type_arg list) * Types.datatype)
val generalise_rigid : FrontendTypeEnv.qual_var_environment -> Types.datatype -> ((Types.quantifier list * Types.type_arg list) * Types.datatype)
val get_quantifiers : FrontendTypeEnv.qual_var_environment -> Types.datatype -> Types.quantifier list
val extract_quantifiers : Types.quantifier list -> Types.quantifier list
val rigidify_quantifier : Types.quantifier -> unit
