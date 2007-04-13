val is_guarded : Types.alias_environment -> int -> Types.datatype -> bool
val is_guarded_row : Types.alias_environment -> int -> Types.row -> bool

val is_negative : Types.alias_environment -> int -> Types.datatype -> bool
val is_negative_row : Types.alias_environment -> int -> Types.row -> bool
val is_negative_field_env : Types.alias_environment -> int -> Types.field_spec_map -> bool
val is_negative_row_var : Types.alias_environment -> int -> Types.row_var -> bool

val is_positive : Types.alias_environment -> int -> Types.datatype -> bool
val is_positive_row : Types.alias_environment -> int -> Types.row -> bool
val is_positive_field_env : Types.alias_environment -> int -> Types.field_spec_map -> bool
val is_positive_row_var : Types.alias_environment -> int -> Types.row_var -> bool
