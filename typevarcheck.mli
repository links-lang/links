val is_guarded : int -> Types.datatype -> bool
val is_guarded_row : int -> Types.row -> bool

val is_negative : int -> Types.datatype -> bool
val is_negative_row : int -> Types.row -> bool
val is_negative_field_env : int -> Types.field_spec_map -> bool
val is_negative_row_var : int -> Types.row_var -> bool

val is_positive : int -> Types.datatype -> bool
val is_positive_row : int -> Types.row -> bool
val is_positive_field_env : int -> Types.field_spec_map -> bool
val is_positive_row_var : int -> Types.row_var -> bool
