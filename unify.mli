type error = [
  `Msg of string
| `PresentAbsentClash of string * Types.row * Types.row
]

exception Failure of error

val datatypes : Types.datatype * Types.datatype -> unit
val rows : Types.row * Types.row -> unit

(* FIXME: move to types.ml/typeUtils.ml *)
val eq_types : Types.datatype * Types.datatype -> bool
val eq_quantifier : Types.quantifier * Types.quantifier -> bool
val eq_rows : Types.row * Types.row -> bool
val eq_presence : Types.presence_flag * Types.presence_flag -> bool
val eq_row_vars : Types.row_var * Types.row_var -> bool
val eq_type_args : Types.type_arg * Types.type_arg -> bool
