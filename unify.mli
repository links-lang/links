type error = [
  `Msg of string
| `PresentAbsentClash of string * Types.row * Types.row
]

exception Failure of error

val datatypes : Types.alias_environment -> Types.datatype * Types.datatype -> unit
val rows : Types.alias_environment -> Types.row * Types.row -> unit
