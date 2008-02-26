(** Type inference via the Damas-Milner algorithm, with support for rows *)

val type_program    : Types.typing_environment -> Syntax.untyped_program ->
  Types.typing_environment * Syntax.program

val type_expression : Types.typing_environment -> Syntax.untyped_expression ->
  Types.typing_environment * Syntax.expression

val unify : Types.datatype * Types.datatype -> unit
