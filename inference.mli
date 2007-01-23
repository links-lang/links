(** Type inference via the Damas-Milner algorithm, with support for rows *)

val type_program    : Inferencetypes.typing_environment -> Syntax.untyped_expression list ->
  Inferencetypes.typing_environment * Syntax.expression list

val type_expression : Inferencetypes.typing_environment -> Syntax.untyped_expression ->
  Inferencetypes.typing_environment * Syntax.expression
