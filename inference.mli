(** Type inference via the Damas-Milner algorithm, with support for rows *)

val type_program    : Types.typing_environment -> Syntax.untyped_expression list ->
  Types.typing_environment * Syntax.expression list

val type_expression : Types.typing_environment -> Syntax.untyped_expression ->
  Types.typing_environment * Syntax.expression

val retype_primitives : Types.environment -> Types.environment
val unretype_primitives : Types.environment -> Types.environment

val remove_mailbox : Types.datatype  -> Types.datatype

