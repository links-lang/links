(** Type inference via the Damas-Milner algorithm, with support for rows **)

val type_program    : Kind.environment -> Syntax.untyped_expression list -> Kind.environment * Syntax.expression list

val type_expression : Kind.environment -> Syntax.untyped_expression      -> Kind.environment * Syntax.expression

val retype_primitives : Kind.environment -> Kind.environment

