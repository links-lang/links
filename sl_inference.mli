(** Type inference via the Damas-Milner algorithm, with support for rows **)

val type_program    : Sl_kind.environment -> Sl_syntax.untyped_expression list -> Sl_kind.environment * Sl_syntax.expression list

val type_expression : Sl_kind.environment -> Sl_syntax.untyped_expression      -> Sl_kind.environment * Sl_syntax.expression
