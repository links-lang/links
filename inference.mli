(** Type inference via the Damas-Milner algorithm, with support for rows **)

val enable_mailbox_typing : bool Settings.setting

val type_program    : Types.environment -> Syntax.untyped_expression list -> Types.environment * Syntax.expression list

val type_expression : Types.environment -> Syntax.untyped_expression      -> Types.environment * Syntax.expression

val retype_primitives : Types.environment -> Types.environment

val remove_mailbox : Types.datatype  -> Types.datatype
