(** Type inference via the Damas-Milner algorithm, with support for rows **)

(* [HACK] *)
(* types for special builtin functions *)
val self_type_mailbox : Types.assumption
val self_type_pure : Types.assumption
val recv_type_mailbox : Types.assumption
val recv_type_pure : Types.assumption
val spawn_type_mailbox : Types.assumption
val spawn_type_pure : Types.assumption


val type_program    : Types.environment -> Syntax.untyped_expression list -> Types.environment * Syntax.expression list

val type_expression : Types.environment -> Syntax.untyped_expression      -> Types.environment * Syntax.expression

val retype_primitives : Types.environment -> Types.environment
val unretype_primitives : Types.environment -> Types.environment

val remove_mailbox : Types.datatype  -> Types.datatype


