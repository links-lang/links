(** Type inference via the Damas-Milner algorithm, with support for rows *)

val type_program    : Types.typing_environment -> Syntax.untyped_expression list ->
  Types.typing_environment * Syntax.expression list

val type_expression : Types.typing_environment -> Syntax.untyped_expression ->
  Types.typing_environment * Syntax.expression

val unify : Types.alias_environment -> Types.datatype * Types.datatype -> unit
val instantiate_datatype : Types.datatype Utility.IntMap.t * Types.row_var Utility.IntMap.t -> Types.datatype -> Types.datatype
