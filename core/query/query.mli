open Utility
open CommonTypes

val reduce_and : QueryLang.t * QueryLang.t -> QueryLang.t
val reduce_where_then : QueryLang.t * QueryLang.t -> QueryLang.t

module Eval :
sig
  val computation : QueryLang.env -> Ir.computation -> QueryLang.t
  val eval : QueryPolicy.t -> Value.t Value.Env.t -> Ir.computation -> QueryLang.t
end

val compile_update : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option * Ir.computation) -> Sql.query

val compile_delete : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option) -> Sql.query