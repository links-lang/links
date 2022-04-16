open Utility
open CommonTypes

val reduce_and : QueryLang.t * QueryLang.t -> QueryLang.t
val reduce_where_then : QueryLang.t * QueryLang.t -> QueryLang.t

module Eval :
sig
  val computation : QueryLang.env -> Ir.computation -> QueryLang.t
  val eval : QueryPolicy.t -> Value.t Value.Env.t -> Ir.computation -> QueryLang.t
  (* Below functions are exported because they are needed by temporalQuery. Ideally
     we wouldn't export them. *)
  (* Calls computation and then normalises result. *)
  val norm_comp : QueryLang.env -> Ir.computation -> QueryLang.t
  (* Translates an IR value *)
  val xlate : QueryLang.env -> Ir.value -> QueryLang.t
end

val compile_update : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option * Ir.computation) -> Sql.query

val compile_delete : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option) -> Sql.query
