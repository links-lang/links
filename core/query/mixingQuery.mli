(*****************************************************************************
 ** mixingQuery.mli                                                         **
 **                                                                         **
 ** Normalisation of queries mixing sets and bags                           **
 **                                                                         **
 **                                                                         **
 ** author:  Wilmer Ricciotti                                               **
 ** created: 30 Jul 2021                                                    **
 *****************************************************************************)

open Utility
open CommonTypes

val flatfield : string -> string -> string
val flattened_pair : QueryLang.t -> QueryLang.t -> QueryLang.t
val flattened_pair_ft : QueryLang.t -> QueryLang.t -> Types.datatype stringmap
val type_of_for_var : QueryLang.t -> Types.datatype

val reduce_where_then : QueryLang.t * QueryLang.t -> QueryLang.t
val reduce_and : QueryLang.t * QueryLang.t -> QueryLang.t

val sql_of_query : QueryLang.t -> Sql.query

(* Specific to nested queries *)
type let_clause = Var.var * QueryLang.t * Var.var * QueryLang.t
type let_query = let_clause list
val sql_of_let_query : let_query -> Sql.query

module Eval :
sig
  val empty_env : QueryPolicy.t -> QueryLang.env
  val env_of_value_env : QueryPolicy.t -> Value.env -> QueryLang.env
  val query_bindings_of_env : QueryLang.env -> (Var.var * QueryLang.t) list
  val computation : QueryLang.env -> Ir.computation -> QueryLang.t
  val contains_free : Var.var list -> QueryLang.t -> bool
  val norm : QueryLang.env -> QueryLang.t -> QueryLang.t
  val eval : QueryPolicy.t -> Value.t Value.Env.t -> Ir.computation -> QueryLang.t
end

val likeify : QueryLang.t -> QueryLang.t option

val compile_update : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option * Ir.computation) -> Sql.query

val compile_delete : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option) -> Sql.query

val insert : string -> string list -> Value.t list list -> Sql.query
