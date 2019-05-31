open Utility
open CommonTypes

module Lang :
sig

  type base_type = | Bool | Char | Float | Int | String

  type tag = int
      [@@deriving show]

  type t =
      | For       of tag option * (Var.var * t) list * t list * t
      | If        of t * t * t
      | Table     of Value.table
      | Database  of (Value.database * string)
      | Singleton of t
      | Concat    of t list
      | Record    of t StringMap.t
      | Project   of t * string
      | Erase     of t * StringSet.t
      | Variant   of string * t
      | XML       of Value.xmlitem
      | Apply     of string * t list
      | Closure   of (Ir.var list * Ir.computation) * env
      | Primitive of string
      | Var       of Var.var * Types.datatype StringMap.t
      | Constant  of Constant.t
  and env = Value.env * t Env.Int.t
      [@@deriving show]

end

val unbox_xml : Lang.t -> Value.xmlitem

val used_database : Lang.t -> Value.database option

val string_of_t : Lang.t -> string

val type_of_expression : Lang.t -> Types.datatype

val default_of_base_type : Primitive.t -> Lang.t

val value_of_expression : Lang.t -> Value.t

val labels_of_field_types : 'a Utility.StringMap.t -> Utility.StringSet.t
val record_field_types : Types.datatype -> Types.datatype StringMap.t
val table_field_types : Value.table -> Types.typ Utility.StringMap.t
val is_list : Lang.t -> bool

val sql_of_query : Lang.t -> Sql.query

(* Specific to nested queries *)
type let_clause = Var.var * Lang.t * Var.var * Lang.t
type let_query = let_clause list
val sql_of_let_query : let_query -> Sql.query

module Eval :
sig
  val env_of_value_env : 'a -> 'a * 'b Env.Int.t
  val bind : 'a * 'b Env.Int.t -> Env.Int.name * 'b -> 'a * 'b Env.Int.t
  val eta_expand_var : Var.var * Types.datatype StringMap.t -> Lang.t
  val computation : Value.t Value.Env.t * Lang.t Env.Int.t -> Ir.computation -> Lang.t
  val reduce_where_then : Lang.t * Lang.t -> Lang.t
  val reduce_and : Lang.t * Lang.t -> Lang.t
  val eval : Value.t Value.Env.t -> Ir.computation -> Lang.t
end

val compile_update : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option * Ir.computation) -> string

val compile_delete : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option) -> string
