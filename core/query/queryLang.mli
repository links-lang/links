(*****************************************************************************
 ** queryLang.mli                                                           **
 **                                                                         **
 ** Common data types and definitions for the NRC-like sublanguage of Links **
 ** (factorised from query.mli)                                             **
 **                                                                         **
 **                                                                         **
 ** author:  Wilmer Ricciotti                                               **
 ** created: 30 Jul 2021                                                    **
 *****************************************************************************)

open Utility
open CommonTypes

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
    | Dedup     of t
    | Prom      of t
    | Record    of t StringMap.t
    | Project   of t * string
    | Erase     of t * StringSet.t
    | Variant   of string * t
    | XML       of Value.xmlitem
    | Apply     of t * t list
    | Closure   of (Ir.var list * Ir.computation) * env
    | Case      of t * (Var.binder * t) StringMap.t * (Var.binder * t) option
    | Primitive of string
    | Var       of Var.var * Types.datatype
    | Constant  of Constant.t
and env = { venv: Value.env; qenv: t Env.Int.t; policy: QueryPolicy.t }
    [@@deriving show]

val query_error : ('a, unit, string, 'b) format4 -> 'a

val nil : t

val bind : env -> Env.Int.name * t -> env

val expression_of_base_value : Value.t -> t

val field_types_of_row : Types.datatype -> Types.datatype StringMap.t

val unbox_xml : t -> Value.xmlitem

val unbox_string : t -> string

val unbox_list : t -> t list

val unbox_pair : t -> t * t

val unbox_record : t -> t StringMap.t

val used_database : t -> Value.database option

val string_of_t : t -> string

val recdty_field_types : Types.datatype -> Types.datatype StringMap.t

val subst : t -> Var.var -> t -> t

val occurs_free_gens : (Var.var * t) list -> t -> (Var.var * t * Types.datatype) option

val type_of_expression : t -> Types.datatype

val eta_expand_var : Var.var * Types.datatype -> t

val eta_expand_list : t -> (Var.var * t) list * t list * t

val default_of_base_type : Primitive.t -> t

val value_of_expression : t -> Value.t

val labels_of_field_types : 'a Utility.StringMap.t -> Utility.StringSet.t
val table_field_types : Value.table -> Types.typ Utility.StringMap.t
val is_list : t -> bool

(*
module Eval :
sig
  val env_of_value_env : QueryPolicy.t -> Value.env -> Lang.env
  val bind : Lang.env -> Env.Int.name * Lang.t -> Lang.env
  val eta_expand_var : Var.var * Types.datatype StringMap.t -> Lang.t
  val computation : Lang.env -> Ir.computation -> Lang.t
  val eval : QueryPolicy.t -> Value.t Value.Env.t -> Ir.computation -> Lang.t
end

val compile_update : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option * Ir.computation) -> Sql.query

val compile_delete : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option) -> Sql.query

val insert : string -> string list -> Value.t list list -> Sql.query
*)