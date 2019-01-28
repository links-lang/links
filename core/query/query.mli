open Utility

type tag = int
  [@@deriving show]

type t =
    [ `For of tag option * (Var.var * t) list * t list * t
    | `If of t * t * t
    | `Table of Value.table
    | `Database of (Value.database * string)
    | `Singleton of t | `Concat of t list
    | `Record of t StringMap.t | `Project of t * string | `Erase of t * StringSet.t
    | `Variant of string * t
    | `XML of Value.xmlitem
    | `Apply of string * t list
    | `Closure of (Ir.var list * Ir.computation) * env
    | `Primitive of string
    | `Var of (Var.var * Types.datatype StringMap.t) | `Constant of Constant.constant ]
and env = Value.env * t Env.Int.t
  [@@deriving show]

val unbox_xml : [> `XML of 'a ] -> 'a

val unbox_pair : [> `Record of 'a Utility.StringMap.t ] -> 'a * 'a

val unbox_list : ([> `Concat of 'a list | `Singleton of 'b ] as 'a) -> 'b list

val unbox_string : [> `Concat of [> `Concat of 'a
                                  | `Singleton of [> `Constant of [> `Char of char ] ] as 'b ]
                list as 'a
            | `Constant of [> `String of string ]
            | `Singleton of 'b ] ->
		string

val used_database : t -> Value.database option

val string_of_t : t -> string

val type_of_expression : t -> Types.datatype

val default_of_base_type : Types.base_type -> [> `Constant of Types.base_type ]

val value_of_expression : ([> `Concat of [> `Singleton of 'a ] list
             | `Constant of Types.base_type
             | `Record of 'a Utility.StringMap.t
             | `Table of Value.table
             | `Variant of string * 'a
             | `XML of Value.xmlitem ]
            as 'a) ->
           Value.t

val labels_of_field_types : 'a Utility.StringMap.t -> Utility.StringSet.t
val record_field_types : Types.datatype -> Types.datatype StringMap.t
val table_field_types : Value.table -> Types.typ Utility.StringMap.t
val is_list : [> `Concat of 'a
            | `For of 'b
            | `If of 'c * 'd * [> `Concat of 'e list ]
            | `Singleton of 'f
            | `Table of 'g ] -> bool

module Eval :
sig
  val env_of_value_env : 'a -> 'a * 'b Env.Int.t
  val bind : 'a * 'b Env.Int.t -> Env.Int.name * 'b -> 'a * 'b Env.Int.t
  val eta_expand_var : Var.var * Types.datatype StringMap.t -> t
  val computation : Value.t Value.Env.t * t Env.Int.t -> Ir.computation -> t
  val reduce_where_then : t * t -> t
  val reduce_and : t * t -> t
  val eval : Value.t Value.Env.t -> Ir.computation -> t
end
