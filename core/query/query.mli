open Utility;;

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
    ;;
val unbox_pair : [> `Record of 'a Utility.StringMap.t ] -> 'a * 'a
    ;;
val unbox_list : ([> `Concat of 'a list | `Singleton of 'b ] as 'a) -> 'b list
    ;;
val unbox_string : [> `Concat of [> `Concat of 'a
                                  | `Singleton of [> `Constant of [> `Char of char ] ] as 'b ]
                list as 'a
            | `Constant of [> `String of string ]
            | `Singleton of 'b ] ->
		string
;;

val used_database : t -> Value.database option
    ;;

val string_of_t : t -> string

val type_of_expression : t -> Types.datatype
    ;;

val default_of_base_type : [> `Bool | `Char | `Float | `Int | `String ] ->
           [> `Constant of
                [> `Bool of bool
                 | `Char of char
                 | `Float of float
                 | `Int of int
                 | `String of string ] ]
;;
val value_of_expression : ([> `Concat of [> `Singleton of 'a ] list
             | `Constant of
                 [> `Bool of bool
                  | `Char of char
                  | `Float of float
                  | `Int of int
                  | `String of string ]
             | `Record of 'a Utility.StringMap.t
             | `Table of Value.table
             | `Variant of string * 'a
             | `XML of Value.xmlitem ]
            as 'a) ->
           Value.t
;;

val labels_of_field_types : 'a Utility.StringMap.t -> Utility.StringSet.t;;
val record_field_types : Types.datatype -> Types.datatype StringMap.t;;
val table_field_types : Value.table -> Types.typ Utility.StringMap.t;;
val is_list : [> `Concat of 'a
            | `For of 'b
            | `If of 'c * 'd * [> `Concat of 'e list ]
            | `Singleton of 'f
            | `Table of 'g ] -> bool;;

module Eval :
sig
  (* val nil : unit *)
  (* val eval_error : unit *)
  val env_of_value_env : 'a -> 'a * 'b Env.Int.t
  (* val (++) : unit *)
  (* val lookup_fun : unit *)
  (* val find_fun : unit *)
  (* val expression_of_value : unit *)
  val bind : 'a * 'b Env.Int.t -> Env.Int.name * 'b -> 'a * 'b Env.Int.t
  (* val lookup : unit *)
  val eta_expand_var : Var.var * Types.datatype StringMap.t -> t
  (* val eta_expand_list : unit *)
  (* val value : unit *)
  (* val apply : unit *)
  val computation : Value.t Value.Env.t * t Env.Int.t -> Ir.computation -> t
  (* val tail_computation : unit *)
  (* val reduce_concat : unit *)
  (* val reduce_for_source : unit *)
  (* val reduce_for_body : unit *)
  (* val reduce_if_condition : unit *)
  val reduce_where_then : t * t -> t
  (* val reduce_if_body : unit *)
  val reduce_and : t * t -> t
  (* val reduce_or : unit *)
  (* val reduce_not : unit *)
  (* val reduce_eq : unit *)
  val eval : Value.t Value.Env.t -> Ir.computation -> t
end
