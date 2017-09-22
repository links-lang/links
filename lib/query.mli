open Utility;;

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

val compile : Value.env -> (int * int) option * Ir.computation -> (Value.database * string * Types.datatype) option;;

val compile_update : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option * Ir.computation) -> string;;

val compile_delete : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option) -> string;;
