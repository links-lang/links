(* Values and environments *)

type binop = [ 
| Syntaxutils.comparison
| `Union
| `App
| `RecExt of string
| `MkTableHandle of Types.row ]

type xmlitem =   Text of string
               | Attr of (string * string)
               | Node of (string * xml)
and xml = xmlitem list
type table = (Result.database * string) * string * Types.row
    
type primitive_value = [
| `Bool of bool
| `Char of char
| `Database of (Result.database * string)
| `Table of table
| `Float of float
| `Int of Num.num
| `XML of xmlitem 
| `NativeString of string ]
        
type t = [
| primitive_value
| `List of t list
| `Record of (string * t) list
| `Variant of string * t 
| `RecFunction of ((Ir.var * (Ir.var list * Ir.computation)) list * env * Ir.var)
| `PrimitiveFunction of string
| `ClientFunction of string
| `Abs of t
| `Continuation of continuation ]
and continuation = (Ir.var * env * Ir.computation) list
and env = t Utility.IntMap.t

val bind  : Ir.var -> t -> env -> env
val lookup : Ir.var -> env -> t option
val shadow : env -> by:env -> env

val project : string -> [> `Record of (string * 'b) list ] -> 'b
val untuple : t -> t list


val box_bool : 'a -> [> `Bool of 'a ]
val unbox_bool : t -> bool
val box_int : 'a -> [> `Int of 'a ]
val unbox_int : t -> Num.num
val box_float : 'a -> [> `Float of 'a ]
val unbox_float : t -> float
val box_char : 'a -> [> `Char of 'a ]
val unbox_char : t -> char
val box_xml : 'a -> [> `XML of 'a ]
val unbox_xml : t -> xmlitem
val box_string : string -> t
val unbox_string : t -> string
val box_list : t list -> t
val unbox_list : t -> t list
val box_unit : unit -> t 
val unbox_unit : t -> unit
val unbox_pair : t -> (t * t)


val string_of_value : t -> string
val string_of_primitive : primitive_value -> string
val string_of_tuple : (string * t) list -> string


