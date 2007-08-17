(* Values and environments *)

type binop = [ 
| Syntaxutils.comparison
| `Union
| `App
| `RecExt of string
| `MkTableHandle of Types.row ]
type xmlitem = Result.xmlitem
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
        
type env

type continuation = (Ir.var * env * Ir.computation) list

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

val bind  : Ir.var -> t -> env -> env
val lookup : Ir.var -> env -> t option
val shadow : env -> by:env -> env
