
type var = int
type name = string

module StringSet = Utility.StringSet
module StringMap = Utility.StringMap

type name_set = StringSet.t
type 'a name_map = 'a StringMap.t



type constant = 
  [ `Float of float
  | `Int of Num.num
  | `String of string
  | `Bool of bool
  | `Char of char
  ]

let escape_string s = (* SQL standard for escaping single quotes in a string *)
  Str.global_replace (Str.regexp "'") "''" s

let string_of_constant = function
    | `Bool value -> string_of_bool value
    | `Int value -> Num.string_of_num value
    | `Char c -> "'"^ Char.escaped c ^"'" 
    | `String s -> "'" ^ escape_string s ^ "'"
    | `Float value   -> string_of_float value



(** Simplified IR to be puted in the runtime **)

type value =
  | Constant of constant
  | Variable of var
  | Primitive of string
  | Extend of value name_map * value option
  | Project of name * value
  | Erase of name_set * value
  | Inject of name * value
  | ApplyPure of value * value list
  | Table of value * value * name_set
  | Database of value
  | Lambda of var list * computation
  
and tail_computation =
  | Return of value
  | Apply of value * value list
  | ApplyDB of value * value list
  | Case of value * (var * computation) name_map * (var * computation) option
  | If of value * computation * computation
  
and binding = 
  | Let of var * computation
  | Fun of var * var list * computation
  | FunQ of var * var list * computation
  
and computation = binding list * tail_computation

type xmlitem =   
  [ `XMLText of string
  | `XMLAttr of (string * string)
  | `XMLNode of (string * xmlitem list)
  ]

type query =
  [ `For of (var * query) list * query list * query
  | `If of query * query * query
  | `Table of string * string * name_set
  | `Database of string
  | `Singleton of query | `Concat of query list
  | `Record of query name_map | `Project of query * name | `Erase of name_set * query
  | `Variant of string * query
  | `Apply of string * query list
  | `Closure of (var list * computation) * env
  | `Primitive of string
  | `Var of var * name_set
  | `Constant of constant
  | xmlitem
  ]
	 
and env = query Env.Int.t
