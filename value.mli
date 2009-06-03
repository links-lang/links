(*pp deriving *)
(* Values and environments *)

class type otherfield
 = object method show : string end

type db_status = [ `QueryOk | `QueryError of string ]

class virtual dbvalue :
  object
    method virtual error : string
    method virtual fname : int -> string
    method virtual get_all_lst : string list list
    method virtual nfields : int
    method virtual status : db_status
  end

class virtual database :
  object
    method virtual driver_name : unit -> string
    method virtual escape_string : string -> string
    method virtual exec : string -> dbvalue
    method make_insert_query : (string * string list * string list list) -> string
    method make_insert_returning_query : (string * string list * string list list * string) -> string list
  end

val eq_database : database Eq.eq
val typeable_database : database Typeable.typeable
val show_database : database Show.show

type db_constructor = string -> database * string

val register_driver : string * db_constructor -> unit
val db_connect : string -> string -> database * string
val parse_db_string : string -> string * string
val reconstruct_db_string : string * string -> string

type xmlitem =   Text of string
               | Attr of (string * string)
               | Node of (string * xml)
and xml = xmlitem list
  deriving (Show)

type table = (database * string) * string * Types.row
  deriving (Show)
    
type primitive_value = [
| `Bool of bool
| `Char of char
| `Database of (database * string)
| `Table of table
| `Float of float
| `Int of Num.num
| `XML of xmlitem 
| `NativeString of string ]

val show_primitive_value : primitive_value Show.show

type t = [
| primitive_value
| `List of t list
| `Record of (string * t) list
| `Variant of string * t 
| `RecFunction of ((Ir.var * (Ir.var list * Ir.computation)) list * 
                     env * Ir.var * Ir.scope)
| `FunctionPtr of (Ir.var * env)
| `PrimitiveFunction of string
| `ClientFunction of string
| `Continuation of continuation ]
and continuation = (Ir.scope * Ir.var * env * Ir.computation) list
and env = (t * Ir.scope) Utility.intmap * Ir.closures
    deriving (Show)

val toplevel_cont : continuation

val empty_env : Ir.closures -> env
val bind  : Ir.var -> (t * Ir.scope) -> env -> env
val find : Ir.var -> env -> t
val lookup : Ir.var -> env -> t option
val lookupS : Ir.var -> env -> (t * Ir.scope) option
val shadow : env -> by:env -> env
val fold : (Ir.var -> (t * Ir.scope) -> 'a -> 'a) -> env -> 'a -> 'a
val globals : env -> env
val get_closures : env -> Ir.closures
val find_closure : env -> Ir.var -> Utility.IntSet.t
val with_closures : env -> Ir.closures -> env
val extend : env -> (t*Ir.scope) Utility.intmap -> env

val localise : env -> Ir.var -> env

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

val intmap_of_record : t -> t Utility.intmap option

val string_as_charlist : string -> t
val charlist_as_string : t -> string
val string_of_value : t -> string
val string_of_primitive : primitive_value -> string
val string_of_tuple : (string * t) list -> string
val string_of_cont : continuation -> string

val marshal_value : t -> string
val marshal_continuation : continuation -> string

type unmarshal_envs =
    env * Ir.scope Utility.IntMap.t *
      Ir.computation Utility.IntMap.t *
      (Ir.var list * Ir.computation) Utility.IntMap.t

val build_unmarshal_envs : env * Ir.var Env.String.t * Types.typing_environment
  -> Ir.program -> unmarshal_envs

val unmarshal_continuation : unmarshal_envs -> string -> continuation
val unmarshal_value : unmarshal_envs -> string -> t

val expr_to_contframe : env -> Ir.tail_computation -> 
  (Ir.scope * Ir.var * env * Ir.computation)
