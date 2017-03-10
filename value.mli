(*pp deriving *)
(* Values and environments *)
open ProcessTypes

class type otherfield
 = object method show : string end

type db_status = [ `QueryOk | `QueryError of string ]

class virtual dbvalue :
  object
    method virtual error : string
    method virtual fname : int -> string
    method virtual get_all_lst : string list list
    method virtual nfields : int
    method virtual ntuples : int
    method map : 'a. ((int -> string) -> 'a) -> 'a list
    method map_array : 'a. (string array -> 'a) -> 'a list
    method fold_array : 'a. (string array -> 'a -> 'a) -> 'a -> 'a
    method virtual getvalue : int -> int -> string
    method virtual gettuple : int -> string array
    method virtual status : db_status
  end

class virtual database :
  object
    method virtual driver_name : unit -> string
    method virtual escape_string : string -> string
    method virtual quote_field : string -> string
    method virtual exec : string -> dbvalue
    method make_insert_query : (string * string list * string list list) -> string
    method make_insert_returning_query : (string * string list * string list list * string) -> string list
  end

module Eq_database : Deriving_Eq.Eq with type a = database
module Typeable_database : Deriving_Typeable.Typeable with type a = database
module Show_database : Deriving_Show.Show with type a = database

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

type table = (database * string) * string * string list list * Types.row
  deriving (Show)

type primitive_value = [
| `Bool of bool
| `Char of char
| `Database of (database * string)
| `Table of table
| `Float of float
| `Int of int
| `XML of xmlitem
| `String of string ]

module Show_primitive_value : Deriving_Show.Show with type a = primitive_value

type spawn_location = [
  | `ClientSpawnLoc of client_id
  | `ServerSpawnLoc (* Will need to add in a server address when we go to n-tier *)
]
  deriving (Show)

type dist_pid = [
  | `ClientPid of (client_id * process_id)
  | `ServerPid of process_id (* Again, will need a server address here later *)
]
  deriving (Show)

type access_point = [
  | `ClientAccessPoint of (client_id * apid)
  | `ServerAccessPoint of apid
]
  deriving (Show)

type chan = (channel_id * channel_id)

type t = [
| primitive_value
| `List of t list
| `Record of (string * t) list
| `Variant of string * t
| `FunctionPtr of (Ir.var * t option)
| `PrimitiveFunction of string * Var.var option
| `ClientFunction of string
| `Continuation of continuation
| `Pid of dist_pid
| `AccessPointID of access_point
| `SessionChannel of chan
| `Socket of in_channel * out_channel
| `SpawnLocation of spawn_location
]
and continuation = (Ir.scope * Ir.var * env * Ir.computation) list
and env
    deriving (Show)

type delegated_chan = (chan * (t list))

val set_request_data : env -> RequestData.request_data -> env
val toplevel_cont : continuation

val empty_env : env
val bind  : Ir.var -> (t * Ir.scope) -> env -> env
val find : Ir.var -> env -> t
val mem : Ir.var -> env -> bool
val lookup : Ir.var -> env -> t option
val lookupS : Ir.var -> env -> (t * Ir.scope) option
val shadow : env -> by:env -> env
val fold : (Ir.var -> (t * Ir.scope) -> 'a -> 'a) -> env -> 'a -> 'a
val globals : env -> env
val request_data : env -> RequestData.request_data
(* used only by json.ml, webif.ml ... *)
val get_parameters : env -> (t*Ir.scope) Utility.intmap

val extend : env -> (t*Ir.scope) Utility.intmap -> env


val localise : env -> Ir.var -> env

val project : string -> [> `Record of (string * 'b) list ] -> 'b
val untuple : t -> t list

val box_bool : 'a -> [> `Bool of 'a ]
val unbox_bool : t -> bool
val box_int : 'a -> [> `Int of 'a ]
val unbox_int : t -> int
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
val box_record : (string * t) list -> t
val unbox_record : t -> (string * t) list
val box_unit : unit -> t
val unbox_unit : t -> unit
val box_pair : t -> t -> t
val unbox_pair : t -> (t * t)
val box_pid : dist_pid -> t
val unbox_pid : t -> dist_pid
val box_socket : in_channel * out_channel -> t
val unbox_socket : t -> in_channel * out_channel
val box_spawn_loc : spawn_location -> t
val unbox_spawn_loc : t -> spawn_location
val box_channel : chan -> t
val unbox_channel : t -> chan
val box_access_point : access_point -> t
val unbox_access_point : t -> access_point

val intmap_of_record : t -> t Utility.intmap option

val string_as_charlist : string -> t
val charlist_as_string : t -> string
val string_of_value : t -> string
val string_of_xml : ?close_tags:bool -> xml -> string
val string_of_primitive : primitive_value -> string
val string_of_tuple : (string * t) list -> string
val string_of_cont : continuation -> string

val marshal_value : t -> string
val marshal_continuation : continuation -> string

val unmarshal_continuation : env -> string -> continuation
val unmarshal_value : env -> string -> t

val expr_to_contframe : env -> Ir.tail_computation ->
  (Ir.scope * Ir.var * env * Ir.computation)

(* Given a value, retreives a list of channels that are contained inside *)
val get_contained_channels : t -> chan list

val value_of_xmlitem : xmlitem -> t

val split_html : xml -> xml * xml

