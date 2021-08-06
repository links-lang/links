(* Values and environments *)
open CommonTypes
open ProcessTypes

class type otherfield
 = object method show : string end

type db_status = [ `QueryOk | `QueryError of string ]

class virtual dbvalue :
  object
    method virtual error : string
    method virtual fname : int -> string
    method virtual nfields : int
    method virtual ntuples : int
    method map : 'a. ((int -> string) -> 'a) -> 'a list
    method map_array : 'a. (string array -> 'a) -> 'a list
    method fold_array : 'a. (string array -> 'a -> 'a) -> 'a -> 'a
    method virtual getvalue : int -> int -> string
    method virtual gettuple : int -> string array
    method virtual status : db_status
  end

class virtual database : Sql.printer ->
  object
    method virtual driver_name : unit -> string
    method virtual escape_string : string -> string
    method virtual exec : string -> dbvalue
    method make_insert_returning_query :
      string (* "returning" field *) ->
      Sql.query ->
      string list
    method virtual supports_shredding : unit -> bool
    method string_of_query : ?range:(Sql.range option) -> Sql.query -> string
    method sql_printer : Sql.printer
  end


val equal_database : database -> database -> bool
val pp_database : Format.formatter -> database -> unit

type db_constructor = string -> database * string

val register_driver : string * db_constructor -> unit
val db_connect : string -> string -> database * string
val parse_db_string : string -> string * string
val reconstruct_db_string : string * string -> string

type xmlitem =   Text of string
               | Attr of (string * string)
               | Node of (string * xml)
               | NsAttr of (string * string * string)
               | NsNode of (string * string * xml)
and xml = xmlitem list
  [@@deriving show,yojson]

type table = (database * string) * string * string list list * Types.row'
  [@@deriving show]

type primitive_value_basis =  [
  | `Bool of bool
  | `Char of char
  | `Float of float
  | `Int of int
  | `XML of xmlitem
  | `String of string ]
  [@@deriving show]

type primitive_value = [
  | primitive_value_basis
  | `Database of (database * string)
  | `Table of table ]
  [@@deriving show]

type spawn_location = [
  | `ClientSpawnLoc of client_id
  | `ServerSpawnLoc (* Will need to add in a server address when we go to n-tier *)
]
  [@@deriving show]

type dist_pid = [
  | `ClientPid of (client_id * process_id)
  | `ServerPid of process_id (* Again, will need a server address here later *)
]
  [@@deriving show]

type access_point = [
  | `ClientAccessPoint of (client_id * apid)
  | `ServerAccessPoint of apid
]
  [@@deriving show]

type chan = (channel_id * channel_id)

module type ENV =
sig
  type 'a t
     [@@deriving show]
  val set_request_data : 'a t -> RequestData.request_data -> 'a t
  val request_data : 'a t -> RequestData.request_data
  val empty : 'a t
  val bind  : Ir.var -> ('a * Ir.scope) -> 'a t -> 'a t
  val find : Ir.var -> 'a t -> 'a
  val mem : Ir.var -> 'a t -> bool
  val lookup : Ir.var -> 'a t -> 'a option
  val lookupS : Ir.var -> 'a t -> ('a * Ir.scope) option
  val shadow : 'a t -> by:'a t -> 'a t
  val fold : (Ir.var -> ('a * Ir.scope) -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val globals : 'a t -> 'a t
  (* used only by json.ml, webif.ml ... *)
  val get_parameters : 'a t -> ('a * Ir.scope) Utility.intmap
  val extend : 'a t -> ('a * Ir.scope) Utility.intmap -> 'a t
  val localise : 'a t -> Ir.var -> 'a t
end

module Env : ENV

module Trap : sig
  type ('v, 'r) result =
    | Trap of (unit -> 'r)
    | SessionTrap of ('v, 'r) session_result
    | UnhandledSessionException of (Ir.computation list)
  and ('v, 'r) session_result = {
    handle_env: 'v Env.t;
    frames: Ir.computation list;
    continuation_thunk: (unit -> 'r)
  }
end
(* Continuation *)
module type FRAME = sig
  type 'v t

  val of_expr : 'v Env.t -> Ir.tail_computation -> 'v t
  val make : Ir.scope -> Ir.var -> 'v Env.t -> Ir.computation -> 'v t
  val decompose : 'v t -> (Ir.scope * Ir.var * 'v Env.t * Ir.computation)
end

module type CONTINUATION_EVALUATOR = sig
  type v
  type result
  type 'v t
  type 'v resumption
  type trap_result = (v, result) Trap.result

  val apply : env:v Env.t ->            (* the current environment *)
              v t ->                    (* the continuation *)
              v ->                      (* the argument *)
              result

  val resume : env:v Env.t ->
               v t ->
               v resumption ->
               v list ->
               result

  (* trap invocation *)
  val trap : v t ->                        (* the continuation *)
             (Name.t * v) ->              (* operation name and its argument *)
             trap_result
end

module type CONTINUATION = sig
  type 'v t
  and 'v resumption
    [@@deriving show]

  module Frame : FRAME

  module Handler : sig
    type 'v t
    val make : env:'v Env.t -> return:(Ir.binder * Ir.computation) -> clauses:Ir.effect_case Ir.name_map -> depth:[`Deep of Ir.var list | `Shallow] -> 'v t
  end
  (* A continuation has a monoidal structure *)
  val empty : 'v t
  val (<>)  : 'v t -> 'v t -> 'v t          (* continuation composition *)
  val (&>)  : 'v Frame.t -> 'v t -> 'v t    (* continuation augmentation *)

  val set_trap_point : handler:'v Handler.t -> 'v t -> 'v t  (* installs a handler *)

  module Evaluation :
    functor(E :
              sig
                type v
                type result
                val error : string -> 'a
                val computation : v Env.t -> v t -> Ir.computation -> result (* computation evaluator *)
                val finish : v Env.t -> v -> result                          (* ends program evaluation *)
                val reify : v resumption -> v                                (* continuation reification *)
            end) ->
    sig
      include CONTINUATION_EVALUATOR with
        type v = E.v
        and type result = E.result
        and type 'v t := 'v t
        and type 'v resumption := 'v resumption
    end

  val to_string : 'v t -> string

  module Inspection: sig
    type persistence =
      | Persistent
      | Intermittent
    type 'v result =
      | Frame of 'v Frame.t
      | Trap of { env: 'v Env.t;
                  name: Ir.var;
                  persistence: persistence }

    val inspect : ('v result -> 'a -> 'a) -> 'v t -> 'a -> 'a
  end
end

module Continuation : CONTINUATION

type t = [
| primitive_value
| `List of t list
| `Record of (string * t) list
| `Lens of Lens.Database.t * Lens.Value.t
| `Variant of string * t
| `FunctionPtr of (Ir.var * t option)
| `PrimitiveFunction of string * Var.var option
| `ClientDomRef of int
| `ClientFunction of string
| `Continuation of continuation
| `Resumption of resumption
| `Pid of dist_pid
| `AccessPointID of access_point
| `SessionChannel of chan
| `Socket of in_channel * out_channel
| `SpawnLocation of spawn_location
| `Alien
]
and continuation = t Continuation.t
and resumption = t Continuation.resumption
and env = t Env.t
    [@@deriving show]

type delegated_chan = (chan * (t list))

val project : string -> t -> t
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
val box_variant : string -> t -> t
val unbox_variant : t -> (string * t)
val box_pid : dist_pid -> t
val unbox_pid : t -> dist_pid
val box_socket : in_channel * out_channel -> t
val unbox_socket : t -> in_channel * out_channel
val box_op : t list -> t -> t
val box    : t list -> t
val box_spawn_loc : spawn_location -> t
val unbox_spawn_loc : t -> spawn_location
val box_channel : chan -> t
val unbox_channel : t -> chan
val box_access_point : access_point -> t
val unbox_access_point : t -> access_point

val intmap_of_record : t -> t Utility.intmap option

val string_of_value : t -> string
val string_of_xml : ?close_tags:bool -> xml -> string
val p_value: Format.formatter -> t -> unit

(* Given a value, retreives a list of channels that are contained inside *)
val get_contained_channels : t -> chan list

val value_of_xmlitem : xmlitem -> t
val value_of_xml : xml -> t
val xml_of_variants : t -> xml
val xmlitem_of_variant : t -> xmlitem

val is_attr : xmlitem -> bool
val is_node : xmlitem -> bool

val split_html : xml -> xml * xml

val is_channel : t -> bool

val session_exception_operation : string

val row_columns_values : t -> string list * t list list
