open ProcessTypes

(* Type synonyms *)
type handler_id = int
type json_string = string
type websocket_url = string

(* Abstract types *)
module JsonState : sig
  type t

  (** Creates a JSON state, given a set of process IDs to spawn, and a set of
   * event handlers to activate *)
  val empty : client_id -> websocket_url option -> t

  (** Adds a process and its mailbox to the state *)
  val add_process : process_id -> Value.t -> Value.t list -> t -> t

  (** Adds an event handler to the state *)
  val add_event_handler : handler_id -> Value.t -> t -> t

  (** Adds an access point ID to the state *)
  val add_ap_id : apid -> t -> t

  val add_carried_channel : Value.chan -> t -> t

  val get_carried_channels : t -> Value.chan list

  (** Adds a buffer to the state *)
  val add_buffer : channel_id -> Value.t list -> t -> t

  (** Serialises the state as a JSON string *)
  val to_json : t -> Yojson.Basic.t
end

type json_state = JsonState.t

(* Jsonization *)

(** Returns a JSON representation of a given value. *)
val jsonize_value : Value.t -> Yojson.Basic.t

(** Given a JSON state (calculated from resolveJsonState) and a value,
 * returns an object of the form {"value" : v, "state" : s}, where v is
 * the value to serialise and s is the state to serialise. *)
val jsonize_value_with_state : Value.t -> json_state -> Yojson.Basic.t

(** Given a json_state, continuation, function name, and list of arguments, JSONises a call
 * to the client. *)
val jsonize_call :
  json_state -> (* State, generated by resolveJsonState *)
  string -> (* Serialised continuation *)
  string -> (* Name of the function *)
  Value.t list -> (* Arguments *)
  Yojson.Basic.t

val json_to_string : Yojson.Basic.t -> string
val nil_literal : Yojson.Basic.t

