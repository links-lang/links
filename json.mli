open ProcessTypes

(* Type synonyms *)
type handler_id = int
type maybe_connection_url = string option
type json_string = string

(* Abstract types *)
module JsonState : sig
  type t

  (** Creates a JSON state, given a set of process IDs to spawn, and a set of
   * event handlers to activate *)
  val empty : client_id -> maybe_connection_url -> t

  (** Adds a process and its mailbox to the state *)
  val add_process : process_id -> Value.t -> Value.t list -> t -> t

  (** Adds an event handler to the state *)
  val add_event_handler : handler_id -> Value.t -> t -> t

  (** Adds an access point ID to the state *)
  val add_ap_id : apid -> t -> t

  (** Serialises the state as a JSON string *)
  val to_string : t -> json_string
end

type json_state = JsonState.t

(* Parsing *)

(** Parses a JSON string into a value *)
val parse_json : json_string -> Value.t

(** Parses a b64-encoded JSON string into a value *)
val parse_json_b64 : string -> Value.t

(* Jsonization *)

(** Returns a JSON representation of a given value. *)
val jsonize_value : Value.t -> json_string

(** Given a JSON state (calculated from resolveJsonState) and a value,
 * returns an object of the form {"value" : v, "state" : s}, where v is
 * the value to serialise and s is the state to serialise. *)
val jsonize_value_with_state : Value.t -> json_state -> json_string

(** Given a json_state, continuation, function name, and list of arguments, JSONises a call
 * to the client. *)
val jsonize_call :
  json_state -> (* State, generated by resolveJsonState *)
  Value.continuation -> (* Continuation to serialise *)
  string -> (* Name of the function *)
  Value.t list -> (* Arguments *)
  json_string
