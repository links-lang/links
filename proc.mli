(** Process management *)

type abort_type = string * string
exception Aborted of abort_type  (* This sucks *)

(* This seems hacky, but added since otherwise there would
 * be a cyclic dep between proc and JSON.
 * JSON requires Proc to pop messages; websockets requires JSON
 * for serialising values; and proc requires websockets for 
 * remote sends. 
 * To get around this, JSONisation happens at the callsite,
 * and the JSONised value is then sent along the websocket.
 * *)
type client_send_result = [
  | `LocalSendOK
  | `RemoteSend of (Websockets.links_websocket)
]

module Proc :
sig
  type thread_result = (Value.env * Value.t)
  type thread = unit -> thread_result Lwt.t

  val debug_process_status : unit -> unit

  val get_current_pid : unit -> ProcessTypes.process_id

  val lookup_client_process : ProcessTypes.client_id -> ProcessTypes.process_id -> Value.t option

  val create_process : bool -> thread -> ProcessTypes.process_id 
  val create_client_process : ProcessTypes.client_id -> Value.t ->
    ProcessTypes.process_id
  val awaken : ProcessTypes.process_id -> unit

  val finish : Value.env * Value.t -> thread_result Lwt.t
  val yield : thread -> thread_result Lwt.t
  val block : thread -> thread_result Lwt.t
  val abort : abort_type -> thread_result Lwt.t

  val atomically : thread -> Value.t

  val singlethreaded : unit -> bool (* Exposed to prevent client calls from killing server-side threads... *)

  val run : (unit -> 'a Lwt.t) -> 'a
end

(* Operations on websockets used to send and receive messages remotely. *)
module Websockets :
  sig
    type links_websocket

    (** Accepts a new websocket connection, creates a new socket, as
     * well as a thread which handles incoming messages. *)
    val accept :
      ProcessTypes.client_id ->
      Cohttp.Request.t ->
      Conduit_lwt_unix.flow ->
      (links_websocket * Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

    (** Sends a message to the given PID.
     * The string is a JSONised value -- should abstract this furhter *)
    val send_message :
      links_websocket ->
      ProcessTypes.process_id ->
      string ->
      unit
  end

module Mailbox :
sig
  val pop_message_for : ProcessTypes.process_id -> Value.t option
  val pop_all_messages_for :
    ProcessTypes.client_id -> ProcessTypes.process_id-> Value.t list
  val pop_message : unit -> Value.t option
  val send_client_message :
    Value.t ->
    ProcessTypes.client_id ->
    ProcessTypes.process_id ->
    client_send_result 
  val send_server_message : Value.t -> ProcessTypes.process_id -> unit
end

exception UnknownProcessID of ProcessTypes.process_id
exception UnknownClientID of ProcessTypes.client_id

module Session :
sig
  type apid = int
  type portid = int
  type pid = int
  type chan = portid * portid

  val new_access_point : unit -> apid
  val accept : apid -> chan * bool
  val request : apid -> chan * bool

  val block : portid -> pid -> unit
  val unblock : portid -> pid option

  val send : Value.t -> portid -> unit
  val receive : portid -> Value.t option

  val link : chan -> chan -> unit

  val unbox_port : Value.t -> portid
  val unbox_chan' : Value.t -> int * int
  val unbox_chan : Value.t -> chan
end
