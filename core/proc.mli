(** Process management *)
open ProcessTypes

type abort_type = string * string
exception Aborted of abort_type  (* This sucks *)

module Proc :
sig
  type thread_result = (Value.env * Value.t)
  type thread = unit -> thread_result Lwt.t

  val debug_process_status : unit -> unit

  val get_current_pid : unit -> process_id

  (* val lookup_client_process : client_id -> process_id -> Value.t option *)

  val create_process : bool -> thread -> process_id Lwt.t
  val create_client_process : client_id -> Value.t ->
    process_id Lwt.t

  val create_spawnwait_process : process_id -> thread -> process_id Lwt.t

  val get_spawnwait_result : process_id -> Value.t option

  val get_and_mark_pending_processes : client_id -> (process_id * Value.t) list

  val resolve_external_processes : Value.t -> unit

  val awaken : process_id -> unit

  val finish : Value.env * Value.t -> thread_result Lwt.t
  val yield : thread -> thread_result Lwt.t
  val block : thread -> thread_result Lwt.t
  val abort : abort_type -> thread_result Lwt.t

  val atomically : thread -> Value.t Lwt.t

  val singlethreaded : unit -> bool (* Exposed to prevent client calls from killing server-side threads... *)

  val start : (unit -> 'a Lwt.t) -> 'a

  val run : (unit -> 'a Lwt.t) -> 'a Lwt.t
end

(* Operations on websockets used to send and receive messages remotely. *)
module type WEBSOCKETS =
  sig
    (** Accepts a new websocket connection, creates a new socket, as
     * well as a thread which handles incoming messages. *)
    val accept :
      client_id ->
      Cohttp.Request.t ->
      Cohttp_lwt_unix.Server.response_action Lwt.t

    (** Sends a message to the given PID. *)
    val deliver_process_message :
      client_id ->
      process_id ->
      Value.t ->
      unit Lwt.t

    (** Sends a response to an AP request / accept *)
    val send_ap_response :
      client_id ->
      process_id ->
      Value.chan ->
      unit Lwt.t

    (** Delivers a message along a session channel *)
    val deliver_session_message :
      client_id ->
      channel_id ->
      Value.delegated_chan list ->
      Value.t ->
      unit Lwt.t

  (** Send a cancellation notification *)
  val send_cancellation :
      client_id ->
      notify_ep:channel_id ->
      cancelled_ep:channel_id ->
      unit Lwt.t
  end

module type MAILBOX =
sig
  val pop_message_for : process_id -> Value.t option
  val pop_all_messages_for :
    client_id -> process_id-> Value.t list
  val pop_message : unit -> Value.t option

  val send_client_message :
    Value.t ->
    client_id ->
    process_id ->
    unit Lwt.t

  val send_server_message : Value.t -> process_id -> unit
end


exception UnknownProcessID of process_id
exception UnknownClientID of client_id

module type SESSION =
sig
  type chan = Value.chan
  val receive_port : chan -> channel_id
  val send_port : chan -> channel_id

  val cancel : chan -> unit Lwt.t
  val is_endpoint_cancelled : channel_id -> bool

  type send_result = SendOK | SendPartnerCancelled
  type receive_result = ReceiveOK of Value.t | ReceiveBlocked | ReceivePartnerCancelled

  val new_server_access_point : unit -> apid
  val new_client_access_point : client_id -> apid

  val get_and_mark_pending_aps : client_id -> apid list

  val accept : apid -> (chan * bool) Lwt.t
  val request : apid -> (chan * bool) Lwt.t
  val ap_request_from_client : client_id -> process_id -> apid -> unit Lwt.t
  val ap_accept_from_client : client_id -> process_id -> apid -> unit Lwt.t

  val block : channel_id -> process_id -> unit
  val unblock : channel_id -> process_id option

  val send_from_server : Value.t -> channel_id -> send_result Lwt.t
  val send_from_client :
    Value.t -> channel_id -> send_result Lwt.t

  val receive : chan -> receive_result

  val handle_remote_cancel :
    notify_ep:channel_id -> cancelled_ep:channel_id -> unit Lwt.t

  val link : chan -> chan -> unit

  val cancel_client_channels : client_id -> unit Lwt.t
  val register_client_channel : client_id -> chan -> unit
  val register_server_channel : chan -> unit

  val get_buffer : channel_id -> (Value.t list) option

  val close : chan -> unit

end

module rec Websockets : WEBSOCKETS
and Mailbox : MAILBOX
and Session : SESSION
