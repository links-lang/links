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

  val lookup_client_process : process_id -> Value.t option

  val create_process : bool -> thread -> process_id
  val create_client_process : Value.t -> process_id
  val awaken : process_id -> unit

  val finish : Value.env * Value.t -> thread_result Lwt.t
  val yield : thread -> thread_result Lwt.t
  val block : thread -> thread_result Lwt.t
  val abort : abort_type -> thread_result Lwt.t

  val atomically : thread -> Value.t

  val singlethreaded : unit -> bool (* Exposed to prevent client calls from killing server-side threads... *)

  val run : (unit -> 'a Lwt.t) -> 'a
end

module Mailbox :
sig
  val pop_message_for : process_id -> Value.t option
  val pop_all_messages_for : process_id -> Value.t list
  val pop_message : unit -> Value.t option
  val send_message : Value.t -> process_id -> unit
end

exception UnknownProcessID of process_id

module Session :
sig
  type chan = Value.chan

  val new_access_point : unit -> apid
  val accept : apid -> chan * bool
  val request : apid -> chan * bool

  val block : channel_id -> process_id -> unit
  val unblock : channel_id -> process_id option

  val send : Value.t -> channel_id -> unit
  val receive : channel_id -> Value.t option

  val link : chan -> chan -> unit
end
