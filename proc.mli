(** Process management *)
module Proc :
sig
  type pid = int (* leaky abstraction what *)
  type thread_result = Value.env * Value.t
  type thread = unit -> thread_result Lwt.t

  val debug_process_status : unit -> unit

  val string_of_pid : pid -> string
  val get_current_pid : unit -> pid

  val lookup_client_process : pid -> Value.t option

  val create_process : bool -> thread -> pid
  val create_client_process : Value.t -> pid
  val awaken : pid -> unit

  val finish : thread_result -> thread_result Lwt.t
  val yield : thread -> thread_result Lwt.t
  val block : thread -> thread_result Lwt.t

  val atomically : thread -> Value.t

  val singlethreaded : unit -> bool (* Exposed to prevent client calls from killing server-side threads... *)

  val run : thread -> Value.env * Value.t
end

module Mailbox :
sig
  val pop_message_for : Proc.pid -> Value.t option
  val pop_all_messages_for : Proc.pid -> Value.t list
  val pop_message : unit -> Value.t option
  val send_message : Value.t -> Proc.pid -> unit
end

exception UnknownProcessID of Proc.pid

module Session :
sig
  type apid = int
  type portid = int
  type chan = portid * portid

  val new_access_point : unit -> apid
  val accept : apid -> chan * bool
  val request : apid -> chan * bool

  val block : portid -> Proc.pid -> unit
  val unblock : portid -> Proc.pid option

  val send : Value.t -> portid -> unit
  val receive : portid -> Value.t option

  val fuse : chan -> chan -> unit

  val unbox_port : Value.t -> portid
  val unbox_chan' : Value.t -> int * int
  val unbox_chan : Value.t -> chan
end
