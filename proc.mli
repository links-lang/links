(** Process management *)

type abort_type = string * string
exception Aborted of abort_type  (* This sucks *)

module Proc :
sig
  (* TODO: Can we make these properly abstract? *)
  type pid = int
  type client_id = int

  type thread_result = (Value.env * Value.t)
  type thread = unit -> thread_result Lwt.t

  val debug_process_status : unit -> unit

  val string_of_pid : pid -> string
  val get_current_pid : unit -> pid

  val lookup_client_process : client_id -> pid -> Value.t option

  val create_process : bool -> thread -> pid
  val create_client_process : client_id -> Value.t -> pid
  val awaken : pid -> unit

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
    val register_websocket : Proc.client_id -> WebsocketOperations.links_websocket -> unit
  end

module Mailbox :
sig
  val pop_message_for : Proc.pid -> Value.t option
  val pop_all_messages_for : Proc.client_id -> Proc.pid -> Value.t list
  val pop_message : unit -> Value.t option
  val send_client_message : Value.t -> Proc.client_id -> Proc.pid -> unit
  val send_server_message : Value.t -> Proc.pid -> unit
end

exception UnknownProcessID of Proc.pid
exception UnknownClientID of Proc.client_id

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

  val link : chan -> chan -> unit

  val unbox_port : Value.t -> portid
  val unbox_chan' : Value.t -> int * int
  val unbox_chan : Value.t -> chan
end
