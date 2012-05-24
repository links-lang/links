(** Process management *)
type pid (* FIXME: don't reveal impl type of `pid' *) = int
type proc_state = Value.continuation * Value.t

type scheduler_state =
  { suspended : (proc_state * pid) Queue.t;
    blocked : (pid, proc_state * pid) Hashtbl.t;
    message_queues : (pid, Value.t Queue.t) Hashtbl.t;
    current_pid : pid ref;
    step_counter : int ref}

val debug_process_status : unit -> unit
val singlethreaded : unit -> bool
val fresh_pid : unit -> pid
val string_of_pid : pid -> string
val pop_message_for : pid -> Value.t option
val pop_message : unit -> Value.t option
val pop_ready_proc : unit -> (proc_state * pid) option
val activate : pid -> unit
val send_message : Value.t -> pid -> unit
val awaken : pid -> unit
val get_current_pid : unit -> pid
val create_process : proc_state -> pid
val suspend_current : proc_state -> unit
val block_current : proc_state -> unit

val count_step : unit -> int
val reset_step_counter : unit -> unit

val is_main : pid -> bool
val current_is_main : unit -> bool

exception UnknownProcessID of pid
