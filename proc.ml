(** Data structures/utilities for process management *)

open Notfound 

(** The abstract type of process identifiers *)
type pid = int

let main_process_pid = 0

(** A process state is a pair of a continuation and a value; running
    the process means applying the continuation to the value. *)
type proc_state = Value.continuation * Value.t 

type scheduler_state =
    { suspended : (proc_state * pid) Queue.t;
      blocked : (pid, proc_state * pid) Hashtbl.t;
      message_queues : (pid, Value.t Queue.t) Hashtbl.t;
      current_pid : pid ref;
      step_counter : int ref
    }

let state = {
  suspended      = Queue.create ();
  blocked        = Hashtbl.create 10000;
  message_queues = Hashtbl.create 10000;
  current_pid    = ref 0;
  step_counter   = ref 0;
}

(* Create the main process's message queue *)
let _ = Hashtbl.add state.message_queues 0 (Queue.create ())

(** Test that there is only one thread total (the running one)? *)
let singlethreaded () = 
  Hashtbl.length state.blocked == 0 &&
    Queue.length state.suspended == 0

(** Dump the number of running and blocked processes to [stderr]. *)
let debug_process_status () =
  prerr_endline("running processes : " ^ 
                  string_of_int (Queue.length state.suspended));
  prerr_endline ("blocked processes : " ^ 
                   string_of_int (Hashtbl.length state.blocked))

(** [fresh_pid()] returns a new globally-fresh process ID. 
    Proposal: if server-spawned processes are ever implemented;
    server-spawned processes have even PIDs, client-spawned ones have
    odd PIDs.
*)
let fresh_pid =
  let current_pid = (ref 0 : pid ref) in
    fun () -> 
      begin
	incr current_pid;
	!current_pid
      end

(** Convert a PID into a string. *)
let string_of_pid = string_of_int

(** Given a PID, return its next queued message (under [Some]) or [None]. *)
let pop_message_for pid = 
  let mqueue = Hashtbl.find state.message_queues pid in
    if not (Queue.is_empty mqueue) then
      Some (Queue.pop mqueue)
    else
      None

(** Pop a message for the current process. *)
let pop_message() = pop_message_for !(state.current_pid)

exception UnknownProcessID of pid

(** Send a message to the identified process. Raises [UnknownProcessID pid]
    if the given [pid] does not exist (does not have a message queue). *)
let send_message msg pid = 
  try 
    Queue.push msg (Hashtbl.find state.message_queues pid)
  with Notfound.NotFound _ -> raise (UnknownProcessID pid)

(** Awaken (unblock) a process: 
    Move it from the blocked state to the runnable queue ([suspended]).
    Ignores if the process does not exist.
*)
let awaken pid =
  try
    Queue.push (Hashtbl.find state.blocked pid) state.suspended;
    Hashtbl.remove state.blocked pid
  with Notfound.NotFound _ -> ()

(** Suspend the current (running) process given its state. This means
    putting it on the [suspended] queue.
*)
let suspend_current pstate =
  Queue.push (pstate, !(state.current_pid)) state.suspended

(** Block the current process, given its state. It will not be
    eligible to run until its [awake]d. Note we don't expose a way of
    blocking suspended (i.e. runnable) processes *)
let block_current pstate =
  let pid = !(state.current_pid) in
  Hashtbl.add state.blocked pid (pstate, pid)

(** Make the given process the active one. Assumes you have already 
    blocked/suspended the previously-active process. *)
let activate pid =
  state.current_pid := pid

(** Return the identifier of the running process. *)
let get_current_pid () = !(state.current_pid)

(** Given a process state, create a new process and return its identifier. *)
let create_process pstate = 
  let new_pid = fresh_pid () in
    Hashtbl.add state.message_queues new_pid (Queue.create ());
    Queue.push (pstate, new_pid) state.suspended;
    new_pid
      
(** If there is any ready (runnable/suspended) process, remove it from
    the suspended queue and return it (under [Some]). Otherwise return
    [None]. *)
let pop_ready_proc() =
  if not (Queue.is_empty state.suspended) then 
    Some (Queue.pop state.suspended)
  else None

(** Increment the scheduler's step counter and return the new value. *)
let count_step () =
  incr state.step_counter;
  !(state.step_counter)

(** Reset (set to 0) the scheduler's step counter. *)
let reset_step_counter() = state.step_counter := 0

let is_main pid = pid == main_process_pid
(** Is the current process is the main process? *)
let current_is_main() = !(state.current_pid) == main_process_pid
