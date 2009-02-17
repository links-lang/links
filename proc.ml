(** Data structures/utilities for process management *)

type pid = int

let main_process_pid = 0

type proc_state = Value.continuation * Value.t 

type scheduler_state =
  { suspended : (proc_state * pid) Queue.t;
    blocked : (pid, proc_state * pid) Hashtbl.t;
    message_queues : (pid, Value.t Queue.t) Hashtbl.t;
    current_pid : pid ref;
    step_counter : int ref;
    atomic : bool ref
  }

let state = {
  suspended      = Queue.create ();
  blocked        = Hashtbl.create 10000;
  message_queues = Hashtbl.create 10000;
  current_pid    = ref 0;
  step_counter   = ref 0;
  atomic         = ref false;
}

let _ = Hashtbl.add state.message_queues 0 (Queue.create ())

let singlethreaded () = 
  Hashtbl.length state.blocked == 0 &&
    Queue.length state.suspended == 0

let debug_process_status () =
  prerr_endline("running processes : " ^ 
                  string_of_int (Queue.length state.suspended));
  prerr_endline ("blocked processes : " ^ 
                   string_of_int (Hashtbl.length state.blocked))

let fresh_pid =
  let current_pid = (ref 0 : pid ref) in
    fun () -> 
      begin
	incr current_pid;
	!current_pid
      end

let string_of_pid = string_of_int

let pop_message_for pid = 
  let mqueue = Hashtbl.find state.message_queues pid in
    if not (Queue.is_empty mqueue) then
      Some (Queue.pop mqueue)
    else
      None

let pop_message() = pop_message_for !(state.current_pid)

exception UnknownProcessID of pid

let send_message msg pid = 
  try 
    Queue.push msg (Hashtbl.find state.message_queues pid)
  with Notfound.NotFound _ -> raise (UnknownProcessID pid)

let awaken pid =
  try
    Queue.push (Hashtbl.find state.blocked pid) state.suspended;
    Hashtbl.remove state.blocked pid
  with Notfound.NotFound _ -> ()

let suspend pstate pid =
  Queue.push (pstate, pid) state.suspended

let suspend_current pstate =
  suspend pstate !(state.current_pid)

let block pid pstate =
  Hashtbl.add state.blocked pid (pstate, pid)

let block_current pstate =
  block !(state.current_pid) pstate

let activate pid =
  state.current_pid := pid

let get_current_pid () = !(state.current_pid)

let create_process pstate = 
  let new_pid = fresh_pid () in
    Hashtbl.add state.message_queues new_pid (Queue.create ());
    Queue.push (pstate, new_pid) state.suspended;
    new_pid
      
let pop_ready_proc() =
  if not (Queue.is_empty state.suspended) then 
    Some (Queue.pop state.suspended)
  else None

let count_step () =
  incr state.step_counter;
  !(state.step_counter)

let reset_step_counter() = state.step_counter := 0

let set_atomic b = state.atomic := b
let get_atomic () = !(state.atomic)

let is_main pid = pid == main_process_pid
let current_is_main() = !(state.current_pid) == main_process_pid
