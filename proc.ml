(** Data structures/utilities for process management *)
open Notfound
open Utility

module Proc =
struct

  (** The abstract type of process identifiers *)
  type pid = int

  let main_process_pid = 0

  let main_running = ref true

  (*
  (** A process state is a pair of a continuation and a value; running
    the process means applying the continuation to the value. *)
  type proc_state = Value.continuation * Value.t
  *)

  type thread = unit -> unit

  type scheduler_state =
      { suspended : (thread * pid) Queue.t;
        blocked : (pid, thread) Hashtbl.t;
        angels : (pid, unit) Hashtbl.t;
        current_pid : pid ref;
        step_counter : int ref;
      }

  let state = {
    suspended      = Queue.create ();
    blocked        = Hashtbl.create 10000;
    angels         = Hashtbl.create 10000;
    current_pid    = ref 0;
    step_counter   = ref 0;
  }

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

  (** Awaken (unblock) a process:
    Move it from the blocked state to the runnable queue ([suspended]).
    Ignores if the process does not exist.
   *)
  let awaken pid =
    try
      Debug.print ("Awakening process: " ^ string_of_int pid);
      Queue.push (Hashtbl.find state.blocked pid, pid) state.suspended;
      Hashtbl.remove state.blocked pid
    with Notfound.NotFound _ ->
      Debug.print ("Attempt to awaken non existent process");
      ()

  (** If there is any ready (runnable/suspended) process, remove it from
      the suspended queue and return it (under [Some]). Otherwise return
      [None]. *)
  let pop_ready_proc() =
    if not (Queue.is_empty state.suspended) then
      Some (Queue.pop state.suspended)
    else None

  (** Make the given process the active one. Assumes you have already
      blocked/suspended the previously-active process. *)
  let activate pid =
    state.current_pid := pid

  (** Increment the scheduler's step counter and return the new value. *)
  let count_step () =
    incr state.step_counter;
    !(state.step_counter)

  (** Reset (set to 0) the scheduler's step counter. *)
  let reset_step_counter() = state.step_counter := 0

  let switch_granularity = 5
  let atomic = ref false
  let result : (Value.env * Value.t) option ref = ref None

  let atomically pfun =
    let previously_atomic = !atomic in
    let previous_result = !result in
    atomic := true;
    pfun ();
    atomic := previously_atomic;
    match !result with
    | None -> assert false
    | Some (_, v) -> result := previous_result; v

  let active_main () = !main_running

  let active_angels () =
    Hashtbl.length state.angels > 0

  let switch_context () =
    match pop_ready_proc () with
    | Some (pstate, pid) ->
       begin
         activate pid;
         pstate ()
       end
    | None ->
       failwith ("Deadlock")

  (** If the current process has executed for than switch_granularity, suspend the current process
      and execute something else.
   *)
  let yield pstate =
    if !atomic || singlethreaded () then pstate ()
    else
      let step_ctr = count_step () in
      if step_ctr mod switch_granularity == 0 then
        begin
          reset_step_counter ();
          Queue.push (pstate, !(state.current_pid)) state.suspended;
          switch_context ()
        end
      else
        pstate ()

  (** Block the current process, given its state. It will not be
      eligible to run until its [awake]d. Note we don't expose a way of
      blocking suspended (i.e. runnable) processes *)
  let block pstate =
    let pid = !(state.current_pid) in
    Hashtbl.add state.blocked pid pstate;
    switch_context ()

  (** Return the identifier of the running process. *)
  let get_current_pid () = !(state.current_pid)

  (** Given a process state, create a new process and return its identifier. *)
  let create_process angel pstate =
    let new_pid = fresh_pid () in
      if angel then Hashtbl.add state.angels new_pid ();
      Queue.push (pstate, new_pid) state.suspended;
      new_pid

  let finish r =
    let pid = get_current_pid () in
    Debug.print ("Finishing thread " ^ string_of_int pid);
    if pid == main_process_pid || !atomic then
      result := Some r;
    if pid == main_process_pid then
      main_running := false
    else if Hashtbl.mem state.angels pid then
      Hashtbl.remove state.angels pid;
    if not !atomic && (active_main () || active_angels ()) then
      switch_context ()
    else
      ()

  let is_main pid = pid == main_process_pid
  (** Is the current process is the main process? *)
  let current_is_main() = !(state.current_pid) == main_process_pid

  let run pfun =
    state.current_pid := main_process_pid;
    state.step_counter := 0;
    main_running := true;
    pfun ();
    match !result with
    | None -> assert false
    | Some r -> r
end

exception UnknownProcessID of Proc.pid

module Mailbox =
struct
  let message_queues : (Proc.pid, Value.t Queue.t) Hashtbl.t = Hashtbl.create 10000

  (* Create the main process's message queue *)
  let _ = Hashtbl.add message_queues 0 (Queue.create ())

  (** Given a PID, return its next queued message (under [Some]) or [None]. *)
  let pop_message_for pid =
    let mqueue =
      try
        Hashtbl.find message_queues pid
      with
        Notfound.NotFound _ ->
        begin
          let mqueue = Queue.create () in
          let _ = Hashtbl.add message_queues pid mqueue in
          mqueue
        end in
    if not (Queue.is_empty mqueue) then
      Some (Queue.pop mqueue)
    else
      None

  (** Pop a message for the current process. *)
  let pop_message () = pop_message_for (Proc.get_current_pid ())

  (** Send a message to the identified process. Raises [UnknownProcessID pid]
    if the given [pid] does not exist (does not have a message queue). *)
  let send_message msg pid =
    try
      Queue.push msg (Hashtbl.find message_queues pid)
    with Notfound.NotFound _ ->
      begin
        let mqueue = Queue.create () in
        Queue.push msg mqueue;
        Hashtbl.add message_queues pid mqueue
      end
end

module Session = struct
  type apid = int              (* access point id *)
  type portid = int
  type pid = int               (* process id *)
  type chan = portid * portid  (* a channel is a pair of ports *)

  type ap_state = Balanced | Accepting of chan list | Requesting of chan list

  let flip_chan (outp, inp) = (inp, outp)

  let access_points = (Hashtbl.create 10000 : (apid, ap_state) Hashtbl.t)

  let buffers = (Hashtbl.create 10000 : (portid, Value.t Queue.t) Hashtbl.t)
  let blocked = (Hashtbl.create 10000 : (portid, pid) Hashtbl.t)
  let forward = (Hashtbl.create 10000 : (portid, portid Unionfind.point) Hashtbl.t)

  let generator () =
    let i = ref 0 in
      fun () -> incr i; !i

  let fresh_apid = generator ()
  let fresh_portid = generator ()
  let fresh_chan () =
    let outp = fresh_portid () in
    let inp = fresh_portid () in
      (outp, inp)

  let new_channel () =
    let (outp, inp) as c = fresh_chan () in
      Hashtbl.add buffers outp (Queue.create ());
      Hashtbl.add buffers inp (Queue.create ());
      Hashtbl.add forward outp (Unionfind.fresh outp);
      Hashtbl.add forward inp (Unionfind.fresh inp);
      c

  let new_access_point () =
    let apid = fresh_apid () in
      Hashtbl.add access_points apid Balanced;
      apid

  let accept : apid -> chan * bool =
    fun apid ->
      let state = Hashtbl.find access_points apid in
      let (c, state', blocked) =
        match state with
        | Balanced             -> let c = new_channel () in (c, Accepting [c], true)
        | Accepting cs         -> let c = new_channel () in (c, Accepting (cs @ [c]), true)
        | Requesting [c]       -> (c, Balanced, false)
        | Requesting (c :: cs) -> (c, Requesting cs, false)
      in
        Hashtbl.replace access_points apid state';
        c, blocked

  let request : apid -> chan * bool =
    fun apid ->
      let state = Hashtbl.find access_points apid in
      let (c, state', blocked) =
        match state with
        | Balanced            -> let c = new_channel () in (c, Requesting [c], true)
        | Requesting cs       -> let c = new_channel () in (c, Requesting (cs @ [c]), true)
        | Accepting [c]       -> (c, Balanced, false)
        | Accepting (c :: cs) -> (c, Accepting cs, false)
      in
        Hashtbl.replace access_points apid state';
        flip_chan c, blocked

  let rec find_active p =
    Unionfind.find (Hashtbl.find forward p)

  let forward inp outp =
    Unionfind.union (Hashtbl.find forward inp) (Hashtbl.find forward outp)

  let block portid pid =
    let portid = find_active portid in
      Hashtbl.add blocked portid pid
  let rec unblock portid =
    let portid = find_active portid in
      if Hashtbl.mem blocked portid then
        begin
          let pid = Hashtbl.find blocked portid in
            Hashtbl.remove blocked portid;
            Some pid
        end
      else
        None

  let rec send msg p =
    (* Debug.print ("Sending along: " ^ string_of_int p); *)
    let p = find_active p in
      Queue.push msg (Hashtbl.find buffers p)

  let rec receive p =
    (* Debug.print ("Receiving on: " ^ string_of_int p); *)
    let p = find_active p in
    let buf = Hashtbl.find buffers p in
      if not (Queue.is_empty buf) then
        Some (Queue.pop buf)
      else
        None

  let fuse (out1, in1) (out2, in2) =
    let out1 = find_active out1 in
    let in1 = find_active in1 in
    let out2 = find_active out2 in
    let in2 = find_active in2 in
      Queue.transfer (Hashtbl.find buffers in1) (Hashtbl.find buffers out2);
      Queue.transfer (Hashtbl.find buffers in2) (Hashtbl.find buffers out1);
      forward in1 out2;
      forward in2 out1

  let unbox_port = Num.int_of_num -<- Value.unbox_int
  let unbox_chan' chan =
    let (outp, inp) = Value.unbox_pair chan in
      (Value.unbox_int outp, Value.unbox_int inp)
  let unbox_chan chan =
    let (outp, inp) = Value.unbox_pair chan in
      (unbox_port outp, unbox_port inp)
end
