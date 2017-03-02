(** Data structures/utilities for process management *)
open Utility
open Lwt
open ProcessTypes

type abort_type = string * string
exception Aborted of abort_type

module Proc =
struct
  let main_process_pid = ProcessTypes.main_process_pid
  let main_running = ref true

  (*
  (** A process state is a pair of a continuation and a value; running
    the process means applying the continuation to the value. *)
  type proc_state = Value.continuation * Value.t
  *)

  type thread_result = (Value.env * Value.t)
  type thread = unit -> thread_result Lwt.t (* Thunked to avoid changing evalir.  Because I'm laaaaaaaaaaaaazy. *)

  type scheduler_state =
      { blocked : (process_id, unit Lwt.u) Hashtbl.t;
        client_processes : (process_id, Value.t * bool) Hashtbl.t;
        angels : (process_id, unit Lwt.t) Hashtbl.t;
        step_counter : int ref }

  let state = {
    blocked          = Hashtbl.create 10000;
    client_processes = Hashtbl.create 10000;
    angels           = Hashtbl.create 10000;
    step_counter     = ref 0 }

  let atomic : bool ref = ref false

  (** Test that there is only one thread total (the running one)? *)
  (* todo: broken *)
  let singlethreaded () =
    Hashtbl.length state.blocked == 0 (* &&
      Queue.length state.suspended == 0 *)

  (** Dump the number of running and blocked processes to [stderr]. *)
  let debug_process_status () =
    (*
    prerr_endline("running processes : " ^
                    string_of_int (Queue.length state.suspended));
     *)
    prerr_endline ("blocked processes : " ^
                     string_of_int (Hashtbl.length state.blocked))

    (** retrieve the body of a client process (for transmission to the
      client if it hasn't already been) *)
  let lookup_client_process pid =
    let v, active =
      try Hashtbl.find state.client_processes pid with
      | NotFound pid ->
        failwith ("Missing client process: " ^ pid) in
    if active then
      None
    else
      begin
        Hashtbl.replace state.client_processes pid (v, true);
        Some v
      end

  let current_pid_key = Lwt.new_key ()
  let angel_done = Lwt.new_key ()

  (** Return the identifier of the running process. *)
  let get_current_pid () =
    match Lwt.get current_pid_key with
    | None -> assert false
    | Some pid -> pid

  (** Awaken (unblock) a process:
    Move it from the blocked state to the runnable queue ([suspended]).
    Ignores if the process does not exist.
   *)
  let awaken pid =
    match Hashtbl.lookup state.blocked pid with
    | None -> () (* process not blocked *)
    | Some doorbell ->
        Debug.print ("Awakening blocked process " ^ (ProcessID.to_string pid));
      Hashtbl.remove state.blocked pid;
      Lwt.wakeup doorbell ()

  (** Increment the scheduler's step counter and return the new value. *)
  let count_step () =
    incr state.step_counter;
    !(state.step_counter)

  (** Reset (set to 0) the scheduler's step counter. *)
  let reset_step_counter() = state.step_counter := 0

  let switch_granularity = 100

  (** If the current process has executed for than switch_granularity, suspend the current process
      and execute something else.
   *)
  let yield pstate =
    let step_ctr = count_step () in
    if not !atomic && step_ctr mod switch_granularity == 0 then
      begin
        (* Debug.print ("yielding"); *)
        reset_step_counter ();
        Lwt_main.yield () >>= pstate
      end
    else
      pstate ()

  (** Block the current process, given its state. It will not be
      eligible to run until its [awake]d. Note we don't expose a way of
      blocking suspended (i.e. runnable) processes *)
  let block pstate =
    let pid = get_current_pid () in
    Debug.print ("Blocking process " ^ (ProcessID.to_string pid));
    let (t, u) = Lwt.wait () in
    Hashtbl.add state.blocked pid u;
    t >>= pstate

  (** Given a process state, create a new process and return its identifier. *)
  let create_process angel pstate =
    (* Unsafe is fine -- we're running in atomic mode *)
    let new_pid = ProcessID.create_unsafe () in
    if angel then
      begin
        let (t, w) = Lwt.task () in
        Hashtbl.add state.angels new_pid t;
        async (fun () -> Lwt.with_value current_pid_key (Some new_pid)
                          (fun () -> Lwt.with_value angel_done (Some w) pstate))
      end
    else
      async (fun () -> Lwt.with_value current_pid_key (Some new_pid) pstate);
    new_pid

  (** Create a new client process and return its identifier *)
  let create_client_process func =
    let new_pid = ProcessID.create_unsafe () in
    Hashtbl.add state.client_processes new_pid (func, false);
    new_pid

  let finish r =
    let pid = get_current_pid () in
    (* This process is only actually finished if we're not executing atomically *)
    if not (!atomic) then
      Debug.print ("Finishing process " ^ (ProcessID.to_string pid));
    if pid == main_process_pid then
      main_running := false
    else if Hashtbl.mem state.angels pid then
      begin
        let w = match Lwt.get angel_done with
          | None -> assert false
          | Some w -> w in
        Lwt.wakeup w ();
        Hashtbl.remove state.angels pid
      end;
    Lwt.return r

  let abort v =
    let pid = get_current_pid () in
    (* This process is only actually finished if we're not executing atomically *)
    if not (!atomic) then
      Debug.print ("Finishing process " ^ (ProcessID.to_string pid));
    if pid == main_process_pid then
      main_running := false
    else if Hashtbl.mem state.angels pid then
      begin
        let w = match Lwt.get angel_done with
          | None -> assert false
          | Some w -> w in
        Lwt.wakeup w ();
        Hashtbl.remove state.angels pid
      end;
    Lwt.fail (Aborted v)

  let run' pfun =
   Lwt_main.run (Lwt.with_value current_pid_key (Some main_process_pid) pfun >>= fun r ->
                 (if not !atomic then
                    Lwt.join (Hashtbl.fold (fun _ t ts -> t :: ts) state.angels [])
                  else
                    Lwt.return ()) >>= fun _ ->
                 Lwt.return r)

  let run pfun =
    reset_step_counter ();
    run' pfun

  let atomically pfun =
    let previously_atomic = !atomic in
    atomic := true;
    let v = run' pfun in
    atomic := previously_atomic;
    snd v
end

exception UnknownProcessID of ProcessTypes.process_id

module Mailbox =
struct
  let message_queues : (process_id, Value.t Queue.t) Hashtbl.t = Hashtbl.create 10000

  (* Create the main process's message queue *)
  let _ = Hashtbl.add message_queues ProcessTypes.main_process_pid (Queue.create ())

  (** Given a PID, return its next queued message (under [Some]) or [None]. *)
  let pop_message_for pid =
    let mqueue =
      match Hashtbl.lookup message_queues pid with
      | Some mqueue -> mqueue
      | None ->
        let mqueue = Queue.create () in
        let _ = Hashtbl.add message_queues pid mqueue in
        mqueue in
    if not (Queue.is_empty mqueue) then
      Some (Queue.pop mqueue)
    else
      None

  (** Pop a message for the current process. *)
  let pop_message () = pop_message_for (Proc.get_current_pid ())

  (** extract an entire message queue (used in transporting messages
      to the client) *)
  let pop_all_messages_for pid =
    match Hashtbl.lookup message_queues pid with
    | Some mqueue ->
      Hashtbl.remove message_queues pid;
      List.rev (Queue.fold (fun xs x -> x :: xs) [] mqueue)
    | None        -> []

  (** Send a message to the identified process. Raises [UnknownProcessID pid]
    if the given [pid] does not exist (does not have a message queue). *)
  let send_message msg pid =
    match Hashtbl.lookup message_queues pid with
    | Some mqueue -> Queue.push msg mqueue
    | None ->
      let mqueue = Queue.create () in
      Queue.push msg mqueue;
      Hashtbl.add message_queues pid mqueue
end

module Session = struct
  type chan = Value.chan

  type ap_state = Balanced | Accepting of chan list | Requesting of chan list

  let flip_chan (outp, inp) = (inp, outp)

  let access_points = (Hashtbl.create 10000 : (apid, ap_state) Hashtbl.t)

  let buffers = (Hashtbl.create 10000 : (channel_id, Value.t Queue.t) Hashtbl.t)
  let blocked = (Hashtbl.create 10000 : (channel_id, process_id) Hashtbl.t)
  let forward = (Hashtbl.create 10000 : (channel_id, channel_id Unionfind.point) Hashtbl.t)

  let fresh_chan () =
    let outp = ChannelID.create_unsafe () in
    let inp = ChannelID.create_unsafe () in
      (outp, inp)

  let new_channel () =
    let (outp, inp) as c = fresh_chan () in
      Hashtbl.add buffers outp (Queue.create ());
      Hashtbl.add buffers inp (Queue.create ());
      Hashtbl.add forward outp (Unionfind.fresh outp);
      Hashtbl.add forward inp (Unionfind.fresh inp);
      c

  let new_access_point () =
    let apid = AccessPointID.create_unsafe () in
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
        | Requesting []        -> assert false (* TODO: check that this is impossible *)
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
        | Accepting []        -> assert false (* TODO: check that this is impossible *)
      in
        Hashtbl.replace access_points apid state';
        flip_chan c, blocked

  let find_active p =
    Unionfind.find (Hashtbl.find forward p)

  let forward inp outp =
    Unionfind.union (Hashtbl.find forward inp) (Hashtbl.find forward outp)

  let block channel_id pid =
    let channel_id = find_active channel_id in
      Hashtbl.add blocked channel_id pid
  let unblock channel_id =
    let channel_id = find_active channel_id in
      if Hashtbl.mem blocked channel_id then
        begin
          let pid = Hashtbl.find blocked channel_id in
            Hashtbl.remove blocked channel_id;
            Some pid
        end
      else
        None

  let send msg p =
    (* Debug.print ("Sending along: " ^ string_of_int p); *)
    let p = find_active p in
      Queue.push msg (Hashtbl.find buffers p)

  let receive p =
    (* Debug.print ("Receiving on: " ^ string_of_int p); *)
    let p = find_active p in
    let buf = Hashtbl.find buffers p in
      if not (Queue.is_empty buf) then
        Some (Queue.pop buf)
      else
        None

  let link (out1, in1) (out2, in2) =
    let out1 = find_active out1 in
    let in1 = find_active in1 in
    let out2 = find_active out2 in
    let in2 = find_active in2 in
      Queue.transfer (Hashtbl.find buffers in1) (Hashtbl.find buffers out2);
      Queue.transfer (Hashtbl.find buffers in2) (Hashtbl.find buffers out1);
      forward in1 out2;
      forward in2 out1
end
