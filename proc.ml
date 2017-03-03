(** Data structures/utilities for process management *)
open Utility
open Lwt
open ProcessTypes

type abort_type = string * string
exception Aborted of abort_type

module Proc =
struct
  let main_process_pid = main_process_pid
  let main_running = ref true

  (*
  (** A process state is a pair of a continuation and a value; running
    the process means applying the continuation to the value. *)
  type proc_state = Value.continuation * Value.t
  *)

  type thread_result = (Value.env * Value.t)
  type thread = unit -> thread_result Lwt.t (* Thunked to avoid changing evalir.  Because I'm laaaaaaaaaaaaazy. *)

  (* A map from process IDs to (Value.t, bool), where Value.t is the function to run,
   * and bool is whether or not the process is active *)
  type client_proc_map = (Value.t * bool) pid_map

  type process_lookup_res = [
      | `ClientNotFound
      | `ProcessNotFound
      | `DeployedProcessFound (* Process found, resides on client *)
      | `UndeployedProcessFound (* Process found, not yet sent to client *)
    ]

  type scheduler_state =
      { blocked : (process_id, unit Lwt.u) Hashtbl.t;
        client_processes : (client_id, client_proc_map) Hashtbl.t;
        (* external_processes maps client IDs to a set of processes spawned by the client.
         * We don't have values to store for these, and they're always active. *)
        external_processes : (client_id, pid_set) Hashtbl.t;
        angels : (process_id, unit Lwt.t) Hashtbl.t;
        step_counter : int ref }

  let state = {
    blocked          = Hashtbl.create 10000;
    client_processes = Hashtbl.create 10000;
    external_processes = Hashtbl.create 10000;
    angels           = Hashtbl.create 10000;
    step_counter     = ref 0 }

  let atomic : bool ref = ref false

  (** Test that there is only one thread total (the running one)? *)
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

(** Checks whether a client process is active -- that is, has been sent to a client to be spawned. *)
  let is_process_active : client_id -> process_id -> process_lookup_res =
    fun client_id pid ->
    let is_in_externals =
      try
        let externals_set = Hashtbl.find state.external_processes client_id in
        PidSet.mem pid externals_set
      with
        NotFound _ -> false in
    if is_in_externals then `DeployedProcessFound else
      let client_table_opt =
        try Some (Hashtbl.find state.client_processes client_id) with
          | NotFound _ -> None in
      match client_table_opt with
        | Some client_table ->
          let proc_pair_res = PidMap.lookup pid client_table in
          begin
            match proc_pair_res with
              | Some (_v, active) ->
                  if active then `DeployedProcessFound else `UndeployedProcessFound
              | None -> `ProcessNotFound
          end
        | None -> `ClientNotFound


  let register_external_process client_id pid =
    Debug.print @@ "Registering external process " ^ (ProcessID.to_string pid) ^
      " to client " ^ (ClientID.to_string client_id);
    let externals_opt = Hashtbl.lookup state.external_processes client_id in
    match externals_opt with
      | Some (pidset) ->
          Hashtbl.replace state.external_processes client_id (PidSet.add pid pidset)
      | None ->
          Hashtbl.add state.external_processes client_id (PidSet.singleton pid)

  (** Given a value that has been sent from a client, inspect for
   * sent process IDs and add to the list of client processes if necessary *)
  let rec resolve_external_processes = function
    | `List xs -> List.iter resolve_external_processes xs
    | `Record xs -> List.iter (resolve_external_processes -<- snd) xs
    | `Variant (_, x) -> resolve_external_processes x
    | `FunctionPtr (_, (Some fvs)) -> resolve_external_processes fvs
    | `Pid (`ClientPid (cid, pid)) ->
        begin
        match is_process_active cid pid with
          | `ProcessNotFound -> register_external_process cid pid
          | _ -> ()
        end
    | _ -> ()

  let current_pid_key = Lwt.new_key ()
  let angel_done = Lwt.new_key ()

  (** Return the identifier of the running process. *)
  let get_current_pid () =
    match Lwt.get current_pid_key with
    | None -> assert false
    | Some pid -> pid

  (** Returns a list of all processes that have been created but not yet
   * dispatched to a particular client, and marks them all as dispatched. *)
  let get_and_mark_pending_processes cid =
    let client_procs_opt = Hashtbl.lookup state.client_processes cid in
    match client_procs_opt with
      | Some (client_procs) ->
          let client_proc_list = PidMap.bindings client_procs in
          (* Returns a (pid, proc) pairing of each inactive process *)
          let ret = filter_map
            (fun (_pid, (_proc, active)) -> not active)
            (fun (pid, (proc, _active)) -> (pid, proc))
            client_proc_list
          in
          (* Mark all processes as active *)
          let marked = PidMap.map (fun (proc, _) -> (proc, true)) client_procs in
          Hashtbl.replace state.client_processes cid marked;
          ret
      | None -> [] (* No processes have been created *)

  (** Awaken (unblock) a process:
    Move it from the blocked state to the runnable queue ([suspended]).
    Ignores if the process does not exist.
   *)
  let awaken pid =
    match Hashtbl.lookup state.blocked pid with
    | None -> () (* process not blocked *)
    | Some doorbell ->
      Debug.print ("Awakening blocked process " ^ ProcessID.to_string pid);
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
    Debug.print ("Blocking process " ^ ProcessID.to_string pid);
    let (t, u) = Lwt.wait () in
    Hashtbl.add state.blocked pid u;
    t >>= pstate

  (** Given a process state, create a new process and return its identifier. *)
  let create_process angel pstate =
    ProcessID.create () >>= fun new_pid ->
    if angel then
      begin
        let (t, w) = Lwt.task () in
        Hashtbl.add state.angels new_pid t;
        async (fun () -> Lwt.with_value current_pid_key (Some new_pid)
                          (fun () -> Lwt.with_value angel_done (Some w) pstate))
      end
    else
      async (fun () -> Lwt.with_value current_pid_key (Some new_pid) pstate);
    Lwt.return new_pid

  (** Create a new client process and return its identifier *)
  let create_client_process client_id func =
    ProcessID.create () >>= fun new_pid ->
    let client_table =
      try Hashtbl.find state.client_processes client_id
      with
        NotFound _ -> PidMap.empty in
    let new_client_table = PidMap.add new_pid (func, false) client_table in
    Hashtbl.add state.client_processes client_id new_client_table;
    Lwt.return new_pid

  let finish r =
    let pid = get_current_pid () in
    (* This process is only actually finished if we're not executing atomically *)
    if not (!atomic) then
      Debug.print ("Finishing process " ^ ProcessID.to_string pid);
    if ProcessID.equal pid main_process_pid then
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
      Debug.print ("Finishing process " ^ ProcessID.to_string pid);
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

  let atomically_inner pfun =
    let previously_atomic = !atomic in
    atomic := true;
    let v = run' pfun in
    atomic := previously_atomic;
    v

  let atomically pfun =
    snd @@ atomically_inner pfun

end

exception UnknownProcessID of process_id
exception UnknownClientID of client_id

module type MAILBOX =
sig
  val pop_message_for : process_id -> Value.t option
  val pop_all_messages_for :
    client_id -> process_id-> Value.t list
  val pop_message : unit -> Value.t option

  val send_client_message : Value.t -> client_id ->  process_id -> unit
  val send_server_message : Value.t -> process_id -> unit
end

module type SESSION =
sig
  type chan = Value.chan

  val new_server_access_point : unit -> apid Lwt.t
  val new_client_access_point : client_id -> apid Lwt.t

  val get_and_mark_pending_aps : client_id -> apid list

  val accept : apid -> (chan * bool) Lwt.t
  val request : apid -> (chan * bool) Lwt.t

  val block : channel_id -> process_id -> unit
  val unblock : channel_id -> process_id option

  val send : Value.t -> channel_id -> unit
  val receive : channel_id -> Value.t option

  val link : chan -> chan -> unit
end

module Mailbox : MAILBOX =
struct

  (* Message queues for processes resident on this server. *)
  let server_message_queues :
    (process_id, Value.t Queue.t) Hashtbl.t = Hashtbl.create 10000

  (* Message queues for processes spawned on a client, but migrated over during an RPC call. *)
  type client_queue_map = Value.t Queue.t pid_map
  let client_message_queues : (client_id, client_queue_map) Hashtbl.t = Hashtbl.create 10000

  (* Create the main process's message queue *)
  (* FIXME: This no longer makes sense. main_process_pid should be generated based on client ID.
   * Currently, this will mean that any sends to the main thread for each request will persist over
   * different requests, which is wholly incorrect behaviour. *)
  let _ = Hashtbl.add server_message_queues
    (Proc.main_process_pid)
    (Queue.create ())

  (** Given a PID, return its next queued message (under [Some]) or [None].
   * Used for receiving on the server, from server processes. *)
  let pop_message_for pid =
    let mqueue =
      match Hashtbl.lookup server_message_queues pid with
      | Some mqueue -> mqueue
      | None ->
        let mqueue = Queue.create () in
        let _ = Hashtbl.add server_message_queues pid mqueue in
        mqueue in
    if not (Queue.is_empty mqueue) then
      Some (Queue.pop mqueue)
    else
      None

  (** Pop a message for the current process. Used to receive on the server. *)
  let pop_message () = pop_message_for (Proc.get_current_pid ())

  (** extract an entire message queue (used in transporting messages
      to the client) *)
  let pop_all_messages_for client_id pid =
    (* Firstly grab the map for the given client ID *)
    match Hashtbl.lookup client_message_queues client_id with
      | Some client_queue_map ->
          (* With the map in hand, we can lookup the PID *)
          begin
            match PidMap.lookup pid client_queue_map with
              | Some queue ->
                  (* And with the queue in hand, we can pop all of the values
                   * and update the hashtable, *)
                  let updated_map = PidMap.remove pid client_queue_map in
                  Hashtbl.replace client_message_queues client_id updated_map;
                  List.rev (Queue.fold (fun xs x -> x :: xs) [] queue)
              | None ->
                (* No queue? No problem! *)
                  []
          end
      | None ->
          (* Should we perhaps raise an exception here? Does it matter? *)
          []

  (** Sends a message to a server process --- that is, a process residing on the
   * server. Creates a message queue if one doesn't exist already. *)
  let send_server_message msg pid =
    match Hashtbl.lookup server_message_queues pid with
    | Some mqueue ->
        Queue.push msg mqueue;
        Proc.awaken pid
    | None ->
        let mqueue = Queue.create () in
        Queue.push msg mqueue;
        Hashtbl.add server_message_queues pid mqueue

  (* Given a client ID which is not yet active, and a PID, adds the message to the queue.
   * If either the client ID or PID doesn't exist in the table, creates the appropriate entries. *)
  let queue_client_message msg client_id pid =
    let client_queue_map =
      try Hashtbl.find client_message_queues client_id with
        | NotFound _ -> PidMap.empty in
      try
        let pid_queue = PidMap.find pid client_queue_map in
        Queue.push msg pid_queue
      with
        | NotFound _ ->
            let new_queue = Queue.create () in
            Queue.push msg new_queue;
            let new_queue_map =
              PidMap.add pid new_queue client_queue_map in
            (Hashtbl.replace client_message_queues client_id new_queue_map)

  let send_client_message (msg : Value.t) client_id pid =
    (* Here, we need to check whether the process is active.
     * If not, we can add the message to the message queue.
     * If so, then we will need to send as a websocket request. *)
    match Proc.is_process_active client_id pid with
      | `ClientNotFound -> raise (UnknownClientID client_id)
      | `ProcessNotFound -> raise (UnknownProcessID pid)
      | `DeployedProcessFound ->
          failwith "Sending client messages unavailable until distribution2 merge"
      | `UndeployedProcessFound ->
          (* If the process has not yet been sent to the client, queue the message *)
          Debug.print "Queueing message, since other process is undeployed";
          queue_client_message msg client_id pid
end


module Session : SESSION = struct
  type chan = Value.chan

  type ap_state = Balanced | Accepting of chan list | Requesting of chan list

  let flip_chan (outp, inp) = (inp, outp)

  (* Access points *)
  (* Server access points --- APs residing on the server *)
  let access_points =
    (Hashtbl.create 10000 : (apid, ap_state) Hashtbl.t)
  (* Client access points --- APs residing on the client. Bool refers to whether
   * this has been delivered to the client yet. *)
  let client_access_points =
    (Hashtbl.create 10000 : (client_id, (apid * bool) list) Hashtbl.t)

  let buffers = (Hashtbl.create 10000 : (channel_id, Value.t Queue.t) Hashtbl.t)
  let blocked = (Hashtbl.create 10000 : (channel_id, process_id) Hashtbl.t)
  let forward = (Hashtbl.create 10000 : (channel_id, channel_id Unionfind.point) Hashtbl.t)

  let fresh_chan () =
    ChannelID.create () >>= fun outp ->
    ChannelID.create () >>= fun inp ->
    Lwt.return (outp, inp)

  let new_channel () =
    fresh_chan () >>= fun c ->
      let (outp, inp) = c in
      Hashtbl.add buffers outp (Queue.create ());
      Hashtbl.add buffers inp (Queue.create ());
      Hashtbl.add forward outp (Unionfind.fresh outp);
      Hashtbl.add forward inp (Unionfind.fresh inp);
      Lwt.return c

  let new_server_access_point () =
    AccessPointID.create () >>= fun apid ->
      Hashtbl.add access_points apid Balanced;
      Lwt.return apid

  let new_client_access_point cid =
    AccessPointID.create () >>= fun apid ->
      begin
      match Hashtbl.lookup client_access_points cid with
        | Some aps -> Hashtbl.replace client_access_points cid ((apid, false) :: aps)
        | None -> Hashtbl.add client_access_points cid [(apid, false)]
      end;
      Lwt.return apid

  let get_and_mark_pending_aps cid =
    match Hashtbl.lookup client_access_points cid with
      | Some aps ->
          let res = filter_map (not -<- snd) fst aps in
          (* Mark all as delivered *)
          Hashtbl.replace client_access_points cid
            (List.map (fun (apid, _) -> (apid, true)) aps);
          res
      | None -> []

  let find_active p =
    Unionfind.find (Hashtbl.find forward p)

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

  let accept : apid -> (chan * bool) Lwt.t =
    fun apid ->
      let state = Hashtbl.find access_points apid in
      let res =
        match state with
        | Balanced             ->
            new_channel () >>= fun c ->
            Lwt.return (c, Accepting [c], true)
        | Accepting cs         ->
            new_channel () >>= fun c ->
            Lwt.return (c, Accepting (cs @ [c]), true)
        | Requesting [c]       ->
            OptionUtils.opt_iter
              (Proc.awaken) (unblock @@ snd c);
            Lwt.return (c, Balanced, false)
        | Requesting (c :: cs) ->
            OptionUtils.opt_iter
              (Proc.awaken) (unblock @@ snd c);
            Lwt.return (c, Requesting cs, false)
        | Requesting []        -> assert false
      in
        res >>= fun (c, state', blocked) ->
        Hashtbl.replace access_points apid state';
        Lwt.return @@ (c, blocked)

  let request : apid -> (chan * bool) Lwt.t =
    fun apid ->
      let state = Hashtbl.find access_points apid in
      let res =
        match state with
        | Balanced            ->
            new_channel () >>= fun c ->
            Lwt.return (c, Requesting [c], true)
        | Requesting cs       ->
            new_channel () >>= fun c ->
            Lwt.return (c, Requesting (cs @ [c]), true)
        | Accepting [c]       ->
            OptionUtils.opt_iter
              (Proc.awaken) (unblock @@ fst c);
            Lwt.return (c, Balanced, false)
        | Accepting (c :: cs) ->
            OptionUtils.opt_iter
              (Proc.awaken) (unblock @@ fst c);
            Lwt.return (c, Accepting cs, false)
        | Accepting []        -> assert false
      in
        res >>= fun (c, state', blocked) ->
        Hashtbl.replace access_points apid state';
        Lwt.return @@ (flip_chan c, blocked)

  let forward inp outp =
    Unionfind.union (Hashtbl.find forward inp) (Hashtbl.find forward outp)

  let send msg p =
    (* Debug.print ("Sending along: " ^ string_of_int p); *)
    let p = find_active p in
    Queue.push msg (Hashtbl.find buffers p);
    OptionUtils.opt_iter Proc.awaken (unblock p)


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
