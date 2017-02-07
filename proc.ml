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
        angels : (process_id, unit Lwt.t) Hashtbl.t;
        step_counter : int ref }

  let state = {
    blocked          = Hashtbl.create 10000;
    client_processes = Hashtbl.create 10000;
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

  (** Retrieves the body of a client process for transmission to the client.
   * This will be done precisely once -- afterwards, the process will be marked
   * as "active" in the HT. *)
  let lookup_client_process client_id pid =
    let client_table =
      try Hashtbl.find state.client_processes client_id with
        | NotFound client_id -> failwith ("Missing client ID: " ^ client_id) in

    let v, active =
      try PidMap.find pid client_table with
        | NotFound _pid ->
            failwith (Printf.sprintf
              "Missing client process %s in client %s: "
              (ProcessID.to_string pid)
              (ClientID.to_string client_id)) in

    if active then
      None
    else
      begin
        let new_client_map = PidMap.add pid (v, true) client_table in
        Hashtbl.replace state.client_processes client_id new_client_map;
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

  let atomically pfun =
    let previously_atomic = !atomic in
    atomic := true;
    let v = run' pfun in
    atomic := previously_atomic;
    snd v
end

exception UnknownProcessID of process_id
exception UnknownClientID of client_id

module Websockets =
struct
  type links_websocket = {
    client_id : client_id;
    send_fn : Websocket_cohttp_lwt.Frame.t option -> unit
  }

  let client_websockets :
    (client_id, links_websocket) Hashtbl.t =
      Hashtbl.create 10000

  let register_websocket = Hashtbl.add client_websockets
  let deregister_websocket = Hashtbl.remove client_websockets
  let lookup_websocket client_id =
    try Some (Hashtbl.find client_websockets client_id) with
      | _ -> None

  let make_links_websocket cid send_fn = {
    client_id = cid;
    send_fn = send_fn
  }

  let send_message wsocket str_msg =
    (* Without buffering, does this guarantee in-order delivery? *)
    (* Websockets *do*, but we might need to maintain an outgoing
    * buffer in case async scheduling doesn't. *)
    async ( fun () ->
      Lwt.wrap1 (wsocket.send_fn) @@ (
        Some (
        Websocket_cohttp_lwt.Frame.of_bytes @@
        BytesLabels.of_string @@
        str_msg)
      )
    )

  let deliver_process_message wsocket pid json_val =
    let str_val =
      "{\"opcode\":\"MESSAGE_DELIVERY\", \"dest_pid\":\"" ^ (ProcessID.to_string pid) ^
      "\", \"val\":" ^ json_val ^ "\"}" in
    send_message wsocket str_val

  (* Debug *)
  let send_raw_string wsocket str =
    send_message wsocket str

end

module Mailbox =
struct
  type client_send_result = [
    | `LocalSendOK
    | `RemoteSend of (Websockets.links_websocket)
  ]


  (* Message queues for processes resident on this server. *)
  let server_message_queues :
    (process_id, Value.t Queue.t) Hashtbl.t = Hashtbl.create 10000

  (* Message queues for processes spawned on a client, but migrated over during an RPC call. *)
  type client_queue_map = Value.t Queue.t pid_map
  let client_message_queues : (client_id, client_queue_map) Hashtbl.t = Hashtbl.create 10000

  (* Create the main process's message queue *)
  (* SJF Assumption -- this is the main *server* thread, i.e. the entrypoint from running ./links <file>
   * Where does the request-main's MB get created? (i.e. the entrypoint from evaluating a request) *)
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
            Hashtbl.replace client_message_queues client_id new_queue_map

  let send_client_message msg client_id pid =
    (* Here, we need to check whether the process is active.
     * If not, we can add the message to the message queue.
     * If so, then we will need to send as a websocket request. *)
    match Proc.is_process_active client_id pid with
      | `ClientNotFound -> raise (UnknownClientID client_id)
      | `ProcessNotFound -> raise (UnknownProcessID pid)
      | `DeployedProcessFound ->
          (* If the process has already been sent to the client, we will need
           * to send the message via the client's websocket *)
          let ws_opt = Websockets.lookup_websocket client_id in
          begin
            match ws_opt with
              | Some ws -> `RemoteSend ws
              | None ->
                  (* TODO: Buffer and send later, instead of failing here *)
                  failwith ("No websocket for Client ID " ^
                    (ClientID.to_string client_id) ^ ".")
          end
      | `UndeployedProcessFound ->
          (* If the process has not yet been sent to the client, queue the message *)
          queue_client_message msg client_id pid;
          `LocalSendOK
end

module Session = struct
  type apid = int              (* access point id *)
  type portid = int
  type chan = portid * portid  (* a channel is a pair of ports *)

  type ap_state = Balanced | Accepting of chan list | Requesting of chan list

  let flip_chan (outp, inp) = (inp, outp)

  let access_points = (Hashtbl.create 10000 : (apid, ap_state) Hashtbl.t)

  let buffers = (Hashtbl.create 10000 : (portid, Value.t Queue.t) Hashtbl.t)
  let blocked = (Hashtbl.create 10000 : (portid, process_id) Hashtbl.t)
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

  let block portid pid =
    let portid = find_active portid in
      Hashtbl.add blocked portid pid
  let unblock portid =
    let portid = find_active portid in
      if Hashtbl.mem blocked portid then
        begin
          let pid = Hashtbl.find blocked portid in
            Hashtbl.remove blocked portid;
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

  let unbox_port = Value.unbox_int
  let unbox_chan' chan =
    let (outp, inp) = Value.unbox_pair chan in
      (Value.unbox_int outp, Value.unbox_int inp)
  let unbox_chan chan =
    let (outp, inp) = Value.unbox_pair chan in
      (unbox_port outp, unbox_port inp)
end
