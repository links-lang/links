(** Data structures/utilities for process management *)
open Utility
open Lwt
open ProcessTypes
open WebsocketMessages

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

  let atomically pfun =
    let previously_atomic = !atomic in
    atomic := true;
    let v = run' pfun in
    atomic := previously_atomic;
    snd v
end

exception UnknownProcessID of process_id
exception UnknownClientID of client_id


module type WEBSOCKETS =
  sig
    (** Accepts a new websocket connection, creates a new socket, as
     * well as a thread which handles incoming messages. *)
    val accept :
      client_id ->
      Cohttp.Request.t ->
      Conduit_lwt_unix.flow ->
      (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

    (** Sends a message to the given PID.
     * The string is a JSONised value -- should abstract this furhter *)
    val deliver_process_message :
      client_id ->
      process_id ->
      Value.t ->
      unit
  end

module type MAILBOX =
sig
  val pop_message_for : process_id -> Value.t option
  val pop_all_messages_for :
    client_id -> process_id-> Value.t list
  val pop_message : unit -> Value.t option

  val send_client_message : Value.t -> client_id ->  process_id -> unit
  val send_server_message : Value.t -> process_id -> unit
end

module rec Websockets : WEBSOCKETS =
struct
  open Websocket_cohttp_lwt

  type links_websocket = {
    client_id : client_id;
    send_fn : Websocket_cohttp_lwt.Frame.t option -> unit
  }

  let client_websockets : (client_id, links_websocket) Hashtbl.t =
    Hashtbl.create 10000

  (* Buffers for messages sent before a socket could be established *)
  let client_buffered_messages : (client_id, string Queue.t) Hashtbl.t =
    Hashtbl.create 10000

  let buffer_message cid str_msg =
    let queue =
      match (Hashtbl.lookup client_buffered_messages cid) with
        | Some queue -> queue
        | None ->
            let q = Queue.create () in
            Hashtbl.add client_buffered_messages cid q;
            q in
    Queue.push str_msg queue



  let register_websocket = Hashtbl.add client_websockets
  let deregister_websocket = Hashtbl.remove client_websockets
  let lookup_websocket_safe client_id =
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

  let send_buffered_messages cid wsocket =
    match (Hashtbl.lookup client_buffered_messages cid) with
      | Some (queue) ->
          let rec drain () =
            if Queue.is_empty queue then () else
            let str_msg = Queue.pop queue in
            Debug.print @@ "Sending buffered message " ^ str_msg;
            send_message wsocket str_msg;
            drain () in
          drain ();
          Hashtbl.remove client_buffered_messages cid
      | None -> ()

  let decode_message json_str =
    Jsonparse.parse_websocket_request Jsonlex.jsonlex
      (Lexing.from_string json_str)

  let decode_and_handle data =
    match decode_message data with
      | ClientToClient (cid, pid, msg) ->
          Mailbox.send_client_message msg cid pid
      | ClientToServer (serv_pid, msg) ->
          Mailbox.send_server_message msg serv_pid

    let recvLoop client_id frame =
    let open Frame in
    let rec loop () =
      match frame.opcode with
        | Opcode.Close ->
            deregister_websocket client_id;
            Debug.print @@
            Printf.sprintf "Websocket closed for client %s\n"
              (ClientID.to_string client_id)
        | _ ->
            let data = frame.content in
            Debug.print @@
              Printf.sprintf "Received: %s from client %s\n"
                data (ClientID.to_string client_id);
            try decode_and_handle data
            with
              | exn -> Debug.print @@
                  Printf.sprintf "Could not decode websocket request: %s\n"
                    (Printexc.to_string exn);
            loop ()
      in
    loop ()

  let accept client_id req flow =
      Websocket_cohttp_lwt.upgrade_connection
        req flow (recvLoop client_id)
      >>= fun (resp, body, send_fn) ->
      let links_ws = make_links_websocket client_id send_fn in
      Debug.print @@ "Registering websocket for client " ^ (ClientID.to_string client_id);
      register_websocket client_id links_ws;
      send_buffered_messages client_id links_ws;
      Lwt.return (resp, body)

  let send_or_buffer_message cid msg =
    match lookup_websocket_safe cid with
      | Some ws ->
          Debug.print @@ "Sending message " ^ msg ^ " to cid " ^ (ClientID.to_string cid);
          send_message ws msg
      | None ->
          Debug.print
            @@ "Could not find websocket for client ID " ^
                 (ClientID.to_string cid) ^ "; buffering";
          buffer_message cid msg

  let deliver_process_message client_id pid v =
    let json_val = Json.jsonize_value v in
    let str_val =
      "{\"opcode\":\"MESSAGE_DELIVERY\", \"dest_pid\":\"" ^ (ProcessID.to_string pid) ^
      "\", \"val\":" ^ json_val ^ "}" in
    Debug.print @@ "Sending or buffering message " ^ str_val ^ " to client " ^ (ClientID.to_string client_id);
    send_or_buffer_message client_id str_val

  (* Debug *)
  let _send_raw_string wsocket str = send_message wsocket str
end

and Mailbox : MAILBOX =
struct

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

  let send_client_message (msg : Value.t) client_id pid =
    (* Here, we need to check whether the process is active.
     * If not, we can add the message to the message queue.
     * If so, then we will need to send as a websocket request. *)
    match Proc.is_process_active client_id pid with
      | `ClientNotFound -> raise (UnknownClientID client_id)
      | `ProcessNotFound -> raise (UnknownProcessID pid)
      | `DeployedProcessFound -> Websockets.deliver_process_message client_id pid msg
      | `UndeployedProcessFound ->
          (* If the process has not yet been sent to the client, queue the message *)
          Debug.print "Queueing message, since other process is undeployed";
          queue_client_message msg client_id pid
end

module Session = struct

  type buffer = Value.t Queue.t

  type channel_state =
    | Local (* Receive endpoint resides on the server *)
    | Remote of client_id (* Receive endpoint resides on client_id *)
    | Delegating of (buffer * client_id) (* Delegation in progress to client_id, storing buffer in the meantime *)

  (* Send channel ID * Receive channel ID *)
  (* Invariant: receive endpoint must reside on the same VM. *)
  type chan = Value.chan

  type ap_state = Balanced | Accepting of chan list | Requesting of chan list

  let flip_chan (outp, inp) = (inp, outp)

  let access_points = (Hashtbl.create 10000 : (apid, ap_state) Hashtbl.t)

  (* States of all endpoints: local, remote, or in the process of delegating? *)
  let endpoint_states = (Hashtbl.create 10000 : (channel_id, channel_state) Hashtbl.t)

  (* Buffers of all channels _where the receive endpoint is on this server_ *)
  let buffers = (Hashtbl.create 10000 : (channel_id, buffer) Hashtbl.t)
  let blocked = (Hashtbl.create 10000 : (channel_id, process_id) Hashtbl.t)
  let forward = (Hashtbl.create 10000 : (channel_id, channel_id Unionfind.point) Hashtbl.t)

  (** Creates a fresh server channel, where both endpoints of the channel reside on the server *)
  let fresh_chan () =
    ChannelID.create () >>= fun outp ->
    ChannelID.create () >>= fun inp ->
      Lwt.return (outp, inp)

  (** Creates a new channel, adds endpoints into the required
   * hashtables (output port, input port, forwarding) *)
  let new_channel () =
    fresh_chan () >>= fun c ->
      let (outp, inp) = c in
      (* Firstly, create the buffers *)
      Hashtbl.add buffers outp (Queue.create ());
      Hashtbl.add buffers inp (Queue.create ());

      (* Secondly, create the entries in the channel state table.
       * Both start out as being local until delivered or delegated. *)
      Hashtbl.add endpoint_states outp Local;
      Hashtbl.add endpoint_states inp Local;

      (* Finally, add to the link table -- although this is defunct at the moment *)
      Hashtbl.add forward outp (Unionfind.fresh outp);
      Hashtbl.add forward inp (Unionfind.fresh inp);
      Lwt.return c

  let new_access_point () =
    AccessPointID.create () >>= fun apid ->
      Hashtbl.add access_points apid Balanced;
      Lwt.return apid

  let accept : apid -> (chan * bool) Lwt.t =
    fun apid ->
      let state = Hashtbl.find access_points apid in
      let res =
        match state with
        | Balanced             ->
            new_channel () >>= fun ch -> Lwt.return (ch, Accepting [ch], true)
        | Accepting cs         ->
            new_channel () >>= fun ch -> Lwt.return (ch, Accepting (cs @ [ch]), true)
        | Requesting [c]       -> Lwt.return (c, Balanced, false)
        | Requesting (c :: cs) -> Lwt.return (c, Requesting cs, false)
        | Requesting []        -> assert false (* TODO: check that this is impossible *)
      in
        res >>= fun (c, state', blocked) ->
        Hashtbl.replace access_points apid state';
        Lwt.return (c, blocked)

  let request : apid -> (chan * bool) Lwt.t =
    fun apid ->
      let state = Hashtbl.find access_points apid in
      let res =
        match state with
        | Balanced            ->
            new_channel () >>= fun ch -> Lwt.return (ch, Requesting [ch], true)
        | Requesting cs       ->
            new_channel () >>= fun ch -> Lwt.return (ch, Requesting (cs @ [ch]), true)
        | Accepting [c]       -> Lwt.return (c, Balanced, false)
        | Accepting (c :: cs) -> Lwt.return (c, Accepting cs, false)
        | Accepting []        -> assert false (* TODO: check that this is impossible *)
      in
        res >>= fun (c, state', blocked) ->
        Hashtbl.replace access_points apid state';
        Lwt.return (flip_chan c, blocked)

  let find_active p =
    Unionfind.find (Hashtbl.find forward p)

  let forward inp outp =
    Unionfind.union (Hashtbl.find forward inp) (Hashtbl.find forward outp)

  let block portid pid =
    let portid = find_active portid in
      Hashtbl.add blocked portid pid

  let unblock portid =
    let portid = find_active portid in
    match (Hashtbl.lookup blocked portid) with
      | Some pid ->
          Hashtbl.remove blocked portid;
          Some pid
      | None -> None

  let send_local msg send_port =
      (* Debug.print ("Sending along: " ^ string_of_int p); *)
      let p = find_active send_port in
      Queue.push msg (Hashtbl.find buffers p)

  let send_remote _msg _cid = failwith "lol not yet"

  let send msg send_port =
    match Hashtbl.find endpoint_states send_port with
      | Local -> send_local msg send_port
      | Remote client_id -> send_remote msg client_id
      | Delegating (buf, _cid) -> Queue.push msg buf

  let receive recv_port =
      (* Debug.print ("Receiving on: " ^ string_of_int p); *)
      let p = find_active recv_port in
      let buf = Hashtbl.find buffers p in
      if not (Queue.is_empty buf) then
        Some (Queue.pop buf)
      else
        None

  (* Currently: all four of these should be server endpoints.
   * Eventually we'll have to do something a bit cleverer... *)
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
