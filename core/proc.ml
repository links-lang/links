(** Data structures/utilities for process management *)
open Lwt
open ProcessTypes
open Utility
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
        (* spawnwait processes: maps spawned process ID to a pair of parent process
         * and return value (initially None, becomes Some after process finishes evaluating *)
        spawnwait_processes : (process_id, (process_id * Value.t option)) Hashtbl.t;
        angels : (process_id, unit Lwt.t) Hashtbl.t;
        step_counter : int ref }

  let state = {
    blocked          = Hashtbl.create 10000;
    client_processes = Hashtbl.create 10000;
    external_processes = Hashtbl.create 10000;
    spawnwait_processes = Hashtbl.create 10000;
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
        Lwt.pause () >>= pstate
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
    let new_pid = ProcessID.create () in
    if angel then
      begin
        let (t, w) = Lwt.task () in
        Hashtbl.add state.angels new_pid t;
        async (fun () ->
            Lwt.with_value current_pid_key (Some new_pid)
              (fun () -> Lwt.with_value angel_done (Some w) pstate) >>= fun _ ->
            Lwt.return_unit)
      end
    else
      async (fun () ->
          Lwt.with_value current_pid_key (Some new_pid) pstate >>= fun _ ->
          Lwt.return_unit
        );
    Lwt.return new_pid

  (** Creates a spawnWait process *)
  let create_spawnwait_process parent_pid pstate =
    let new_pid = ProcessID.create () in
    Hashtbl.add state.spawnwait_processes new_pid (parent_pid, None);
    async (fun () ->
        Lwt.with_value current_pid_key (Some new_pid) pstate >>= fun _ ->
        Lwt.return_unit
      );
    Lwt.return new_pid

  (* Grabs the result of a finished spawnWait process *)
  let get_spawnwait_result child_pid =
    let (_, res) = Hashtbl.find state.spawnwait_processes child_pid in
    match res with
      | Some res ->
          Hashtbl.remove state.spawnwait_processes child_pid;
          Some res
      | None -> None

  (** Create a new client process and return its identifier *)
  let create_client_process client_id func =
    let new_pid = ProcessID.create () in
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
    (* Finally check whether we're a spawnWait process: if so,
     * then we'll need to set the message in the hashtable, and wake up the parent. *)
    begin
    match Hashtbl.lookup state.spawnwait_processes pid with
      | Some (parent_pid, _) ->
          Hashtbl.replace state.spawnwait_processes pid (parent_pid, (Some (snd r)));
          awaken parent_pid
      | None -> ()
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
    (Lwt.with_value current_pid_key (Some main_process_pid) pfun >>= fun r ->
     (if not !atomic then
        Lwt.join (Hashtbl.fold (fun _ t ts -> t :: ts) state.angels [])
      else
        Lwt.return ()) >>= fun _ ->
     Lwt.return r)

  let run pfun =
    reset_step_counter ();
    run' pfun

  let start pfun =
    Lwt_main.run (run pfun)

  let atomically_inner pfun =
    let previously_atomic = !atomic in
    atomic := true;
    run' pfun >>= fun v ->
    atomic := previously_atomic;
    Lwt.return v

  let atomically pfun =
    atomically_inner pfun >>= fun v -> Lwt.return (snd v)
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
      Cohttp_lwt_unix.Server.response_action Lwt.t

    (** Sends a message to the given PID. *)
    val deliver_process_message :
      client_id ->
      process_id ->
      Value.t ->
      unit Lwt.t

    (** Sends a response to an AP request / accept *)
    val send_ap_response :
      client_id ->
      process_id ->
      Value.chan ->
      unit Lwt.t

    (** Delivers a message along a session channel *)
    val deliver_session_message :
      client_id ->
      channel_id ->
      Value.delegated_chan list ->
      Value.t ->
      unit Lwt.t

  (** Send a cancellation notification *)
  val send_cancellation :
      client_id ->
      notify_ep:channel_id ->
      cancelled_ep:channel_id ->
      unit Lwt.t
  end

module type MAILBOX =
sig
  val pop_message_for : process_id -> Value.t option
  val pop_all_messages_for :
    client_id -> process_id-> Value.t list
  val pop_message : unit -> Value.t option

  val send_client_message : Value.t -> client_id ->  process_id -> unit Lwt.t
  val send_server_message : Value.t -> process_id -> unit
end

module type SESSION =
sig
  type chan = Value.chan
  type send_result = SendOK | SendPartnerCancelled
  type receive_result =
    | ReceiveOK of Value.t
    | ReceiveBlocked
    | ReceivePartnerCancelled

  val receive_port : chan -> channel_id
  val send_port : chan -> channel_id

  val cancel : chan -> unit Lwt.t
  val is_endpoint_cancelled : channel_id -> bool

  val new_server_access_point : unit -> apid
  val new_client_access_point : client_id -> apid

  val get_and_mark_pending_aps : client_id -> apid list

  val accept : apid -> (chan * bool) Lwt.t
  val request : apid -> (chan * bool) Lwt.t
  val ap_request_from_client : client_id -> process_id -> apid -> unit Lwt.t
  val ap_accept_from_client : client_id -> process_id -> apid -> unit Lwt.t

  val block : channel_id -> process_id -> unit
  val unblock : channel_id -> process_id option

  val send_from_server : Value.t -> channel_id -> send_result Lwt.t
  val send_from_client : Value.t -> channel_id -> send_result Lwt.t

  val receive : chan -> receive_result

  val handle_remote_cancel :
    notify_ep:channel_id -> cancelled_ep:channel_id -> unit Lwt.t

  val link : chan -> chan -> unit

  val cancel_client_channels : client_id -> unit Lwt.t
  val register_client_channel : client_id -> chan -> unit
  val register_server_channel : chan -> unit

  val get_buffer : channel_id -> (Value.t list) option

  val close : chan -> unit
end

module rec Websockets : WEBSOCKETS =
struct

  (* TODO(dhil): The field `_client_id` is never read. It seems that
     the client ID is manually threaded through every function. This
     module possibly warrants a refactor/redesign at some point. *)
  type links_websocket = {
    _client_id : client_id;
    send_fn : Websocket.Frame.t option -> unit
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
    _client_id = cid;
    send_fn = send_fn
  }

  let send_message wsocket str_msg =
    Lwt.wrap1 (wsocket.send_fn) @@ (
      Some (Websocket.Frame.create ~content:str_msg ())
    )

  let send_buffered_messages cid wsocket =
    match (Hashtbl.lookup client_buffered_messages cid) with
      | Some (queue) ->
          let rec drain () =
            if Queue.is_empty queue then Lwt.return () else
            let str_msg = Queue.pop queue in
            Debug.print @@ "Sending buffered message " ^ str_msg;
            send_message wsocket str_msg >>= fun _ ->
            drain () in
          drain () >>= fun _ ->
          Lwt.return @@ Hashtbl.remove client_buffered_messages cid
      | None -> Lwt.return ()

  let decode_message json_str =
    WebsocketMessages.from_json (Yojson.Basic.from_string json_str)

  let decode_and_handle client_id data =
    match decode_message data with
      | ClientToClient (cid, pid, msg) ->
          Proc.resolve_external_processes msg;
          Mailbox.send_client_message msg cid pid
      | ClientToServer (serv_pid, msg) ->
          Proc.resolve_external_processes msg;
          Lwt.return @@ Mailbox.send_server_message msg serv_pid
      | APRequest (blocked_pid_on_client, apid) ->
          Session.ap_request_from_client client_id blocked_pid_on_client apid
      | APAccept (blocked_pid_on_client, apid) ->
          Session.ap_accept_from_client client_id blocked_pid_on_client apid
      | ChanSend (chan_id, v) ->
          Debug.print @@ "Got ChanSend message from PID " ^ (ChannelID.to_string chan_id);
          Proc.resolve_external_processes v;
          Session.send_from_client v chan_id >>= fun _ -> Lwt.return ()
      | ChannelCancellation cancel_data ->
          let notify_ep = cancel_data.notify_ep in
          let cancelled_ep = cancel_data.cancelled_ep in
          Session.handle_remote_cancel ~notify_ep:notify_ep ~cancelled_ep:cancelled_ep

    let recvLoop client_id frame =
    let open Websocket.Frame in
    let rec loop () =
      match frame.opcode with
        | Opcode.Close ->
            deregister_websocket client_id;
            async (fun () -> Session.cancel_client_channels client_id);
            Debug.print @@
            Printf.sprintf "Websocket closed for client %s\n"
              (ClientID.to_string client_id)
        | _ ->
            let data = frame.content in
            Debug.print @@
              Printf.sprintf "Received: %s from client %s\n"
                data (ClientID.to_string client_id);
            begin
            try async (fun () -> decode_and_handle client_id data)
            with
              | exn ->
                  (Debug.print
                  (Printf.sprintf "Could not decode websocket request: %s\n"
                    (Printexc.to_string exn))); loop()

            end
      in
    loop ()

  let accept client_id req =
      Websocket_cohttp_lwt.upgrade_connection
        req (recvLoop client_id)
      >>= fun (resp, send_fn) ->
      let links_ws = make_links_websocket client_id send_fn in
      Debug.print @@ "Registering websocket for client " ^ (ClientID.to_string client_id);
      register_websocket client_id links_ws;
      send_buffered_messages client_id links_ws >>= fun _ ->
      Lwt.return resp

  let send_or_buffer_message cid msg =
    match lookup_websocket_safe cid with
      | Some ws ->
          Debug.print @@ "Sending message " ^ msg ^ " to cid " ^ (ClientID.to_string cid);
          send_message ws msg
      | None ->
          Debug.print
            @@ "Could not find websocket for client ID " ^
                 (ClientID.to_string cid) ^ "; buffering";
          Lwt.return @@ buffer_message cid msg

  let deliver_process_message client_id pid v =
    let json_val = Json.jsonize_value v in
    let json =
      `Assoc [
        ("opcode", `String "MESSAGE_DELIVERY");
        ("dest_pid", ProcessID.to_json pid);
        ("val", json_val)] in
    let str_json = Json.json_to_string json in
    Debug.print @@ "Sending or buffering message " ^ str_json ^ " to client " ^ (ClientID.to_string client_id);
    send_or_buffer_message client_id str_json

  let send_ap_response cid pid ch =
    let json_ch = Json.jsonize_value (Value.box_channel ch) in
    let json_val =
      `Assoc [
        ("opcode", `String "AP_RESPONSE");
        ("blocked_pid", ProcessID.to_json pid);
        ("chan", json_ch)] in
    let str_json = Json.json_to_string json_val in
    send_or_buffer_message cid str_json

  let deliver_session_message client_id session_ep deleg_chans v =
    let json_val = Json.jsonize_value v in

    let jsonize_deleg_chan (chan, msgs) =
      (* Client representation of a buffer is the reverse of the server one, alas *)
      let json_buf =
        List.map (Json.jsonize_value) msgs
        |> List.rev in
      let json_buf = `List json_buf in
      `Assoc [
        ("ep_id", ChannelID.to_json (snd chan));
        ("buf", json_buf)] in
    let deleg_chans_json = `List (List.map jsonize_deleg_chan deleg_chans) in
    let json =
      `Assoc [
        ("opcode", `String "SESSION_MESSAGE_DELIVERY");
        ("ep_id", ChannelID.to_json session_ep);
        ("deleg_chans", deleg_chans_json);
        ("msg", json_val)] in
    let json_str = Json.json_to_string json in
    send_or_buffer_message client_id json_str

  let send_cancellation client_id ~notify_ep ~cancelled_ep =
    let json =
      `Assoc [
        ("opcode", `String "CHANNEL_CANCELLATION");
        ("notify_ep", ChannelID.to_json notify_ep);
        ("cancelled_ep", ChannelID.to_json cancelled_ep)] in
    let json_str = Json.json_to_string json in
    send_or_buffer_message client_id json_str

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
        Lwt.return @@ Queue.push msg pid_queue
      with
        | NotFound _ ->
            let new_queue = Queue.create () in
            Queue.push msg new_queue;
            let new_queue_map =
              PidMap.add pid new_queue client_queue_map in
            Lwt.return (Hashtbl.replace client_message_queues client_id new_queue_map)

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

and Session : SESSION = struct

  type buffer = Value.t Queue.t

  type channel_state =
    | Local (* Receive endpoint resides on the server *)
    | Remote of client_id (* Receive endpoint resides on client_id *)

  (* Send channel ID * Receive channel ID *)
  (* Invariant: receive endpoint must reside on the same VM. *)
  type chan = Value.chan

  type send_result = SendOK | SendPartnerCancelled
  type receive_result = ReceiveOK of Value.t | ReceiveBlocked | ReceivePartnerCancelled

  let send_port = fst
  let receive_port = snd

  (* ap_req represents requests to an access point.
   * A chan will be constructed on the first request to the AP (either request or accept)
   * and the flipped version will be given to the first matching partner.
   *
   * ClientRequest also contains a client ID and process ID, since these will be blocked
   * while the AP request takes place, and need to be explicitly unblocked in the response. *)
  type ap_req = ClientRequest of (client_id * process_id * chan) | ServerRequest of chan

  type ap_state = Balanced | Accepting of ap_req list | Requesting of ap_req list

  let flip_chan (outp, inp) = (inp, outp)

  (* Access points *)
  (* Server access points --- APs residing on the server *)
  let access_points = (Hashtbl.create 10000 : (apid, ap_state) Hashtbl.t)
  (* Client access points --- APs residing on the client. Bool refers to whether
   * this has been delivered to the client yet. *)
  let client_access_points = (Hashtbl.create 10000 : (client_id, (apid * bool) list) Hashtbl.t)

  (* States of all endpoints: local, or remote? *)
  let endpoint_states = (Hashtbl.create 10000 : (channel_id, channel_state) Hashtbl.t)

  (* Buffers of all channels _where the receive endpoint is on this server_ *)
  let buffers = (Hashtbl.create 10000 : (channel_id, buffer) Hashtbl.t)

  (* Endpoint Cancellation *)
  let cancelled_endpoints = ref ChannelIDSet.empty
  let cancel_endpoint c =
    cancelled_endpoints := ChannelIDSet.add c (!cancelled_endpoints)
  let is_endpoint_cancelled c = ChannelIDSet.mem c (!cancelled_endpoints)

  (* Maps client IDs to the list of channels they own *)
  (* Channels of the form (peer EP, ep on client) *)
  let client_channel_map = (Hashtbl.create 10000 : (client_id, chan list) Hashtbl.t)

  let lookup_endpoint = Hashtbl.find endpoint_states

  let blocked = (Hashtbl.create 10000 : (channel_id, process_id) Hashtbl.t)
  let forward_tbl = (Hashtbl.create 10000 : (channel_id, channel_id Unionfind.point) Hashtbl.t)

  (* foldM_ specialised to LWT *)
  let sequence act =
    List.fold_left (fun acc c -> acc >>= fun _ -> (act c)) (Lwt.return ())


  let get_buffer ch_id =
    match Hashtbl.lookup buffers ch_id with
      | Some queue -> Some (Queue.to_list queue)
      | None -> None

  let register_channel location ch =
    begin
    match Hashtbl.lookup endpoint_states (snd ch) with
      | Some Local -> ()
      | Some (Remote old_cid) ->
          (* If we're reassigning a channel, we need to firstly remove it from
           * the previous client's channel list *)
          begin
            match Hashtbl.lookup client_channel_map old_cid with
              | Some chans ->
                  let chans = List.filter (fun x -> x != ch) chans in
                  Hashtbl.replace client_channel_map old_cid chans
              | None -> ()
          end;
      | None -> ()
      end;
    (* Next, we need to ensure that if the location is a client, then it's stored
     * in the client_channel_map *)
    begin
      match location with
        | Local -> ()
        | Remote new_cid ->
            begin
              match Hashtbl.lookup client_channel_map new_cid with
                | Some(chans) ->
                    Hashtbl.replace client_channel_map new_cid (ch :: chans)
                | None ->
                    Hashtbl.add client_channel_map new_cid [ch]
            end
    end;
    (* Finally, we need to update the endpoint_states table with the new CID *)
    Hashtbl.replace endpoint_states (snd ch) location

  let register_server_channel = register_channel (Local)

  let register_client_channel client_id =
    register_channel (Remote client_id)

  let cancel chan =
    let rec cancel_inner = function
      | `List vs -> sequence (cancel_inner) vs
      | `Record r -> sequence (fun (_, v) -> cancel_inner v) r
      | `Variant (_, v) -> cancel_inner v
      | `FunctionPtr (_, (Some fvs)) -> cancel_inner fvs
      | (`SessionChannel c) as cv ->
          Debug.print ("cancelling affected channel: " ^ (Value.string_of_value cv));
          cancel_chan c
      | v ->
          Debug.print ("not cancelling non-affected value: " ^ (Value.string_of_value v));
          Lwt.return ()
    and cancel_chan chan =
      let send_ep = send_port chan in
      let local_ep = receive_port chan in
      let notify_peer owner_cl_id =
        match lookup_endpoint send_ep with
          | Local ->
              OptionUtils.opt_iter (Proc.awaken) (Session.unblock send_ep);
              Lwt.return ()
          | Remote cl_id ->
              let do_cancellation =
                Websockets.send_cancellation
                  cl_id ~notify_ep:send_ep ~cancelled_ep:local_ep in
              begin
              match owner_cl_id with
                | Some owner_cl_id ->
                    if cl_id != owner_cl_id then do_cancellation
                    else Lwt.return ()
                | None -> do_cancellation
              end in

      (* Cancelling a channel twice is a no-op *)
      if ((is_endpoint_cancelled local_ep) ||
         (not @@ Settings.get Basicsettings.Sessions.exceptions_enabled)) then Lwt.return () else
      begin
      cancel_endpoint local_ep;
      match lookup_endpoint local_ep with
        | Local ->
            let buf = Hashtbl.find buffers local_ep |> Queue.to_list in
            sequence (cancel_inner) buf >>= fun _ ->
            notify_peer None
        | Remote client_id ->
            (* Trying to cancel remote buffer. This happens when a client goes
             * offline and we cancel all the channels which were resident on that client. *)
            (* We don't need to inspect buffers, since all will be contained in the map of
             * cancelled channels. We do, however, need to notify the peers of the failure. *)
            notify_peer (Some client_id)
      end in
  cancel_chan chan

  let cancel_client_channels client_id =
    match Hashtbl.lookup client_channel_map client_id with
      | Some channels ->
          (sequence (fun c -> cancel c) channels) >>= fun _ ->
          Hashtbl.remove client_channel_map client_id;
          Lwt.return ()
      | None -> Lwt.return ()


  let handle_remote_cancel ~notify_ep ~cancelled_ep =
    match lookup_endpoint notify_ep with
      | Local ->
          cancel_endpoint cancelled_ep;
          OptionUtils.opt_iter (Proc.awaken) (Session.unblock notify_ep);
          Lwt.return ()
      | Remote client_id ->
          Websockets.send_cancellation client_id ~notify_ep ~cancelled_ep

  (** Creates a fresh server channel, where both endpoints of the channel reside on the server *)
  let fresh_chan () =
    let outp = ChannelID.create () in
    let inp = ChannelID.create () in
    (outp, inp)

  (** Creates a new channel, adds endpoints into the required
   * hashtables (output port, input port, forwarding) *)
  let new_channel () =
    let c = fresh_chan () in
      let (outp, inp) = c in
      (* Firstly, create the buffers *)
      Hashtbl.add buffers outp (Queue.create ());
      Hashtbl.add buffers inp (Queue.create ());

      (* Secondly, create the entries in the channel state table.
       * Both start out as being local until delivered or delegated. *)
      Hashtbl.add endpoint_states outp Local;
      Hashtbl.add endpoint_states inp Local;

      (* Finally, add to the link table -- although this is defunct at the moment *)
      Hashtbl.add forward_tbl outp (Unionfind.fresh outp);
      Hashtbl.add forward_tbl inp (Unionfind.fresh inp);
      c

  let new_server_access_point () =
    let apid = AccessPointID.create () in
      Hashtbl.add access_points apid Balanced;
      apid

  let new_client_access_point cid =
    let apid = AccessPointID.create () in
    begin
    match Hashtbl.lookup client_access_points cid with
      | Some aps -> Hashtbl.replace client_access_points cid ((apid, false) :: aps)
      | None -> Hashtbl.add client_access_points cid [(apid, false)]
    end;
    apid

  let get_and_mark_pending_aps cid =
    match Hashtbl.lookup client_access_points cid with
      | Some aps ->
          let res = filter_map (not -<- snd) fst aps in
          (* Mark all as delivered *)
          Hashtbl.replace client_access_points cid
            (List.map (fun (apid, _) -> (apid, true)) aps);
          res
      | None -> []

  (* Returns: channel to be returned, whether to block this thread.
   * Side effects: if an accept happens successfully (i.e. blocked is false), then
   * the other end of the channel will be unblocked -- in the case of a server request, then
   * the appropriate stuff will be done within Session.unblock and Proc.unblock.
   * In the case of a client request, an unblock message will be sent.
   * Also updates the AP state.
   * *)
  let accept_core apid client_info_opt =
      let state = Hashtbl.find access_points apid in

      let make_req ch =
        match client_info_opt with
          | Some (cid, pid) -> ClientRequest (cid, pid, ch)
          | None -> ServerRequest ch in

      let register_and_notify_peer their_cid their_pid ch =
        let their_end_of_chan = flip_chan ch in
        register_client_channel their_cid their_end_of_chan;
        Websockets.send_ap_response their_cid their_pid their_end_of_chan in

      let res =
        match state with
        | Balanced             ->
            let ch = new_channel () in
            let r = make_req ch in
            Lwt.return (ch, Accepting [r], (fun () -> Lwt.return ()), true)
        | Accepting rs         ->
            let ch =  new_channel () in
            let r = make_req ch in
            Lwt.return (ch, Accepting (rs @ [r]), (fun () -> Lwt.return ()), true)
        | Requesting [req]       ->
            begin
            match req with
              | ServerRequest ch ->
                  let action =
                    fun () ->
                      let their_end_of_chan = flip_chan ch in
                      Lwt.return @@
                      OptionUtils.opt_iter (Proc.awaken)
                        (Session.unblock @@ snd their_end_of_chan) in
                  Lwt.return (ch, Balanced, action, false)
              | ClientRequest (their_cid, their_pid, ch) ->
                  let action =
                    fun () -> register_and_notify_peer their_cid their_pid ch in
                  Lwt.return (ch, Balanced, action, false)
            end
        | Requesting (r :: rs) ->
            begin
            match r with
              | ServerRequest ch ->
                  let action =
                    fun () ->
                      Lwt.return @@
                      let their_end_of_chan = flip_chan ch in
                      OptionUtils.opt_iter (Proc.awaken)
                        (Session.unblock @@ snd their_end_of_chan) in
                  Lwt.return (ch, Requesting rs, action, false)
              | ClientRequest (their_cid, their_pid, ch) ->
                  let action =
                    fun () -> register_and_notify_peer their_cid their_pid ch in
                  Lwt.return (ch, Requesting rs, action, false)
            end
        | Requesting []        ->
            assert false
      in
        res >>= fun (c, state', action, blocked) ->
        Hashtbl.replace access_points apid state';
        Lwt.return (c, action, blocked)


  let accept : apid -> (chan * bool) Lwt.t = fun apid ->
    accept_core apid None >>= fun (c, action, blocked) ->
    action () >>= fun _ ->
    Lwt.return (c, blocked)

  let ap_accept_from_client cid pid apid =
    accept_core apid (Some (cid, pid)) >>= fun (ch, action, blocked) ->
    (* Mark endpoint as remote *)
    register_client_channel cid ch;
    begin
    if not blocked then
      Websockets.send_ap_response cid pid ch
    else
      Lwt.return ()
    end >>= fun _ ->
    action ()

  let request_core apid client_info_opt =
      let make_req ch =
        match client_info_opt with
          | Some (cid, pid) -> ClientRequest (cid, pid, ch)
          | None -> ServerRequest ch in

      let register_and_notify_peer their_cid their_pid ch =
        register_client_channel their_cid ch;
        Websockets.send_ap_response their_cid their_pid ch in

      let state = Hashtbl.find access_points apid in
      let res =
        match state with
        | Balanced            ->
            let ch = new_channel () in
            let r = make_req ch in
            Lwt.return (ch, Requesting [r], (fun () -> Lwt.return ()), true)
        | Requesting rs       ->
            let ch = new_channel () in
            let r = make_req ch in
            Lwt.return (ch, Requesting (rs @ [r]), (fun () -> Lwt.return ()), true)
        | Accepting [r]       ->
            begin
            match r with
              | ServerRequest ch ->
                  let action = fun () ->
                    Lwt.return @@
                    OptionUtils.opt_iter (Proc.awaken)
                      (Session.unblock @@ snd ch) in
                  Lwt.return (ch, Balanced, action, false)
              | ClientRequest (their_cid, their_pid, ch) ->
                  let action =
                    fun () -> register_and_notify_peer their_cid their_pid ch in
                  Lwt.return (ch, Balanced, action, false)
            end
        | Accepting (r :: rs) ->
            begin
            match r with
              | ServerRequest ch ->
                  let action =
                    fun () ->
                      Lwt.return @@
                      OptionUtils.opt_iter (Proc.awaken)
                        (Session.unblock @@ snd ch) in
                  Lwt.return (ch, Accepting rs, action, false)
              | ClientRequest (their_cid, their_pid, ch) ->
                  let action =
                    fun () -> register_and_notify_peer their_cid their_pid ch in
                  Lwt.return (ch, Accepting rs, action, false)
            end
        | Accepting []        ->
            assert false
      in
        res >>= fun (c, state', action, blocked) ->
        Hashtbl.replace access_points apid state';
        let our_end_of_chan = flip_chan c in
        Lwt.return (our_end_of_chan, action, blocked)

  let request : apid -> (chan * bool) Lwt.t = fun apid ->
    request_core apid None >>= fun (c, action, blocked) ->
    action () >>= fun _ ->
    Lwt.return (c, blocked)

  let ap_request_from_client cid pid apid =
    request_core apid (Some (cid, pid)) >>= fun (ch, action, blocked) ->
    register_client_channel cid ch;
    begin
    if not blocked then
      Websockets.send_ap_response cid pid ch
    else
      Lwt.return ()
    end >>= fun _ ->
    action ()

  let find_active p =
    Unionfind.find (Hashtbl.find forward_tbl p)

  let forward inp outp =
    Unionfind.union (Hashtbl.find forward_tbl inp) (Hashtbl.find forward_tbl outp)

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

  (* Sends *)
  let do_local_send msg send_port =
    let p = find_active send_port in
    (* We'll either have a local buffer if channel is not in the process of
     * being delegated, or an entry in the orphans table otherwise. *)
    let queue = Hashtbl.find buffers p in
    Queue.push msg queue;
    OptionUtils.opt_iter Proc.awaken (Session.unblock send_port)

  (* Given a message, return an association list mapping each delegated endpoint
   * to its buffer contents. *)
  let delegated_buffers msg =
    Value.get_contained_channels msg
    |> List.map (fun ((_, ep2) as ch) ->
        (ch, Hashtbl.find buffers ep2 |> Queue.to_list))

  (* Send 'msg' to 'port', where the send originated from the server *)
  let send_from_server msg port =
    (* Check if EP is cancelled *)
    if is_endpoint_cancelled port then Lwt.return SendPartnerCancelled else
    (* If not, then route accordingly *)
    match lookup_endpoint port with
      | Local ->
          (* If we're doing a local -> local, we don't need to do anything special. *)
          do_local_send msg port;
          Lwt.return SendOK
      | Remote dest_id ->
          (* If we're sending a message from the server to client, then for each
             delegated endpoint, we need to:
               1) Update the recorded location to be the destination client
               2) Retrieve and send along the contents of the endpoint's buffer.  *)
          let deleg_chans = delegated_buffers msg in
          (* Update endpoint states channels *)
          List.iter (fun (chan, _buf) ->
            register_client_channel dest_id chan) deleg_chans;
          Websockets.deliver_session_message dest_id port deleg_chans msg >>=
            fun _ ->
          Lwt.return SendOK

  (* A send which originated on a client. Idea is to route the message to either
     a server process or another client process.
     Assumption: Remote send does not contain any endpoints. This will have been
     checked on the client. *)
  let send_from_client msg port =
    assert (List.length (Value.get_contained_channels msg) = 0);
    if is_endpoint_cancelled port then Lwt.return SendPartnerCancelled else
    match lookup_endpoint port with
      | Local ->
          do_local_send msg port;
          Lwt.return SendOK
      | Remote cid ->
          Websockets.deliver_session_message cid port [] msg >>= fun _ ->
          Lwt.return SendOK

  (* Closing a channel endpoint results in its queue in the buffers hashtable
   * being deleted. *)
  let close chan =
    let p = find_active (receive_port chan) in
    match lookup_endpoint p with
      | Local -> Hashtbl.remove buffers p
      | _ -> ()

  (* Receives from the given channel. *)
  let receive chan =
      let recv_port = receive_port chan in
      let partner_ep = send_port chan in
      match lookup_endpoint recv_port with
        | Local ->
          (* Debug.print ("Receiving on: " ^ string_of_int p); *)
          let p = find_active recv_port in
          let buf = Hashtbl.find buffers p in
          if (not (Queue.is_empty buf)) then
              ReceiveOK (Queue.pop buf)
          else
            if is_endpoint_cancelled partner_ep then
              ReceivePartnerCancelled
            else
              ReceiveBlocked
        | Remote _ ->
            (* This should not happen. *)
            raise (Errors.internal_error ~filename:"proc.ml" ~message:"Cannot receive on remote port!")

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
