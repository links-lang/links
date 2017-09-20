open Webserver_types
open Ir
open Lwt
open Utility
open Proc
open Pervasives

let lookup_fun = Tables.lookup Tables.fun_defs
let find_fun = Tables.find Tables.fun_defs

let dynamic_static_routes = Basicsettings.Evalir.dynamic_static_routes
let allow_static_routes = ref true

module type EVALUATOR = sig
  type v = Value.t
  type result = Proc.thread_result Lwt.t

  val reify : Value.continuation -> v
  val error : string -> 'a
  val computation : Value.env -> Value.continuation -> Ir.computation -> result
  val finish : Value.env -> v -> result

  val apply : Value.continuation -> Value.env -> v * v list -> result
  val apply_cont : Value.continuation -> Value.env -> v -> result
  val run_program : Value.env -> Ir.program -> (Value.env * v)
  val run_defs : Value.env -> Ir.binding list -> Value.env
end

module Exceptions = struct
  exception EvaluationError of string
  exception Wrong
end

module Evaluator = functor (ContEval : Value.CONTINUATION_EVALUATOR with type v = Value.t
                                                                    and type result = Proc.thread_result Lwt.t
                                                                    and type 'v t := 'v Value.Continuation.t)
                           (Webs : WEBSERVER) ->
struct
  type v = Value.t
  type result = Proc.thread_result Lwt.t
  type continuation = Value.continuation

  module K = struct
    include Value.Continuation
    module Eval = ContEval
  end

  let error msg : 'a = raise (Exceptions.EvaluationError msg)

  let eval_error fmt : 'r =
    Printf.kprintf error fmt

  let db_connect : Value.t -> Value.database * string = fun db ->
    let driver = Value.unbox_string (Value.project "driver" db)
    and name = Value.unbox_string (Value.project "name" db)
    and args = Value.unbox_string (Value.project "args" db) in
    let params =
      (if args = "" then name
       else name ^ ":" ^ args)
    in
      Value.db_connect driver params

  let lookup_fun_def f =
    match lookup_fun f with
    | None -> None
    | Some (finfo, _, None, location) ->
      begin
        match location with
        | `Server | `Unknown ->
          (* TODO: perhaps we should actually use env here - and make
             sure we only call this function when it is sufficiently
             small *)
          Some (`FunctionPtr (f, None))
        | `Client ->
          Some (`ClientFunction (Js.var_name_binder (f, finfo)))
        | `Native -> assert false
      end
    | _ -> assert false

  let find_fun_def f =
    val_of (lookup_fun_def f)

  (* TODO: explicitly distinguish functions and variables in the
     IR so we don't have to do this check every time we look up a
     variable *)
  let lookup_var var env =
    if Lib.is_primitive_var var then Lib.primitive_stub_by_code var
    else
      match lookup_fun_def var with
      | None ->
        Value.Env.find var env
      | Some v -> v

   let serialize_call_to_client req_data ((continuation : continuation), name, args) =
     let open Json in
     let client_id = RequestData.get_client_id req_data in
     let conn_url =
       if (Webs.is_accepting_websocket_requests ()) then
         Some (Settings.get_value Basicsettings.websocket_url) else
         None in
     let st = List.fold_left
       (fun st_acc arg -> ResolveJsonState.add_val_event_handlers arg st_acc)
       (JsonState.empty client_id conn_url) args in
     let st = ResolveJsonState.add_ap_information client_id st in
     let st = ResolveJsonState.add_process_information client_id st in
     Json.jsonize_call st continuation name args

   let client_call :
     RequestData.request_data ->
     string ->
     Value.continuation ->
     Value.t list ->
     result =

       fun req_data name cont args ->
         if not(Settings.get_value Basicsettings.web_mode) then
           failwith "Can't make client call outside web mode.";
         (*if not(Proc.singlethreaded()) then
           failwith "Remaining procs on server at client call!"; *)
         Debug.print("Making client call to " ^ name);
  (*        Debug.print("Call package: "^serialize_call_to_client (cont, name, args)); *)
         let call_package =
           Utility.base64encode @@
             serialize_call_to_client req_data (cont, name, args) in
         Proc.abort ("text/plain", call_package)

  (** {0 Evaluation} *)
  let rec value env : Ir.value -> Value.t = function
    | `Constant `Bool b -> `Bool b
    | `Constant `Int n -> `Int n
    | `Constant `Char c -> `Char c
    | `Constant `String s -> Value.box_string s
    | `Constant `Float f -> `Float f
    | `Variable var -> lookup_var var env
(*
        begin
          match lookup_var var env with
            | Some v -> v
            | _      -> eval_error "Variable not found: %d" var
        end
*)
    | `Extend (fields, r) ->
        begin
          match opt_app (value env) (`Record []) r with
            | `Record fs ->
                (* HACK

                   Pre-pending the fields to r in this order shouldn't
                   be necessary but without the List.rev, deriving
                   somehow manages to serialise things in the wrong
                   order on the "Your Shopping Cart" page of the
                   winestore example. *)
                `Record (List.rev
                           (StringMap.fold
                              (fun label v fs ->
                                 if List.mem_assoc label fs then
                                   (* (label, value env v) :: (List.remove_assoc label fs) *)
                                   eval_error
                                     "Error adding fields: label %s already present" label
                                 else
                                   (label, value env v)::fs)
                              fields
                              []) @ fs)
(*                 `Record (StringMap.fold  *)
(*                            (fun label v fs -> *)
(*                               (label, value env v)::fs) *)
(*                            fields *)
(*                            fs) *)
            | _ -> eval_error "Error adding fields: non-record"
        end
    | `Project (label, r) ->
        begin
          match value env r with
            | `Record fields when List.mem_assoc label fields ->
                List.assoc label fields
            | _ -> eval_error "Error projecting label %s" label
        end
    | `Erase (labels, r) ->
        begin
          match value env r with
            | `Record fields when
                StringSet.for_all (fun label -> List.mem_assoc label fields) labels ->
                `Record (StringSet.fold (fun label fields -> List.remove_assoc label fields) labels fields)
            | _ -> eval_error "Error erasing labels {%s}" (String.concat "," (StringSet.elements labels))
        end
    | `Inject (label, v, _) -> `Variant (label, value env v)
    | `TAbs (_, v) -> value env v
    | `TApp (v, _) -> value env v
    | `XmlNode (tag, attrs, children) ->
        let children =
          List.fold_right
            (fun v children ->
               let v = value env v in
                 List.map Value.unbox_xml (Value.unbox_list v) @ children)
            children [] in
        let children =
          StringMap.fold
            (fun name v attrs ->
               Value.Attr (name, Value.unbox_string (value env v)) :: attrs)
            attrs children
        in
          Value.box_list [Value.box_xml (Value.Node (tag, children))]
    | `ApplyPure (f, args) ->
      Proc.atomically (fun () -> apply K.empty env (value env f, List.map (value env) args))
    | `Closure (f, v) ->
      (* begin *)

      (* TODO: consider getting rid of `ClientFunction *)
      (* Currently, it's only necessary for built-in client
         functions *)

      (* let (finfo, _, z, location) = find_fun f in *)
      (* match location with *)
      (* | `Server | `Unknown | `Client -> *)
      `FunctionPtr (f, Some (value env v))
      (* | `Client -> *)
      (*   `ClientFunction (Js.var_name_binder (f, finfo)) *)
      (* end *)
    | `Coerce (v, _) -> value env v
  and apply_access_point (cont : continuation) env : Value.spawn_location -> result = function
      | `ClientSpawnLoc cid ->
          let apid = Session.new_client_access_point cid in
          apply_cont cont env (`AccessPointID (`ClientAccessPoint (cid, apid)))
      | `ServerSpawnLoc ->
          let apid = Session.new_server_access_point () in
          apply_cont cont env (`AccessPointID (`ServerAccessPoint apid))
  and apply (cont : continuation) env : Value.t * Value.t list -> result =
    function
    | `FunctionPtr (f, fvs), ps ->
      let (_finfo, (xs, body), z, _location) = find_fun f in
      let env =
        match z, fvs with
        | None, None            -> env
        | Some z, Some fvs -> Value.Env.bind z (fvs, `Local) env
        | _, _ -> assert false in

      (* extend env with arguments *)
      let env = List.fold_right2 (fun x p -> Value.Env.bind x (p, `Local)) xs ps env in
      computation_yielding env cont body
    | `PrimitiveFunction ("registerEventHandlers",_), [hs] ->
      let key = EventHandlers.register hs in
      apply_cont cont env (`String (string_of_int key))
    (* start of mailbox stuff *)
    | `PrimitiveFunction ("Send",_), [pid; msg] ->
        let req_data = Value.Env.request_data env in
        if Settings.get_value Basicsettings.web_mode && not (Settings.get_value Basicsettings.concurrent_server) then
          client_call req_data "_SendWrapper" cont [pid; msg]
        else
          let unboxed_pid = Value.unbox_pid pid in
          (try
             match unboxed_pid with
              (* Send a message to a process which lives on the server *)
              | `ServerPid serv_pid ->
                  Lwt.return @@ Mailbox.send_server_message msg serv_pid
              (* Send a message to a process which lives on another client *)
              | `ClientPid (client_id, process_id) ->
                  Mailbox.send_client_message msg client_id process_id
           with
                 UnknownProcessID _ ->
                   (* FIXME: printing out the message might be more useful. *)
                   failwith("Couldn't deliver message because destination process has no mailbox.")) >>= fun _ ->
            apply_cont cont env (`Record [])
    | `PrimitiveFunction ("spawnAt",_), [func; loc] ->
        let req_data = Value.Env.request_data env in
        if Settings.get_value Basicsettings.web_mode && not (Settings.get_value Basicsettings.concurrent_server) then
            client_call req_data "_spawnWrapper" cont [func; loc]
        else
          begin
            match loc with
              | `SpawnLocation (`ClientSpawnLoc client_id) ->
                Proc.create_client_process client_id func >>= fun new_pid ->
                apply_cont cont env (`Pid (`ClientPid (client_id, new_pid)))
              | `SpawnLocation (`ServerSpawnLoc) ->
                let var = Var.dummy_var in
                let frame = K.Frame.make `Local var Value.Env.empty ([], `Apply (`Variable var, [])) in
                Proc.create_process false
                  (fun () -> apply_cont K.(frame &> empty) env func) >>= fun new_pid ->
                apply_cont cont env (`Pid (`ServerPid new_pid))
              | _ -> assert false
          end
    | `PrimitiveFunction ("spawnAngelAt",_), [func; loc] ->
        let req_data = Value.Env.request_data env in
        if Settings.get_value Basicsettings.web_mode && not (Settings.get_value Basicsettings.concurrent_server) then
            client_call req_data "_spawnWrapper" cont [func; loc]
        else
          begin
            match loc with
              | `SpawnLocation (`ClientSpawnLoc client_id) ->
                Proc.create_client_process client_id func >>= fun new_pid ->
                apply_cont cont env (`Pid (`ClientPid (client_id, new_pid)))
              | `SpawnLocation (`ServerSpawnLoc) ->
                let var = Var.dummy_var in
                let frame = K.Frame.make `Local var Value.Env.empty ([], `Apply (`Variable var, [])) in
                Proc.create_process true
                  (fun () -> apply_cont K.(frame &> empty) env func) >>= fun new_pid ->
                apply_cont cont env (`Pid (`ServerPid new_pid))
              | _ -> assert false
          end
    | `PrimitiveFunction ("spawnWait", _), [func] ->
        let our_pid = Proc.get_current_pid () in
        (* Create the new process *)
        let var = Var.dummy_var in
        let frame = K.Frame.make `Local var Value.Env.empty ([], `Apply (`Variable var, [])) in
        Proc.create_spawnwait_process our_pid
          (fun () -> apply_cont K.(frame &> empty) env func) >>= fun child_pid ->
        (* Now, we need to block this process until the spawned process has evaluated to a value.
         * The idea here is that we have a second function, spawnWait', which grabs the result
         * from proc.ml. *)
        let fresh_var = Var.fresh_raw_var () in
        let extended_env =
          Value.Env.bind fresh_var (Value.box_pid (`ServerPid child_pid), `Local) env in
        let grab_frame =
          K.Frame.of_expr extended_env
                          (Lib.prim_appln "spawnWait'" [`Variable fresh_var]) in

        (* Now, check to see whether we already have the result; if so, we can
         * grab and continue. Otherwise, we need to block. *)
        begin
          match Proc.get_spawnwait_result child_pid with
            | Some v -> apply_cont cont env v
            | None ->
                Proc.block (fun () -> apply_cont K.(grab_frame &> cont) env (`Record []))
        end
    | `PrimitiveFunction ("spawnWait'", _), [child_pid] ->
        let unboxed_pid = Value.unbox_pid child_pid in
        begin
        match unboxed_pid with
          | `ServerPid server_pid ->
              let v = OptionUtils.val_of @@ Proc.get_spawnwait_result server_pid in
              apply_cont cont env v
          | _ -> assert false
        end
    | `PrimitiveFunction ("recv",_), [] ->
        (* If there are any messages, take the first one and apply the
           continuation to it.  Otherwise, block the process (put its
           continuation in the blocked_processes table) and let the
           scheduler choose a different thread.  *)
(*         if (Settings.get_value Basicsettings.web_mode) then *)
(*             Debug.print("receive in web server mode--not implemented."); *)
        let req_data = Value.Env.request_data env in
        if Settings.get_value Basicsettings.web_mode && not (Settings.get_value Basicsettings.concurrent_server) then
          client_call req_data "_recvWrapper" cont []
        else
        begin match Mailbox.pop_message () with
            Some message ->
              Debug.print("delivered message.");
              apply_cont cont env message
          | None ->
              let recv_frame = K.Frame.of_expr env (Lib.prim_appln "recv" []) in
              Proc.block (fun () -> apply_cont K.(recv_frame &> cont) env (`Record []))
        end
    (* end of mailbox stuff *)
    (* start of session stuff *)
    | `PrimitiveFunction ("new", _), [] ->
        apply_access_point cont env `ServerSpawnLoc
    | `PrimitiveFunction ("newAP", _), [loc] ->
        let unboxed_loc = Value.unbox_spawn_loc loc in
        apply_access_point cont env unboxed_loc
    | `PrimitiveFunction ("newClientAP", _), [] ->
        (* Really this should be desugared properly into "there"... *)
        let client_id = RequestData.get_client_id @@ Value.Env.request_data env in
        apply_access_point cont env (`ClientSpawnLoc client_id)
    | `PrimitiveFunction ("newServerAP", _), [] ->
        apply_access_point cont env `ServerSpawnLoc
    | `PrimitiveFunction ("accept", _), [ap] ->
      let ap = Value.unbox_access_point ap in
      begin
        match ap with
          | `ClientAccessPoint _ ->
              (* TODO: Work out the semantics of this *)
              failwith "Cannot *yet* accept on a client AP on the server"
          | `ServerAccessPoint apid ->
              Session.accept apid >>= fun ((_, c) as ch, blocked) ->
              let boxed_channel = Value.box_channel ch in
              Debug.print ("Accepting: " ^ (Value.string_of_value boxed_channel));
              if blocked then
                  (* block my end of the channel *)
                  (Session.block c (Proc.get_current_pid ());
                   Proc.block (fun () -> apply_cont cont env boxed_channel))
              else
                (* other end will have been unblocked in proc *)
                apply_cont cont env boxed_channel
      end
    | `PrimitiveFunction ("request", _), [ap] ->
      let ap = Value.unbox_access_point ap in
      begin
        match ap with
          | `ClientAccessPoint _ ->
              (* TODO: Work out the semantics of this *)
              failwith "Cannot *yet* request from a client-spawned AP on the server"
          | `ServerAccessPoint apid ->
              Session.request apid >>= fun ((_, c) as ch, blocked) ->
              let boxed_channel = Value.box_channel ch in
              if blocked then
                (* block my end of the channel *)
                (Session.block c (Proc.get_current_pid ());
                Proc.block (fun () -> apply_cont cont env boxed_channel))
              else
                (* Otherwise, other end will have been unblocked in proc.ml,
                 * return new channel EP *)
                apply_cont cont env boxed_channel
      end
    | `PrimitiveFunction ("send", _), [v; chan] ->
      Debug.print ("sending: " ^ Value.string_of_value v ^ " to channel: " ^ Value.string_of_value chan);
      let (outp, _) = Value.unbox_channel chan in
      Session.send_from_local v outp >>= fun _ ->
      apply_cont cont env chan
    | `PrimitiveFunction ("receive", _), [chan] ->
      begin
        Debug.print("receiving from channel: " ^ Value.string_of_value chan);
        let unboxed_chan = Value.unbox_channel chan in
        let (_outp, inp) = unboxed_chan in
        match Session.receive inp with
          | Some v ->
            Debug.print ("grabbed: " ^ Value.string_of_value v);
            apply_cont cont env (Value.box_pair v chan)
          | None ->
            (* Here, we have to extend the environment with a fresh variable
             * representing the channel, since we can't create an IR application
             * involving a Value.t (only an Ir.value).
             * This *should* be safe, but still feels a bit unsatisfactory.
             * It would be nice to refine this further. *)
            let fresh_var = Var.fresh_raw_var () in
            let extended_env = Value.Env.bind fresh_var (chan, `Local) env in
            let grab_frame = K.Frame.of_expr extended_env (Lib.prim_appln "receive" [`Variable fresh_var]) in
              let inp = (snd unboxed_chan) in
              Session.block inp (Proc.get_current_pid ());
              Proc.block (fun () -> apply_cont K.(grab_frame &> cont) env (`Record []))
      end
    | `PrimitiveFunction ("link", _), [chanl; chanr] ->
      let unblock p =
        match Session.unblock p with
        | Some pid -> (*Debug.print("unblocked: "^string_of_int p); *)
                      Proc.awaken pid
        | None     -> () in
      Debug.print ("linking channels: " ^ Value.string_of_value chanl ^ " and: " ^ Value.string_of_value chanr);
      let (out1, in1) = Value.unbox_channel chanl in
      let (out2, in2) = Value.unbox_channel chanr in
      Session.link (out1, in1) (out2, in2);
      unblock out1;
      unblock out2;
      apply_cont cont env (`Record [])
    (* end of session stuff *)
    | `PrimitiveFunction ("unsafeAddRoute", _), [pathv; handler; error_handler] ->
       let path = Value.unbox_string pathv in
       let is_dir_handler = String.length path > 0 && path.[String.length path - 1] = '/' in
       let path = if String.length path == 0 || path.[0] <> '/' then "/" ^ path else path in
       Webs.add_route is_dir_handler path (Right {Webs.request_handler = (env, handler); Webs.error_handler = (env, error_handler)});
       apply_cont cont env (`Record [])
    | `PrimitiveFunction ("addStaticRoute", _), [uriv; pathv; mime_typesv] ->
       if not (!allow_static_routes) then
         eval_error "Attempt to add a static route after they have been disabled";
       let uri = Value.unbox_string uriv in
       let uri = if String.length uri == 0 || uri.[0] <> '/' then "/" ^ uri else uri in
       let path = Value.unbox_string pathv in
       let mime_types = List.map (fun v -> let (x, y) = Value.unbox_pair v in (Value.unbox_string x, Value.unbox_string y)) (Value.unbox_list mime_typesv) in
       Webs.add_route true uri (Left (path, mime_types));
       apply_cont cont env (`Record [])
    | `PrimitiveFunction ("servePages", _), [] ->
       if not (Settings.get_value(dynamic_static_routes)) then
         allow_static_routes := false;
       begin
         Webs.start env >>= fun () ->
         apply_cont cont env (`Record [])
       end
    | `PrimitiveFunction ("serveWebsockets", _), [] ->
        Webs.set_accepting_websocket_requests true;
        apply_cont cont env (`Record [])
    (*****************)
    | `PrimitiveFunction (n,None), args ->
       apply_cont cont env (Lib.apply_pfun n args (Value.Env.request_data env))
    | `PrimitiveFunction (_, Some code), args ->
       apply_cont cont env (Lib.apply_pfun_by_code code args (Value.Env.request_data env))
    | `ClientFunction name, args ->
        let req_data = Value.Env.request_data env in
        client_call req_data name cont args
    | `Continuation c,      [p] -> apply_cont c env p
    | `Continuation _,       _  ->
       eval_error "Continuation applied to multiple (or zero) arguments"
    | `ReifiedContinuation cont', [p] ->
       apply_cont K.(cont' <> cont) env p
    | `ReifiedContinuation _, _ ->
       eval_error "Continuation applied to multiple (or zero) arguments"
    | _                        -> eval_error "Application of non-function"
  and apply_cont (cont : continuation) env v =
    Proc.yield (fun () -> K.Eval.apply ~env cont v)
  and computation_yielding env cont body =
    Proc.yield (fun () -> computation env cont body)
  and computation env (cont : continuation) (bindings, tailcomp) : result =
    match bindings with
      | [] -> tail_computation env cont tailcomp
      | b::bs -> match b with
        | `Let ((var, _) as b, (_, tc)) ->
           let locals = Value.Env.localise env var in
           let cont' =
             K.(let frame = Frame.make (Var.scope_of_binder b) var locals (bs, tailcomp) in
                frame &> cont)
           in
           tail_computation env cont' tc
          (* function definitions are stored in the global fun map *)
          | `Fun _ ->
            computation env cont (bs, tailcomp)
          | `Rec _ ->
            computation env cont (bs, tailcomp)
          | `Alien _ ->
            computation env cont (bs, tailcomp)
          | `Module _ -> failwith "Not implemented interpretation of modules yet"
  and tail_computation env (cont : continuation) : Ir.tail_computation -> result = function
    | `Return v      -> apply_cont cont env (value env v)
    | `Apply (f, ps) -> apply cont env (value env f, List.map (value env) ps)
    | `Special s     -> special env cont s
    | `Case (v, cases, default) ->
      begin match value env v with
        | `Variant (label, _) as v ->
          begin
            match StringMap.lookup label cases, default, v with
            | Some ((var,_), c), _, `Variant (_, v)
            | _, Some ((var,_), c), v ->
              computation (Value.Env.bind var (v, `Local) env) cont c
            | None, _, #Value.t -> eval_error "Pattern matching failed on %s" label
            | _ -> assert false (* v not a variant *)
          end
        | _ -> eval_error "Case of non-variant"
      end
    | `If (c,t,e)    ->
        computation env cont
          (match value env c with
             | `Bool true     -> t
             | `Bool false    -> e
             | _              -> eval_error "Conditional was not a boolean")
  and special env (cont : continuation) : Ir.special -> result = function
    | `Wrong _                    -> raise Exceptions.Wrong
    | `Database v                 -> apply_cont cont env (`Database (db_connect (value env v)))
    | `Table (db, name, keys, (readtype, _, _)) ->
      begin
        (* OPTIMISATION: we could arrange for concrete_type to have
           already been applied here *)
        match value env db, value env name, value env keys, (TypeUtils.concrete_type readtype) with
          | `Database (db, params), name, keys, `Record row ->
	      let unboxed_keys =
		List.map
		  (fun key ->
		    List.map Value.unbox_string (Value.unbox_list key))
		  (Value.unbox_list keys)
	      in
              apply_cont cont env (`Table ((db, params), Value.unbox_string name, unboxed_keys, row))
          | _ -> eval_error "Error evaluating table handle"
      end
    | `Query (range, e, _t) ->
       let range =
         match range with
         | None -> None
         | Some (limit, offset) ->
            Some (Value.unbox_int (value env limit), Value.unbox_int (value env offset)) in
       if Settings.get_value Basicsettings.Shredding.shredding then
         begin
           match Queryshredding.compile_shredded env (range, e) with
           | None -> computation env cont e
           | Some (db, p) ->
               begin
		 if db#driver_name() <> "postgresql"
		 then raise (Errors.Runtime_error "Only PostgreSQL database driver supports shredding");
		 let get_fields t =
                   match t with
                   | `Record fields ->
                       StringMap.to_list (fun name p -> (name, `Primitive p)) fields
                   | _ -> assert false
		 in
                 let execute_shredded_raw (q, t) =
		   Database.execute_select_result (get_fields t) q db, t in
		 let raw_results =
		   Queryshredding.Shred.pmap execute_shredded_raw p in
		 let mapped_results =
		   Queryshredding.Shred.pmap Queryshredding.Stitch.build_stitch_map raw_results in
                 apply_cont cont env
		   (Queryshredding.Stitch.stitch_mapped_query mapped_results)
               end
	 end
       else (* shredding disabled *)
         begin
           match Query.compile env (range, e) with
           | None -> computation env cont e
           | Some (db, q, t) ->
               let (fieldMap, _, _), _ =
		 Types.unwrap_row(TypeUtils.extract_row t) in
               let fields =
		 StringMap.fold
                   (fun name t fields ->
                     match t with
                     | `Present t -> (name, t)::fields
                     | `Absent -> assert false
                     | `Var _ -> assert false)
                   fieldMap
                   []
               in
               apply_cont cont env (Database.execute_select fields q db)
	 end
    | `Update ((xb, source), where, body) ->
      let db, table, field_types =
        match value env source with
          | `Table ((db, _), table, _, (fields, _, _)) ->
            db, table, (StringMap.map (function
                                        | `Present t -> t
                                        | _ -> assert false) fields)
          | _ -> assert false in
      let update_query =
        Query.compile_update db env ((Var.var_of_binder xb, table, field_types), where, body) in
      let () = ignore (Database.execute_command update_query db) in
        apply_cont cont env (`Record [])
    | `Delete ((xb, source), where) ->
      let db, table, field_types =
        match value env source with
          | `Table ((db, _), table, _, (fields, _, _)) ->
            db, table, (StringMap.map (function
                                        | `Present t -> t
                                        | _ -> assert false) fields)
          | _ -> assert false in
      let delete_query =
        Query.compile_delete db env ((Var.var_of_binder xb, table, field_types), where) in
      let () = ignore (Database.execute_command delete_query db) in
        apply_cont cont env (`Record [])
    | `CallCC f ->
       apply cont env (value env f, [`Continuation cont])
    (* Handlers *)
    | `Handle { ih_comp = m; ih_clauses = clauses; ih_depth = depth } ->
       let handler = K.Handler.make ~env ~clauses ~depth in
       let cont = K.set_trap_point ~handler cont in
       computation env cont m
    | `DoOperation (name, v, _) ->
       let vs = List.map (value env) v in
       K.Eval.trap cont (name, Value.box vs)
    (* Session stuff *)
    | `Select (name, v) ->
      let chan = value env v in
      Debug.print ("selecting: " ^ name ^ " from: " ^ Value.string_of_value chan);
      let ch = Value.unbox_channel chan in
      let (outp, _inp) = ch in
      Session.send_from_local (Value.box_variant name (Value.box_unit ())) outp >>= fun _ ->
      OptionUtils.opt_iter Proc.awaken (Session.unblock outp);
      apply_cont cont env chan
    | `Choice (v, cases) ->
      begin
        let chan = value env v in
        Debug.print("choosing from: " ^ Value.string_of_value chan);
        let (_, inp) = Value.unbox_channel chan in
        match Session.receive inp with
          | Some v ->
            let label = fst @@ Value.unbox_variant v in
            Debug.print ("chose label: " ^ label);

              begin
                match StringMap.lookup label cases with
                | Some ((var,_), body) ->
                  computation (Value.Env.bind var (chan, `Local) env) cont body
                | None -> eval_error "Choice pattern matching failed"
              end
          | None ->
             let choice_frame =
               K.Frame.of_expr env (`Special (`Choice (v, cases)))
            in
            Session.block inp (Proc.get_current_pid ());
            Proc.block (fun () -> apply_cont K.(choice_frame &> cont) env (`Record []))
      end
  and finish env v = Proc.finish (env, v)
    (*****************)

  let reify k = `ReifiedContinuation k
  let eval : Value.env -> program -> result =
    fun env -> computation env K.empty

  let run_program_with_cont : Value.continuation -> Value.env -> Ir.program ->
    (Value.env * Value.t) =
    fun cont env program ->
      try (
        Proc.run (fun () -> computation env cont program)
      ) with
        | NotFound s -> failwith ("Internal error: NotFound " ^ s ^
                                     " while interpreting.")

  let run_program : Value.env -> Ir.program -> (Value.env * Value.t) =
    fun env program ->
      try (
        Proc.run (fun () -> eval env program)
      ) with
        | NotFound s -> failwith ("Internal error: NotFound " ^ s ^
                                     " while interpreting.")
        | Not_found  -> failwith ("Internal error: Not_found while interpreting.")

  let run_defs : Value.env -> Ir.binding list -> Value.env =
    fun env bs ->
    let (env, _value) = run_program env (bs, `Return(`Extend(StringMap.empty, None))) in env

  (** [apply_cont_toplevel cont env v] applies a continuation to a value
      and returns the result. Finishing the main thread normally comes
      here immediately. *)
  let apply_cont_toplevel (cont : continuation) env v =
    try snd (Proc.run (fun () -> apply_cont cont env v)) with
    | NotFound s -> failwith ("Internal error: NotFound " ^ s ^
                                " while interpreting.")

  let apply_with_cont (cont : continuation) env (f, vs) =
    try snd (Proc.run (fun () -> apply cont env (f, vs))) with
    |  NotFound s -> failwith ("Internal error: NotFound " ^ s ^
                                 " while interpreting.")


  let apply_toplevel env (f, vs) = apply_with_cont K.empty env (f, vs)

  let eval_toplevel env program =
    try snd (Proc.run (fun () -> eval env program)) with
    | NotFound s -> failwith ("Internal error: NotFound " ^ s ^
                                " while interpreting.")
end

module type EVAL = functor (Webs : WEBSERVER) -> sig
    include EVALUATOR
end
module Eval : EVAL = functor (Webs : WEBSERVER) ->
struct
  module rec Eval : EVALUATOR
    with type result = Proc.thread_result Lwt.t = Evaluator(Value.Continuation.Evaluation(Eval))(Webs)
  include Eval
end
