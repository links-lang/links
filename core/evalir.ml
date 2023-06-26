open CommonTypes
open Webserver_types
open Ir
open Lwt
open Utility
open Proc
open Var

let internal_error message =
  Errors.internal_error ~filename:"evalir.ml" ~message

let lookup_fun = Tables.lookup Tables.fun_defs
let find_fun = Tables.find Tables.fun_defs

let dynamic_static_routes
  = Settings.(flag "dynamic_static_routes"
              |> convert parse_bool
              |> sync)
let allow_static_routes = ref true


module type EVALUATOR = sig
  type v = Value.t
  type result = Proc.thread_result Lwt.t

  val reify : Value.resumption -> v
  val error : string -> 'a
  val computation : Value.env -> Value.continuation -> Ir.computation -> result
  val finish : Value.env -> v -> result

  val apply : Value.continuation -> Value.env -> v * v list -> result
  val apply_cont : Value.continuation -> Value.env -> v -> result
  val run_program : Value.env -> Ir.program -> (Value.env * v)
end

module Exceptions = struct
  exception EvaluationError of string
  exception Wrong
end

module Evaluator = functor (ContEval : Value.CONTINUATION_EVALUATOR with type v = Value.t
                                                                    and type result = Proc.thread_result Lwt.t
                                                                    and type 'v t := 'v Value.Continuation.t
                                                                    and type 'v resumption := 'v Value.Continuation.resumption)
                   (Webs : WEBSERVER) ->
struct
  type v = Value.t
  type result = Proc.thread_result Lwt.t
  type continuation = Value.continuation
  type resumption = Value.resumption

  module K = struct
    include Value.Continuation
    module Eval = ContEval
  end

  let error msg : 'a = raise (Exceptions.EvaluationError msg)

  let eval_error fmt : 'r =
    Printf.ksprintf error fmt

  let type_error ~action expected value =
    eval_error "Attempting to %s %s (need %s instead)" action (Value.string_of_value value) expected

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
    | Some (_finfo, _, None, location) ->
      begin
        match location with
        | Location.Server | Location.Unknown ->
          (* TODO: perhaps we should actually use env here - and make
             sure we only call this function when it is sufficiently
             small *)
          Some (`FunctionPtr (f, None))
        | Location.Client ->
          Some (`ClientFunction (Js.var_name_var f))
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
         Some (Webs.get_websocket_url ())
       else
         None
     in
     let st = List.fold_left
       (fun st_acc arg -> ResolveJsonState.add_value_information arg st_acc)
       (JsonState.empty client_id conn_url) args in
     let st = ResolveJsonState.add_ap_information client_id st in
     let st = ResolveJsonState.add_process_information client_id st in
     let st = ResolveJsonState.add_channel_information client_id st in
     Json.jsonize_call st (Serialisation.MarshalSerialiser.Continuation.save continuation) name args
      |> Json.json_to_string

   let client_call :
     RequestData.request_data ->
     string ->
     Value.continuation ->
     Value.t list ->
     result =

       fun req_data name cont args ->
         if not(Settings.get webs_running) then
           raise (Errors.forbidden_client_call name "outside of web mode");
         if not(RequestData.is_ajax_call (RequestData.get_cgi_parameters req_data)) then
           raise (Errors.forbidden_client_call name "before server page is ready");
         (*if not(Proc.singlethreaded()) then
           raise (internal_error "Remaining procs on server at client call!"); *)
         Debug.print("Making client call to " ^ name);
  (*        Debug.print("Call package: "^serialize_call_to_client (cont, name, args)); *)
         let call_package =
           Utility.base64encode @@
             serialize_call_to_client req_data (cont, name, args) in
         Proc.abort ("text/plain", call_package)

    let handle_session_exception raise_env frames =
      let affected_channels =
        ChannelVarUtils.affected_channels raise_env frames in
      (* List.iter (fun c -> Printf.printf "%s\n" (Value.string_of_value c)) affected_channels *)
      List.fold_left (fun acc v -> acc >>= (fun _ -> Value.unbox_channel v |> Session.cancel))
        (Lwt.return ())
        affected_channels

  (** {0 Evaluation} *)
  let rec value env : Ir.value -> Value.t Lwt.t = fun v ->
    let constant = function
      | Constant.Bool   b -> `Bool b
      | Constant.Int    n -> `Int n
      | Constant.Char   c -> `Char c
      | Constant.String s -> Value.box_string s
      | Constant.Float  f -> `Float f
      | Constant.DateTime dt -> Value.box_datetime dt in

    match v with
    | Constant c -> Lwt.return (constant c)
    | Variable var -> Lwt.return (lookup_var var env)
    | Extend (fields, r) ->
        begin
          opt_app (value env) (Lwt.return (`Record [])) r >>= fun res ->
          match res with
            | `Record fs ->
                let fields = StringMap.bindings fields in
                LwtHelpers.foldr_lwt
                   (fun (label, v) (fs: (string * Value.t) list)  ->
                      if List.mem_assoc label fs then
                        eval_error
                          "Error adding fields: label %s already present" label
                      else
                        value env v >>= fun v ->
                        Lwt.return ((label, v)::fs))
                   fields
                   (Lwt.return []) >>= fun res ->
                Lwt.return (`Record (res @ fs))
            | v -> type_error ~action:"add field to" "record" v
        end
    | Project (label, r) ->
        value env r >>= fun v ->
        begin
          match v with
            | `Record fields when List.mem_assoc label fields ->
                Lwt.return (List.assoc label fields)
            | v -> type_error ~action:("projecting label " ^ label) "record" v
        end
    | Erase (labels, r) ->
        value env r >>= fun v ->
        begin
          match v with
            | `Record fields when
                StringSet.for_all (fun label -> List.mem_assoc label fields) labels ->
                  Lwt.return (
                `Record (StringSet.fold (fun label fields -> List.remove_assoc label fields) labels fields))
            | v ->
               type_error ~action:(Printf.sprintf "erase labels {%s}" (String.concat "," (StringSet.elements labels)))
                 "record" v
        end
    | Inject (label, v, _) ->
        value env v >>= fun v -> Lwt.return (`Variant (label, v))
    | TAbs (_, v) -> value env v
    | TApp (v, _) -> value env v
    | XmlNode (tag, attrs, children) ->
          LwtHelpers.foldr_lwt
            (fun v children ->
              value env v >>= fun v ->
               Lwt.return (List.map Value.unbox_xml (Value.unbox_list v) @ children))
            children (Lwt.return []) >>= fun children ->
          let attrs = StringMap.bindings attrs in
          LwtHelpers.foldr_lwt
            (fun (name, v) attrs ->
               value env v >>= fun str ->
               Lwt.return (Value.Attr (name, Value.unbox_string str) :: attrs))
            (List.rev attrs) (Lwt.return children) >>= fun children ->
          Lwt.return (Value.box_list [Value.box_xml (Value.Node (tag, children))])
    | ApplyPure (f, args) ->
      value env f >>= fun f ->
      (LwtHelpers.sequence (List.map (value env) args)) >>= fun args ->
      Proc.atomically (fun () -> apply K.empty env (f, args))
    | Closure (f, _, v) ->
      value env v >>= fun v ->
      Lwt.return (`FunctionPtr (f, Some v))
    | Coerce (v, _) -> value env v
  and apply_access_point (cont : continuation) env : Value.spawn_location -> result = function
      | `ClientSpawnLoc cid ->
          let apid = Session.new_client_access_point cid in
          apply_cont cont env (`AccessPointID (`ClientAccessPoint (cid, apid)))
      | `ServerSpawnLoc ->
          let apid = Session.new_server_access_point () in
          apply_cont cont env (`AccessPointID (`ServerAccessPoint apid))
  and apply (cont : continuation) env : Value.t * Value.t list -> result =
    let invoke_session_exception () =
      special env cont
        (DoOperation (Value.session_exception_operation, [], Types.Not_typed))
    in
    function
    | `FunctionPtr (f, fvs), ps ->
      let (_finfo, (xs, body), z, _location) =
        try find_fun f
        with NotFound _ ->
          raise (internal_error ("Failed to find function name: " ^ (string_of_int f)))
      in
      let env =
        match z, fvs with
        | Some z, Some fvs -> Value.Env.bind z (fvs, Scope.Local) env
        | None, None -> env
        | _, _ -> assert false
      in
      (* extend env with arguments *)
      let env =
        List.fold_right2 (fun x p -> Value.Env.bind x (p, Scope.Local)) xs ps env
      in
      computation_yielding env cont body
    | `PrimitiveFunction ("registerEventHandlers",_), [hs] ->
      let key = EventHandlers.register hs in
      apply_cont cont env (`String (string_of_int key))
    (* start of mailbox stuff *)
    | `PrimitiveFunction ("Send",_), [pid; msg] ->
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
           UnknownProcessID id ->
           Debug.print (
               "Couldn't deliver message because destination process " ^
                 (ProcessTypes.ProcessID.to_string id) ^ " has no mailbox.");
           Lwt.return ()) >>= fun _ ->
        apply_cont cont env (`Record [])
    | `PrimitiveFunction ("spawnAt",_), [loc; func] ->
        begin match loc with
          | `SpawnLocation (`ClientSpawnLoc client_id) ->
             Proc.create_client_process client_id func >>= fun new_pid ->
             apply_cont cont env (`Pid (`ClientPid (client_id, new_pid)))
          | `SpawnLocation (`ServerSpawnLoc) ->
             let var = Var.dummy_var in
             let frame = K.Frame.make Scope.Local var Value.Env.empty ([], Apply (Variable var, [])) in
             Proc.create_process false
               (fun () -> apply_cont K.(frame &> empty) env func) >>= fun new_pid ->
             apply_cont cont env (`Pid (`ServerPid new_pid))
          | _ -> assert false
        end
    | `PrimitiveFunction ("spawnAngelAt",_), [loc; func] ->
        begin match loc with
        | `SpawnLocation (`ClientSpawnLoc client_id) ->
           Proc.create_client_process client_id func >>= fun new_pid ->
           apply_cont cont env (`Pid (`ClientPid (client_id, new_pid)))
        | `SpawnLocation (`ServerSpawnLoc) ->
           let var = Var.dummy_var in
           let frame = K.Frame.make Scope.Local var Value.Env.empty ([], Apply (Variable var, [])) in
           Proc.create_process true
             (fun () -> apply_cont K.(frame &> empty) env func) >>= fun new_pid ->
           apply_cont cont env (`Pid (`ServerPid new_pid))
        | _ -> assert false
        end
    | `PrimitiveFunction ("spawnWait", _), [func] ->
        let our_pid = Proc.get_current_pid () in
        (* Create the new process *)
        let var = Var.dummy_var in
        let frame = K.Frame.make Scope.Local var Value.Env.empty ([], Apply (Variable var, [])) in
        Proc.create_spawnwait_process our_pid
          (fun () -> apply_cont K.(frame &> empty) env func) >>= fun child_pid ->
        (* Now, we need to block this process until the spawned process has evaluated to a value.
         * The idea here is that we have a second function, spawnWait', which grabs the result
         * from proc.ml. *)
        let fresh_var = Var.fresh_raw_var () in
        let extended_env =
          Value.Env.bind fresh_var (Value.box_pid (`ServerPid child_pid), Scope.Local) env in
        let grab_frame =
          K.Frame.of_expr extended_env
                          (Lib.prim_appln "spawnWait'" [Variable fresh_var]) in

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
              raise (internal_error "Cannot *yet* accept on a client AP on the server")
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
              raise (internal_error "Cannot *yet* request from a client-spawned AP on the server")
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
      let open Session in
      Debug.print ("sending: " ^ Value.string_of_value v ^ " to channel: " ^ Value.string_of_value chan);
      let unboxed_chan = Value.unbox_channel chan in
      let outp = Session.send_port unboxed_chan in
      Session.send_from_server v outp >>= fun res ->
        begin
          match res with
            | SendOK -> apply_cont cont env chan
            | SendPartnerCancelled ->
                (* If send fails, we need to cancel all carried channels *)
                let contained_channels = Value.get_contained_channels v in
                List.fold_left
                  (fun acc c -> acc >>= fun _ -> Session.cancel c)
                  (Lwt.return ()) contained_channels >>= fun _ ->
                apply_cont cont env chan
        end
    | `PrimitiveFunction ("receive", _), [chan] ->
      begin
        let open Session in
        Debug.print("receiving from channel: " ^ Value.string_of_value chan);
        let unboxed_chan = Value.unbox_channel chan in
        let peer_ep = Session.send_port unboxed_chan in
        let block () =
          (* Here, we have to extend the environment with a fresh variable
           * representing the channel, since we can't create an IR application
           * involving a Value.t (only an Ir.value).
           * This *should* be safe, but still feels a bit unsatisfactory.
           * It would be nice to refine this further. *)
          let fresh_var = Var.fresh_raw_var () in
          let extended_env = Value.Env.bind fresh_var (chan, Scope.Local) env in
          let grab_frame = K.Frame.of_expr extended_env (Lib.prim_appln "receive" [Variable fresh_var]) in
          let inp = (snd unboxed_chan) in
          Session.block inp (Proc.get_current_pid ());
          Proc.block (fun () -> apply_cont K.(grab_frame &> cont) env (`Record [])) in

        let throw_or_block () =
          if Settings.get (Basicsettings.Sessions.exceptions_enabled) then
            invoke_session_exception ()
          else block () in

        if Session.is_endpoint_cancelled peer_ep then
          throw_or_block ()
        else
          match Session.receive unboxed_chan with
            | ReceiveOK v ->
              Debug.print ("grabbed: " ^ Value.string_of_value v);
              apply_cont cont env (Value.box_pair v chan)
            | ReceiveBlocked -> block ()
            | ReceivePartnerCancelled ->
              Session.cancel unboxed_chan >>= fun _ ->
              throw_or_block ()
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
    | `PrimitiveFunction ("cancel", _), [chan] ->
        Session.cancel (Value.unbox_channel chan) >>= fun _ ->
        apply_cont cont env (`Record [])
    | `PrimitiveFunction ("close", _), [chan] ->
        Session.close (Value.unbox_channel chan);
        apply_cont cont env (`Record [])
    (* end of session stuff *)
    | `PrimitiveFunction ("unsafeAddRoute", _), [pathv; handler; error_handler] ->
       let path = Value.unbox_string pathv in
       let is_dir_handler = String.length path > 0 && path.[String.length path - 1] = '/' in
       let path = if String.length path == 0 || path.[0] <> '/' then "/" ^ path else path in
       let path =
         match Settings.get (Webserver_types.internal_base_url) with
         | None -> path
         | Some base_url ->
            let base_url = Utility.strip_slashes base_url in
            "/" ^ base_url ^ path
       in
       Webs.add_route is_dir_handler path (Right {Webs.request_handler = (env, handler); Webs.error_handler = (env, error_handler)});
       apply_cont cont env (`Record [])
    | `PrimitiveFunction ("addStaticRoute", _), [uriv; pathv; mime_typesv] ->
       if not (!allow_static_routes) then
         eval_error "Attempt to add a static route after they have been disabled";
       let uri = Value.unbox_string uriv in
       let uri = if String.length uri == 0 || uri.[0] <> '/' then "/" ^ uri else uri in
       let uri =
         match Webs.get_internal_base_url () with
         | None -> uri
         | Some base_uri ->
            let base_uri = Utility.strip_slashes base_uri in
            "/" ^ base_uri ^ uri
       in
       let path = Value.unbox_string pathv in
       let mime_types = List.map (fun v -> let (x, y) = Value.unbox_pair v in (Value.unbox_string x, Value.unbox_string y)) (Value.unbox_list mime_typesv) in
       Webs.add_route true uri (Left (path, mime_types));
       apply_cont cont env (`Record [])
    | `PrimitiveFunction ("servePages", _), [] ->
       if not (Settings.get (dynamic_static_routes)) then
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
    | `Resumption r, vs ->
       resume env cont r vs
    | `Alien, _ -> eval_error "Cannot make alien call on the server.";
    | `ClientClosure index, vs ->
       let req_data = Value.Env.request_data env in
       client_call req_data "_$ClosureTable.apply" cont (`Int index :: vs)
    | v, _ -> type_error ~action:"apply" "function" v
  and resume env (cont : continuation) (r : resumption) vs =
    Proc.yield (fun () -> K.Eval.resume ~env cont r vs)
  and apply_cont (cont : continuation) env v =
    Proc.yield (fun () -> K.Eval.apply ~env cont v)
  and computation_yielding env cont body =
    Proc.yield (fun () -> computation env cont body)
  and computation env (cont : continuation) (bindings, tailcomp) : result =
    match bindings with
      | [] -> tail_computation env cont tailcomp
      | b::bs ->
         match b with
         | Let (b, (_, tc)) ->
            let var = Var.var_of_binder b in
            let locals = Value.Env.localise env var in
            let cont' =
              K.(let frame = Frame.make (Var.scope_of_binder b) var locals (bs, tailcomp) in
                 frame &> cont)
            in
            tail_computation env cont' tc
         (* function definitions are stored in the global fun map *)
         | Fun _ ->
            computation env cont (bs, tailcomp)
         | Rec _ ->
            computation env cont (bs, tailcomp)
         | Alien { binder; _ } ->
            let var = Var.var_of_binder binder in
            let scope = Var.scope_of_binder binder in
            computation (Value.Env.bind var (`Alien, scope) env) cont (bs, tailcomp)
         | Module _ -> raise (internal_error "Not implemented interpretation of modules yet")
  and tail_computation env (cont : continuation) : Ir.tail_computation -> result = function
    | Ir.Return v   ->
        value env v >>= fun v ->
        apply_cont cont env v
    | Apply (f, ps) ->
        value env f >>= fun f ->
        LwtHelpers.sequence (List.map (value env) ps) >>= fun ps ->
        apply cont env (f, ps)
    | Special s     -> special env cont s
    | Case (v, cases, default) ->
      value env v >>= fun v ->
      begin match v with
        | `Variant (label, _) as v ->
          begin
            match StringMap.lookup label cases, default, v with
            | Some (b, c), _, `Variant (_, v)
            | _, Some (b, c), v ->
               let var = Var.var_of_binder b in
               computation (Value.Env.bind var (v, Scope.Local) env) cont c
            | None, _, #Value.t -> eval_error "Pattern matching failed on %s" label
            | _ -> assert false (* v not a variant *)
          end
        | v -> type_error ~action:"take case of" "variant" v
      end
    | If (c,t,e)    ->
        value env c >>= fun c ->
        computation env cont
          (match c with
             | `Bool true     -> t
             | `Bool false    -> e
             | _              -> eval_error "Conditional was not a boolean")

  and eval_nested_query env cont (db: Value.database) pkg =
    let get_fields t =
      match t with
        | `Record fields ->
            StringMap.to_list (fun name p -> (name, Types.Primitive p)) fields
        | _ -> assert false
    in
    let execute_shredded_raw (q, t) =
      let q = Sql.inline_outer_with q in
      let q = db#string_of_query q in
      Database.execute_select_result (get_fields t) q db, t in
    let raw_results =
      EvalNestedQuery.Shred.pmap execute_shredded_raw pkg in
    let mapped_results =
      EvalNestedQuery.Shred.pmap EvalNestedQuery.Stitch.build_stitch_map raw_results in
    apply_cont cont env
      (EvalNestedQuery.Stitch.stitch_mapped_query mapped_results)

  and special env (cont : continuation) : Ir.special -> result =
    let unpack_lens l = match l with | `Lens l -> l | _ -> raise (internal_error "Expected a lens.") in
    let invoke_session_exception () =
      special env cont (DoOperation (Value.session_exception_operation,
        [], Types.Not_typed)) in
    function
    | Wrong _                    -> raise Exceptions.Wrong
    | Database v                 ->
        value env v >>= fun v ->
        apply_cont cont env (`Database (db_connect v))
    | Lens (table, t) ->
      begin
          let sort = Lens.Type.sort t in
          value env table >>= fun table ->
          let open Value.Table in
          match table with
            | `Table tinfo ->
              let (db, cstr) = tinfo.database in
              let table = tinfo.name in
              let database = Lens_database_conv.lens_db_of_db cstr db in
              let sort = Lens.Sort.update_table_name sort ~table in
              let table = Lens_database_conv.lens_table_of_table tinfo in
                 apply_cont cont env (`Lens (database, Lens.Value.Lens { sort; table; }))
            | `List records ->
              let records = List.map Lens_value_conv.lens_phrase_value_of_value records in
              apply_cont cont env (`Lens (Lens.Database.dummy_database, Lens.Value.LensMem { records; sort; }))
            | _ -> raise (internal_error ("Unsupported underlying lens value."))
      end
    | LensSerial { lens; columns; _ } ->
      let open Lens in
      value env lens >>= fun lens ->
      let db, lens = unpack_lens lens in
      let lens = Value.set_serial ~columns lens in
      apply_cont cont env (`Lens (db, lens))
    | LensDrop {lens; drop; key; default; _} ->
        let open Lens in
        value env lens >|= unpack_lens >>= fun (db, lens) ->
        value env default >|= Lens_value_conv.lens_phrase_value_of_value >>= fun default ->
        let sort =
          Lens.Sort.drop_lens_sort
            (Lens.Value.sort lens)
            ~drop:[drop]
            ~default:[default]
            ~key:(Alias.Set.singleton key)
          |> Lens_errors.unpack_type_drop_lens_result ~die:(eval_error "%s")
        in
        apply_cont cont env (`Lens (db, Value.LensDrop { lens; drop; key; default; sort }))
    | LensSelect { lens; predicate; _ } ->
        let open Lens in
        value env lens >|= unpack_lens >>= fun (db, lens) ->
        let predicate =
          match predicate with
          | Static predicate -> predicate
          | Dynamic predicate ->
            let p = Lens_ir_conv.lens_sugar_phrase_of_ir predicate env
                    |> Lens_ir_conv.Of_ir_error.unpack_exn ~die:(eval_error "%s") in
            p in
        let sort =
          Lens.Sort.select_lens_sort
            (Lens.Value.sort lens)
            ~predicate
          |> Lens_errors.unpack_sort_select_result ~die:(eval_error "%s")
        in
        apply_cont cont env (`Lens (db, Value.LensSelect {lens; predicate; sort}))
    | LensJoin { left; right; on; del_left; del_right; _ } ->
        let open Lens in
        value env left >|= unpack_lens >>= fun (db1, lens1) ->
        value env right >|= unpack_lens >>= fun (db2, lens2) ->
        let left, right=
          if Lens.Sort.join_lens_should_swap
               (Lens.Value.sort lens1)
               (Lens.Value.sort lens2) ~on
          then lens2, lens1
          else lens1, lens2
        in
        let on = List.map (fun a -> a, a, a) on in
        let sort, on =
          Lens.Sort.join_lens_sort
            (Lens.Value.sort lens1)
            (Lens.Value.sort lens2) ~on
          |> Lens_errors.unpack_sort_join_result ~die:(eval_error "%s") in
        let open Lens.Database in
        if db1.serialize () <> db2.serialize ()
        then eval_error "Lenses require the same database connection.";
        apply_cont cont env (`Lens (db1, Value.LensJoin {left; right; on; del_left; del_right; sort}))
    | LensCheck (lens, _typ) ->
        (* TODO: defer dynamic lens check failures to
           the lens check evaluation *)
        value env lens >>= apply_cont cont env
    | LensGet (lens, _rtype) ->
        value env lens >|= unpack_lens >>= fun (db, lens) ->
        (* let callfn = fun fnptr -> fnptr in *)
        let res = Lens.Value.lens_get ~db lens in
        let res = List.map Lens_value_conv.value_of_lens_phrase_value res |> Value.box_list in
          apply_cont cont env res
    | LensPut (lens, data, _rtype) ->
        value env lens >|= unpack_lens >>= fun (db, lens) ->
        value env data >|= Value.unbox_list >>= fun data ->
        let data = List.map Lens_value_conv.lens_phrase_value_of_value data in
        let behaviour =
          if Settings.get Lens.classic_lenses
          then Lens.Eval.Classic
          else Lens.Eval.Incremental in
        Lens.Eval.put ~behaviour ~db lens data |> Lens_errors.unpack_eval_error ~die:(eval_error "%s");
        Value.box_unit () |> apply_cont cont env
    | Table { database = db; table = name; keys; temporal_fields;
                table_type = (temporality, readtype, _, _) } ->
      begin
        (* OPTIMISATION: we could arrange for concrete_type to have
           already been applied here *)
        value env db >>= fun db ->
        value env name >>= fun name ->
        value env keys >>= fun keys ->
        match db, name, keys, (TypeUtils.concrete_type readtype) with
          | `Database (db, params), name, keys, Types.Record (Types.Row row) ->
            let unboxed_keys =
              List.map
                (fun key ->
                  List.map Value.unbox_string (Value.unbox_list key))
                (Value.unbox_list keys)
            in
            let tbl =
                Value.make_table ~database:(db, params) ~name:(Value.unbox_string name)
                    ~keys:unboxed_keys ~temporality ~temporal_fields ~row
            in
            apply_cont cont env (`Table tbl)
          | _ -> eval_error "Error evaluating table handle"
      end
    | Query (range, policy, e, _t) ->
        begin
          match range with
          | None -> Lwt.return None
          | Some (limit, offset) ->
              value env limit >>= fun limit ->
              value env offset >>= fun offset ->
              Lwt.return (Some (Value.unbox_int limit, Value.unbox_int offset))
       end >>= fun range ->
        (* wricciot - this code matches over the policy twice: first to check whether Nested evaluation was requested,
           and, if it wasn't, to choose between the standard (Flat) or Mixing evaluator...
           can we separate the logic for Nested and that for Flat/Mixing more cleanly? *)
         begin match policy with
           | QueryPolicy.Nested ->
               begin
                 if range != None then eval_error "Range is not supported for nested queries";
                 match EvalNestedQuery.compile_shredded env e with
                   | None -> computation env cont e
                   | Some (db, pkg) when db#supports_shredding () ->
                       eval_nested_query env cont db pkg
                   | Some(db,_) ->
                       let error_msg =
                         Printf.sprintf
                           "The database driver '%s' does not support nested query results."
                           (db#driver_name ())
                       in
                       raise (Errors.runtime_error error_msg)
               end
           | _ ->
               let evaluator e =
                 match policy with
                 | QueryPolicy.Flat when not (Settings.get Database.mixing_norm) -> EvalQuery.compile env (range, e)
                 | _ -> EvalMixingQuery.compile_mixing ~delateralize:policy env (range, e)
               in
               begin
                  match evaluator e with
                  | None -> computation env cont e
                  | Some (db, q, t) ->
                      let q = db#string_of_query ~range q in
                      let (fieldMap, _, _) =
                        let r, _ = Types.unwrap_row (TypeUtils.extract_row t) in
                        TypeUtils.extract_row_parts r in
                      let fields =
                        StringMap.fold
                          (fun name t fields ->
                            let open Types in
                            match t with
                              | Present t -> (name, t)::fields
                              | _ -> assert false)
                          fieldMap
                          []
                      in
                      apply_cont cont env (Database.execute_select fields q db)
               end
         end
    | TemporalJoin (tmp, e, _t) ->
        begin
          match EvalNestedQuery.compile_temporal_join tmp env e with
            | (db, pkg) when db#supports_shredding () ->
                eval_nested_query env cont db pkg
            | (db, _) ->
                let error_msg =
                  Printf.sprintf
                    "The database driver '%s' does not support nested query results."
                    (db#driver_name ())
                in
                raise (Errors.runtime_error error_msg)
        end
    | InsertRows (tmp, source, rows) ->
        begin
          value env source >>= fun source ->
          value env rows >>= fun raw_rows ->
          let open Value.Table in
          match source, raw_rows with
          | `Table _, `List [] ->  apply_cont cont env (`Record [])
          | `Table { database = (db, _) ; name = table_name; temporal_fields; _ }, raw_rows ->
              let (field_names,rows) = Value.row_columns_values raw_rows in
              let q =
                  match tmp with
                    (* None: Current time insertion *)
                    | None ->
                        QueryLang.insert table_name field_names rows
                        |> db#string_of_query
                    | Some TransactionTimeInsertion ->
                        let (from_field, to_field) = Option.get temporal_fields in
                        TemporalQuery.TransactionTime.insert
                            table_name field_names from_field to_field rows
                        |> db#string_of_query
                    | Some (ValidTimeInsertion CurrentInsertion)  ->
                        let (from_field, to_field) = Option.get temporal_fields in
                        TemporalQuery.ValidTime.Insert.current
                            table_name field_names from_field to_field rows
                        |> db#string_of_query
                    | Some (ValidTimeInsertion SequencedInsertion)  ->
                        let (from_field, to_field) = Option.get temporal_fields in
                        TemporalQuery.ValidTime.Insert.sequenced
                            table_name from_field to_field raw_rows
                        |> db#string_of_query
              in
              Debug.print ("RUNNING INSERT QUERY:\n" ^ q);
              let () = ignore (Database.execute_command q db) in
              apply_cont cont env (`Record [])
          | _ -> raise (internal_error "insert row into non-database")
        end
  (* FIXME:

     Choose a semantics for InsertReturning.

     Currently it is well-defined if exactly one row is inserted, but
     is not necessarily well-defined otherwise.

     Perhaps the easiest course of action is to restrict it to the
     case of inserting a single row.
  *)
    | InsertReturning (_tmp, source, rows, returning) ->
        begin
          value env source >>= fun source ->
          value env rows >>= fun rows ->
          value env returning >>= fun returning ->
          match source, rows, returning with
          | `Table _, `List [], _ ->
              raise (internal_error "InsertReturning: undefined for empty list of rows")
          | `Table Value.Table.{ database = (db, _); name = table_name; _ }, rows, returning ->
              let (field_names,vss) = Value.row_columns_values rows in
              let returning = Value.unbox_string returning in
              let q = QueryLang.insert table_name field_names vss in
              Debug.print ("RUNNING INSERT ... RETURNING QUERY:\n" ^
                           String.concat "\n"
                             (db#make_insert_returning_query returning q));
              apply_cont cont env (Database.execute_insert_returning returning q db)
          | _ -> raise (internal_error "insert row into non-database")
        end
    | Update (upd, (xb, source), where, body) ->
      begin
        value env source >>= fun source ->
        match source with
          | `Table { Value.Table.database = (db, _); name = table;
                     row = (fields, _, _); temporal_fields; _ } ->
              let field_types =
                StringMap.map
                    (function
                       | Types.Present t -> t
                       | _ -> assert false) fields
              in
              Lwt.return
                (db, table, field_types, temporal_fields)
          | _ -> assert false
      end >>= fun (db, table, field_types, temporal_fields) ->
      let update_query =
        begin
            match upd with
                | Some (ValidTimeUpdate upd) ->
                    (* 'get' safe as VT updates only possible on VT tables *)
                    let (from_field, to_field) = Option.get temporal_fields in
                    TemporalQuery.ValidTime.compile_update upd db env
                      ((Var.var_of_binder xb, table, field_types), where, body)
                      from_field to_field
                | Some TransactionTimeUpdate ->
                    (* 'get' safe as TT updates only possible on TT tables *)
                    let (from_field, to_field) = Option.get temporal_fields in
                    TemporalQuery.TransactionTime.compile_update env
                      ((Var.var_of_binder xb, table, field_types), where, body)
                      from_field to_field
                | None ->
                    Query.compile_update db env
                        ((Var.var_of_binder xb, table, field_types), where, body)
        end
      in
      let () = ignore (Database.execute_command (db#string_of_query update_query) db) in
        apply_cont cont env (`Record [])
    | Delete (del, (xb, source), where) ->
        value env source >>= fun source ->
        let open Value.Table in
        begin
            match source with
              | `Table { database = (db, _); name = table; row = (fields, _, _); temporal_fields; _ } ->
                  let field_types =
                    StringMap.map
                        (function
                           | Types.Present t -> t
                           | _ -> assert false) fields
                  in
                  Lwt.return
                    (db, table, field_types, temporal_fields)
              | _ -> assert false
        end
      >>= fun (db, table, field_types, temporal_fields) ->
      let delete_query =
          (* Same justifications apply for Option.get here *)
          match del with
            | Some (ValidTimeDeletion del) ->
                let (from_field, to_field) = Option.get temporal_fields in
                TemporalQuery.ValidTime.compile_delete
                    del db env
                    ((Var.var_of_binder xb, table, field_types), where)
                    from_field to_field
            | Some TransactionTimeDeletion ->
                let to_field = Option.get temporal_fields |> snd in
                TemporalQuery.TransactionTime.compile_delete
                    db env
                    ((Var.var_of_binder xb, table, field_types), where)
                    to_field
            | None ->
                Query.compile_delete db env
                    ((Var.var_of_binder xb, table, field_types), where)
      in
      let () = ignore (Database.execute_command (db#string_of_query delete_query) db) in
        apply_cont cont env (`Record [])
    | CallCC f ->
       value env f >>= fun f ->
       apply cont env (f, [`Continuation cont])
    (* Handlers *)
    | Handle { ih_comp = m; ih_cases = clauses; ih_return = return; ih_depth = depth } ->
       (* Slight hack *)
       begin
         match depth with
         | Shallow -> Lwt.return (env, `Shallow)
         | Deep params ->
            LwtHelpers.foldr_lwt
              (fun (b, initial_value) (env, vars) ->
                let var = Var.var_of_binder b in
                value env initial_value >>= fun initial_value ->
                Lwt.return (Value.Env.bind var (initial_value, Scope.Local) env, var :: vars))
              params (Lwt.return (env, [])) >>= fun (env, vars) ->
            Lwt.return (env, `Deep vars)
       end >>= fun (env, depth) ->
       let handler = K.Handler.make ~env ~return ~clauses ~depth in
       let cont = K.set_trap_point ~handler cont in
       computation env cont m
    | DoOperation (name, vs, _) ->
       let open Value.Trap in
       begin
         match List.map (value env) vs with
         | [v] -> v
         | vs  ->
             LwtHelpers.sequence vs >>= fun vs ->
             Lwt.return (Value.box vs)
       end >>= fun v ->
       begin
       match K.Eval.trap cont (name, v) with
         | Trap cont_thunk -> cont_thunk ()
         | SessionTrap st_res ->
             handle_session_exception env st_res.frames >>= fun _ ->
             st_res.continuation_thunk ()
         | UnhandledSessionException frames ->
             Debug.print ("unhandled session exception");
             handle_session_exception env frames >>= fun _ ->
             Proc.finish (env, Value.box_unit())
       end
    (* Session stuff *)
    | Select (name, v) ->
      value env v >>= fun chan ->
      Debug.print ("selecting: " ^ name ^ " from: " ^ Value.string_of_value chan);
      let ch = Value.unbox_channel chan in
      let (outp, _inp) = ch in
      Session.send_from_server (Value.box_variant name (Value.box_unit ())) outp >>= fun _ ->
      OptionUtils.opt_iter Proc.awaken (Session.unblock outp);
      apply_cont cont env chan
    | Choice (v, cases) ->
      begin
        let open Session in
        value env v >>= fun chan ->
        Debug.print("choosing from: " ^ Value.string_of_value chan);
        let unboxed_chan = Value.unbox_channel chan in
        let inp = receive_port unboxed_chan in
        let block () =
          let choice_frame =
             K.Frame.of_expr env (Special (Choice (v, cases)))
          in
             Session.block inp (Proc.get_current_pid ());
             Proc.block (fun () -> apply_cont K.(choice_frame &> cont) env (`Record [])) in

        match Session.receive unboxed_chan with
          | ReceiveOK v ->
            let label = fst @@ Value.unbox_variant v in
            Debug.print ("chose label: " ^ label);

              begin
                match StringMap.lookup label cases with
                | Some (b, body) ->
                   let var = Var.var_of_binder b in
                   computation (Value.Env.bind var (chan, Scope.Local) env) cont body
                | None -> eval_error "Choice pattern matching failed"
              end
          | ReceiveBlocked -> block ()
          | ReceivePartnerCancelled ->
              (* If session exceptions enabled, then cancel this endpoint and
               * invoke the session exception. Otherwise, block, as per old semantics. *)
              if (Settings.get Basicsettings.Sessions.exceptions_enabled) then
                begin
                  Session.cancel unboxed_chan >>= fun _ ->
                  invoke_session_exception ()
                end
              else block ()
      end
  and finish env v = Proc.finish (env, v)
    (*****************)

  let reify (r : resumption) = `Resumption r
  let eval : Value.env -> program -> result =
    fun env -> computation env K.empty

  let run_program : Value.env -> Ir.program -> (Value.env * Value.t) =
    fun env program ->
      try (
        Proc.start (fun () -> eval env program)
      ) with
        | NotFound s ->
            raise (internal_error ("NotFound " ^ s ^
              " while interpreting."))
        | Not_found  -> raise (internal_error ("Not_found while interpreting."))
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
