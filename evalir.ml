open Webserver_types
open Ir
open Lwt
open Utility
open Proc

let lookup_fun = Tables.lookup Tables.fun_defs
let find_fun = Tables.find Tables.fun_defs

let dynamic_static_routes = Settings.add_bool ("dynamic_static_routes", false, `User)
let allow_static_routes = ref true

module Eval = functor (Webs : WEBSERVER) ->
struct

  exception EvaluationError of string
  exception Wrong

  let eval_error fmt : 'a =
    let error msg = raise (EvaluationError msg) in
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
        Value.find var env
      | Some v -> v

   let serialize_call_to_client (continuation, name, arg) =
     Json.jsonize_call continuation name arg

   let client_call : string -> Value.continuation -> Value.t list -> Proc.thread_result Lwt.t =
     fun name cont args ->
       if not(Settings.get_value Basicsettings.web_mode) then
         failwith "Can't make client call outside web mode.";
       if not(Proc.singlethreaded()) then
         failwith "Remaining procs on server at client call!";
       Debug.print("Making client call to " ^ name);
(*        Debug.print("Call package: "^serialize_call_to_client (cont, name, args)); *)
       let call_package = Utility.base64encode
                            (serialize_call_to_client (cont, name, args)) in
       Proc.abort ("text/plain", call_package)

  (** {0 Evaluation} *)
  let rec value env : Lib.requestData -> Ir.value ->  Value.t = fun req_data v ->
    match v with
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
          match opt_app (value env req_data) (`Record []) r with
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
                                   (label, value env req_data v)::fs)
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
          match value env req_data r with
            | `Record fields when List.mem_assoc label fields ->
                List.assoc label fields
            | _ -> eval_error "Error projecting label %s" label
        end
    | `Erase (labels, r) ->
        begin
          match value env req_data r with
            | `Record fields when
                StringSet.for_all (fun label -> List.mem_assoc label fields) labels ->
                `Record (StringSet.fold (fun label fields -> List.remove_assoc label fields) labels fields)
            | _ -> eval_error "Error erasing labels {%s}" (String.concat "," (StringSet.elements labels))
        end
    | `Inject (label, v, _) -> `Variant (label, value env req_data v)
    | `TAbs (_, v) -> value env req_data v
    | `TApp (v, _) -> value env req_data v
    | `XmlNode (tag, attrs, children) ->
        let children =
          List.fold_right
            (fun v children ->
               let v = value env req_data v in
                 List.map Value.unbox_xml (Value.unbox_list v) @ children)
            children [] in
        let children =
          StringMap.fold
            (fun name v attrs ->
               Value.Attr (name, Value.unbox_string (value env req_data v)) :: attrs)
            attrs children
        in
          Value.box_list [Value.box_xml (Value.Node (tag, children))]
    | `ApplyPure (f, args) ->
      Proc.atomically (fun () -> apply [] env req_data (value env req_data f, List.map (value env req_data) args) )
    | `Closure (f, v) ->
      (* begin *)

      (* TODO: consider getting rid of `ClientFunction *)
      (* Currently, it's only necessary for built-in client
         functions *)

      (* let (finfo, _, z, location) = find_fun f in *)
      (* match location with *)
      (* | `Server | `Unknown | `Client -> *)
      `FunctionPtr (f, Some (value env req_data v))
      (* | `Client -> *)
      (*   `ClientFunction (Js.var_name_binder (f, finfo)) *)
      (* end *)
    | `Coerce (v, _) -> value env req_data v

  and apply cont env : Lib.requestData -> Value.t * Value.t list -> Proc.thread_result Lwt.t = fun req_data x ->
    match x with
    | `FunctionPtr (f, fvs), ps ->
      let (_finfo, (xs, body), z, _location) = find_fun f in
      let env =
        match z, fvs with
        | None, None            -> env
        | Some z, Some fvs -> Value.bind z (fvs, `Local) env
        | _, _ -> assert false in

      (* extend env with arguments *)
      let env = List.fold_right2 (fun x p -> Value.bind x (p, `Local)) xs ps env in
      computation env req_data cont body
    | `PrimitiveFunction ("registerEventHandlers",_), [hs] ->
      let key = EventHandlers.register hs in
      apply_cont cont env req_data (`String (string_of_int key))
    (* start of mailbox stuff *)
    | `PrimitiveFunction ("Send",_), [pid; msg] ->
        if Settings.get_value Basicsettings.web_mode && not (Settings.get_value Basicsettings.concurrent_server) then
           client_call "_SendWrapper" cont [pid; msg]
        else
          let (pid, _location) = Value.unbox_pid pid in
            (try
               Mailbox.send_message msg pid;
               Proc.awaken pid
             with
                 UnknownProcessID _ ->
                   (* FIXME: printing out the message might be more useful. *)
                   failwith("Couldn't deliver message because destination process has no mailbox."));
            apply_cont cont env req_data (`Record [])
    | `PrimitiveFunction ("spawn",_), [func] ->
        if Settings.get_value Basicsettings.web_mode && not (Settings.get_value Basicsettings.concurrent_server) then
           client_call "_spawnWrapper" cont [func]
        else
          begin
            let var = Var.dummy_var in
            let cont' = (`Local, var, Value.empty_env,
                         ([], `Apply (`Variable var, []))) in
            let new_pid = Proc.create_process false (fun () -> apply_cont (cont'::Value.toplevel_cont) env req_data func) in
            let location = `Unknown in
            apply_cont cont env req_data (`Pid (new_pid, location))
          end
    | `PrimitiveFunction ("spawnClient",_), [func] ->
      let new_pid = Proc.create_client_process func in
      apply_cont cont env req_data (`Pid (new_pid, `Client))
    | `PrimitiveFunction ("spawnAngel",_), [func] ->
        if Settings.get_value Basicsettings.web_mode && not (Settings.get_value Basicsettings.concurrent_server) then
           client_call "_spawnWrapper" cont [func]
        else
          begin
            (* if Settings.get_value Basicsettings.web_mode then *)
            (*   failwith("Can't spawn at the server in web mode."); *)
            let var = Var.dummy_var in
            let cont' = (`Local, var, Value.empty_env,
                         ([], `Apply (`Variable var, []))) in
            let new_pid = Proc.create_process true (fun () -> apply_cont (cont'::Value.toplevel_cont) env req_data func) in
            let location = `Unknown in
            apply_cont cont env req_data (`Pid (new_pid, location))
          end
    | `PrimitiveFunction ("recv",_), [] ->
        (* If there are any messages, take the first one and apply the
           continuation to it.  Otherwise, block the process (put its
           continuation in the blocked_processes table) and let the
           scheduler choose a different thread.  *)
(*         if (Settings.get_value Basicsettings.web_mode) then *)
(*             Debug.print("receive in web server mode--not implemented."); *)
        if Settings.get_value Basicsettings.web_mode && not (Settings.get_value Basicsettings.concurrent_server) then
           client_call "_recvWrapper" cont []
        else
        begin match Mailbox.pop_message () with
            Some message ->
              Debug.print("delivered message.");
              apply_cont cont env req_data message
          | None ->
              let recv_frame = Value.expr_to_contframe
                env (Lib.prim_appln "recv" [])
              in
              Proc.block (fun () -> apply_cont (recv_frame::cont) env req_data (`Record []))
        end
    (* end of mailbox stuff *)
    (* start of session stuff *)
    | `PrimitiveFunction ("new", _), [] ->
      let apid = Session.new_access_point () in
        apply_cont cont env req_data (`Int apid)
    | `PrimitiveFunction ("accept", _), [ap] ->
      let apid = Value.unbox_int ap in
      let (c, d), blocked = Session.accept apid in
      Debug.print ("accepting: (" ^ string_of_int c ^ ", " ^ string_of_int d ^ ")");
        if blocked then
          let accept_frame =
              Value.expr_to_contframe env
                (`Return (`Extend (StringMap.add "1" (`Constant (`Int c))
                                     (StringMap.add "2" (`Constant (`Int d))
                                        StringMap.empty), None)))
            in
              (* block my end of the channel *)
              Session.block c (Proc.get_current_pid ());
              Proc.block (fun () -> apply_cont (accept_frame::cont) env req_data (`Record []))
        else
          begin
            begin
              (* unblock the other end of the channel *)
              match Session.unblock d with
              | Some pid -> Proc.awaken pid
              | None     -> assert false
            end;
            apply_cont cont env req_data
              (Value.box_pair (Value.box_int c) (Value.box_int d))
          end
    | `PrimitiveFunction ("request", _), [ap] ->
      let apid = Value.unbox_int ap in
      let (c, d), blocked = Session.request apid in
      Debug.print ("requesting: (" ^ string_of_int c ^ ", " ^ string_of_int d ^ ")");
        if blocked then
          let request_frame =
              Value.expr_to_contframe env
                (`Return (`Extend (StringMap.add "1" (`Constant (`Int c))
                                     (StringMap.add "2" (`Constant (`Int d))
                                        StringMap.empty), None)))
            in
              (* block my end of the channel *)
              Session.block c (Proc.get_current_pid ());
              Proc.block (fun () -> apply_cont (request_frame::cont) env req_data (`Record []))
        else
          begin
            begin
              (* unblock the other end of the channel *)
              match Session.unblock d with
              | Some pid -> Proc.awaken pid
              | None     -> assert false
            end;
            apply_cont cont env req_data
              (Value.box_pair
               (Value.box_int c)
               (Value.box_int d))
          end
    | `PrimitiveFunction ("send", _), [v; chan] ->
      Debug.print ("sending: " ^ Value.string_of_value v ^ " to channel: " ^ Value.string_of_value chan);
      let (outp, _) = Session.unbox_chan chan in
      Session.send v outp;
      begin
        match Session.unblock outp with
          Some pid -> Proc.awaken pid
        | None     -> ()
      end;
      apply_cont cont env req_data chan
    | `PrimitiveFunction ("receive", _), [chan] ->
      begin
        Debug.print("receiving from channel: " ^ Value.string_of_value chan);
        let (out', in') = Session.unbox_chan' chan in
        let inp = in' in
          match Session.receive inp with
          | Some v ->
            Debug.print ("grabbed: " ^ Value.string_of_value v);
            apply_cont cont env req_data (Value.box_pair v chan)
          | None ->
            let grab_frame =
              Value.expr_to_contframe env (Lib.prim_appln "receive" [`Extend (StringMap.add "1" (`Constant (`Int out'))
                                                                                (StringMap.add "2" (`Constant (`Int in'))
                                                                                   StringMap.empty), None)])
            in
              Session.block inp (Proc.get_current_pid ());
              Proc.block (fun () -> apply_cont (grab_frame::cont) env req_data (`Record []))
      end
    | `PrimitiveFunction ("link", _), [chanl; chanr] ->
      let unblock p =
        match Session.unblock p with
        | Some pid -> (*Debug.print("unblocked: "^string_of_int p); *)
                      Proc.awaken pid
        | None     -> () in
      Debug.print ("linking channels: " ^ Value.string_of_value chanl ^ " and: " ^ Value.string_of_value chanr);
      let (out1, in1) = Session.unbox_chan chanl in
      let (out2, in2) = Session.unbox_chan chanr in
      Session.link (out1, in1) (out2, in2);
      unblock out1;
      unblock out2;
      apply_cont cont env req_data (`Record [])
    (* end of session stuff *)
    | `PrimitiveFunction ("unsafeAddRoute", _), [pathv; handler] ->
       let path = Value.unbox_string pathv in
       let is_dir_handler = String.length path > 0 && path.[String.length path - 1] = '/' in
       let path = if String.length path == 0 || path.[0] <> '/' then "/" ^ path else path in
       Webs.add_route is_dir_handler path (Right (env, handler));
       apply_cont cont env req_data (`Record [])
    | `PrimitiveFunction ("addStaticRoute", _), [uriv; pathv; mime_typesv] ->
       if not (!allow_static_routes) then
         eval_error "Attempt to add a static route after they have been disabled";
       let uri = Value.unbox_string uriv in
       let uri = if String.length uri == 0 || uri.[0] <> '/' then "/" ^ uri else uri in
       let path = Value.unbox_string pathv in
       let mime_types = List.map (fun v -> let (x, y) = Value.unbox_pair v in (Value.unbox_string x, Value.unbox_string y)) (Value.unbox_list mime_typesv) in
       Webs.add_route true uri (Left (path, mime_types));
       apply_cont cont env req_data (`Record [])
    | `PrimitiveFunction ("servePages", _), [] ->
       if not (Settings.get_value(dynamic_static_routes)) then
         allow_static_routes := false;
       begin
         Webs.start env >>= fun () ->
         apply_cont cont env req_data (`Record [])
       end
    (*****************)
    | `PrimitiveFunction (n,None), args ->
       apply_cont cont env req_data (Lib.apply_pfun n args req_data)
    | `PrimitiveFunction (_, Some code), args ->
       apply_cont cont env req_data (Lib.apply_pfun_by_code code args req_data)
    | `ClientFunction name, args -> client_call name cont args
    | `Continuation c,      [p] -> apply_cont c env req_data p
    | `Continuation _,       _  ->
        eval_error "Continuation applied to multiple (or zero) arguments"
    | _                        -> eval_error "Application of non-function"
  and apply_cont cont env req_data v =
    Proc.yield (fun () -> apply_cont' cont env req_data v)
  and apply_cont' cont env req_data v : Proc.thread_result Lwt.t =
    match cont with
    | [] -> Proc.finish (env, v)
    | (scope, var, locals, comp) :: cont ->
       let env = Value.bind var (v, scope) (Value.shadow env ~by:locals) in
       computation env req_data cont comp
  and computation env req_data cont (bindings, tailcomp) : Proc.thread_result Lwt.t =
    match bindings with
      | [] -> tail_computation env req_data cont tailcomp
      | b::bs -> match b with
        | `Let ((var, _) as b, (_, tc)) ->
              let locals = Value.localise env var in
              let cont' = (((Var.scope_of_binder b, var, locals, (bs, tailcomp))
                           ::cont) : Value.continuation) in
              tail_computation env req_data cont' tc
          (* function definitions are stored in the global fun map *)
          | `Fun _ ->
            computation env req_data cont (bs, tailcomp)
          | `Rec _ ->
            computation env req_data cont (bs, tailcomp)
          | `Alien _ ->
            computation env req_data cont (bs, tailcomp)
          | `Module _ -> failwith "Not implemented interpretation of modules yet"
  and tail_computation env req_data cont : Ir.tail_computation -> Proc.thread_result Lwt.t = function
    (* | `Return (`ApplyPure _ as v) -> *)
    (*   let w = (value env v) in *)
    (*     Debug.print ("ApplyPure"); *)
    (*     Debug.print ("  value term: " ^ Show.show Ir.show_value v); *)
    (*     Debug.print ("  cont: " ^ Value.string_of_cont cont); *)
    (*     Debug.print ("  value: " ^ Value.string_of_value w); *)
    (*     apply_cont cont env w *)
    | `Return v      -> apply_cont cont env req_data (value env req_data v)
    | `Apply (f, ps) ->
        apply cont env req_data (value env req_data f, List.map (value env req_data) ps)
    | `Special s     -> special env req_data cont s
    | `Case (v, cases, default) ->
      begin match value env req_data v with
        | `Variant (label, _) as v ->
          begin
            match StringMap.lookup label cases, default, v with
            | Some ((var,_), c), _, `Variant (_, v)
            | _, Some ((var,_), c), v ->
              computation (Value.bind var (v, `Local) env) req_data cont c
            | None, _, #Value.t -> eval_error "Pattern matching failed"
            | _ -> assert false (* v not a variant *)
          end
        | _ -> eval_error "Case of non-variant"
      end
    | `If (c,t,e)    ->
        computation env req_data cont
          (match value env req_data c with
             | `Bool true     -> t
             | `Bool false    -> e
             | _              -> eval_error "Conditional was not a boolean")
  and special env req_data cont : Ir.special -> Proc.thread_result Lwt.t = function
    | `Wrong _                    -> raise Wrong
    | `Database v                 -> apply_cont cont env req_data (`Database (db_connect (value env req_data v)))
    | `Table (db, name, keys, (readtype, _, _)) ->
      begin
        (* OPTIMISATION: we could arrange for concrete_type to have
           already been applied here *)
        match value env req_data db, value env req_data name, value env req_data keys, (TypeUtils.concrete_type readtype) with
          | `Database (db, params), name, keys, `Record row ->
	      let unboxed_keys =
		List.map
		  (fun key ->
		    List.map Value.unbox_string (Value.unbox_list key))
		  (Value.unbox_list keys)
	      in
              apply_cont cont env req_data (`Table ((db, params), Value.unbox_string name, unboxed_keys, row))
          | _ -> eval_error "Error evaluating table handle"
      end
    | `Query (range, e, _t) ->
       let range =
         match range with
         | None -> None
         | Some (limit, offset) ->
            Some (Value.unbox_int (value env req_data limit), Value.unbox_int (value env req_data offset)) in
       if Settings.get_value Basicsettings.Shredding.shredding then
         begin
           match Queryshredding.compile_shredded env (range, e) with
           | None -> computation env req_data cont e
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
                 apply_cont cont env req_data
		   (Queryshredding.Stitch.stitch_mapped_query mapped_results)
               end
	 end
       else (* shredding disabled *)
         begin
           match Query.compile env (range, e) with
           | None -> computation env req_data cont e
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
               apply_cont cont env req_data (Database.execute_select fields q db)
	 end
    | `Update ((xb, source), where, body) ->
      let db, table, field_types =
        match value env req_data source with
          | `Table ((db, _), table, _, (fields, _, _)) ->
            db, table, (StringMap.map (function
                                        | `Present t -> t
                                        | _ -> assert false) fields)
          | _ -> assert false in
      let update_query =
        Query.compile_update db env ((Var.var_of_binder xb, table, field_types), where, body) in
      let () = ignore (Database.execute_command update_query db) in
        apply_cont cont env req_data (`Record [])
    | `Delete ((xb, source), where) ->
      let db, table, field_types =
        match value env req_data source with
          | `Table ((db, _), table, _, (fields, _, _)) ->
            db, table, (StringMap.map (function
                                        | `Present t -> t
                                        | _ -> assert false) fields)
          | _ -> assert false in
      let delete_query =
        Query.compile_delete db env ((Var.var_of_binder xb, table, field_types), where) in
      let () = ignore (Database.execute_command delete_query db) in
        apply_cont cont env req_data (`Record [])
    | `CallCC f                   ->
      apply cont env req_data (value env req_data f, [`Continuation cont])
    (* Session stuff *)
    | `Select (name, v) ->
      let chan = value env req_data v in
      Debug.print ("selecting: " ^ name ^ " from: " ^ Value.string_of_value chan);
      let (outp, _) = Session.unbox_chan chan in
      Session.send (Value.box_string name) outp;
      begin
        match Session.unblock outp with
          Some pid -> Proc.awaken pid
        | None     -> ()
      end;
      apply_cont cont env req_data chan
    | `Choice (v, cases) ->
      begin
        let chan = value env req_data v in
        Debug.print("choosing from: " ^ Value.string_of_value chan);
        let (_, in') = Session.unbox_chan' chan in
        let inp = in' in
          match Session.receive inp with
          | Some v ->
            Debug.print ("chose: " ^ Value.string_of_value v);
            let label = Value.unbox_string v in
              begin
                match StringMap.lookup label cases with
                | Some ((var,_), body) ->
                  computation (Value.bind var (chan, `Local) env) req_data cont body
                | None -> eval_error "Choice pattern matching failed"
              end
          | None ->
            let choice_frame =
              Value.expr_to_contframe env (`Special (`Choice (v, cases)))
            in
              Session.block inp (Proc.get_current_pid ());
              Proc.block (fun () -> apply_cont (choice_frame::cont) env req_data (`Record []))
      end
    (*****************)

  let eval : Value.env -> Lib.requestData -> program ->  Proc.thread_result Lwt.t =
    fun env req_data -> computation env req_data Value.toplevel_cont

  let run_program_with_cont : Value.continuation -> Value.env -> Lib.requestData -> Ir.program ->
    (Value.env * Value.t) =
    fun cont env req_data program ->
      try (
        Proc.run (fun () -> computation env req_data cont program)
      ) with
        | NotFound s -> failwith ("Internal error: NotFound " ^ s ^
                                     " while interpreting.")

  let run_program : Value.env -> Lib.requestData -> Ir.program -> (Value.env * Value.t) =
    fun env req_data program ->
      try (
        Proc.run (fun () -> eval env req_data program)
      ) with
        | NotFound s -> failwith ("Internal error: NotFound " ^ s ^
                                     " while interpreting.")
        | Not_found  -> failwith ("Internal error: Not_found while interpreting.")

  let run_defs : Value.env -> Lib.requestData -> Ir.binding list -> Value.env =
    fun env req_data bs ->
    let (env, _value) = run_program env req_data (bs, `Return(`Extend(StringMap.empty, None))) in env

  (** [apply_cont_toplevel cont env v] applies a continuation to a value
      and returns the result. Finishing the main thread normally comes
      here immediately. *)
  let apply_cont_toplevel cont env req_data v =
    try snd (Proc.run (fun () -> apply_cont cont env req_data v)) with
    | NotFound s -> failwith ("Internal error: NotFound " ^ s ^
                                " while interpreting.")

  let apply_with_cont cont env req_data (f, vs) =
    try snd (Proc.run (fun () -> apply cont env req_data (f, vs))) with
    |  NotFound s -> failwith ("Internal error: NotFound " ^ s ^
                                 " while interpreting.")


  let apply_toplevel env req_data (f, vs) = apply_with_cont [] env req_data (f, vs)

  let eval_toplevel env req_data program =
    try snd (Proc.run (fun () -> eval env req_data program)) with
    | NotFound s -> failwith ("Internal error: NotFound " ^ s ^
                                " while interpreting.")

end
