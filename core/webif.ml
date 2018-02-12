(*pp deriving *)

open Notfound
open List
open Proc
open ProcessTypes

open Webserver_types
open Performance
open Utility

let realpages = Basicsettings.Webif.realpages
let ( >>= ) = Lwt.bind

module WebIf = functor (Webs : WEBSERVER) ->
struct

  module Eval = Evalir.Eval(Webs)

  type web_request =
    | ServerCont of
        Value.t                (* thunk *)
    | ClientReturn of
        Value.continuation *   (* continuation *)
        Value.t                (* argument *)
    | RemoteCall of
        Value.t *              (* function *)
        Value.env *            (* closure environment *)
        Value.t list           (* arguments *)
    | EvalMain
        deriving (Show)

  (** Does at least one of the functions have to run on the client? *)
  let is_client_program : Ir.program -> bool =
    fun (bs, _main) ->
      exists
        (function
           | `Fun (_, _, _, `Client)
           | `Alien (_, _, "javascript") -> true
           | `Rec defs ->
               exists
                 (fun (_, _, _, location) -> location = `Client)
                 defs
           | _ -> false)
        bs

  let parse_remote_call (valenv, _, _) cgi_args =
    let fname = Utility.base64decode (assoc "__name" cgi_args) in
    let args = Utility.base64decode (assoc "__args" cgi_args) in
    (* Debug.print ("args: " ^ Value.Show_t.show (Json.parse_json args)); *)
    let args = Value.untuple (Json.parse_json args) in

    let fvs = Json.parse_json_b64 (assoc "__env" cgi_args) in

    let func =
      match fvs with
      | `Record [] -> `FunctionPtr (int_of_string fname, None)
      | _          -> `FunctionPtr (int_of_string fname, Some fvs) in
    RemoteCall(func, valenv, args)

  (** Boolean tests for cgi parameters *)

  (** remote client->server call *)
  let is_remote_call params =
    mem_assoc "__name" params && mem_assoc "__args" params

  (** return __result from server->client call with server continuation __continuation *)
  let is_client_return params =
    mem_assoc "__continuation" params && mem_assoc "__result" params

  (** invoke server continuation _k
      (e.g. from a hypertext link or a formlet post)
   *)
  let is_server_cont args =
    mem_assoc "_k" args

  (** Extract continuation thunk from the CGI parameter _k *)
  let parse_server_cont (valenv, _, _) params =
    ServerCont (Value.unmarshal_value valenv (assoc "_k" params))

  let parse_client_return (valenv, _, _) cgi_args =
    let fixup_cont =
      (* At some point, '+' gets replaced with ' ' in our base64-encoded
         string. Here we put it back as it was. *)
      Str.global_replace (Str.regexp " ") "+"
    in
    let cont =
      Value.unmarshal_continuation
        valenv
        (fixup_cont (assoc "__continuation" cgi_args))
    in
    (* Debug.print("continuation: " ^ Value.Show_continuation.show continuation); *)
    let arg = Json.parse_json_b64 (assoc "__result" cgi_args) in
    (* Debug.print ("arg: "^Value.Show_t.show arg); *)
      ClientReturn(cont, arg)

  let error_page_stylesheet =
    "<style>pre {border : 1px solid #c66; padding: 4px; background-color: #fee} code.typeError {display: block; padding:1em;}</style>"

  let error_page body =
    "<html>\n  <head>\n    <title>Links error</title>\n    " ^
      error_page_stylesheet ^
      "\n  </head>\n  <body>" ^
      body ^
      "\n  </body></html>\n"

  let is_multipart () =
    ((safe_getenv "REQUEST_METHOD") = "POST" &&
        string_starts_with (safe_getenv "CONTENT_TYPE") "multipart/form-data")

  let get_cgi_args() =
    if is_multipart() then
      map (fun (name, { Cgi.value=value; _ }) -> (name, value))
        (Cgi.parse_multipart_args())
    else
      Cgi.parse_args()

  let should_contain_client_id cgi_args =
    (is_remote_call cgi_args) || (is_client_return cgi_args)

  (* jcheney: lifted from serve_request, to de-clutter *)
  let parse_request env cgi_args  =
    if      (is_remote_call cgi_args)
    then parse_remote_call env cgi_args
    else if (is_client_return cgi_args)
    then parse_client_return env cgi_args
    else if (is_server_cont cgi_args)
    then parse_server_cont env cgi_args
    else EvalMain

  (** In web mode, we wrap the continuation of the whole program in a
      call to renderPage. We also return the resulting continuation so
      that we can use it elsewhere (i.e. in processing ServerCont).
  *)
  let wrap_with_render_page (nenv, {Types.tycon_env=tycon_env; _ })
                            (bs, body) =
    let xb, x = Var.fresh_global_var_of_type (Instantiate.alias "Page" [] tycon_env) in
    let render_page = Env.String.lookup nenv "renderPage" in
    let tail = `Apply (`Variable render_page, [`Variable x]) in
    let frame = Value.Continuation.Frame.make `Global x Value.Env.empty ([], tail) in
    let cont = Value.Continuation.(frame &> empty) in
      (bs @ [`Let (xb, ([], body))], tail), cont

  let get_websocket_url () =
    if Webs.is_accepting_websocket_requests () then
      Some (Settings.get_value Basicsettings.websocket_url)
      else None

  let resolve_json_state req_data v =
    let client_id = RequestData.get_client_id req_data in
    let json_state = Json.JsonState.empty client_id (get_websocket_url ()) in
    (* Add event handlers *)
    let json_state = ResolveJsonState.add_value_information v json_state in
    (* Add AP and process information *)
    let json_state = ResolveJsonState.add_ap_information client_id json_state in
    let json_state = ResolveJsonState.add_process_information client_id json_state in
    ResolveJsonState.add_channel_information client_id json_state

  let perform_request valenv run render_cont render_servercont_cont req =
    let req_data = Value.Env.request_data valenv in
    let client_id = RequestData.get_client_id req_data in
    let client_id_str = ClientID.to_string client_id in
    match req with
      | ServerCont t ->
        Debug.print("Doing ServerCont for client ID " ^ client_id_str);
        Eval.apply render_cont valenv (t, []) >>= fun (_, v) ->
        let res = render_servercont_cont v in
        Lwt.return ("text/html", res)
      | ClientReturn(cont, arg) ->
        Debug.print("Doing ClientReturn for client ID " ^ client_id_str);
        Proc.resolve_external_processes arg;
        Eval.apply_cont cont valenv arg >>= fun (_, result) ->
        let json_state = resolve_json_state req_data result in
        let result_json = Json.jsonize_value_with_state result json_state in
        Lwt.return ("text/plain", Utility.base64encode result_json)
      | RemoteCall(func, env, args) ->
        Debug.print("Doing RemoteCall for function " ^ Value.string_of_value func
          ^ ", client ID: " ^ client_id_str);
        (* Debug.print ("func: " ^ Value.Show_t.show func); *)
        (* Debug.print ("args: " ^ mapstrcat ", " Value.Show_t.show args); *)
        Proc.resolve_external_processes func;
        List.iter Proc.resolve_external_processes args;
        List.iter (Proc.resolve_external_processes -<- fst -<- snd)
          (IntMap.bindings (Value.Env.get_parameters env));
        Eval.apply Value.Continuation.empty env (func, args) >>= fun (_, r) ->
        (* Debug.print ("result: "^Value.Show_t.show result); *)
        (*
        if not(Proc.singlethreaded()) then
          (prerr_endline "Remaining  procs on server after remote call!";
           assert(false));
           *)
        let json_state = resolve_json_state req_data r in
        Lwt.return
          ("text/plain",
            Utility.base64encode (Json.jsonize_value_with_state r json_state))
      | EvalMain ->
         Debug.print("Doing EvalMain");
         run ()

  let run_main (valenv, _, _) (globals, (locals, main)) cgi_args (external_files : string list) () =
    ("text/html",
     if is_client_program (globals @ locals, main) then
       if Settings.get_value realpages then
         begin
           Debug.print "Running client program from server";
           let (valenv, v) = Eval.run_program valenv (locals, main) in
           (* Debug.print ("valenv" ^ Value.Show_env.show valenv); *)
           Irtojs.generate_real_client_page
             ~cgi_env:cgi_args
             (Lib.nenv, Lib.typing_env)
             (globals @ locals)
             (valenv, v)
             (get_websocket_url ())
             external_files
         end
       else
         let program = (globals @ locals, main) in
         Debug.print "Running client program.";
         let res =
           lazy (Irtojs.generate_program_page
                   ~cgi_env:cgi_args
                   (Lib.nenv, Lib.typing_env)
                   program external_files) in
         measure_as res "irtojs"
     else
       let program = locals, main in
       Debug.print "Running server program";
       let (_env, v) = Eval.run_program valenv program in
       Value.string_of_value v)

  let do_request ((valenv, _, _) as env) cgi_args run render_cont render_servercont_cont response_printer =
    let request = parse_request env cgi_args in
    let (>>=) f g = Lwt.bind f g in
    Lwt.catch
      (fun () -> perform_request valenv run render_cont render_servercont_cont request )
      (function
       | Aborted r -> Lwt.return r
       | Failure msg as e ->
          prerr_endline msg;
          Lwt.return ("text/html; charset=utf-8", error_page (Errors.format_exception_html e))
       | exc -> Lwt.return ("text/html; charset=utf-8", error_page (Errors.format_exception_html exc)))
    >>= fun (content_type, content) ->
    response_printer [("Content-type", content_type)] content

  let serve_request_program
      (valenv, env2, env3)
      (globals, (locals, main), render_cont)
      response_printer
      cgi_args
      req_data
      (external_files : string list) =
    let valenv' = Value.Env.set_request_data valenv req_data in
    let env = (valenv', env2, env3) in
    let render_servercont_cont = (fun (v: Value.t) ->
      Irtojs.generate_real_client_page
           ~cgi_env:cgi_args
           (Lib.nenv, Lib.typing_env)
           (globals @ locals)
           (valenv, v)
           (get_websocket_url ())
           external_files) in

    Proc.run (fun () -> do_request env cgi_args
                                   (fun () -> Lwt.return (run_main env (globals, (locals, main)) cgi_args external_files ()))
                                   render_cont
                                   render_servercont_cont
                                   (fun headers body -> Lwt.return (response_printer headers body))
                                   )

  (* does the preprocessing to turn prelude+filename into a program *)
  (* result can be cached *)

  let make_program (_, nenv, tyenv) prelude filename =
    (* Warning: cache call nested inside another cache call *)
    let source =
      Errors.display_fatal (Loader.load_file (nenv, tyenv)) filename
    in
    let open Loader in
    let (nenv', tyenv') = source.envs in
    let (globals, (locals, main), t) = source.program in
    let external_files = source.external_dependencies in
    begin
      try
        Unify.datatypes (t, Instantiate.alias "Page" [] tyenv.Types.tycon_env)
      with
        Unify.Failure error ->
        begin match error with
          | `Msg s -> Debug.print ("Unification error: " ^ s)
          | _ -> ()
        end;
        failwith("Web programs must have type Page but this one has type "
                 ^ Types.string_of_datatype t)
    end;

    (* Debug.print ("un-closure-converted IR: " ^ Ir.Show_program.show (prelude@globals@locals, main)); *)

    let nenv'' = Env.String.extend nenv nenv' in
    let tyenv'' = Types.extend_typing_environment tyenv tyenv' in

    (* let module Show_IntStringEnv = Env.Int.Show_t(Deriving_Show.Show_string) in *)
    (* let module Show_StringIntEnv = Env.String.Show_t(Deriving_Show.Show_int) in *)

    (* Debug.print ("nenv''" ^ Show_StringIntEnv.show nenv''); *)

    let tenv0 = Var.varify_env (nenv, tyenv.Types.var_env) in
    let gs0 = Env.String.fold (fun _name var vars -> IntSet.add var vars) nenv IntSet.empty in
    (* Debug.print("gs0: "^Show_intset.show gs0); *)
    let globals = Closures.bindings tenv0 gs0 globals in

    let tenv1 = Var.varify_env (nenv'', tyenv''.Types.var_env) in
    let gs1 = Env.String.fold (fun _name var vars -> IntSet.add var vars) nenv'' IntSet.empty in
    let (locals, main) = Closures.program tenv1 gs1 (locals, main) in

    (* Debug.print ("closure-converted locals: " ^ Ir.Show_program.show (locals, main)); *)

    let (locals, main), render_cont = wrap_with_render_page (nenv, tyenv) (locals, main) in
    let globals = prelude@globals in
    (* Debug.print ("closure-converted IR: " ^ Ir.Show_program.show (globals@locals, main)); *)

    BuildTables.program tenv0 Lib.primitive_vars ((globals @ locals), main);
    (render_cont, (nenv'', tyenv''), (globals, (locals, main)), external_files)

  (* Processes a CGI-based request *)
  let serve_request ((valenv, _, _) as envs) prelude filename : unit =
    let cgi_args = get_cgi_args() in
    Debug.print ("cgi_args: " ^ mapstrcat "," (fun (k, v) -> k ^ "="  ^ v) cgi_args);
    let cookies =
      begin
        match getenv "HTTP_COOKIE" with
        | Some header ->
           let cookies = Str.split (Str.regexp "[ \t]*;[ \t]*") header in
           concat_map
             (fun str ->
              match Str.split (Str.regexp "[ \t]*=[ \t]*") str with
              | [nm; vl] -> [nm, vl]
              | _ -> Debug.print ("Warning: ill-formed cookie: "^str); [])
             cookies
        | None ->
           []
      end in

    (* Set up record containing mutable fields used for primitive library calls.
     * This record is specific to this request. All fields are mutable since the
     * library functions may need to modify the environments, and we don't want
     * to do a state-passing transformation. *)
    (* Client ID is always 0 in CGI mode. *)
    let req_data =
      RequestData.new_request_data cgi_args cookies dummy_client_id in

    (* Compute cacheable stuff in one call *)
    let (render_cont, (nenv,tyenv), ((globals : Ir.binding list), ((locals : Ir.binding list), main)), external_files) =
      Loader.wpcache "program" (fun () ->
        make_program envs prelude filename
     )
    in

    (* We can evaluate the definitions here because we know they are pure. *)
    let valenv = Eval.run_defs valenv globals in

    Errors.display (lazy (serve_request_program
  			  (valenv, nenv, tyenv)
  			  (globals, (locals, main),
                           render_cont)
          (fun hdrs bdy -> Lib.print_http_response hdrs bdy req_data)
          cgi_args
          req_data
          external_files
      )
    )
end
