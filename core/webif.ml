
open Notfound
open List
open Proc
open ProcessTypes

open Webserver_types
open Utility

let ( >>= ) = Lwt.bind

module WebIf = functor (Webs : WEBSERVER) ->
struct
  module S = Serialisation.MarshalSerialiser
  module U = Serialisation.UnsafeJsonSerialiser
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


  let parse_remote_call (valenv, _, _) cgi_args =
    let fname = Utility.base64decode (assoc "__name" cgi_args) in
    let args = Utility.base64decode (assoc "__args" cgi_args) in
    let args = Value.untuple (U.Value.load (Yojson.Basic.from_string args)) in

    let fvs = U.Value.load (Yojson.Basic.from_string (Utility.base64decode (assoc "__env" cgi_args))) in

    (* There is an artificial distinction between primitive
       functions (builtin functions) and function pointers (functions
       defined as part of a Links program). This is the point where we
       have to do something about it in order for attempts to remotely
       call primitive functions to work properly. *)

    let i_fname = int_of_string fname in
    let func,args =
      match Lib.is_primitive_var i_fname, fvs with
      | true, `Record [] -> `PrimitiveFunction (Lib.primitive_name i_fname, Some i_fname), args
      | false, `Record [] ->
        (* Should ideally handle failure to find function gracefully here. *)
        let (_finfo, (_xs, _body), z, _location) =
           Tables.find Tables.fun_defs i_fname in
        (* This is a workaround for the fact that client-side code passes
           the environment back as an ordinary (first) argument, while
           the environment is expected as the second argument to the
           FunctionPtr constructor. *)
        begin match z with
          None -> `FunctionPtr (i_fname, None), args
        | Some _ -> `FunctionPtr (i_fname, Some (List.hd args)), List.tl args
        end
      | _ -> `FunctionPtr (i_fname, Some fvs), args
    in
    RemoteCall (func, valenv, args)

  (** Boolean tests for cgi parameters *)

  (** invoke server continuation _k
      (e.g. from a hypertext link or a formlet post)
   *)
  let is_server_cont args =
    mem_assoc "_k" args

  (** Extract continuation thunk from the CGI parameter _k *)
  let parse_server_cont (valenv, _, _) params =
    ServerCont (S.Value.load ~globals:valenv (assoc "_k" params))

  let parse_client_return (valenv, _, _) cgi_args =
    let fixup_cont =
      (* At some point, '+' gets replaced with ' ' in our base64-encoded
         string. Here we put it back as it was. *)
      Str.global_replace (Str.regexp " ") "+"
    in
    let cont =
      S.Continuation.load
        ~globals:valenv
        (fixup_cont (assoc "__continuation" cgi_args))
    in
    (* Debug.print("continuation: " ^ Value.show_continuation continuation); *)
    let arg = U.Value.load (Yojson.Basic.from_string (Utility.base64decode (assoc "__result" cgi_args))) in
    (* Debug.print ("arg: "^Value.show arg); *)
      ClientReturn(cont, arg)

  let error_page_stylesheet =
    "<style>pre {border : 1px solid #c66; padding: 4px; background-color: #fee} code.typeError {display: block; padding:1em;}</style>"

  let error_page body =
    "<html>\n  <head>\n    <title>Links error</title>\n    " ^
      error_page_stylesheet ^
      "\n  </head>\n  <body>" ^
      body ^
      "\n  </body></html>\n"

  let parse_request env cgi_args  =
    if      (RequestData.is_remote_call cgi_args)
    then parse_remote_call env cgi_args
    else if (RequestData.is_client_return cgi_args)
    then parse_client_return env cgi_args
    else if (is_server_cont cgi_args)
    then parse_server_cont env cgi_args
    else EvalMain


  let get_websocket_url () =
    if Webs.is_accepting_websocket_requests ()
    then Some (Webs.get_websocket_url ())
    else None

  let generate_json_state req_data v =
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
      | ClientReturn (cont, arg) ->
        Debug.print("Doing ClientReturn for client ID " ^ client_id_str);
        Proc.resolve_external_processes arg;
        Eval.apply_cont cont valenv arg >>= fun (_, result) ->
        let json_state = generate_json_state req_data result in
        let result_json =
          Json.jsonize_value_with_state result json_state |> Json.json_to_string in
        Lwt.return ("text/plain", Utility.base64encode result_json)
      | RemoteCall (func, env, args) ->
        Debug.print("Doing RemoteCall for function " ^ Value.string_of_value func
          ^ ", client ID: " ^ client_id_str);
        Proc.resolve_external_processes func;
        List.iter Proc.resolve_external_processes args;
        List.iter (Proc.resolve_external_processes -<- fst -<- snd)
          (IntMap.bindings (Value.Env.get_parameters env));
        Eval.apply Value.Continuation.empty env (func, args) >>= fun (_, r) ->
        (* Debug.print ("result: "^Value.show result); *)
        (*
        if not(Proc.singlethreaded()) then
          (prerr_endline "Remaining  procs on server after remote call!";
           assert(false));
           *)
        let json_state = generate_json_state req_data r in
        let jsonized_val =
          Json.jsonize_value_with_state r json_state
            |> Json.json_to_string in
        Lwt.return
          ("text/plain",
            Utility.base64encode jsonized_val)
      | EvalMain ->
         Debug.print("Doing EvalMain");
         run ()

  let do_request ((valenv, _, _) as env) cgi_args run render_cont render_servercont_cont response_printer =
    let request = parse_request env cgi_args in
    let (>>=) f g = Lwt.bind f g in
    (* We need to be a bit careful about what we respond here. If we are evaluating
     * a ServerCont or EvalMain, that is fine -- but we need to construct a b64-encoded
     * JSON object if we're responding to a ClientReturn or RemoteCall. *)
    let handle_ajax_error e =
      let json =
        `Assoc [("error", `String (Errors.format_exception e))] in
      Lwt.return
        ("text/plain", Utility.base64encode (Yojson.Basic.to_string json))
    in
    let handle_html_error e =
      let mime_type = "text/html; charset=utf-8" in
      match e with
       | Failure msg as e ->
          Debug.print (Printf.sprintf "Failure(%s)" msg);
          Lwt.return (mime_type, error_page (Errors.format_exception_html e))
       | exc ->
          Lwt.return (mime_type, error_page (Errors.format_exception_html exc))
    in
    let handle_exception = function
      | Aborted r -> Lwt.return r (* Aborts are not "real" errors, as
                                     every client call throws a
                                     Proc.Aborted. *)
      | e ->
         let req_data = Value.Env.request_data valenv in
         RequestData.set_http_response_code req_data 500;
         if (RequestData.is_ajax_call cgi_args)
         then handle_ajax_error e
         else handle_html_error e
    in
    Lwt.catch
      (fun () -> perform_request valenv run render_cont render_servercont_cont request)
      handle_exception >>=
      fun (content_type, content) ->
      response_printer [("Content-type", content_type)] content
end
