
open Notfound
open List
open Proc
open ProcessTypes

open Webserver_types
open Utility

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


  let parse_remote_call (valenv, _, _) cgi_args =
    let fname = Utility.base64decode (assoc "__name" cgi_args) in
    let args = Utility.base64decode (assoc "__args" cgi_args) in
    (* Debug.print ("args: " ^ Value.show (Json.parse_json args)); *)
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
    (* Debug.print("continuation: " ^ Value.show_continuation continuation); *)
    let arg = Json.parse_json_b64 (assoc "__result" cgi_args) in
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
        (* Debug.print ("func: " ^ Value.show func); *)
        (* Debug.print ("args: " ^ mapstrcat ", " Value.show args); *)
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
        let json_state = resolve_json_state req_data r in
        Lwt.return
          ("text/plain",
            Utility.base64encode (Json.jsonize_value_with_state r json_state))
      | EvalMain ->
         Debug.print("Doing EvalMain");
         run ()

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
end
