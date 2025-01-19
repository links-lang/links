open Cohttp
open Lwt
open ProcessTypes
open Utility
open Webserver_types
open Var

let jslib_dir = Settings.(option ~default:(Some Linkspath.jslib) "jslibdir"
                          |> synopsis "Server-side (physical) location of the JavaScript runtime"
                          |> to_string from_string_option
                          |> convert Utility.(Sys.expand ->- some)
                          |> sync)

let hostname =
  Settings.(option ~default:(Some "0.0.0.0") "host"
            |> synopsis "The host address of the app-server"
            |> to_string from_string_option
            |> convert Utility.some
            |> sync)

let port =
  Settings.(option ~default:(Some 8080) "port"
            |> synopsis "The port for listening to incoming requests for the app-server"
            |> to_string (function Some i -> string_of_int i | None -> "<none>")
            |> convert (fun n -> Some (int_of_string n))
            |> sync)

(* SSL settings *)
let ssl_cert_file =
  Settings.(option "ssl_cert_file"
            |> privilege `System
            |> synopsis "The SSL certificate"
            |> convert Utility.(Sys.expand ->- some)
            |> to_string from_string_option
            |> sync)

let ssl_key_file =
  Settings.(option "ssl_key_file"
            |> privilege `System
            |> synopsis "The SSL key file"
            |> convert Utility.(Sys.expand ->- some)
            |> to_string from_string_option
            |> sync)

let ssl =
  Settings.(flag ~default:false "ssl"
            |> synopsis "Toggles secure mode for all connections"
            |> convert parse_bool
            |> sync)

(* Base URL for websocket connections *)
let websocket_url
  = let parse_ws_url url =
      if String.length url = 0
      then raise (Invalid_argument "the websocket endpoint cannot be empty")
      else Some url
    in
    Settings.(option ~default:(Some "/ws/") "websocket_url"
              |> synopsis "Endpoint for requests over websockets"
              |> to_string from_string_option
              |> convert parse_ws_url
              |> sync)

module Trie =
struct
  type ('a, 'b) t =
    | Trie of 'b option * ('a * ('a, 'b) t) list

  let empty : ('a, 'b) t = Trie (None, [])

  let rec transform : 'a list -> ('b -> 'b) -> 'b -> ('a, 'b) t -> ('a, 'b) t =
    fun key xf b trie ->
    let rec down key (Trie (here, children)) =
      match key, here with
      | [], None -> Trie (Some b, children)
      | [], Some b' -> Trie (Some (xf b'), children)
      | hk :: tk, _ ->
         let rec right = function
           | [] -> [hk, transform tk xf b empty]
           | ((hc, child) :: tc) ->
              if hk = hc then
                (hc, down tk child) :: tc
              else
                (hc, child) :: right tc in
         Trie (here, right children) in
    down key trie

  (* This has ended up being remarkably specialized *)
  let rec longest_match : 'a list -> ('a, 'b) t -> ('a list * 'b) list =
    fun key (Trie (here, children)) ->
    match key, here with
    | [], None -> []
    | [], Some v -> [key, v]
    | hk :: tk, _ ->
       let rec right = function
         | [] -> begin
             match here with
             | None -> []
             | Some v -> [key, v]
           end
         | ((hc, child) :: tc) ->
            if hk = hc then
              match here with
              | None -> longest_match tk child
              | Some v -> longest_match tk child @ [key, v]
            else
              right tc in
       right children
end

module rec Webserver : WEBSERVER =
struct

  module Eval = Evalir.Eval(Webserver)
  module Webif = Webif.WebIf(Webserver)

  type path = string
  type mime_type = (string * string)
  type static_resource = path * mime_type list
  type request_handler_fn = { request_handler: Value.env * Value.t; error_handler: Value.env * Value.t }

  type provider = (static_resource, request_handler_fn) either option
  type providers = { as_directory: provider; as_page: provider }  (* Really, at least one should be Some *)
  type routing_table = (string, providers) Trie.t

  let rt : routing_table ref = ref Trie.empty

  let env : (Value.env * Ir.var Env.String.t * Types.typing_environment) ref =
    ref (Value.Env.empty, Env.String.empty, Types.empty_typing_environment)
  let prelude : Ir.binding list ref = ref []
  let globals : Ir.binding list ref = ref []

  (* Keeps track of whether websocket requests are allowed. *)
  let accepting_websocket_requests = ref false
  let is_accepting_websocket_requests () =
    !accepting_websocket_requests
  let set_accepting_websocket_requests v =
    accepting_websocket_requests := v

  let get_websocket_url () =
    val_of (Settings.get websocket_url)

  let get_internal_base_url () =
    Settings.get internal_base_url

  let get_external_base_url () =
    Settings.get external_base_url

  let set_prelude bs =
    prelude := bs

  let get_prelude () = !prelude

  let external_files : (string list) ref = ref []

  let init some_env some_globals some_external_files =
    env := some_env;
    globals := some_globals;
    external_files := some_external_files;
    ()


  let add_route is_directory path thread_starter =
    rt := Trie.transform
            (Str.split (Str.regexp "/") path)
            (fun p -> if is_directory then
                        {p with as_directory = Some thread_starter}
                      else
                        {p with as_page = Some thread_starter})
            (if is_directory then
               { as_directory = Some thread_starter; as_page = None }
             else
               { as_directory = None; as_page = Some thread_starter })
            !rt

  let extract_client_id cgi_args =
    Utility.lookup "__client_id" cgi_args

  let get_client_id_or_die cgi_args =
    match extract_client_id cgi_args with
      | Some client_id ->
          Debug.print ("Found client ID: " ^ client_id);
          let decoded_client_id = Utility.base64decode client_id in
          Debug.print ("Decoded client ID: " ^ decoded_client_id);
          ClientID.of_string decoded_client_id
      | None -> raise (Errors.runtime_error "Client ID expected but not found.")


  let get_or_make_client_id cgi_args =
    if (RequestData.is_ajax_call cgi_args) then
      get_client_id_or_die cgi_args
    else
      ClientID.create ()

  let start tl_valenv =
    let is_prefix_of s t = String.length s <= String.length t && s = String.sub t 0 (String.length s) in
    let ( / ) = Filename.concat in

    let parse_post_body s =
      let assocs = split '&' s in
      let one_assoc s =
        try
          let i = String.index s '=' in
          String.sub s 0 i,
          RequestData.DecodeRequestHeaders.decode (String.sub s (succ i) (String.length s - i - 1))
        with
        | Not_found -> s,"" in
      List.map one_assoc assocs in

    let callback rt render_cont _conn req body =
      let req_hs = Request.headers req in
      let content_type = Header.get req_hs "content-type" in
      Cohttp_lwt.Body.to_string body >>= fun body_string ->

      let cgi_args : (string * string) list =
        match Request.meth req, content_type with
        | `POST, Some content_type when string_starts_with content_type "multipart/form-data" ->
           List.map (fun (name, { Multipart.value=value; _ }) -> (name, value))
                    (Multipart.parse_multipart_args content_type body_string)
        | `POST, _ ->
           parse_post_body body_string
        | `GET, _ ->
           List.map (fun (k, vs) -> (k, String.concat "," vs)) (Uri.query (Request.uri req))
        | _, _ -> [] (* FIXME: should possibly do something else here *) in

      (* Add headers as cgi args. Is this really what we want to do? *)
      let cgi_args = cgi_args @ Header.to_list (Request.headers req) in
      let cookies = Cohttp.Cookie.Cookie_hdr.extract (Request.headers req) in
      Debug.print (Printf.sprintf "%n cgi_args:" (List.length cgi_args));
      List.iter (fun (k, v) -> Debug.print (Printf.sprintf "   %s: \"%s\"" k v)) cgi_args;
      let path = Uri.path (Request.uri req) in

      (* Precondition: valenv has been initialised with the correct request data *)
      let run_page (valenv, v) (error_valenv, error_v) () =
        let cid = RequestData.get_client_id (Value.Env.request_data valenv) in
        let applier env vp =
          Eval.apply (render_cont ()) env vp >>= fun (valenv, v) ->
          let open Page in
          let page =
            RealPage.page
              ~wsconn_url:(if !accepting_websocket_requests then Some (get_websocket_url ()) else None)
              (Lib.nenv, Lib.typing_env)
              (* hypothesis: local definitions shouldn't matter,
               * they should all end up in valenv... *)
              (!prelude @ !globals)
              (valenv, v)
              !external_files
          in
          Lwt.return ("text/html", page) in
        try
          applier valenv (v, [`String path; `SpawnLocation (`ClientSpawnLoc cid)])
        with
        | Evalir.Exceptions.Wrong ->
           applier error_valenv (error_v, [`String path; `String "Error in string matching (perhaps you have an over-specific route function)."; `SpawnLocation (`ClientSpawnLoc cid)])
        | Evalir.Exceptions.EvaluationError s ->
           applier error_valenv (error_v, [`String path; `String s; `SpawnLocation (`ClientSpawnLoc cid)]) in

      let render_servercont_cont valenv v =
        let open Page in
        RealPage.page
          ~wsconn_url:(if !accepting_websocket_requests then Some (get_websocket_url ()) else None)
          (Lib.nenv, Lib.typing_env)
          (!prelude @ !globals)
          (valenv, v)
          !external_files in

      let serve_static base uri_path mime_types =
          let fname =
            let fname = base / uri_path in
            let n = String.length fname in
            if n > 1 && String.get fname (n-1) = '/' then
              fname / "index.html"
            else
              base / uri_path in

          let headers =
            let rec loop = function
              | [] -> Cohttp.Header.init ()
              | ((ext, content_type) :: rest) ->
                 if String.equal (Filename.extension fname) ("." ^ ext) then
                   Cohttp.Header.init_with "content-type" content_type
                 else
                   loop rest in
            loop mime_types in
          Debug.print (Printf.sprintf "Responding to static request;\n    Requested: %s\n    Providing: %s\n" path fname);
          Cohttp_lwt_unix.Server.respond_file ~headers ~fname () >>= fun resp ->
          Lwt.return (`Response resp) in

      let is_websocket_request = is_prefix_of (get_websocket_url ()) in

      let route rt =
        let rec up = function
          | [], _ ->
              Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"<html><body><h1>Nope</h1></body></html>" () >>= fun resp ->
              Lwt.return (`Response resp)
          | ([] as remaining, { as_page = Some (Left (file_path, mime_types)); _ }) :: _, true
          | (remaining, { as_directory = Some (Left (file_path, mime_types)); _ }) :: _, true ->
             serve_static file_path (String.concat "/" remaining) mime_types
          | (remaining, { as_directory = Some (Left (file_path, mime_types)); _ }) :: _, false ->
             serve_static file_path (String.concat "/" remaining / "index.html") mime_types
          | ([], { as_page = Some (Right { request_handler = (valenv, v); error_handler = (error_valenv, error_v) }); _}) :: _, true
          | (_, { as_directory = Some (Right { request_handler = (valenv, v); error_handler = (error_valenv, error_v) }); _}) :: _, _ ->
             let (_, nenv, tyenv) = !env in
             let cid = get_or_make_client_id cgi_args in
             let req_data = RequestData.new_request_data cgi_args cookies cid in
             let req_env = Value.Env.set_request_data (Value.Env.shadow tl_valenv ~by:valenv) req_data in
             let req_error_env = Value.Env.set_request_data (Value.Env.shadow tl_valenv ~by:error_valenv) req_data in
             Webif.do_request
               (req_env, nenv, tyenv)
               cgi_args
               (run_page (req_env, v) (req_error_env, error_v))
               (render_cont ())
               (render_servercont_cont req_env)
               (fun hdrs bdy -> Lib.cohttp_server_response hdrs bdy req_data)
          | _ :: t, path_is_file -> up (t, path_is_file) in
        up (Trie.longest_match (Str.split (Str.regexp "/") path) !rt, String.length path == 1 ||path.[String.length path - 1] <> '/') in

        let prefixed_lib_url =
          let js_url = from_option "" (Settings.get jslib_url) in
          match Settings.get internal_base_url with
          | None ->
             "/" ^ (Utility.strip_slashes js_url) ^ "/"
          | Some base_url ->
             "/" ^
               (base_url |> Utility.strip_slashes) ^ "/" ^
                 (js_url |> Utility.strip_slashes) ^ "/"
        in
        Debug.print ("Prefixed_lib_url: " ^ prefixed_lib_url) ;
        Debug.print ("Path: " ^ path) ;

        if is_prefix_of prefixed_lib_url path then
        let liburl_length = String.length prefixed_lib_url in
        let uri_path = (String.sub path liburl_length (String.length path - liburl_length)) in
        let linkslib = match Settings.get jslib_dir with
          | None | Some "" ->
             begin
               (match Utility.getenv "LINKS_LIB" with
                | None -> Filename.dirname Sys.executable_name
                | Some path -> path) / "js"
             end
          | Some s -> s
        in
        if not (Sys.file_exists linkslib)
         then raise (
             Errors.SettingsError (
               Format.asprintf
                 "The javascript library path '%s' does not exist."
                 linkslib));
        serve_static linkslib uri_path []
      (* Handle websocket connections *)
      else if (is_websocket_request path) then
        let ws_url = get_websocket_url () in
        let ws_url_length = String.length ws_url in
        (* TODO: Sanity checking of client ID here *)
        let client_id = ClientID.of_string @@
          String.sub path ws_url_length ((String.length path) - ws_url_length) in
        Debug.print (Printf.sprintf "Creating websocket for client with ID %s\n"
          (ClientID.to_string client_id));
        Cohttp_lwt.Body.drain_body body >>= fun () ->
        Proc.Websockets.accept client_id req
      else
        route rt in

    let start_server host port rt =

      let render_cont () =
        let (_, nenv, {Types.tycon_env = tycon_env; _ }) = !env in
        let _, x = Var.fresh_global_var_of_type (Instantiate.alias "Page" [] tycon_env) in
        let render_page = Env.String.find "renderPage" nenv in
        let tail = Ir.Apply (Ir.Variable render_page, [Ir.Variable x]) in
        Hashtbl.add Tables.scopes x Scope.Global;
        Hashtbl.add Tables.cont_defs x ([], tail);
        Hashtbl.add Tables.cont_vars x IntSet.empty;
        let frame = Value.Continuation.Frame.make Scope.Global x Value.Env.empty ([], tail) in
        Value.Continuation.(frame &> empty)
      in
      Conduit_lwt_unix.init ~src:host () >>= fun ctx ->
      let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
      Debug.print ("Starting server (2)?\n");
      let mode =
        if Settings.get ssl
        then match Settings.get ssl_cert_file, Settings.get ssl_key_file with
          | Some cert, Some key ->
                `TLS ((`Crt_file_path cert), (`Key_file_path key), `No_password, (`Port port))
          | _, _ -> raise Errors.MissingSSLCertificate
        else `TCP (`Port port)
      in
      Cohttp_lwt_unix.Server.create ~ctx ~mode
        (Cohttp_lwt_unix.Server.make_response_action ~callback:(callback rt render_cont) ()) in

    Debug.print ("Starting server?\n");
    Lwt.async_exception_hook :=
      (fun exn -> Debug.print ("Caught asynchronous exception: " ^ (Printexc.to_string exn)));
    Settings.set webs_running true;
    start_server (val_of (Settings.get hostname)) (val_of (Settings.get port)) rt
end
