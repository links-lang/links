open Webserver_types
open Cohttp
open Cohttp_lwt_unix
open Lwt
open Utility

let jslibdir : string Settings.setting = Settings.add_string("jslibdir", "", `User)
let host_name = Settings.add_string ("host", "0.0.0.0", `User)
let port = Settings.add_int ("port", 8080, `User)

module rec Webserver : WEBSERVER =
struct

  module Eval = Evalir.Eval(Webserver)
  module Webif = Webif.WebIf(Webserver)

  type routing_table = (bool * string * (string * (string * string) list, Value.env * Value.t) either) list
  (* type t = routing_table * Ir.binding list * Value.env *)

  let rt : routing_table ref = ref []
  let env : (Value.env * Ir.var Env.String.t * Types.typing_environment) ref = ref (Value.empty_env, Env.String.empty, Types.empty_typing_environment)
  let prelude : Ir.binding list ref = ref []
  let globals : Ir.binding list ref = ref []

  let set_prelude bs =
    prelude := bs

  let init some_env some_globals =
    env := some_env;
    globals := some_globals;
    ()

  let add_route is_directory path thread_starter =
    rt := (is_directory, path, thread_starter) :: !rt

  let start tl_valenv =
    let is_prefix_of s t = String.length s <= String.length t && s = String.sub t 0 (String.length s) in
    let ( / ) = Filename.concat in

    let parse_post_body s =
      let assocs = split '&' s in
      let one_assoc s =
        try
          let i = String.index s '=' in
          String.sub s 0 i,
          Cgi.decode (String.sub s (succ i) (String.length s - i - 1))
        with
        | Not_found -> s,"" in
      List.map one_assoc assocs in

    let callback rt render_cont _ req body =
      let query_args = List.map (fun (k, vs) -> (k, String.concat "," vs)) (Uri.query (Request.uri req)) in
      Cohttp_lwt_body.to_string body >>= fun body_string ->
      let body_args = parse_post_body body_string in
      let cgi_args = body_args @ query_args @ Header.to_list (Request.headers req) in
      Lib.cgi_parameters := cgi_args;
      Lib.cookies := Cohttp.Cookie.Cookie_hdr.extract (Request.headers req);
      Lib.http_response_code := 200;
      Debug.print (Printf.sprintf "%n cgi_args:" (List.length cgi_args));
      List.iter (fun (k, v) -> Debug.print (Printf.sprintf "   %s: \"%s\"" k v)) cgi_args;
      let path = Uri.path (Request.uri req) in

      let run_page (_dir, _s, (valenv, v)) () =
        Eval.apply (render_cont ()) (Value.shadow tl_valenv ~by:valenv) (v, [`String path]) >>= fun (valenv, v) ->
        let page = Irtojs.generate_real_client_page
                     ~cgi_env:cgi_args
                     (Lib.nenv, Lib.typing_env)
                     (!prelude @ !globals)          (* hypothesis: local definitions shouldn't matter, they should all end up in valenv... *)
                     (valenv, v)
        in
        Lwt.return ("text/html", page) in

      let serve_static base uri_path mime_types =
          let fname = base / uri_path in
          let headers =
            (* Filename.extension not defined until 4.04, because who would want such a thing *)
            let rec loop = function
              | [] -> Cohttp.Header.init ()
              | ((ext, content_type) :: rest) ->
                 if Filename.check_suffix fname ("." ^ ext) then
                   Cohttp.Header.init_with "content-type" content_type
                 else
                   loop rest in
            loop mime_types in
          Debug.print (Printf.sprintf "Responding to static request;\n    Requested: %s\n    Providing: %s\n" path fname);
          Server.respond_file ~headers ~fname () in

      let rec route = function
        | [] ->
           Debug.print "No cases matched!\n";
           Server.respond_string ~status:`Not_found ~body:"<html><body><h1>Nope</h1></body></html>" ()
        | ((_, s, Left (file_path, mime_types)) :: _rest) when is_prefix_of s path ->
           Debug.print (Printf.sprintf "Matched static case %s\n" s);
           let uri_path = String.sub path (String.length s) (String.length path - String.length s) in
           serve_static file_path uri_path mime_types
        | ((dir, s, Right (valenv, v)) :: _rest) when (dir && is_prefix_of s path) || (s = path) ->
           Debug.print (Printf.sprintf "Matched case %s\n" s);
           let (_, nenv, tyenv) = !env in
           Webif.do_request (Value.shadow tl_valenv ~by:valenv, nenv, tyenv) cgi_args (run_page (dir, s, (valenv, v))) (render_cont ()) Lib.cohttp_server_response
        | ((_, s, _) :: rest) ->
           Debug.print (Printf.sprintf "Skipping case for %s\n" s);
           route rest in

      if is_prefix_of (Settings.get_value Basicsettings.Js.lib_url) path then
        let liburl_length = String.length (Settings.get_value Basicsettings.Js.lib_url) in
        let uri_path = (String.sub path liburl_length (String.length path - liburl_length)) in
        let linkslib = match Settings.get_value jslibdir with
          | "" ->
             begin
               (match Utility.getenv "LINKS_LIB" with
                | None -> Filename.dirname Sys.executable_name
                | Some path -> path) / "lib" / "js"
             end
          | s -> s in
        serve_static linkslib uri_path []
      else
        route rt in

    let start_server host port rt =

      let render_cont () =
        let (_, nenv, {Types.tycon_env = tycon_env; _ }) = !env in
        let _, x = Var.fresh_global_var_of_type (Instantiate.alias "Page" [] tycon_env) in
        let render_page = Env.String.lookup nenv "renderPage" in
        let tail = `Apply (`Variable render_page, [`Variable x]) in
        Hashtbl.add Tables.scopes x `Global;
        Hashtbl.add Tables.cont_defs x ([], tail);
        Hashtbl.add Tables.cont_vars x IntSet.empty;
        [(`Global, x, Value.empty_env, ([], tail))] in

      Conduit_lwt_unix.init ~src:host () >>= fun ctx ->
      let ctx = Cohttp_lwt_unix_net.init ~ctx () in
      Debug.print ("Starting server (2)?\n");
      Server.create ~ctx ~mode:(`TCP (`Port port)) (Server.make ~callback:(callback rt render_cont) ()) in

    Debug.print ("Starting server?\n");
    Settings.set_value Basicsettings.web_mode true;
    Settings.set_value webs_running true;
    start_server (Settings.get_value host_name) (Settings.get_value port) !rt
end
