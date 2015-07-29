open Webserver_types
open Cohttp
open Cohttp_lwt_unix
open Ir
open List
open Lwt
open Proc
open Utility
open Webif

module rec Webserver : WEBSERVER =
struct

  module Eval = Evalir.Eval(Webserver)
  module Webif = Webif.WebIf(Webserver)

  type routing_table = (bool * string * (string -> Ir.computation Lwt.t)) list
  type renderer = (Ir.var * Value.t) list -> Ir.computation -> Proc.thread_result Lwt.t
  (* The fake environment part seems super dodgy to me *)

  type t = routing_table * Ir.binding list * Value.env

  let rt : routing_table ref = ref []
  let env : (Value.env * Ir.var Env.String.t * Types.typing_environment) ref = ref (Value.empty_env, Env.String.empty, Types.empty_typing_environment)

  let init some_env =
    env := some_env;
    ()

  let add_route is_directory path thread_starter =
    rt := (is_directory, path, thread_starter) :: !rt

  let rec parse_request valenv params = parse_request valenv params

  let rec start renderer =
    let is_prefix_of s t = String.length s <= String.length t && s = String.sub t 0 (String.length s) in

    let parse_post_body s =
      let assocs = Cgi.split '&' s in
      let one_assoc s =
        try
          let i = String.index s '=' in
          String.sub s 0 i,
          Cgi.decode (String.sub s (succ i) (String.length s - i - 1))
        with
        | Not_found -> s,"" in
      List.map one_assoc assocs in

    let callback rt conn req body =
      let query_args = List.map (fun (k, vs) -> (k, String.concat "," vs)) (Uri.query (Request.uri req)) in
      Cohttp_lwt_body.to_string body >>= fun body_string ->
      let body_args = parse_post_body body_string in
      let cgi_args = body_args @ query_args @ Header.to_list (Request.headers req) in
      Lib.cgi_parameters := cgi_args;
      Lib.cookies := Cohttp.Cookie.Cookie_hdr.extract (Request.headers req);
      Debug.print (Printf.sprintf "%n cgi_args:" (List.length cgi_args));
      List.iter (fun (k, v) -> Debug.print (Printf.sprintf "   %s: \"%s\"" k v)) cgi_args;
      let path = Uri.path (Request.uri req) in

      let rec run_page (dir, s, handler) () = run_page (dir, s, handler) () in
      let rec render_cont () = render_cont () in

      let rec route = function
        | [] -> Server.respond_string ~status:`Not_found ~body:"<h1>Nope</h1>" ()
        | ((dir, s, handler) :: rest) when (dir && is_prefix_of s path) || (s = path) ->
             Webif.do_request !env cgi_args (run_page (dir, s, handler)) (render_cont ()) Lib.cohttp_server_response
        | (_ :: rest) -> route rest in

      route rt in

    let start_server host port rt =
      Conduit_lwt_unix.init ~src:host () >>= fun ctx ->
      let ctx = Cohttp_lwt_unix_net.init ~ctx () in
      Server.create ~ctx ~mode:(`TCP (`Port port)) (Server.make ~callback:(callback rt) ()) in

    Settings.set_value Basicsettings.web_mode true;
    start_server (Settings.get_value Basicsettings.host_name) (Settings.get_value Basicsettings.port) !rt
end
