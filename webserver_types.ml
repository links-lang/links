open Utility

type websocket_url = string
let webs_running = Basicsettings.Webserver_types.webs_running

module type WEBSERVER =
sig
  type request_handler_fn = { request_handler: Value.env * Value.t; error_handler: Value.env * Value.t }

  val init : (Value.env * Ir.var Env.String.t * Types.typing_environment) -> Ir.binding list -> Loader.ext_dep list -> unit
  val set_prelude : Ir.binding list -> unit
  val add_route : bool -> string -> (string * (string * string) list, request_handler_fn) either -> unit
  val start : Value.env -> unit Lwt.t

  (* Returns whether the server is accepting websocket requests. *)
  val is_accepting_websocket_requests : unit -> bool
  (* Sets whether the server is accepting websocket requests. *)
  val set_accepting_websocket_requests : bool -> unit

end
