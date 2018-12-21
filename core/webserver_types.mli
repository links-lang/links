open Utility

type websocket_url = string
val webs_running : bool Settings.setting



module type WEBSERVER =
sig
  type request_handler_fn = { request_handler: Value.env * Value.t; error_handler: Value.env * Value.t }

  val set_context : Evaluation_env.t -> unit
  val add_route : bool -> string -> (string * (string * string) list, request_handler_fn) either -> unit

  val start : Value.env -> unit Lwt.t

  val is_accepting_websocket_requests : unit -> bool
  val set_accepting_websocket_requests : bool -> unit
end
