open Utility

type websocket_url = string
val webs_running : bool Settings.setting
val jslib_url : string option Settings.setting
val internal_base_url : string option Settings.setting
val external_base_url : string option Settings.setting

module type WEBSERVER =
sig
  type request_handler_fn = { request_handler: Value.env * Value.t; error_handler: Value.env * Value.t }

  val get_websocket_url : unit -> string
  val get_internal_base_url : unit -> string option
  val get_external_base_url : unit -> string option

  val init : (Value.env * Ir.var Env.String.t * Types.typing_environment) -> Ir.binding list -> string list -> unit
  val set_prelude : Ir.binding list -> unit
  val get_prelude : unit -> Ir.binding list (* TODO(dhil): This is a temporary hack to thread the prelude through the compiler. *)
  val add_route : bool -> string -> (string * (string * string) list, request_handler_fn) either -> unit

  val start : Value.env -> unit Lwt.t

  val is_accepting_websocket_requests : unit -> bool
  val set_accepting_websocket_requests : bool -> unit
end
