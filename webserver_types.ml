open Utility

type websocket_url = string
let webs_running = Settings.add_bool ("webs_running", false, `System)

module type WEBSERVER =
sig
  val init : (Value.env * Ir.var Env.String.t * Types.typing_environment) -> Ir.binding list -> string list -> unit
  val set_prelude : Ir.binding list -> unit
  val add_route : bool -> string -> (string * (string * string) list, Value.env * Value.t) either -> unit
  val start : Value.env -> unit Lwt.t

  (* Returns whether the server is accepting websocket requests. *)
  val is_accepting_websocket_requests : unit -> bool
  (* Sets whether the server is accepting websocket requests. *)
  val set_accepting_websocket_requests : bool -> unit

end
