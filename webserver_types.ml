open Utility

let webs_running = Settings.add_bool ("webs_running", false, `System)

module type WEBSERVER =
sig
  val init : (Value.env * Ir.var Env.String.t * Types.typing_environment) -> Ir.binding list -> unit
  val set_prelude : Ir.binding list -> unit
  val add_route : bool -> string -> (string * (string * string) list, Value.env * Value.t) either -> unit

  (** Sets up the server to accept incoming websocket requests at the given (relative) path.
   * Returns true if this was successful (moreover, that the function has not been called before),
   * and false if not. *)
  val accept_websocket_connections : string -> bool
  val start : Value.env -> unit Lwt.t
end
