open Utility

let webs_running = Settings.add_bool ("webs_running", false, `System)

module type WEBSERVER =
sig
  val init : (Value.env * Ir.var Env.String.t * Types.typing_environment) -> Ir.binding list -> unit
  val set_prelude : Ir.binding list -> unit
  val add_route : bool -> string -> (string, Value.env * Value.t) either -> unit
  val start : Value.env -> unit Lwt.t
end
