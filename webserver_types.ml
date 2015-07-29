open Ir
open Proc

module type WEBSERVER =
sig
  type renderer = (Ir.var * Value.t) list -> Ir.computation -> Proc.thread_result Lwt.t (* eh ... *)

  val init : (Value.env * Ir.var Env.String.t * Types.typing_environment) -> unit
  val add_route : bool -> string -> (string -> Ir.computation Lwt.t) -> unit
  val start : renderer -> unit Lwt.t
end
