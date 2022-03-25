open Utility

type websocket_url = string
let webs_running
  = Settings.(flag "webs_running"
              |> privilege `System
              |> convert parse_bool
              |> sync)

let jslib_url =
  Settings.(option ~default:(Some "/lib/") "jsliburl"
            |> synopsis "Endpoint for client-side requests against the JavaScript runtime"
            |> to_string from_string_option
            |> convert Utility.(Sys.expand ->- some)
            |> sync)


let external_base_url
  = Settings.(option "external_base_url"
              |> to_string from_string_option
              |> convert Utility.some
              |> sync)

let internal_base_url
  = Settings.(option "internal_base_url"
              |> to_string from_string_option
              |> convert Utility.some
              |> sync)

module type WEBSERVER =
sig
  type request_handler_fn = { request_handler: Value.env * Value.t; error_handler: Value.env * Value.t }

  val get_websocket_url : unit -> string
  val get_internal_base_url : unit -> string option
  val get_external_base_url : unit -> string option

  val init : (Value.env * Ir.var Env.String.t * Types.typing_environment) -> Ir.binding list -> string list -> unit
  val set_prelude : Ir.binding list -> unit
  val get_prelude : unit -> Ir.binding list
  val add_route : bool -> string -> (string * (string * string) list, request_handler_fn) either -> unit
  val start : Value.env -> unit Lwt.t

  (* Returns whether the server is accepting websocket requests. *)
  val is_accepting_websocket_requests : unit -> bool
  (* Sets whether the server is accepting websocket requests. *)
  val set_accepting_websocket_requests : bool -> unit

end
