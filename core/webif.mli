(** Web interface *)

open Webserver_types

module WebIf : functor (Webs : WEBSERVER) ->
sig

  val do_request :
    (Value.env * Ir.var Env.String.t * Types.typing_environment) ->
    (string * string) list ->
    (unit -> (string * string) Lwt.t) ->
    Value.continuation ->
    (Value.t -> string) ->
    ((string * string) list -> string -> 'b Lwt.t) ->
    'b Lwt.t

end
