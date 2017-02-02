(* Interface for websocket operations. *)

(* IN PROGRESS SKETCH -- WILL CHANGE *)

(** Abstract type of control websockets, used to communicate with clients *)
type links_websocket

val accept :
  (* Environments --- valenv should be initialised with cgi args and client id.
   * CHECK: Do we really need all of these? *)
  (Value.env * Ir.var Env.String.t * Types.typing_environment) ->
  (links_websocket * Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

(** Sends a message to the given PID *)
val send_message : links_websocket -> ProcessTypes.pid -> Value.t -> unit
