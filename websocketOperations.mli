(* Handles websocket acceptance and message receives. *)
(* It sucks that this can't be in Proc.Websockets due to the cyclic dep with JSON, but alas. *)

(** Accepts a new websocket connection, creates a new socket, as
 * well as a thread which handles incoming messages. *)
val accept :
  ProcessTypes.client_id ->
  Cohttp.Request.t ->
  Conduit_lwt_unix.flow ->
  (Proc.Websockets.links_websocket * Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
