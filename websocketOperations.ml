open Lwt
open Proc.Websockets
open ProcessTypes
open Websocket_cohttp_lwt

let recvLoop client_id frame =
  let open Frame in
  let rec loop () =
    match frame.opcode with
      | Opcode.Close ->
          deregister_websocket client_id;
          Printf.printf "Websocket closed for client %s\n"
            (ClientID.to_string client_id)
      | _ ->
          Printf.printf "Received: %s from client %s\n" frame.content (ClientID.to_string client_id);
          loop ()
    in
  loop ()

let accept client_id req flow =
    Websocket_cohttp_lwt.upgrade_connection
      req flow (recvLoop client_id)
    >>= fun (resp, body, send_fn) ->
    let links_ws = make_links_websocket client_id send_fn in
    register_websocket client_id links_ws;
    Lwt.return (links_ws, resp, body)

