
type links_websocket = {
  client_id : int;
  send_fn : Websocket.Frame.t option -> unit
}

let mk_links_websocket cid send_fn = {
  client_id = cid;
  send_fn = send_fn
}

let accept (valenv, nenv, tenv) req flow =
    let client_id = RequestData.get_client_id (Value.request_data valenv) in
    Websocket_cohttp_lwt.upgrade_connection req flow (
      fun frame ->
        match frame with
          | Websocket.Frame.Opcode.Close ->
              (* TODO: Will need to deregister from Proc here. But cycles... *)
              Printf.printf "Websocket closed for client %d\n" client_id;
          | _ ->
              Printf.printf "Received: %s\n" frame.Websocket.Frame.Content
    ) >>= fun (resp, body, send_fn) ->
    let links_ws = mk_links_websocket client_id send_fn in
    Lwt.return (links_ws, resp, body))

let send_message = failwith "unimplemented"
