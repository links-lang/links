open ProcessTypes

type incoming_websocket_message =
  (* Mailbox message from client to other client *)
  | ClientToClient of (client_id * process_id * Value.t)
  (* Mailbox message from client to server *)
  | ClientToServer of (process_id * Value.t)
  (* Request on a server access point *)
  | APRequest of (process_id * apid)
  (* Accept on a server access point *)
  | APAccept of (process_id * apid)
