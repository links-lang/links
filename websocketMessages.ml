open ProcessTypes

type incoming_websocket_message =
  | ClientToClient of (client_id * process_id * Value.t)
  | ClientToServer of (process_id * Value.t)

