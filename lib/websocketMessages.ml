open ProcessTypes

type channel_cancellation = {
  notify_ep: channel_id;
  cancelled_ep: channel_id
}

type incoming_websocket_message =
  (* Mailbox message from client to other client *)
  | ClientToClient of (client_id * process_id * Value.t)
  (* Mailbox message from client to server *)
  | ClientToServer of (process_id * Value.t)
  (* Request on a server access point *)
  | APRequest of (process_id * apid)
  (* Accept on a server access point *)
  | APAccept of (process_id * apid)
  (* Send a message to a remote channel. *)
  | ChanSend of (channel_id * (Value.delegated_chan list) * Value.t)
  (* Lost message response, containing lost messages for a set of channels *)
  | LostMessageResponse of (channel_id * ((channel_id * (Value.t list)) list))
  (* Channel cancellation notification: a channel has been cancelled on a client,
   * where the other EP is either on the server or another client *)
  | ChannelCancellation of channel_cancellation
