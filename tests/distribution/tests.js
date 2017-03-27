const Browser = require("zombie");
const WebSocket = require("ws");

let ws_url_base = "ws://localhost:8080/ws/";

module.exports = {
  make_request : function(url) {
    const browser = new Browser();
    let promise = new Promise(function(resolve, reject) {
      browser.visit(url, function(status) {
        // Grab the client ID
        let cid = browser.window._jsonState.client_id;

        // Next, make a websocket
        let socket = new WebSocket(ws_url_base + cid);
        socket.on('open', function open() { resolve( {cid : cid, socket : socket }) });
      });
    });
    return promise;
  },

  sendRemoteClientMessage : function(ws, destClientId, destPid, msg) {
    let to_send_json =
      { opcode: "CLIENT_TO_CLIENT", destClientId: destClientId,
        destPid : destPid, msg: msg};
    ws.send(JSON.stringify(to_send_json));
  },

  sendRemoteServerMessage : function(ws, destServerPid, msg) {
    let to_send_json =
      { opcode: "CLIENT_TO_SERVER", destPid: destServerPid, msg: msg};
    ws.send(JSON.stringify(to_send_json));
  },

  sendRemoteAPRequest : function(ws, current_pid, remote_apid) {
    let to_send_json =
      { opcode: "SERVER_AP_REQUEST", blockedClientPid: current_pid, serverAPID: remote_apid};
    ws.send(JSON.stringify(to_send_json));
  },

  sendRemoteAPAccept : function(ws, current_pid, remote_apid) {
    let to_send_json =
      { opcode: "SERVER_AP_ACCEPT", blockedClientPid: current_pid, serverAPID: remote_apid};
    ws.send(JSON.stringify(to_send_json));
  },

  sendRemoteSessionMessage : function (ws, channel, delegated_sessions, val) {
    let remote_ep = channel._sessEP1;
    let to_send_json =
      { opcode: "REMOTE_SESSION_SEND", remoteEP: remote_ep,
        delegatedSessions: delegated_sessions, msg: val };
    ws.send(JSON.stringify(to_send_json));
  },

  sendLostMessageResponse : function (ws, ep_id, lost_message_table) {
    let to_send_json =
      { opcode: "LOST_MESSAGES", epID: ep_id, msgs: lost_message_table };
    ws.send(JSON.stringify(to_send_json));
  }
}
