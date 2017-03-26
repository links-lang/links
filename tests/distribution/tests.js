var ws_url_base = "ws://localhost:8080/ws/";

module.exports = {
  // url -> client ID
  make_request : function(url, cb, msg_cb) {
    var page = require('webpage').create();
    // jesus fucking christ, how does anyone program like this?
    page.open(url, function(status) {
      // Grab the client ID
      var cid = page.evaluate(function () {
        return _client_id;
      });


      // Next, make a websocket
      var socket = new WebSocket(ws_url_base + cid);
      socket.onopen = function (evt) {
        cb(cid, socket)
      }
      socket.onmessage = msg_cb;
    });
  },

  sendRemoteClientMessage : function(ws, destClientId, destPid, msg) {
    var to_send_json =
      { opcode: "CLIENT_TO_CLIENT", destClientId: destClientId,
        destPid : destPid, msg: msg};
    ws.send(JSON.stringify(to_send_json));
  },

  sendRemoteServerMessage : function(ws, destServerPid, msg) {
    var to_send_json =
      { opcode: "CLIENT_TO_SERVER", destPid: destServerPid, msg: msg};
    ws.send(JSON.stringify(to_send_json));
  },

  sendRemoteAPRequest : function(ws, current_pid, remote_apid) {
    var to_send_json =
      { opcode: "SERVER_AP_REQUEST", blockedClientPid: current_pid, serverAPID: remote_apid};
    ws.send(JSON.stringify(to_send_json));
  },

  sendRemoteAPAccept : function(ws, current_pid, remote_apid) {
    var to_send_json =
      { opcode: "SERVER_AP_ACCEPT", blockedClientPid: current_pid, serverAPID: remote_apid};
    ws.send(JSON.stringify(to_send_json));
  },

  sendRemoteSessionMessage : function (ws, channel, delegated_sessions, val) {
    var remote_ep = channel._sessEP1;
    var to_send_json =
      { opcode: "REMOTE_SESSION_SEND", remoteEP: remote_ep,
        delegatedSessions: delegated_sessions, msg: val };
    ws.send(JSON.stringify(to_send_json));
  },

  sendLostMessageResponse : function (ws, ep_id, lost_message_table) {
    var to_send_json =
      { opcode: "LOST_MESSAGES", epID: ep_id, msgs: lost_message_table };
    ws.send(JSON.stringify(to_send_json));
  }
}
