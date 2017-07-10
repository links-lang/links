let tests = require('../tests.js');
let url = "http://localhost:8080";
// Hack: We're assuming that we're requesting on the first server
// access point created, and that it's called srvAp_0.
// A better way of doing this would be to have a calling pattern for
// the client, and inspecting the environment...
let srvAp = "srvAp_0";

let step = 0;
let sum = 0;

let _ws = undefined;
let _chan = undefined;

function do_comm(chan) {
  tests.sendRemoteSessionMessage(_ws, chan, [], 1);
  tests.sendRemoteSessionMessage(_ws, chan, [], 2);
}

function handle_incoming(ep_id, msg) {
  console.log("Got message " + (JSON.stringify(msg)) + " on " + ep_id);
  do_comm(msg, _chan);
}

function msg_callback(data) {
  console.log("received message " + data);
  let parsed = JSON.parse(data);
  let opcode = parsed.opcode;
  switch (opcode) {
    case "AP_RESPONSE":
      let chan = parsed.chan;
      _chan = chan;
      do_comm(chan);
      break;
    case "SESSION_MESSAGE_DELIVERY":
      let ep_id = parsed.ep_id;
      let msg = parsed.msg;
      handle_incoming(ep_id, msg);
      break;
    default:
      throw ("unexpected opcode " + opcode);
  }
}


function doActions(client_id, websocket) {
   websocket.on('message', function incoming(data, flags) { msg_callback(data) } );
   _ws = websocket;
   tests.sendRemoteAPAccept(websocket, "cl_0", srvAp);
}

let promise = tests.make_request(url, msg_callback);
promise.then(ret => {
  doActions(ret.cid, ret.socket)
});
