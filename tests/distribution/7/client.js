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

let deleg_ch = undefined;

let buf = []

// because who would want this as a fucking library function anyway
function arraysEqual(a, b) {
  if (a === b) return true;
  if (a == null || b == null) return false;
  if (a.length != b.length) return false;

  // If you don't care about the order of the elements inside
  // the array, you should sort both arrays here.

  for (var i = 0; i < a.length; ++i) {
    if (a[i] !== b[i]) return false;
  }
  return true;
}

function handle_incoming(ep_id, deleg_chans, msg) {
  console.log("CL: RECEIVED " + msg);
}

function handle_incoming_deleg(ep_id, deleg_chans, msg) {
  let top = deleg_chans.pop();
  buf = top.buf;
  _chan = msg;
  console.log("CL: RECEIVED " + msg + " WITH BUFFER " + (JSON.stringify(buf)));
}


function msg_callback(data) {
  console.log("received message " + data);
  let parsed = JSON.parse(data);
  let opcode = parsed.opcode;
  switch (opcode) {
    case "AP_RESPONSE":
      let chan = parsed.chan;
      deleg_ch = chan;
      break;
    case "SESSION_MESSAGE_DELIVERY":
      let ep_id = parsed.ep_id;
      let msg = parsed.msg;
      let deleg_chans = parsed.deleg_chans;
      if (ep_id == deleg_ch.sessEP2) {
        handle_incoming_deleg(ep_id, deleg_chans, msg);
      } else {
        handle_incoming(ep_id, deleg_chans, msg);
      }
      break;
    default:
      throw ("unexpected opcode " + opcode);
  }
}


function doActions(client_id, websocket) {
   websocket.on('message', function incoming(data, flags) { msg_callback(data) } );
   _ws = websocket;
   tests.sendRemoteAPRequest(websocket, "cl_0", srvAp);
}

let promise = tests.make_request(url, msg_callback);
promise.then(ret => {
  doActions(ret.cid, ret.socket)
});
