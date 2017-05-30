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

let ch1 = undefined;
let ch2 = undefined;

let deleg_ch = undefined;

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

function delegate() {
  // sending receive end of channel (so ch1)
  tests.sendRemoteSessionMessage(_ws, deleg_ch, [{chan : ch1, buffer : [2, 1]}], ch1);
}

function handle_incoming(ep_id, msg) {
  console.assert(msg == 6, "bad message");
  console.log("CL: RECEIVED " + msg)
}

function msg_callback(data) {
  console.log("received message " + data);
  let parsed = JSON.parse(data);
  let opcode = parsed.opcode;
  switch (opcode) {
    case "AP_RESPONSE":
      let chan = parsed.chan;
      deleg_ch = chan;
      delegate();
      break;
    case "SESSION_MESSAGE_DELIVERY":
      let ep_id = parsed.ep_id;
      let msg = parsed.msg;
      handle_incoming(ep_id, msg);
      break;
    case "GET_LOST_MESSAGES":
      let remote_ep = parsed.carrier_ep;
      let key = ch1._sessEP2;
      console.assert(arraysEqual(parsed.ep_ids, [key]), "bad GLM endpoint");
      let ret = {};
      ret[key] = [3];
      tests.sendLostMessageResponse(_ws, remote_ep, ret);
      break;
    default:
      throw ("unexpected opcode " + opcode);
  }
}


function doActions(client_id, websocket) {
   websocket.on('message', function incoming(data, flags) { msg_callback(data) } );
   _ws = websocket;
   ch1 = { _sessEP1: "cl0_0", _sessEP2: "cl0_1" };
   ch2 = { _sessEP1: "cl0_1", _sessEP2: "cl0_0" };
   tests.sendRemoteAPRequest(websocket, "cl_0", srvAp);
}

let promise = tests.make_request(url, msg_callback);
promise.then(ret => {
  doActions(ret.cid, ret.socket)
});
