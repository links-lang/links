var tests = require('../tests.js');
var url = "http://localhost:8080";
// Hack: We're assuming that we're requesting on the first server
// access point created, and that it's called srvAp_0.
// A better way of doing this would be to have a calling pattern for
// the client, and inspecting the environment...
var srvAp = "srvAp_0";

var step = 0;
var sum = 0;

var _ws = undefined;
var _chan = undefined;

function callback(client_id, websocket) {
   console.log(client_id);
   _ws = websocket;
   tests.sendRemoteAPRequest(websocket, "cl_0", srvAp);
}

function do_comm(i, chan) {
  sum += i;
  if (step < 1) {
    step++;
  } else {
    tests.sendRemoteSessionMessage(_ws, chan, [], sum);
  }
}

function handle_incoming(ep_id, msg) {
  console.log("Got message " + (JSON.stringify(msg)) + " on " + ep_id);
  do_comm(msg, _chan);
}

function msg_callback(evt) {
  console.log("received message " + evt.data);
  var parsed = JSON.parse(evt.data);
  var opcode = parsed.opcode;
  switch (opcode) {
    case "AP_RESPONSE":
      var chan = parsed.chan;
      _chan = chan;
      break;
    case "SESSION_MESSAGE_DELIVERY":
      var ep_id = parsed.ep_id;
      var msg = parsed.msg;
      handle_incoming(ep_id, msg);
      break;
    default:
      throw ("unexpected opcode " + opcode);
  }
}

tests.make_request(url, callback, msg_callback);
