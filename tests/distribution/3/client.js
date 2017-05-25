let tests = require('../tests.js');
let url = "http://localhost:8080/";
// Hack: We're assuming that we're requesting on the first server
// access point created, and that it's called srvAp_0.
// A better way of doing this would be to have a calling pattern for
// the client, and inspecting the environment...
let srvAp = "srvAp_0";

let step = 0;
let sum = 0;

let client1 = {
  _ws : undefined,
  _chan : undefined,

  setup : function(websocket) {
     client1._ws = websocket;
     websocket.on('message', function incoming(data, flags) { client1.msg_callback(data) } );
     tests.sendRemoteAPAccept(websocket, "cl_0", srvAp);
  },

  do_comm : function(chan) {
    tests.sendRemoteSessionMessage(client1._ws, chan, [], 1);
    console.log("CL1: SENT 1");
    tests.sendRemoteSessionMessage(client1._ws, chan, [], 2);
    console.log("CL1: SENT 2");
  },

  msg_callback : function(data) {
    let parsed = JSON.parse(data);
    let opcode = parsed.opcode;
    switch (opcode) {
      case "AP_RESPONSE":
        console.log("CL1: AP_RESPONSE");
        let chan = parsed.chan;
        client1._chan = chan;
        client1.do_comm(chan);
        break;
      case "SESSION_MESSAGE_DELIVERY":
        let ep_id = parsed.ep_id;
        let msg = parsed.msg;
        console.log("CL1: RECEIVED " + JSON.stringify(msg));
        break;
      default:
        throw ("unexpected opcode " + opcode);
    }
  }
}


let client2 = {
  _ws : undefined,
  _chan : undefined,
  _sum : 0,
  _count : 0,

  setup : function(websocket) {
    client2._ws = websocket;
    websocket.on('message', function incoming(data, flags) { client2.msg_callback(data) } );
    tests.sendRemoteAPRequest(websocket, "cl_0", srvAp);
  },

  msg_callback : function(data) {
    console.log("received message " + data);
    let parsed = JSON.parse(data);
    let opcode = parsed.opcode;
    switch (opcode) {
      case "AP_RESPONSE":
        console.log("CL2: AP_RESPONSE");
        let chan = parsed.chan;
        client2._chan = chan;
        break;
      case "SESSION_MESSAGE_DELIVERY":
        let ep_id = parsed.ep_id;
        let msg = parsed.msg;
        console.log("CL2: RECEIVED " + JSON.stringify(msg));
        if (client2._count <= 1) {
          client2._count++;
          client2._sum += msg;

          if (client2._count == 2) {
            console.log("CL2: SENT " + client2._sum);
            tests.sendRemoteSessionMessage(client2._ws, client2._chan, [], client2._sum);
          }
        }
        break;
      default:
        throw ("unexpected opcode " + opcode);
    }
  }
}


var p1 = tests.make_request(url);
var p2 = tests.make_request(url);

p1.then((res1) => {
p2.then((res2) => {
  client1.setup(res1.socket);
  client2.setup(res2.socket);
})})

