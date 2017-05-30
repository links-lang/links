let tests = require('../tests.js');
let url = "http://localhost:8080/";
// Hack: We're assuming that we're requesting on the first server
// access point created, and that it's called srvAp_0.
// A better way of doing this would be to have a calling pattern for
// the client, and inspecting the environment...
let srvAp0 = "srvAp_0";
let srvAp1 = "srvAp_1";

let step = 0;
let sum = 0;


// - Client A and B connect via a server access point to get channel c,
//   with channels {c1,c2} and {c2,c1}. A gets {c1, c2}. B gets {c2, c1}.
// - Client B and C connect via a server access point to get channel d.
// (- Client A sends 1, 2 along c.)
// - Client B then wants to delegate {c2,c1} along d so, sends {c2, c1} to C
//   along {d1, d2}.

function assert(b, str) {
  if (!b) {
    throw("Assertion failed: " + str);
  }
}

let clientA = {
  chan : undefined,
  ws : undefined,

  setup : function(ws) {
    clientA.ws = ws;
    ws.on('message', function incoming(data, flags) { clientA.message_callback(data) } );
    tests.sendRemoteAPAccept(ws, "clA_0", srvAp0);
  },

  message_callback : function(data) {
    let parsed = JSON.parse(data);
    switch (parsed.opcode) {
      case "AP_RESPONSE":
        clientA.chan = parsed.chan;
        clientA.send_pre_lost_message();
        break;
      default:
        throw("Unexpected opcode: " + opcode);
    }
  },

  send_pre_lost_message : function() {
    tests.sendRemoteSessionMessage(clientA.ws, clientA.chan, [], 1);
  },

  send_lost_messages : function() {
    tests.sendRemoteSessionMessage(clientA.ws, clientA.chan, [], 2);
    tests.sendRemoteSessionMessage(clientA.ws, clientA.chan, [], 3);
  },

  send_messages : function() {
    tests.sendRemoteSessionMessage(clientA.ws, clientA.chan, [], 4);
  }

}

let clientB = {
  carrier_chan: undefined,
  carried_chan: undefined,
  ws : undefined,
  buf : [],
  lost_msgs : [],

  setup : function(ws) {
    clientB.ws = ws;
    tests.sendRemoteAPRequest(ws, "clB_0", srvAp0);
    ws.on('message', function incoming(data, flags) { clientB.message_callback(data) } );
  },

  message_callback : function(data) {
    let parsed = JSON.parse(data);
    switch (parsed.opcode) {
      case "AP_RESPONSE":
        console.log("In ap response client B");
        if (clientB.carried_chan == undefined) {
          clientB.carried_chan = parsed.chan;
          tests.sendRemoteAPAccept(clientB.ws, "clB_0", srvAp1);
        } else {
          clientB.carrier_chan = parsed.chan;
        }
        break;
      case "GET_LOST_MESSAGES":
        let remote_ep = parsed.carrier_ep;
        let key = parsed.ep_ids[0];
        assert(key != undefined, "key undefined in clB GLM!");
        let ret = {};
        ret[key] = clientB.lost_msgs;
        tests.sendLostMessageResponse(clientB.ws, remote_ep, ret);
        clientB.lost_msgs = [];
        break;
      case "SESSION_MESSAGE_DELIVERY":
        if (clientB.buf != undefined) {
          clientB.buf.unshift(parsed.msg);
          clientB.delegate()
        } else {
          clientB.lost_msgs.unshift(parsed.msg);
        }
        break;
    }
  },

  delegate : function() {
    assert(clientB.carrier_chan != undefined &&
           clientB.carried_chan != undefined,
      "Undefined channel when trying to delegate in clientB!");

    // Now send carried chan along carrier chan.
    let carried_ep = clientB.carrier_chan._sessEP2;
    let deleg_dict = [{ chan : clientB.carried_chan, buffer: clientB.buf }];
    clientA.send_lost_messages();
    tests.sendRemoteSessionMessage(clientB.ws,
      clientB.carrier_chan, deleg_dict, clientB.carried_chan);

    // We don't have the carried_chan anymore.
    clientB.carried_chan = undefined;
    clientB.buf = undefined;
  }

}

let clientC = {
  chan : undefined,
  received_chan : undefined,
  ws : undefined,
  buf : [],

  setup : function(ws) {
    clientC.ws = ws;
    ws.on('message', function incoming(data, flags) { clientC.message_callback(data) } );
    tests.sendRemoteAPRequest(ws, "clC_0", srvAp1);
  },

  deliver_message : function(msg) {
    clientC.buf.unshift(msg);
    if (clientC.buf.length == 4) {
      clientC.print_buf();
    }
  },

  message_callback : function (data) {
    let parsed = JSON.parse(data);
    switch(parsed.opcode) {
      case "AP_RESPONSE":
        clientC.chan = parsed.chan;
        break;
      case "DELIVER_LOST_MESSAGES":
        let msgs = parsed.lost_messages[clientC.received_chan._sessEP2];
        let msgs_len = msgs.length;
        for (var i = 0; i < msgs_len; i++) {
          clientC.deliver_message(msgs.pop());
        }
        break;
      case "SESSION_MESSAGE_DELIVERY":
        if (clientC.received_chan == undefined) {
          let received_chan = parsed.msg;
          clientC.received_chan = received_chan;
          clientC.buf = parsed.deleg_chans[0].buf;
          assert(clientC.received_chan != undefined, "Received chan in C undefined!");
          // Now, trigger A sending the messages
          clientA.send_messages();
        } else {
          clientC.deliver_message(parsed.msg);
        }
        break;
    }
  },

  print_buf : function() {
    let buf_len = clientC.buf.length;
    for (var i = 0; i < buf_len; i++) {
      let v = clientC.buf.pop();
      console.log("RECEIVED: " + JSON.stringify(v));
    }
  }

}

let p1 = tests.make_request(url);
let p2 = tests.make_request(url);
let p3 = tests.make_request(url);

p1.then((res1) => {
p2.then((res2) => {
p3.then((res3) => {
  clientA.setup(res1.socket);
  clientB.setup(res2.socket);
  clientC.setup(res3.socket);
})})})

