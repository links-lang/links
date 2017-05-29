let tests = require('../tests.js');
let url = "http://localhost:8080/";
// Hack: We're assuming that we're requesting on the first server
// access point created, and that it's called srvAp_0.
// A better way of doing this would be to have a calling pattern for
// the client, and inspecting the environment...
let ap_S = "srvAp_0";
let ap_SPrime = "srvAp_1";
let ap_SPrimePrime = "srvAp_2"

let step = 0;
let sum = 0;

let initialised_channel_count = 0;

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

function check_chans_initialised() {
  initialised_channel_count++;
  if (initialised_channel_count == 6) {
    // Now, we will have everything set up, and can start
    // the interactions. Firstly, B sends s over s'.
    clientB.sendSOverSPrime();
  }
}


function send_lost_message_response(ws, ep_id, chan_ids) {
  var ret = {};
  for (var i = 0; i < chan_ids.length; i++) {
    ret[chan_ids[i]] = [];
  }
  tests.sendLostMessageResponse(ws, ep_id, ret);
}

let clientA = {
  ch_s : undefined,
  ws : undefined,

  setup : function(ws) {
    clientA.ws = ws;
    ws.on('message', function incoming(data, flags) { clientA.message_callback(data) } );
    tests.sendRemoteAPRequest(clientA.ws, "clA_0", ap_S);
  },

  message_callback : function(data) {
    let parsed = JSON.parse(data);
    switch (parsed.opcode) {
      case "AP_RESPONSE":
        console.log("handling AP response in A");
        if (clientA.ch_s == undefined) {
          clientA.ch_s = parsed.chan;
        }
        // first response will be for S
        check_chans_initialised();
        break;
      case "GET_LOST_MESSAGES":
        send_lost_message_response(clientA.ws, parsed.carrier_ep, parsed.ep_ids);
        break;
      default:
        console.log("Client A received " + data);
        break;
    }
  },

  sendMessagesAlongS : function() {
    for (var i = 0; i < 4; i++) {
      tests.sendRemoteSessionMessage(clientA.ws, clientA.ch_s, [], i);
    }
  }
}

let clientB = {
  ch_s : undefined,
  ch_s_prime : undefined,
  ws : undefined,

  setup : function(ws) {
    clientB.ws = ws;
    ws.on('message', function incoming(data, flags) { clientB.message_callback(data) } );
    tests.sendRemoteAPAccept(clientB.ws, "clB_0", ap_S);
  },

  message_callback : function(data) {
    let parsed = JSON.parse(data);
    switch (parsed.opcode) {
      case "AP_RESPONSE":
        if (clientB.ch_s == undefined) {
          clientB.ch_s = parsed.chan;
          tests.sendRemoteAPAccept(clientB.ws, "clB_0", ap_SPrime);
        } else if (clientB.ch_s_prime == undefined) {
          clientB.ch_s_prime = parsed.chan;
        }
        check_chans_initialised();
        break;
      case "GET_LOST_MESSAGES":
        send_lost_message_response(clientB.ws, parsed.carrier_ep, parsed.ep_ids);
        break;
      default:
        console.log("Client B received " + data);
        break;
    }
  },

  sendSOverSPrime : function () {
    let deleg_dict = [{chan : clientB.ch_s, buffer : []}];
    tests.sendRemoteSessionMessage(clientB.ws, clientB.ch_s_prime,
                                   deleg_dict, clientB.ch_s);
    clientB.ch_s = undefined;

    // now, try to delegate the other end of ch_s
    clientC.sendSPrimeOverSDoublePrime();
  },
}

let clientC = {
  ws : undefined,
  ch_s_prime : undefined,
  ch_s_double_prime : undefined,

  setup : function(ws) {
    clientC.ws = ws;
    ws.on('message', function incoming(data, flags) { clientC.message_callback(data) } );
    tests.sendRemoteAPRequest(clientC.ws, "clC_0", ap_SPrime);
  },

  message_callback : function (data) {
    let parsed = JSON.parse(data);
    switch (parsed.opcode) {
      case "AP_RESPONSE":
        if (clientC.ch_s_prime == undefined) {
          clientC.ch_s_prime = parsed.chan;
          tests.sendRemoteAPAccept(clientC.ws, "clC_0", ap_SPrimePrime);
        } else if (clientC.ch_s_double_prime == undefined) {
          clientC.ch_s_double_prime = parsed.chan;
        }
        check_chans_initialised();
        break;
      case "GET_LOST_MESSAGES":
        send_lost_message_response(clientC.ws, parsed.carrier_ep, parsed.ep_ids);
        break;
      default:
        console.log("Client C received " + data);
        break;
    }
  },

  sendSPrimeOverSDoublePrime : function() {
    let deleg_dict = [{chan : clientC.ch_s_prime, buffer : []}];
    tests.sendRemoteSessionMessage(clientB.ws, clientC.ch_s_double_prime,
                                   deleg_dict, clientC.ch_s_prime);
    clientC.ch_s_prime = undefined;

    // finally send a bunch of messages along S
    clientA.sendMessagesAlongS();
  }
}

let clientD = {
  ws : undefined,
  ch_s_double_prime : undefined,

  setup : function(ws) {
    clientD.ws = ws;
    ws.on('message', function incoming(data, flags) { clientD.message_callback(data) } );
    tests.sendRemoteAPRequest(clientD.ws, "clD_0", ap_SPrimePrime);
  },

  message_callback : function (data) {
    let parsed = JSON.parse(data);
    switch (parsed.opcode) {
      case "AP_RESPONSE":
        if (clientD.ch_s_double_prime == undefined) {
          clientD.ch_s_double_prime = parsed.chan;
        }
        check_chans_initialised();
        break;
      case "GET_LOST_MESSAGES":
        send_lost_message_response(clientD.ws, parsed.carrier_ep, parsed.ep_ids);
        break;
      default:
        console.log("Client D received " + data);
        break;
    }
  },
}



let p1 = tests.make_request(url);
let p2 = tests.make_request(url);
let p3 = tests.make_request(url);
let p4 = tests.make_request(url);

p1.then((res1) => {
p2.then((res2) => {
p3.then((res3) => {
p4.then((res4) => {
  clientA.setup(res1.socket);
  clientB.setup(res2.socket);
  clientC.setup(res3.socket);
  clientD.setup(res4.socket);
})})})})

