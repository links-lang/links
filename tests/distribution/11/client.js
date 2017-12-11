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

function assert(b, str) {
  if (!b) {
    throw("Assertion failed: " + str);
  }
}

function check_chans_initialised() {
  initialised_channel_count++;
  if (initialised_channel_count == 6) {
    clientD.sendInts1();
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
  ch_s_double_prime : undefined,
  ws : undefined,

  setup : function(ws) {
    clientA.ws = ws;
    ws.on('message', function incoming(data, flags) { clientA.message_callback(data) } );
    tests.sendRemoteAPAccept(clientA.ws, "clA_0", ap_S);
  },

  message_callback : function(data) {
    let parsed = JSON.parse(data);
    switch (parsed.opcode) {
      case "AP_RESPONSE":
        console.log("handling AP response in A");
        // first response will be for S
        if (clientA.ch_s == undefined) {
          clientA.ch_s = parsed.chan;
          console.log("sending s'' request");
          tests.sendRemoteAPRequest(clientA.ws, "clA_0", ap_SPrimePrime);
        } else if (clientA.ch_s_double_prime == undefined) {
          clientA.ch_s_double_prime = parsed.chan;
        }
        check_chans_initialised();
        break;
      case "GET_LOST_MESSAGES":
        send_lost_message_response(clientA.ws, parsed.carrier_ep, parsed.ep_ids);
        // Now, D should send messages over s''.
        clientD.sendInts2();
        break;
      default:
        console.log("Client A received " + data);
        break;
    }
  },

  sendSDoublePrimeOverS : function(data) {
    let deleg_dict = [{chan : clientA.ch_s_double_prime, buffer: []}];
    tests.sendRemoteSessionMessage(clientA.ws, clientA.ch_s,
                                   deleg_dict, clientA.ch_s_double_prime);
  }
}

let clientB = {
  ch_s : undefined,
  ch_s_prime : undefined,
  ws : undefined,

  setup : function(ws) {
    clientB.ws = ws;
    ws.on('message', function incoming(data, flags) { clientB.message_callback(data) } );
    tests.sendRemoteAPRequest(clientB.ws, "clB_0", ap_S);
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

  sendSOverSPrime : function(data) {
    let deleg_dict = [{chan : clientB.ch_s, buffer: []}];
    tests.sendRemoteSessionMessage(clientB.ws, clientB.ch_s_prime,
                                   deleg_dict, clientB.ch_s);
    // Next, to confuse matters, we want A to send s'' over s (carried channel)
    clientA.sendSDoublePrimeOverS();
  }
}

let clientC = {
  ws : undefined,
  ch_s_prime : undefined,

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
  }
}

let clientD = {
  ws : undefined,
  ch_s_double_prime : undefined,

  setup : function(ws) {
    clientD.ws = ws;
    ws.on('message', function incoming(data, flags) { clientD.message_callback(data) } );
    tests.sendRemoteAPAccept(clientD.ws, "clD_0", ap_SPrimePrime);
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

  sendInts1 : function() {
    for (var i = 0; i < 2; i++) {
      tests.sendRemoteSessionMessage(clientD.ws, clientD.ch_s_double_prime, [], i);
    }
  },

  sendInts2 : function() {
    for (var i = 2; i < 4; i++) {
      tests.sendRemoteSessionMessage(clientD.ws, clientD.ch_s_double_prime, [], i);
    }
  }

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

