'use strict';

/* LIST MANIPULATING FUNCTIONS */
const LINKEDLIST =  (function(){

  return Object.freeze({


    Nil: null,

    isNil: function(n) {return n === null},

    Cons: function(x,xs) {DEBUG.assert(LINKEDLIST.isLinkedList(xs), "Second argument must be linked list"); return {_head: x, _tail: xs}},



    isLinkedList: function(v) {
      return LINKEDLIST.isNil(v)
        || v.hasOwnProperty("_head") && v.hasOwnProperty("_tail");
        //could test list-property recursively, but seems overly expensive
        //&& LINKEDLIST.isLinkedList(v._tail)

    },

    forEach: function(list, f) {
      while(!LINKEDLIST.isNil(list)) {
        let cur = list._head;
        f(cur);
        list = list._tail;
      }
    },

    map: function(list, f) {
      let cur = LINKEDLIST.singleton(null);
      const res = cur;
      while(!LINKEDLIST.isNil(list)) {
        cur._tail = LINKEDLIST.Cons(f(list._head),Nil);
        cur = cur.tail;
        list = list._tail;
      }
      return res._tail;
    },

    mapFromArray: function(array, f) {
      let cur = LINKEDLIST.singleton(null);
      const res = cur;
      for(let i = 0; i < array.length; i++) {
        cur._tail = LINKEDLIST.Cons(f(array[i]),Nil);
        cur = cur.tail;
        list = list._tail;
      }
      return res._tail;
    },

    toArray: function(list) {
      const res = [];
      let cur = list;
      while(!LINKEDLIST.isNil(cur)) {
        res[res.length] = cur._head;
        cur = cur._tail;
      }
      return res;
    },


    snoc: function(xs, x) { return LINKEDLIST.append(xs, LINKEDLIST.singleton(x)); },

    singleton: function(head) { return LINKEDLIST.Cons(head,Nil); },

    lsFromArray: function(arr) {
      let out = Nil;
      for (let i = arr.length - 1; i >= 0; --i) {
        out = LINKEDLIST.Cons(arr[i], out);
      }
      return out;
    },

    take: function (n, xs) {
      let arr = [ ];
      while (!LINKEDLIST.isNil(xs) && n > 0) {
        arr.push(xs._head);
        xs = xs._tail;
        --n;
      }
      return LINKEDLIST.lsFromArray(arr);
    },

    drop: function(n, xs) {
      while (!LINKEDLIST.isNil(xs) && n > 0) {
        xs = xs._tail;
        --n;
      }
      return xs;
    },

    length: function (xs) {
      let out = 0;
      while (!LINKEDLIST.isNil(xs)) {
        out += 1;
        xs = xs._tail;
      }
      return out;
    },

    equal: function(l,r) {
      let curL = l;
      let curR = r;
      while(!LINKEDLIST.isNil(curL) && !LINKEDLIST.isNil(curR)) {
        if(!LINKS.eq(curL._head,curR._head))
          return false;
        curL = curL._tail;
        curR = curR._tail;

      }
      return LINKEDLIST.isNil(curL) && LINKEDLIST.isNil(curR)
    },

    head: function(v) {
      DEBUG.assert(TYPES.isLinkedList(v), "Not a linked list");
      return LINKEDLIST.isNil(v) ? _error('head') : v._head;
      },

    tail: function(v) {
      DEBUG.assert(TYPES.isLinkedList(v), "Not a linked list");
      return LINKEDLIST.isNil(v) ? _error('tail') : v._tail;
      },

    last: function(xs) {
      if (LINKEDLIST.isNil(xs)) {
        return _error('last');
      }
      var out = xs._head;
      while (!LINKEDLIST.isNil(xs)) {
        out = xs._head;
        xs = xs._tail;
      }
      return out;
    },

    revAppend: function(xs, ys) {
      let out = ys;
      while (!LINKEDLIST.isNil(xs)) {
        out = LINKEDLIST.Cons(LINKEDLIST.head(xs), out);
        xs = LINKEDLIST.tail(xs);
      }
      return out;
    },

    append: function(xs, ys) {
      DEBUG.assert(TYPES.isLinkedList(xs) && TYPES.isLinkedList(ys), "Not linked lists");
      if (LINKEDLIST.isNil(xs)) {
        return ys;
      }
      const origxs = xs;
      const rootEl = _Cons(xs._head, Nil);
      let curr = rootEl;
      xs = xs._tail;
      while (!LINKEDLIST.isNil(xs)) {
        curr._tail = _Cons(xs._head, Nil);
        xs = xs._tail;
        curr = curr._tail;
      }
      curr._tail = ys;
      return rootEl;
    },


    at: function(xs, i) {
      let out = null;
      while (!LINKEDLIST.isNil(xs) && i >= 0) {
        out = xs._head;
        xs = xs._tail;
        --i;
      }
      // FIXME should check i / index out of bounds here
      return out;
    },

    empty: function(xs) {
      return LINKEDLIST.isNil(xs);
    },

    zip: function(xs, ys) {
      let arr = [ ];
      while (!LINKEDLIST.isNil(xs) && !LINKEDLIST.isNil(ys)) {
        arr.push({ "1": xs._head, "2": ys._head }); // { ctor:"_Tuple2", _0:x, _1:y }
        xs = xs._tail;
        ys = ys._tail;
      }
      return LINKEDLIST.lsFromArray(arr);
    },

    range:  function(a, b) {
      let lst = Nil;
      if (a <= b) {
        do {
          lst = LINKEDLIST.Cons(b, lst);
        } while (b-- > a);
      }
      return lst;
    },

    replicate: function(n, item) {
      let out = Nil;
      while (n > 0) {
        out = LINKEDLIST.Cons(item, out);
        --n;
      }
      return out;
    },

    // faster than _replicate? not really?
    repeat: function(n, x) {
      let arr = [ ];
      let pattern = [ x ];
      while (n > 0) {
        if (n & 1) {
          arr = arr.concat(pattern);
        }
        n >>= 1;
        pattern = pattern.concat(pattern);
      }
      return LINKEDLIST.lsFromArray(arr);
    },

    // FIXME: max and min rely on '<' and '>', which
  // may not do the right thing for non-primitive types
  // (of course, we really want something like type classes
  // in order to be able to handle this kind of situation
  // more robustly)

    min: function(xs) {
      let currentMin = LINKEDLIST.head(xs);
      while (!LINKEDLIST.isNil(xs)) {
        if (xs._head < currentMin) currentMin = xs._head;
        xs = xs._tail;
      }
      return currentMin;
    },

    max: function(xs) {
      let currentMax = LINKEDLIST.head(xs);
      while (!LINKEDLIST.isNil(xs)) {
        if (xs._head > currentMax) currentMax = xs._head;
        xs = xs._tail;
      }
      return currentmax;
    }
  });
}());

/* Core functions */

// Support events in firefox, which doesn't support window.event.
var event = undefined;

// Set up optimised setZeroTimeout
(function() {
  var timeouts = [];

  var messageName = "0TMsg";

  function setZeroTimeout(fn) {
    timeouts.push(fn);
    window.postMessage(messageName, "*");
  }

  function handleMessage(_event) {
    if (_event.source == window && _event.data == messageName) {
      event = _event;
      _event.stopPropagation();

      if (timeouts.length > 0) {
        timeouts.shift()();
      }
    }
  }

  window.addEventListener("message", handleMessage, true);

  window.setZeroTimeout = setZeroTimeout;
  return;
})();

/**
 * Provides a number of type-checking functions
 */
const TYPES = {
  isUnit: function (val) { return (TYPES.isObject(val) && val.constructor === Object && Object.keys(val).length === 0); },
  isObject: function (val) { return (val && val instanceof Object && !Array.isArray(val)); },
  isNumber: function (val) { return (typeof val === 'number'); },
  isString: function (val) { return (typeof val === 'string'); },
  isBoolean: function (val) { return (typeof val === 'boolean'); },
  isXmlNode: function (val) {
    try {
      return (val instanceof Node);
    } catch (err) {
      return Boolean(val.nodeType);
    }
  },
  isArray: function (val) { return Array.isArray(val); },
  isLinkedList: LINKEDLIST.isLinkedList,
  isCharlist: function (val) { return (TYPES.isArray(val) && !val.some(function (c) { return !TYPES.isString(c) })); },
  isEvent: function (val) { return (val instanceof Event); },
  isTextnode: function (val) { return (TYPES.isXmlNode(val) && val.nodeType === document.TEXT_NODE); },
  isUndefined: function (val) { return (typeof val === 'undefined'); },
  isNull: function (val) { return (val === null); },
  isFunction: function (val) { return (typeof val === 'function'); },
  getType: function (val) {
    if (TYPES.isNumber (val))    return 'number';
    else if (TYPES.isString (val))      return 'string';
    else if (TYPES.isBoolean (val))     return 'boolean';
    else if (TYPES.isTextnode (val))    return 'textnode';
    else if (TYPES.isXmlnode (val))     return 'xmlnode';
    else if (TYPES.isArray (val))       return 'array';
    else if (TYPES.isLinkedList (val))  return 'linkedlist';
    else if (TYPES.isEvent (val))       return 'event';
    else if (TYPES.isUndefined (val))   return 'undefined';
    else if (TYPES.isNull (val))        return 'null';
    else if (typeof(val) == 'object' && 'constructor' in val) return new String(val.constructor); // makes garbage
    else return 'Unknown Type';
  }
};

const DEBUG = Object.freeze({
  debug: function (...msg) { (DEBUGGING && console.debug(...msg)); return; },
  show: function (any) { return (TYPES.isXmlNode(any)) ? DEBUG.xmldump(any) : LINKS.stringify(any); },
  xmldump: function (xml) { return (new XMLSerializer()).serializeToString(xml); },
  assert: function (condition, message) {
    if (DEBUGGING && !Boolean(condition)) {
      throw new Error(`Assertion failed: ${message}`);
    }
    return true;
  },
});

const CONSTANTS = Object.freeze({
  AJAX: Object.freeze({
    XHR_STATUS_IS_LOADED: 2,
    XHR_STATUS_IS_COMPLETE: 4,
    HTTP_STATUS_OK: 200,
  }),
  UNIT: Object.freeze({ }),
  NO_PROCESS: -99,
});

/* Functions for handling the websocket connection to the server. */
const WEBSOCKET = {
  buffer : [],
  is_connected : false,

  make_uri : function(path) {
    var loc = window.location;
    // Remove leading and trailing slashes if necessary
    if (path.length > 0) {
      var startIdx = (path.charAt(0) == '/') ? 1 : 0;
      var endIdx = (path.charAt(path.length - 1) == '/') ? path.length - 1 : path.length;
      if (startIdx < endIdx) {
        path = path.substring(startIdx, endIdx);
      } else {
        path = "";
      }
    }

    return "ws://" + loc.host + "/" + path + "/" + _client_id;
  },

  connect_if_required: function (state) {
    if ("ws_conn_url" in state) {
      WEBSOCKET.connect(WEBSOCKET.make_uri(state.ws_conn_url));
    } else {
      DEBUG.debug("No ws_conn_url in JSON state; not connecting to websocket server");
    }
    return;
  },

  connect: function (ws_uri) {
    DEBUG.assert(
      _client_id != undefined,
      "Trying to start websocket connection with undefined client ID"
    );

    DEBUG.debug("Connecting to websocket at address ", ws_uri);
    _socket = new WebSocket(ws_uri);

    /* Set up all of the event handlers */
    _socket.onopen = function (evt) {
      WEBSOCKET.onOpen(evt);
    };

    _socket.onclose = function (evt) {
      WEBSOCKET.onClose(evt);
    };

    _socket.onerror = function (evt) {
      WEBSOCKET.onError(evt);
    };

    _socket.onmessage = function (evt) {
      WEBSOCKET.onMessage(evt);
    }
    return;
  },

  onOpen: function (evt) {
    DEBUG.debug("Successfully opened websocket connection.");
    WEBSOCKET.is_connected = true;
    WEBSOCKET.drain_buffer();
    return;
  },

  onClose: function (evt) {
    // TODO: Fancier handling of errors here.
    // Should also make these prints as opposed to debugs?
    DEBUG.debug("Lost connection to the server. Please refresh the page.");
    WEBSOCKET.is_connected = false;
    return;
  },

  onError: function (evt) {
    DEBUG.debug("Error encountered when connecting to server. Please refresh the page.");
    WEBSOCKET.is_connected = false;
    return;
  },

  onMessage : function(evt) {
    const js_parsed = JSON.parse(evt.data);
    DEBUG.debug("Received message ", js_parsed || evt.data);
    if (TYPES.isObject(js_parsed)) {
      if ("opcode" in js_parsed) {
        switch (js_parsed.opcode) {
          case "MESSAGE_DELIVERY":
            DEBUG.debug("In message delivery case");
            var local_pid = js_parsed.dest_pid;
            var msg = js_parsed.val;
            LINKS.deliverMessage(local_pid, msg);
            break;
          case "AP_RESPONSE":
            var blocked_pid = js_parsed.blocked_pid;
            var chan = js_parsed.chan;
            _returned_channels[blocked_pid] = chan;
            _buffers[chan._sessEP2] = [];
            _wakeup(blocked_pid);
            break;
          case "SESSION_MESSAGE_DELIVERY":
            var ep_id = js_parsed.ep_id;
            var message = js_parsed.msg;
            var delegated_chans = js_parsed.deleg_chans;
            var requires_lost_messages = js_parsed.requires_lost_messages;
            migrateDelegatedSessions(delegated_chans, requires_lost_messages);
            deliverSessionMessage(ep_id, message);
            break;
          case "GET_LOST_MESSAGES":
            var remote_ep = js_parsed.carrier_ep;
            var ep_ids = js_parsed.ep_ids;
            handleGetLostMessages(remote_ep, ep_ids);
            break;
          case "DELIVER_LOST_MESSAGES":
            var ep_id = js_parsed.ep_id;
            var lost_message_table = js_parsed.lost_messages;
            handleLostMessages(ep_id, lost_message_table);
            break;
          case "CHANNEL_CANCELLATION":
            var notify_ep = js_parsed.notify_ep;
            var cancelled_ep = js_parsed.cancelled_ep;
            handleChannelCancellation(notify_ep, cancelled_ep);
            break;
          default:
            DEBUG.debug("Unhandled message: ", evt.data);
            break;
        }
      }
    }
    return;
  },

  serialise_and_send: function (msg) {
    _socket.send(JSON.stringify(msg));
    return;
  },

  drain_buffer: function () {
    while (WEBSOCKET.buffer.length > 0) {
      var msg = WEBSOCKET.buffer.pop();
      DEBUG.debug("Sending buffered message: ", msg);
      WEBSOCKET.serialise_and_send(msg);
    }
    return;
  },

  try_send: function (msg) {
    // If we're connected, send along. Otherwise add to the buffer.
    if (WEBSOCKET.is_connected) {
      _socket.send(JSON.stringify(msg));
    } else {
      DEBUG.debug("No connection yet; buffering message ", msg);
      WEBSOCKET.buffer.unshift(msg);
    }
    return;
  },

  sendRemoteClientMessage: function (destClientId, destPid, msg) {
    DEBUG.debug(
      "Sending message bound for client ",
      destClientId,
      ", process ",
      destPid,
      ", msg: ",
      msg
    );

    const to_send_json = {
      opcode: "CLIENT_TO_CLIENT",
      destClientId: destClientId,
      destPid: destPid,
      msg: msg,
    };
    WEBSOCKET.try_send(to_send_json);
    return;
  },

  sendRemoteServerMessage: function (destServerPid, msg) {
    DEBUG.debug(
      "Sending message bound for server process ",
      destServerPid,
      ", msg: ",
      msg
    );

    const to_send_json = {
      opcode: "CLIENT_TO_SERVER",
      destPid: destServerPid,
      msg: msg,
    };
    WEBSOCKET.try_send(to_send_json);
    return;
  },

  sendRemoteAPRequest: function (current_pid, remote_apid) {
    DEBUG.debug("Requesting a connection from remote AP ", remote_apid);
    const to_send_json = {
      opcode: "SERVER_AP_REQUEST",
      blockedClientPid: current_pid,
      serverAPID: remote_apid,
    };
    WEBSOCKET.try_send(to_send_json);
    return;
  },

  sendRemoteAPAccept : function(current_pid, remote_apid) {
    DEBUG.debug("Accepting a connection from remote AP ", remote_apid);
    const to_send_json = {
      opcode: "SERVER_AP_ACCEPT",
      blockedClientPid: current_pid,
      serverAPID: remote_apid,
    };
    WEBSOCKET.try_send(to_send_json);
    return;
  },

  sendRemoteSessionMessage: function (channel, delegated_sessions, val) {
    var remote_ep = channel._sessEP1;
    DEBUG.debug("Sending value ", val, " to remote endpoint ", remote_ep);
    const to_send_json = {
      opcode: "REMOTE_SESSION_SEND",
      remoteEP: remote_ep,
      delegatedSessions: delegated_sessions,
      msg: val,
    };
    WEBSOCKET.try_send(to_send_json);
    return;
  },

  sendLostMessageResponse: function (ep_id, lost_message_table) {
    DEBUG.debug(
      "Delivering lost messages for remote EP ",
      ep_id,
      "message table: ",
      lost_message_table,
    );
    const to_send_json = {
      opcode: "LOST_MESSAGES",
      epID: ep_id,
      msgs: lost_message_table,
    };
    WEBSOCKET.try_send(to_send_json);
  },

  sendRemoteCancellation : function(notify_ep, cancelled_ep) {
    DEBUG.debug("Sending remote cancellation. Notify ep ", notify_ep,
                ", cancelled ep ", cancelled_ep);
    const to_send_json =
      { opcode: "CHANNEL_CANCELLATION", notify_ep: notify_ep,
        cancelled_ep: cancelled_ep };
    WEBSOCKET.try_send(to_send_json);
  }
};


var _maxPid = 0;             // the highest process id allocated so far
var _mainPid = "MAIN";       // the process id of the main process
var _current_pid = _mainPid; // the process id of the currently active process
var _client_id = undefined;  // the unique ID given to this client
var _socket = undefined;     // reference to the websocket used to communicate with the server
var _handlingEvent = false;
var _aps = [];                // list of active client access points
var _returned_channels = {};  // channels that have been returned from an AP request.
                              // blocked PID |-> returned channel EP.
var _closureTable = {};

/** A package of functions used internally, not callable from Links code. */
const LINKS = new (function() {

  var _formkey = null;

  /**
   * @TODO Doc
   *
   * @param {any} str
   * @returns
   */
  function _removeCGIArgs (str) {
    return str.replace(/\?.*/, "");
  }

  /**
   * Continue a thread at server after a client call has finished.
   *
   * @param {any} kappa local (client) continuation, for use when the server is really finished
   * @param {any} continuation server-side continuation which the server asked client to invoke it with
   * @param {any} mailbox
   * @param {boolean} sync
   */
  function _remoteContinue (kappa, continuation, mailbox) {
    return _makeCont(function (res) {
      DEBUG.debug('Continuing at server with value', res, 'and continuation', continuation);
      const request = new XMLHttpRequest();
      const rootUrl = _removeCGIArgs(location.href);

      request.open('POST', rootUrl);

      request.onreadystatechange = remoteCallHandler(kappa, request);

      request.setRequestHeader(
        'Content-Type',
        'application/x-www-form-urlencoded'
      );

      request.pid = _current_pid;

      const resultJSON = LINKS.stringify(res);

      return request.send(
        "__continuation=" + continuation +
        "&__result=" + LINKS.base64encode(resultJSON) +
        "&__client_id=" + LINKS.base64encode(_client_id)
      );
    });
  }

  /**
   * Resolve the JSON state for a top-level client program
   *
   * @param {any} state
   * @param {any} handlers
   */
  function _resolveJsonState(state, handlers) {
    for (let i in handlers) {
      const h = handlers[i];
      h.clientKey = _registerMobileKey(state, h.key);

      // Update nodes with the client keys
      const nodes = document.querySelectorAll('[key="' + h.key + '"]');
      Array.prototype.map.call(nodes, function (node) {
        node.setAttribute('key', h.clientKey);
      });
    }
    return;
  }


  /**
   * Register event handlers and spawn processes captured by the JSON
   * state for a top-level client program
   *
   * @param {any} state
   * @param {any} clientId
   * @param {any} processes
   * @param {any} handlers
   * @param {any} aps
   */
  function _activateJsonState(state, clientId, processes, handlers, aps, buffers) {
    // set client ID
    DEBUG.debug("Setting client ID to ", clientId);
    _client_id = clientId;

    // Register event handlers
    for (let i in handlers) {
      var h = handlers[i];
      var hs = eval(h.eventHandlers);
      resolveServerValue(state, hs);
      _registerMobileEventHandlers(h.clientKey, hs);
    }

    // Resolve and create mobile access points
    // Needs to be done before processes, since processes may (will!) reference
    for (let i in aps) {
      var ap = aps[i];
      newWithID(ap);
    }

    // Resolve and spawn the mobile processes
    for (let i in processes) {
      var p = processes[i];
      resolveServerValue(state, p);
      _spawnWithMessages(p.pid, p.process, p.messages);
    }

    for (let i in buffers) {
      let entry = buffers[i];
      let id = entry.buf_id;
      let buf = entry.values;
      for (let val in buf) {
        resolveServerValue(state, val);
      }
      _buffers[id] = buf;
    }
    return;
  }

  /**
   * Resolve, spawn, and register, serialised client processes
   * received from the server
   *
   * it is important to do this is two stages as the process and message
   * values may themselves reference the mobile processes which must
   * have been registered
   *
   * @param {any} state
   * @param {any} processes
   * @param {any} handlers
   */
  function resolveMobileState(state, processes, handlers) {
    // register event handler keys
    for (var i in handlers) {
      var h = handlers[i];
      h.clientKey = _registerMobileKey(state, h.key);
    }

    // register event handlers
    for (var i in handlers) {
      var h = handlers[i];
      var hs = eval(h.eventHandlers);
      resolveServerValue(state, hs);
      _registerMobileEventHandlers(h.clientKey, hs);
    }

    // resolve and spawn the mobile processes
    for (var i in processes) {
      var p = processes[i];
      resolveServerValue(state, p);
      _spawnWithMessages(p.pid, p.process, p.messages);
    }
    return;
  }


  /**
   * Resolve function references in the object `obj`, specified as records
   * { function:f, environment:e }, where the environment is optional. If
   * an environment is specified, we assume that the function denoted by
   * f is actually a wrapper and that f(e) is the desired function.
   * Without an environment, f itself denotes the desired function, a
   * standard CPS compiled Links function. This is recursive, so each
   * object in `obj` also has its functions resolved.
   *
   * @param {any} state
   * @param {any} obj
   */
  function resolveServerValue (state, obj) {
    if (obj instanceof Object) {
      if (obj.type) {
        // XML, skip
        return;
      }
      for (let i in obj) {
        if(LINKEDLIST.isNil(obj[i]))
          continue;

         resolveServerValue(state, obj[i]);

         if (obj[i].func) {
           const f = (!TYPES.isObject(obj[i].environment)) ?
             eval(obj[i].func) :
             partialApply(eval(obj[i].func), eval(obj[i].environment));
           f.location = obj[i].location; // This may be set to 'server' by the server serializer.
           f.func = obj[i].func;

          obj[i] = f;
        } else if (obj[i].key) {
          obj[i].key = _lookupMobileKey(state, obj[i].key)
        }
      }
    }
    return;
  }

  /**
   * @param {any[]} xs
   * @param {any} x
   * @returns {any[]} a new array with the elements of xs followed by x
   */
  function append (xs, x) {
    const out = new Array(xs.length + 1);
    xs.forEach(function (x, i) { out[i] = x });
    out[xs.length] = x;
    return out;
  }

  /**
   * Perform a client call as specified in _callPackage, then re-invoke
   * the server using _remoteContinue
   * NOTE: variables defined within this function could shadow
   * the Links function we're trying to execute. Hence all local
   * vars are prefixed with underscore. Beware also of package variables
   * above shadowing.
   *
   * @param {any} kappa
   * @param {any} callPackage
   * @param {any} sync
   */
  function _invokeClientCall (kappa, callPackage) {
    DEBUG.debug('Invoking client call to ', callPackage.__name);
    DEBUG.debug('arguments: ', callPackage.__args);
    DEBUG.debug('arguments: ', callPackage.__args);

    // FIXME: the eval is redundant, because done in
    // remoteCallHandler; also this name may actually be a
    // closure-table reference, expecting "request" to be defined.
    const f = eval(callPackage.__name);

    const args = callPackage.__args;
    const k = _remoteContinue(
      kappa,
      callPackage.__continuation,
      _mailboxes[_current_pid] || [ ]
    );

    return _yield(function () { return f.apply(f, append(args, k)) });
  }

  /**
   * remoteCallHandler is the trampoline that tunnels symmetrical
   * client-server calls over the request/response link.
   *
   * @param {any} kappa
   * @param {any} request
   * @returns
   */
  function remoteCallHandler (kappa, request) {
    return function () {
      if (request.readyState == CONSTANTS.AJAX.XHR_STATUS_IS_COMPLETE && !request.finished) {
        _current_pid = CONSTANTS.NO_PROCESS;

        // The 'finished' field guards against the callback being called more
        // than once on the same request object, an anomaly observed by EEKC.
        request.finished = true;

        DEBUG.debug("Server response: ", LINKS.base64decode(request.responseText));

        const serverResponse = LINKS.parseB64Safe(request.responseText);

        if (!serverResponse) {
          throw new Error("Fatal error: nonsense returned from server.");
        }

        // Any state that we need for resolving values
        // (currently just a mapping between server and client pids)
        const state = { mobileKeys: { } };

        resolveMobileState(
          state,
          serverResponse.content.state.processes,
          serverResponse.content.state.handlers
        );

        const box = { content: serverResponse.content.value };
        resolveServerValue(state, box);
        const serverValue = box.content;

        DEBUG.debug("Server response decoded: ", serverValue);

        // Check whether we are bouncing the trampoline with a client call
        // or continuing with a final result.
        // TBD: Would be more elegant to use JS constructors instead of
        // using a signal member like __continuation.

        if ((serverValue instanceof Object) && ('__continuation' in serverValue)) {
          // Bouncing the trampoline

          DEBUG.debug("Client function name, before evaluation, is ", serverValue.__name);

          _current_pid = request.pid;
          return _invokeClientCall(kappa, serverValue);
        } else {
          DEBUG.debug("Client continuing after remote server call, value ", serverValue);
          // it's the final result: return it.

          return _applyCont(kappa,serverValue);
        }
      }
      return;
    }
  }

  var nextFuncID = 0;

  // Used to emulate DOMRef serialisation
  var domRefId = 0;
  var domRefs = { };

  function storeDomRef(ref) {
    const id = domRefId++;
    domRefs[id] = ref;
    return { _domRefKey : id };
  }


  function replacer (key, value) {
    DEBUG.debug("In replacer with key: ", key);
    DEBUG.debug("typeof value: ", typeof value);
    DEBUG.debug("value: ", value);

    if (typeof value === 'function') {
      if (value.location === 'server') {
        return {
          _serverFunc: value.func,
          _env: value.environment,
        };
      }
      const id = nextFuncID++;
      _closureTable[id] = function (env) { return value };

      return { _closureTable: id };
    } else if ( // SL: HACK for sending XML to the server
      key !== '_xml' &&
      _isXmlItem(value)
    ) {
      return { _xml: value };
    } else if(value === Nil) {
      return Nil;
    } else if (value.nodeType !== undefined) {
      return storeDomRef(value);
    }
    return value;
  }

  const LINKS = {
    resolveJsonState : function (s) {
      const state = { mobileKeys: { } };
      _resolveJsonState(state, s.handlers);
      return state;
    },

    activateJsonState : function (state, s) {
      return _activateJsonState(state, s.client_id, s.processes, s.handlers, s.access_points, s.buffers);
    },

    resolveValue : function (state, v) {
      return resolveServerValue(state, v);
    },

    // JS uses UCS2 internally.
    // The (un)escape / URI nonsense converts back and forth between UCS2 and UTF-8
    // The btoa / atob methods convert back and forth between UTF-8 and base 64.
    base64encode : function (s) {
      return btoa(unescape(encodeURIComponent(s)));
    },

    base64decode : function (s) {
      return decodeURIComponent(escape(atob(s)));
    },

    unimpl: function (name) { throw new Error('Fatal error: function ' + name + ' not available on client.') },

    /**
     * Turns a direct-style js function into a continuationized one under the
     * Links calling conventions. "trivial" means it cannot call back to a
     * Links function, and that the scheduler can safely be suspended
     * while it runs.
     */
    kify: function (f) {
      return function () {
        const kappa = arguments[arguments.length - 1];
        const args = Array.prototype.slice.call(arguments, 0, arguments.length - 1);
        return _applyCont(kappa, f.apply(f, args));
      }
    },


    eq: undefined,

    /**
     * String concatenation
     * @param {string} s1
     * @param {string} s2
     * @returns {string}
     */
    jsStrConcat: function (s1, s2) { return s1 + s2 },

    /**
     * Concatenate two lists
     * @param {any[]} l
     * @param {any[]} r
     */
    concat: function (l, r) { return l.concat(r) },

    /**
     * Concatenate multiple lists
     * @param {any[][]} lists
     */
    concatList: function (lists) { return [ ].concat(...lists) },

    singleXmlToDomNodes: undefined,

    map: function (f, list) { return list.map(f) },

    /*
      Implictly converts from linked list to JS Array, use with caution!
    */
    XmlToDomNodes : function (xmlForest) {
      DEBUG.assert(
        TYPES.isLinkedList(xmlForest),
        'LINKS.XmlToDomNodes expected a linked list, but got ',
        xmlForest
      );
      var domNodeArray = [];
      LINKEDLIST.forEach(xmlForest,function(xmlNode){
        domNodeArray.push(LINKS.singleXmlToDomNodes(xmlNode))
      });
      return domNodeArray;
    },

    /**
     * Create a an XML value representation
     * @param {string} tag
     * @param {{ [attr: string]: string }} attr
     * @param { } body (linked list)
     */
    XML : function (tag, attrs, body) {
      var children = Nil;
      LINKEDLIST.forEach(body,function(e) {
        if(TYPES.isLinkedList(e)) {
          children = LINKEDLIST.append(children,e);
        } else {
          children = LINKEDLIST.snoc(children,e);
        }
      });
      return _Cons(
        {
          type: 'ELEMENT',
          tagName: tag,
          attrs: attrs,
          children: children
        },
         Nil);
    },


    // Records

    /**
     * Compute the union of dictionaries r and s
     * Precondition: r and s are disjoint
     */
    union: function (r, s) {
      return Object.assign({}, r, s);
    },

    /**
     * Project a field of a record
     * @param {Object} object
     * @param {any} name
     * @returns {any}
     */
    project: function (object, name) { return object[name] },

    /**
     * Erase one ore more fields of a record.
     *
     * @param {Object} r
     * @param linkedlist labels
     * @returns {Object}
     */
     erase: function (r, labels) {
      let s = {};
      let ls = new Set();
      LINKEDLIST.forEach(labels, function(l) { return ls.add(l); });

      for (let l in r) {
         if (ls.has(l)) continue;
         else s[l] = r[l]
      }
      return s;
    },

    vrntLbl: function (o) { return o['_label'] },
    vrntVal: function (o) { return o['_value'] },

    deliverMessage: function(pid, msg) {
      if (!_mailboxes[pid]) {
        _makeMailbox(pid);
      }
      _mailboxes[pid].unshift(msg);
      _wakeup(pid);
      return DEBUG.debug(pid, ' now has ', _mailboxes[pid].length, ' message(s)');
    },

    // Remote calls

    remoteCall: function (kappa) {
      return function (name, env, args) {
        DEBUG.debug("Making remote call to: ", name);
        const currentPID = _current_pid;

        // setpid_kappa: Re-establish the process identifier and continue
        // with kappa.
        const setpidKappa = _makeCont(function (response) {
          _current_pid = currentPID;
          _applyCont(kappa, response);
        });

        const request = new XMLHttpRequest();

        // Posting to location.href works in both Firefox and IE
        // (unlike posting to '#', which IE mistakenly urlencodes as %23)
        request.open('POST', location.href);
        request.setRequestHeader(
          'Content-Type',
          'application/x-www-form-urlencoded'
        );

        request.onreadystatechange = remoteCallHandler(setpidKappa, request);

        request.pid = _current_pid;
        const argsJSON = LINKS.stringify(args);

        // TODO: get rid of env - this should be handled by closure conversion

        if (!env) {
          env = { };
        }

        const envJSON = LINKS.stringify(env);

        // request.funcs = _compose(argsJSON.funcs, envJSON.funcs);

        var argString =
          "__name=" + LINKS.base64encode(name) +
          "&__args=" + LINKS.base64encode(argsJSON) +
          "&__env=" + LINKS.base64encode(envJSON) +
          "&__client_id=" + LINKS.base64encode(_client_id)

        for (var i = 0; i < cgiEnv.length; ++i) {
          argString = argString + "&" + cgiEnv[i][1] + "=" + cgiEnv[i][2];
        };

        return request.send(argString);
      }
    },

    /**
     * Return the input value for the
     * input field whose name is 'name' in the current form
     * (identified by formkey)
     */
    fieldVal: function (name) {
      const forms = document.getElementsByTagName('form');
      var containingForm = null;

      // find the containing form
      for (let i = 0; i < forms.length; ++i) {
        const key = forms[i].getAttribute('key');
        if (key === formkey) {
          containingForm = forms[i];
          break;
        }
      }

      DEBUG.assert(Boolean(containingForm), "Form does not exist!")

      // find the input value
      const xs = document.getElementsByName(name);
      for(var i = 0; i < xs.length; ++i) {
        const node = xs[i];
        while(node) {
          if(node == containingForm) {
              return xs[i].value;
          }
          node = node.parentNode;
        }
      }

      return DEBUG.assert(false, "Form element with name '" + name +"' does not exist!");
    },

    /**
     * apply f to every node in the DOM tree rooted at root
     *
     * NOTE:
     * appDom is deliberately defined non-recursively as
     * JavaScript implementations have very ropey support
     * for recursive functions.
     *
     * It is implemented as a state machine that traverses
     * the tree.
     */
    appDom: function (root, f) {
      var down = 1;
      var right = 2;
      var up = 3;

      f(root);
      if(!root.firstChild)
        return;
      var node = root.firstChild;
      var direction = down;
      while(node != root) {
        switch(direction) {
          case down:
            f(node);
            if(node.firstChild) {
              node = node.firstChild;
              direction = down;
            } else {
              direction = right;
            }
            break;
          case right:
            if(node.nextSibling) {
              node = node.nextSibling;
              direction = down;
            } else {
              direction = up;
            }
            break;
          case up:
            node = node.parentNode;
            direction = right;
            break;
        }
      }
      return;
    },

    /**
     * Bind all handlers to the DOM node
     * @param {ElementNode} node
     */
    activateHandlers: function (node) {
      if (!isElement(node)) {
        return;
      }

      function activate (node) {
        if (!isElement(node)) {
          return;
        }

        const key = node.getAttribute('key');
        if (key) {
          const handlers = _eventHandlers[key];
          Object.keys(handlers || { }).forEach(function (event) {
            const target = event.match(/page$/) ? document.documentElement : node;
            const eventName = event.replace(/page$/, "").replace(/^on/, "");
            return target.addEventListener(eventName, function (e) {
              _formkey = key;
              handlers[event](e);
              e.stopPropagation();
              return e.preventDefault();
            }, false);
          });
        }
      }

      return LINKS.appDom(node, activate);
    },

    stringify: function (v) {
      DEBUG.debug("stringifying: ", v);
      const t = JSON.stringify(v, replacer);
      DEBUG.debug("stringified: ", t);
      if (typeof t === 'string') {
        return t;
      }
      throw new Error("Internal error: unable to JSONize " + v);
    },

    stringifyB64: function (v) { return LINKS.b64encode(LINKS.stringify(v)); },
    parseB64: function (text) { return { content: JSON.parse(LINKS.base64decode(text)) }; },
    parseB64Safe: function (text) { return LINKS.parseB64(text.replace('\n', '')); },
  };


  LINKS.singleXmlToDomNodes = function (xmlObj) {
    DEBUG.assert(
      _isXmlItem(xmlObj),
      'LINKS.singleXmlToDomNodes expected a XmlItem, but got ' + xmlObj
    );

    const type = xmlObj.type;
    switch (type) {
      case 'ELEMENT':
        const tagName = xmlObj.tagName;
        const attributes = xmlObj.attrs;
        const children = xmlObj.children !== undefined ? LINKEDLIST.toArray(xmlObj.children) : [ ];
        const namespace = xmlObj.namespace;

        const node = namespace ?
          document.createElementNS(namespace, tagName) :
          document.createElement(tagName);

        Object.keys(attributes).forEach(function (k) {
          const splitRes = k.split(':');

          if (splitRes.length === 1) {
            const name = splitRes[0];
            node.setAttribute(name, attributes[k]);
          } else if (splitRes.length === 2) {
            const ns = splitRes[0];
            const name = splitRes[1];
            node.setAttributeNS(ns, name, attributes[k]);
          } else {
            throw new Error('attribute names can contain one or no colon. `' + k + '` found.');
          }
        });
        if(xmlObj.children) {
          LINKEDLIST.forEach(xmlObj.children,function(c) {
            node.appendChild(LINKS.singleXmlToDomNodes(c));
          });
        }

        return node;
      case 'TEXT':
        return document.createTextNode(xmlObj.text || '');
      default:
        return null;
    }
  };

  LINKS.eq = function(l,r) {
    if (l == r)
      return true;

    if (l == null)
      return (r == null);
    else if (r == null)
      return false;

    if (TYPES.isUnit(l) && TYPES.isUnit(r))
      return true;

    if (TYPES.isArray(l) && l != null &&
        TYPES.isArray(r) && r != null) {
      if (l.length != r.length)
        return false;

      for (var i = 0; i < l.length; ++i) {
        if (!LINKS.eq(l[i], r[i])) return false;
      }

      return true;
    }
    if (TYPES.isLinkedList(l) && l != null &&
        TYPES.isLinkedList(r) && r != null) {
      return LINKEDLIST.equal(l,r)
    }
    else if(typeof(l) == 'object' && l != undefined && l != null &&
            typeof(r) == 'object' && r != undefined && r != null) {
      if(l.constructor != r.constructor)
        return false;

      // DODGEYNESS:
      //   - it isn't clear that structural equality is always the same as
      //   referential equality for DOM nodes

      for(let p in l) {
        if (!LINKS.eq(l[p], r[p])) {
          return false;
        }
      }
      for(let p in r) {
        if (!LINKS.eq(l[p], r[p]))
          return false;
      }
      return true;
    }
    return false;
  };

  return LINKS;
})();

const _debug = DEBUG.debug;
const debug = LINKS.kify(DEBUG.debug);

function _regexCompile(expr){
  switch(expr._label){
    case "StartAnchor": return "^";
    case "EndAnchor": return "$";
    case "Any": return ".";
    case "Simply": return expr._value;
    case "Group": return "(" + _regexCompile(expr._value) + ")";
    case "Quote": return "\\" + _regexCompile(expr._value);
    case "Replace": _error("\"Replace\" not implemented");
    case "Seq": {
      const regexes = LINKEDLIST.toArray(expr._value);
      let acc = "";
      for(let regex of regexes){
        acc = acc + _regexCompile(regex);
      }
      return acc;
    }
    case "Repeat": {
      let op = undefined;
      switch(expr._value[1]._label){
        case "Plus": op = "+"; break;
        case "Star": op = "*"; break;
        case "Question": op = "?"; break;
        default: _error("Unrecognised operation '" + expr._value[1]._label +"'");
      }
      const regex = _regexCompile(expr._value[2]);
      return regex + op;
    }
    case "Range": {
      const first = expr._value[1]._c;
      const second = expr._value[2]._c;
      return "[" + first + "-" + second + "]";
    }
    case "Alternate":{
      const first = _regexCompile(expr._value[1]);
      const second = _regexCompile(expr._value[2]);
      return first + "|" + second;
    }
    default: _error("Unrecognised label '"+ expr._label + "'");
  }
  return null; // Can never be reached
}

function _tilde(s, regex) {
  const r = _regexCompile(regex);
  return (new RegExp(r)).test(s);
}
var tilde = LINKS.kify(_tilde);

var _intToString = function (x) { return String(x); }
var _stringToInt = function (x) { return parseInt(x); }
var _intToFloat = Number;
var _floatToInt = Math.floor;
var _floatToString = function (x) { return String(x); }
var _stringToFloat = function (x) { return parseFloat(x); }

var intToString = LINKS.kify(_intToString);
var stringToInt = LINKS.kify(_stringToInt);
var intToFloat = LINKS.kify(_intToFloat);
var floatToInt = LINKS.kify(_floatToInt);
var floatToString = LINKS.kify(_floatToString);
var stringToFloat = LINKS.kify(_stringToFloat);

function _not(x) { return !x; } // should be inlined
var _Concat = LINKEDLIST.append;
var _empty = LINKEDLIST.empty;
var _Cons = LINKEDLIST.Cons;
var _hd = LINKEDLIST.head;
var _tl = LINKEDLIST.tail;
var _length = LINKEDLIST.length;
var _take = LINKEDLIST.take;
var _drop = LINKEDLIST.drop;
var _max = LINKEDLIST.max;
var _min = LINKEDLIST.min;
var _singleton = LINKEDLIST.singleton;



var not    = LINKS.kify(_not);
var Nil    = LINKEDLIST.Nil;
var Cons   = LINKS.kify(LINKEDLIST.Cons);
var Concat = LINKS.kify(LINKEDLIST.Concat);
var empty  = LINKS.kify(LINKEDLIST.empty);
var hd     = LINKS.kify(LINKEDLIST.head);
var tl     = LINKS.kify(LINKEDLIST.tail);
var length = LINKS.kify(LINKEDLIST.length);
var take   = LINKS.kify(LINKEDLIST.take);
var drop   = LINKS.kify(LINKEDLIST.drop);
var max    = LINKS.kify(LINKEDLIST.max);
var min    = LINKS.kify(LINKEDLIST.min);



function _charAt (s, i) {
  return {_c:s.charAt(i)};
}

function _strlen (s) {
  return s.length;
}

function _strsub (s, start, len) {
  return s.substr(start, len);
}

function _explode (s) {
  var cs = Nil;
  for (var i = s.length-1; i >= 0; --i) {
    cs = LINKEDLIST.Cons({ _c: s.charAt(i) },cs)
  }
  return cs;
}

function _implode(cs) {
  var s = "";
  LINKEDLIST.forEach(cs,function(e) {
    s += e._c;
  });
  return s;
}

var charAt  = LINKS.kify(_charAt);
var strsub  = LINKS.kify(_strsub);
var strlen  = LINKS.kify(_strlen);
var explode = LINKS.kify(_explode);
var implode = LINKS.kify(_implode);

// These should all be redundant now
// as they're dealt with explicitly by the
// JS compiler.
//
//// Basic operators in direct-style
////   (perhaps move these into LINKS package)
// function __minus(l,r)  { return l - r; }
// function __plus(l,r)   { return l + r ; }
// function __times(l,r)  { return l * r ; }
// function __divide(l,r) { return l / r ; }
// function _le(l,r)     {   return l <= r ; }
// function _ge(l,r)     {   return l >= r ; }
// function _gt(l,r)     {   return l >  r ; }

// var _plus   = LINKS.kify(__plus);
// var _times  = LINKS.kify(__times);
// var _divide = LINKS.kify(__divide);
// var _minus  = LINKS.kify(__minus);
// var _hyphen = _minus;
// var _star   = _times;
// var _slash  = _divide;

// var _plus_fullstop   = _plus;
// var _hyphen_fullstop = _minus;
// var _star_fullstop   = _times;
// var _slash_fullstop  = _divide;

function _debugObj(obj) {
  if (obj == undefined) {
    DEBUG.debug(obj, " : undefined");
  } else {
    DEBUG.debug(obj, " : ", typeof(obj), (typeof(obj) == 'object' ? obj.constructor : ''));
  }
  return [];
}
var debugObj = LINKS.kify(_debugObj);

function _dump(obj) {
  console.info(obj);
}
var dump = LINKS.kify(_dump);

function _show(obj) {
  return JSON.stringify(obj, null, 2);
}

var show = LINKS.kify(_show);

function _negate(x) { return -x; }
var negate = LINKS.kify(_negate);

var _negatef = _negate;
var negatef = negate;

function _error(msg) {
  console.error(msg);
  throw new Error("Error: " + msg);
}

var error = LINKS.kify(_error);

// partialApply : ((a0, a1, ..., an) -> b, a0) -> (a1, ..., an) -> b
// the partialApply function is used to construct closures
// Note that we annotate the closure with its environment, so that it
// can be inspected
function partialApplySE(f, x) {
  const res = function () {
      f.apply(this, [x].concat(Array.prototype.slice.call(arguments)));
  }
  res.__closureEnv = x;
  return res;
}

function partialApply(f, x) {
    return function() {
        return f.apply(this, [x].concat(Array.prototype.slice.call(arguments)));
    }
}

// DOM interaction

//insertBeforeXml : xml -> domRef -> ()
function _insertBefore(insertXml, beforeNode) {
  var parent = beforeNode.parentNode;
  var nodes = LINKS.XmlToDomNodes(insertXml);
  for (var i=0; i < nodes.length; i++) {
    parent.insertBefore(nodes[i], beforeNode);
    LINKS.activateHandlers(nodes[i]);
  }
  return CONSTANTS.UNIT;
}

//appendChildXml : xml -> domRef -> ()
function _appendChildren(appendXml, parentNode) {
  var nodes = LINKS.XmlToDomNodes(appendXml);
  for (let i = 0; i < nodes.length; i++) {
    parentNode.appendChild(nodes[i]);
    LINKS.activateHandlers(nodes[i]);
  }
  return CONSTANTS.UNIT;
}

//removeNode : domRef -> ()
function _removeNode(nodeRef) {
  if (nodeRef.parentNode) {
    nodeRef.parentNode.removeChild(nodeRef);
  } else {
    throw new Error("Cannot remove DOM root node");
  }

  return CONSTANTS.UNIT;
}

function _cloneNode(nodeRef, deep) {
  return nodeRef.cloneNoe(deep);
}

//replaceNode : (xml, domRef) -> ()
function _replaceNode(withXml, replaceNode) {
  _insertBefore(withXml, replaceNode);
  _removeNode(replaceNode);
  return CONSTANTS.UNIT;
}

//replaceDocument : xml -> ()
var replaceDocument = LINKS.kify(_replaceDocument);


// WARNING: insertBeforeRef MOVES a DOM node
//insertBeforeRef : domRef -> domRef -> ()
function _domInsertBeforeRef(insertNode, beforeNode) {
  var parent = beforeNode.parentNode;
  parent.insertBefore(insertNode, beforeNode)
  LINKS.activateHandlers(insertNode);
  return CONSTANTS.UNIT;
}

//appendChildRef : domRef -> domRef -> ()
function _domAppendChildRef(appendNode, parentNode) {
  parentNode.appendChild(appendNode);
  LINKS.activateHandlers(appendNode);
  return CONSTANTS.UNIT;
}

//getDocRef : () -> domRef
function _getDocumentNode() {
  return document.documentElement;
}

//getRefById : string -> domRef
function _getNodeById(id) {
  const ref = document.getElementById(id);

  if (!ref) {
    console.warn("getNodeById: ID ", id, " does not exist");
  }
  return ref;
}

//isNullRef : domRef -> bool
function _isNull(node) {
  return node == null;
}

var insertBefore = LINKS.kify(_insertBefore);
var appendChildren = LINKS.kify(_appendChildren);
var replaceNode = LINKS.kify(_replaceNode);

var domInsertBeforeRef = LINKS.kify(_domInsertBeforeRef);
var domAppendChildRef = LINKS.kify(_domAppendChildRef);
var removeNode = LINKS.kify(_removeNode);
var cloneNode = LINKS.kify(_cloneNode);

var getDocumentNode = LINKS.kify(_getDocumentNode);
var getNodeById = LINKS.kify(_getNodeById);
var isNull = LINKS.kify(_isNull);


// XML datatype manipulation.

/*
 * The XML representation in JavaScript has type:
 *
 * type XmlItem = {
 *   type: 'ELEMENT' | 'TEXT',
 *   text?: string,
 *   tagName?: string,
 *   attrs?: { [attrName: string]: string },
 *   children?: XmlItem[]
 *      (in our XML representation, we use our custom linked lists for the children,
 *       whereas JS node objects use  JS Arrays to represent the children)
 * };
 */

function _nodeTextContent(node) {
  return node.textContent || node.data;
}

// getInputValue : String -> String
function _getInputValue(id) {
  var element = document.getElementById(id);
  DEBUG.assert(element != null, "invalid input node (id " + id + ")");
  DEBUG.assert(element.value != undefined, "invalid input value in id " + id)
  return element.value;
}

var getInputValue = LINKS.kify(_getInputValue);

//getValue : domRef -> xml
//    NOTE: this is recursive.
function _getValue(nodeRef) {
  if (nodeRef.nodeType == document.TEXT_NODE) {
    return _Cons(
      {
        type: 'TEXT',
        text: _nodeTextContent(nodeRef)
      },
      Nil);
  } else if (nodeRef.nodeType == document.ELEMENT_NODE ) {
    var children = Nil;

    // @TODO
    for (var i=nodeRef.childNodes.length -1; i <= 0; i++) {
      children = LINKEDLIST.Cons(_getValue(nodeRef.childNodes[i]),children);
    }

    var attrs = { };
    for (var i=0; i < nodeRef.attributes.length; i++) {
      attrs[nodeRef.attributes[i].name] =
        nodeRef.attributes[i].value
    }

    DEBUG.assert(
      !children.some(function (e) { return e.type !== 'ELEMENT' && e.type !== 'TEXT'; }),
      'Invalid children constructed in _getValue'
    );

    return _Cons(
      {
        type: 'ELEMENT',
        tagName: nodeRef.tagName,
        attrs: attrs,
        children: children,
      },Nil
    );

  } else {
    throw new Error("Unknown node type " + nodeRef.nodeType + " in GetXml");
  }
}
const getValue = LINKS.kify(_getValue);

// Accessors for DomRefs
function _domGetTagNameFromRef(nodeRef) {
  return nodeRef.nodeName;
}

function _domHasAttribute(nodeRef, attr) {
  return nodeRef.hasAttribute(attr);
}

function _domGetAttributeFromRef(nodeRef, attr) {
  if (attr == 'offsetTop') {
    return _intToString(nodeRef.offsetTop);
  } else if (attr == 'offsetLeft') {
    return _intToString(nodeRef.offsetLeft);
  }

  return nodeRef.getAttribute(attr);
}

function _domGetPropertyFromRef(nodeRef, propertyName) {
  return String(nodeRef[propertyName]);
}

function _domSetPropertyFromRef(nodeRef, propertyName, newVal) {
  nodeRef[propertyName] = newVal;
  return newVal;
}


function _domSetAttributeFromRef(nodeRef, attr, value) {
  return nodeRef.setAttribute(attr, value);
}

function _domRemoveAttributeFromRef(nodeRef, attr) {
  nodeRef.removeAttribute(attr);
  return CONSTANTS.UNIT;
}

function _domSetStyleAttrFromRef(nodeRef, attr, value) {
  return nodeRef.style[attr] = value;
}

function _domGetStyleAttrFromRef(nodeRef, attr) {
  return nodeRef.style[attr];
}

/**
 * domGetChildrenFromRef : (DomNode) ~> [ DomNode ]
 * Returns all children of a DOM node
 *
 * @param {DomNode} nodeRef
 * @returns {[ DomNode ]}
 */
function _domGetChildrenFromRef (nodeRef) {
  // map(id, xs) transforms from NodeList to Array
  return Array.prototype.map.call(nodeRef.childNodes, function (a) { return a; });
}
const domGetChildrenFromRef = LINKS.kify(_domGetChildrenFromRef);

function _domGetNodeValueFromRef(node) {
  return node.value;
}

function _domSetAnchor(anchorRef) {
  window.location.hash = anchorRef;
  return CONSTANTS.UNIT;
}

var domGetNodeValueFromRef = LINKS.kify(_domGetNodeValueFromRef)

var domGetTagNameFromRef = LINKS.kify(_domGetTagNameFromRef);
var domHasAttribute = LINKS.kify(_domHasAttribute);
var domGetAttributeFromRef = LINKS.kify(_domGetAttributeFromRef);
var domSetAttributeFromRef = LINKS.kify(_domSetAttributeFromRef);
var domRemoveAttributeFromRef = LINKS.kify(_domRemoveAttributeFromRef);
var domSetStyleAttrFromRef = LINKS.kify(_domSetStyleAttrFromRef);
var domGetStyleAttrFromRef = LINKS.kify(_domGetStyleAttrFromRef);
var domGetPropertyFromRef = LINKS.kify(_domGetPropertyFromRef);
var domSetPropertyFromRef = LINKS.kify(_domSetPropertyFromRef);
var domSetAnchor = LINKS.kify(_domSetAnchor);
// basic dom navigation
function _parentNode(nodeRef) {
  return nodeRef.parentNode;
}
function _firstChild(nodeRef) {
  return nodeRef.childNodes[0];
}
function _nextSibling(nodeRef) {
  return nodeRef.nextSibling;
}
var parentNode = LINKS.kify(_parentNode);
var firstChild = LINKS.kify(_firstChild);
var nextSibling = LINKS.kify(_nextSibling);

// useful DOM operations

//swapNodes : (domRef, domRef) -> ()
function _swapNodes(x, y) {
  DEBUG.assert(
    x.parentNode != null && y.parentNode != null,
    "cannot swap root nodes"
  );
  DEBUG.assert(x.parentNode != y, "cannot swap a node with its parent");
  DEBUG.assert(y.parentNode != x, "cannot swap a node with its parent");

  var xNextSibling = x.nextSibling;
  var yNextSibling = y.nextSibling;
  var xParent = x.parentNode;
  var yParent = y.parentNode;

  if (xNextSibling != y) {
    xParent.insertBefore(y, xNextSibling);
  }

  if (yNextSibling != x) {
    yParent.insertBefore(x, yNextSibling);
  }

  return CONSTANTS.UNIT;
}
var swapNodes = LINKS.kify(_swapNodes);

/**
 * domReplaceChildren : xml -> domRef -> ()
 * @param {Xml} xml
 * @param {DomNode} parent
 * @returns {()}
 */
function _domReplaceChildren(xml, parent) {
  const newNodes = LINKS.XmlToDomNodes(xml);

  parent.innerHTML = '';

  newNodes.forEach(function (node) { return _domAppendChildRef(node, parent) });
  return CONSTANTS.UNIT;
}

var domReplaceChildren = LINKS.kify(_domReplaceChildren);

// for server generated event handlers
function _registerMobileEventHandlers(key, handlers) {
  LINKEDLIST.forEach(handlers, function(h){
    var event = h[1];
    var handler =_wrapEventHandler(h[2]);
    if (!_eventHandlers[key]) {
      _eventHandlers[key] = {};
    }
    _eventHandlers[key][event] = handler;
  });
  return key;
}

function _registerEventHandlers(handlers) {
  var key = '_key' + _get_fresh_node_key();

  LINKEDLIST.forEach(handlers, function(h) {
    var event = h[1];
    var handler = _wrapEventHandler(h[2]);
    if (!_eventHandlers[key]) {
      _eventHandlers[key] = {};
    }
    _eventHandlers[key][event] = handler;
  });
  return key;
}

/**
 * Event polyfill for IE
 *
 * @param {Event} event
 * @returns {Event}
 */
function polyFillEvent (event) {
  // stackoverflow.com/questions/7790725/javascript-track-mouse-position
  event = event || window.event; // IE-ism

  // If pageX/Y aren't available and clientX/Y are,
  // calculate pageX/Y - logic taken from jQuery.
  // (This is to support old IE)

  if (event.pageX == null && event.clientX != null) {

    let eventDoc = (event.target && event.target.ownerDocument) || document;
    let doc = eventDoc.documentElement;
    let body = eventDoc.body;

    event.pageX = event.clientX +
      (doc && doc.scrollLeft || body && body.scrollLeft || 0) -
      (doc && doc.clientLeft || body && body.clientLeft || 0);
    event.pageY = event.clientY +
      (doc && doc.scrollTop  || body && body.scrollTop  || 0) -
      (doc && doc.clientTop  || body && body.clientTop  || 0 );
  }
  return event;
}

function _getTarget(event) { return event.target }
function _getTargetValue(event) {
  return _getTarget(event).value;
}
function _getTargetElement(event) {
  return event.target
}
function _getPageX(event) { return polyFillEvent(event).pageX; }
function _getPageY(event) { return polyFillEvent(event).pageY;  }
function _getFromElement(event) {
  if (event.type === "mouseover") {
    return event.relatedTarget;
  } else if (event.type === "mouseout") {
    return event.target;
  } else {
    throw new Error("Can only get the from element for mouseover and mouseout events");
  }
}

function _getToElement(event) {
  if (event.type === "mouseover") {
    return event.target;
  } else if (event.type === "mouseout") {
    return event.relatedTarget;
  } else {
    throw new Error("Can only get the to element for mouseover and mouseout events");
  }
}
function _getTime(event) { return event.timeStamp || -1; }
function _getCharCode(event) { return event.keyCode || -1; }

var getTarget = LINKS.kify(_getTarget)
var getTargetValue = LINKS.kify(_getTargetValue)
var getPageX = LINKS.kify(_getPageX)
var getPageY =LINKS.kify(_getPageY)
var getFromElement = LINKS.kify(_getFromElement)
var getToElement = LINKS.kify(_getToElement)
var getTime = LINKS.kify(_getTime)
var getCharCode = LINKS.kify(_getCharCode)

function innerHTML(x) { return x.innerHTML }

var stringifyB64 = LINKS.stringifyB64;

// (IE) hack
var _cssText = "";

// TBD: put these _isXml functions in the DEBUG module?

function _isXmlItem(obj) {
  if (!obj || !obj.type) {
    return false;
  }

  if (obj.type === 'ELEMENT') {

    if(!TYPES.isLinkedList(obj.children)) {
      return false;
    }
    var childrenOkay = true;
    LINKEDLIST.forEach(obj.children, function(child) {
      childrenOkay &= _isXmlItem(child)
    });
    if(!childrenOkay)
      return false;


    return Boolean(
      obj.tagName &&
      TYPES.isString(obj.tagName) &&

      obj.attrs &&
      Object.keys(obj.attrs)
        .map(function (key) { return TYPES.isString(key) && TYPES.isString(obj.attrs[key]); })
        .reduce(function (a, b) { return a && b; }, true)
    );
  }

  if (obj.type === 'TEXT') {
    return TYPES.isString(obj.text);
  }

  return false;
}

function _isXml(obj) {
  if (!TYPES.isLinkedList(obj)) return false;
  var childrenOkay = true;
  LINKEDLIST.forEach(obj, function(c) {
    childrenOkay &= _isXmlItem(c);
  });
}

function _stringToXml(s) {
//  DEBUG.assert(TYPES.isArray(s), "_stringToXml Expected an array, got: " + s);
  var res = _Cons(  { type: 'TEXT',text: s},Nil);
  return res;
}

function _intToXml(i) {
  return _stringToXml(_intToString(i));
}
function _floatToXml(f) {
  return _stringToXml(_floatToString(f));
}

var stringToXml = LINKS.kify(_stringToXml);
var intToXml = LINKS.kify(_intToXml);

// not in library.ml yet
var floatToXml = LINKS.kify(_floatToXml);


function _getTagName(xml) {
  var first = LINKEDLIST.head(xml);
  if (first.type !== "ELEMENT") {
    throw new Error("getTagName() applied to non-element node");
  }
  return first.tagName;
}

function _getNamespace (xml) {
  var first = LINKEDLIST.head(xml);
  if (first.type !== 'ELEMENT') {
    throw new Error('getNameSpace applied to non-element node');
  }
  return first.namespace || '';
}

function _getChildNodes(xml) {
  var first = LINKEDLIST.head(xml);
  if (first.type !== "ELEMENT") {
    throw new Error("getChildNodes() applied to non-element node");
  }
  return first.children || [ ];
}

function _getTextContent(xml) {
  var first = LINKEDLIST.head(xml);
  if (first.type !== "TEXT") {
    throw new Error("getTextContent() applied to non-text node");
  }
  return first.text || '';
}

function _getAttributes(xml) {
  var first = LINKEDLIST.head(xml);
  if (first.type !== "ELEMENT") {
    throw new Error("getAttributes() applied to non-element node");
  }

  return Object.keys(first.attrs).map(function (key) {
    return {
      1: key,
      2: first.attrs[key],
    };
  });
}

function _hasAttribute(xml, attrName) {
  var first = LINKEDLIST.head(xml);
  if (first.type !== "ELEMENT") {
    throw new Error("hasAttribute() applied to non-element node");
  }

  return Boolean(first.attrs[attrName]);
}

function _getAttribute(xml, attrName) {
  var first = LINKEDLIST.head(xml);
  if (first.type !== "ELEMENT") {
    throw new Error("getAttribute() applied to non-element node");
  }
  return first.attrs[attrName];
}

// addAttributes : (Xml, [(String, String)]) -> Xml
function _addAttributes(xml, attrs) {
  var first = LINKEDLIST.head(xml);
  if(first.type != "ELEMENT") {
    throw new Error("addAttributes() applied to non-element node");
  }

  const xmlItem = JSON.parse(JSON.stringify(first));

  for (let attr in attrs) {
    xmlItem.attrs[attr] = attrs[attr];
  }

  return LINKEDLIST.singleton( xmlItem );
}

const getTagName = LINKS.kify(_getTagName);
const getAttributes = LINKS.kify(_getAttributes);
const hasAttribute = LINKS.kify(_hasAttribute);
const getAttribute = LINKS.kify(_getAttribute);
const getChildNodes = LINKS.kify(_getChildNodes);
const getTextContent = LINKS.kify(_getTextContent);

function _fail(str) {
  return _alert("Internal error: " + str);
}


function _isElementNode(node) {
  return (node != null && node.nodeType == document.ELEMENT_NODE);
}

// DomNode -> bool
var isElementNode = LINKS.kify(_isElementNode);

function isElement(node) {
  return (node.nodeType == document.ELEMENT_NODE);
}

function isElementWithTag(node, tag) {
  return (isElement(node) && (node.tagName.toLowerCase() == tag));
}

// (IE) style hacks
function _activateStyleElement() {
  if(!_cssText)
    return;

  var styleElement = document.getElementsByTagName('style')[0]
  if(!styleElement || !styleElement.styleSheet)
    throw ("style element doesn't have a style sheet")

  styleElement.styleSheet.cssText = _cssText;
  _cssText = null;
  return;
}

// time in seconds since the beginning of 1970
function _clientTime () { return Date.now(); }
var clientTime = LINKS.kify(_clientTime);

function _dateToLinksDate(d) {
  return {
    year: d.getFullYear(),
    month: d.getMonth(),
    day: d.getDate(),
    hours:d.getHours(),
    minutes:d.getMinutes(),
    seconds:d.getSeconds()
  };
}

function _linksDateToDate(d) {
  return new Date(d.year, d.month, d.day, d.hours, d.minutes, d.seconds); // makes garbage
}

// convert seconds since beginning of 1970 to a date
function _intToDate(t) {
  return _dateToLinksDate(new Date(t * 1000)); // makes garbage
}
var intToDate = LINKS.kify(_intToDate);

// convert a date to seconds since beginning of 1970
function _dateToInt(date) {
  return Math.floor(_linksDateToDate(date).getTime() / 1000);
}
var dateToInt = LINKS.kify(_dateToInt);


var _pageTimer;

function _startTimer() {
  _pageTimer = _clientTime();
  return;
}
function _stopTimer() {
  _pageTimer = _clientTime() - _pageTimer;
  DEBUG.debug("Page drawn in ", _pageTimer, "ms");
  return;
}

/// focus stuff
var _focused = null;
function _focus() {
  if (_focused) {
    var y = document.getElementById(_focused);
    if (y) {
      y.focus();
    }
  }
  return;
}

// Page update

//  _replaceDocument(tree)
//    Replace the current page with `tree'.
function _replaceDocument(tree) {
  DEBUG.assert(tree != null, "No argument given to _replaceDocument");
  var firstChild = LINKEDLIST.head(tree);
  DEBUG.assert(firstChild != null, "Null tree passed to _replaceDocument");
  DEBUG.assert(
    firstChild.type === "ELEMENT",
    "New document value was not an XML element (it was non-XML or was an XML text node)."
  );
  tree = LINKS.XmlToDomNodes(tree);

  // save here
  var _saved_fieldvals = [];
  var inputFields = document.getElementsByTagName("input");
  for (var i = 0; i < inputFields.length; i++) {
     var current = inputFields[i];
     if (current.id != null && current.id != "") { // only store fields with an id!
       _saved_fieldvals.push({'field' : current.id, 'value' : current.value});
     }
  }

  // delete the DOM except for the html tag and the body
  // (IE) IE doesn't allow these tags to be deleted
  var d = document.documentElement;
  var body;
  while (d.hasChildNodes()) {
    if (isElementWithTag(d.firstChild, 'body')) {
      body = d.firstChild;
      var bodyLength = body.childNodes.length;
      while (body.hasChildNodes()) {
        body.removeChild(body.firstChild);
      }
      break; // (IE) no more nodes allowed after the body
    }
    d.removeChild(d.firstChild);
  }

  // insert new dom nodes
  for (var p = tree[0].firstChild; p != null; p = p.nextSibling) {
    if (isElementWithTag(p, 'body')) {
     // insert body nodes inside the existing body node
      for (var q = p.firstChild; q != null; q = q.nextSibling) {
        var it = q.cloneNode(true);
        body.appendChild(it);
        LINKS.activateHandlers(it);
      }
      break; // (IE) no more nodes allowed after the body
    }
    var it = p.cloneNode(true);
    d.insertBefore(it, body);
    LINKS.activateHandlers(it);
  }

  // (IE) hack to activate a style element in IE
  _activateStyleElement();

  // restore here
  for (var i = 0; i < _saved_fieldvals.length; i++) {
     var current = _saved_fieldvals[i];
     var elem = document.getElementById(current.field);
     if (elem) {
        elem.value = current.value;
     }
  }

  _focus();

  return CONSTANTS.UNIT;
}

function _start(page) {
  _stopTimer();
  return _replaceDocument(page);
//  renderPage(page, _replaceDocument)
}

function _startRealPage() {
  var state = LINKS.resolveJsonState(_jsonState);
  _initVars(state); // resolve JSONized values for toplevel let bindings received from the server
  LINKS.activateJsonState(state, _jsonState); // register event handlers + spawn processes
  LINKS.activateHandlers(_getDocumentNode());
  // Create a websocket connection
  return WEBSOCKET.connect_if_required(_jsonState);
}

// generate a fresh key for each node
var _node_key = 0;
function _get_fresh_node_key() {
  return _node_key++;
}

var _eventHandlers = {};

// Wrap an event handler in a function that sets the main process id
// at the beginning of the handler and unsets it at the end.
function _wrapEventHandler(handler) {
  return function (event) {
    // set process id here
    var active_pid = _current_pid;
    _current_pid = _mainPid;
    _handlingEvent = true;

    var _cont = function () { return handler(event, _idy); }
    // A trampoline for use while handling events.
    // Since we don't yield to the browser event loop in event
    // handlers, we quickly run out of stack. To avoid that we
    // throw away the stack periodically by throwing an
    // exception containing the current continuation, which we
    // invoke when the exception is caught.
    for (; ;) {
      try {
        _cont();

        // restore process id here.
        _handlingEvent = false;
        _current_pid = active_pid;
        return;
      } catch (e) {
        if (e instanceof _Continuation) {
          _cont = e.v;
          continue;
        } else {
          _handlingEvent = false;
          _current_pid = active_pid;
          throw e;
        }
      }
    }
  }
}

// SL: I think this function is no longer used
function _registerFormEventHandlers(actions) {
   var key = '_key' + _get_fresh_node_key();

  for (var i = 0; i < actions.length; i++) {
    var action = actions[i];
      // FIXME: Shouldn't we need to clone the actions[i] objs?

    //DEBUG.debug("adding " + action.evName + " to object " + key);
    if (!_eventHandlers[key])
      _eventHandlers[key] = [];
    _eventHandlers[key][action.evName] = _wrapEventHandler(action.handler);
  }

  return key; // returns the ID to give to the elt
}




// db functions (via remote calls)
// TODO!

var javascript = true;

// identity: a "toplevel" continuation
//
// (this needn't actually return a value as _yield and _yieldcont
// don't actually return values)
//
// function _idy(x) { return; }

function _efferr(z, ks) { return _error("Unhandled operation `" + z._label + "'.") }

function _isChannel(v) {
   return (("_sessEP1" in v) && ("_sessEP2" in v));
}

// We want to inspect each variable to get the contained channels, even
// if these things are, for example, tuples, lists, or closures.
// Luckily we've already done this -- in the inspection for delegated sessions!
function resolveSet(set) {
  const ret = new Set();
  for (let obj of set) {
    const chans = getContainedSessions(obj);
    for (let c of chans) {
      ret.add(c);
    }
  }
  return ret;
}

function _handleSessionException(cont_variables) {
  let lst = cont_variables;
  const cont_set = new Set();
  while (lst != null && lst != undefined) {
    cont_set.add(_hd(lst));
    lst = _tl(lst);
  }
  const resolved_cont_set = resolveSet(cont_set);
  const affected_channels = Array.from(resolved_cont_set);
  _debug("In handle session exception. Affected channels:");

  affected_channels.forEach(v => {
    _debug("cancelling channel: " + JSON.stringify(v));
    _cancel(v);
  });
}

function _cancel(c) {
  if (isEndpointCancelled(c)) {
    return; // Re-cancelling sessions is a no-op
  }

  DEBUG.assert(c.hasOwnProperty("_sessEP1"), "Cancelling non-channel");

  const peer_ep = c._sessEP1;
  const local_ep = c._sessEP2;

  function cancelContainedValues() {
    const buf_set = new Set(_buffers[local_ep]);
    const chans = Array.from(resolveSet(buf_set));
    for (let contained_chan of chans) {
      _cancel(contained_chan);
    }
  }

  function notifyPeer() {
    if (peer_ep in _buffers) {
      _wakeup(_chan_blocked[peer_ep]);
      delete _chan_blocked[peer_ep];
    } else {
      if (!isEndpointCancelled(peer_ep)) {
        WEBSOCKET.sendRemoteCancellation(peer_ep, local_ep);
      }
    }
  }

  if (!(local_ep in _buffers)) {
    _debug("Trying to cancel non-local buffer. Probably shouldn't happen!");
    return;
  }

  cancelContainedValues();
  addCancelledEndpoint(local_ep);
  notifyPeer();
}


function handleChannelCancellation(notify_ep, cancelled_ep) {
  // Need to add cancelled_ep to cancelled EP set, and wake up
  // any processes blocked on notify_ep
  addCancelledEndpoint(cancelled_ep);
  if (notify_ep in _chan_blocked) {
    _wakeup(_chan_blocked[notify_ep]);
    delete _chan_blocked[notify_ep];
  }
}


// The following function is somewhat magical. It creates a
// parameterised resumption which when invoked updates the handler's
// environment with the new parameter values.
//
// ptr: the pointer to the parameter box
//   h: the current handler
//   s: the reversed resumption stack
function _make_parameterised_resumption(ptr, s) {
    return function() {
        const op_arg = arguments[0];
        const ks = arguments[arguments.length - 1];
        for (let i = 1; i < arguments.length - 1; i++) {
            ptr[i-1] = arguments[i];
        }

        return _yieldCont(LINKEDLIST.revAppend(s, ks), op_arg);
    }
}

// Called "fun" in the CPS paper
function _make_resumption (s) {
  return function (x, ks) {
    return _yieldCont(LINKEDLIST.revAppend(s, ks), x);
  }
}

function _vmapOp (f, z) {
  return {_label : z._label, _value : {p : z._value.p, s : f(z._value.s)}};
}

// [NOTE]
//   The current strategy is to compile top-level lets to an inlined version of the
// following function _run.
//
// [BUG]
// This doesn't work with setTimeouts as if f yields then x can be returned without
// having been asssigned to.
//
// function _run(f) {
//  var x;
//  f(function (v) {x = v});
//  return x
// }
//
// Something like this version could be made to work...
// ...but clearly it breaks concurrency as it stands
//
// function _run(f) {
//   _handlingEvent = true;
//   var x;
//   var cont = function () {return f(function (v) {x = v})};
//
//   while(true) {
//     try {
//       cont();
//       break;
//     }
//     catch (e) {
//       if (e instanceof _Continuation) {
//          cont = e.v;
//          continue;
//       }
//       else {
//         _handlingEvent = false;
//         throw e;
//       }
//     }
//   }
//
//   _handlingEvent = false;
//   return x
// }
//
// Alternative include adapting our CPS so that it does return a value and
// somehow making sure that the complete program (or at least the side-effecting part of it)
// is a single CPS term.

// [IMPORTANT]
//   The convention is that each setTimeout or XMLHttpRequest callback
//   is responsible for correctly setting the _current_pid. The only other time
//   _current_pid is modified is in _wrapEventHandlers in order to temporarily
//   set it to _mainPid.

var _mailboxes = {0:[]};
var _blocked_procs = {};
//var _suspended_procs = [];

function _dumpSchedStatus() {
  DEBUG.debug("--------\nMailbox status:");
  for (var i in _mailboxes) {
    if (_mailboxes[i].length > 0)
      DEBUG.debug("pid ", i, ": ", _mailboxes[i].length, " msgs waiting");
  }
  var blockedPids = "";
  for (var i in _blocked_procs) {
    if (blockedPids != "") blockedPids += ", ";
    blockedPids += i
  }
  if (blockedPids != "")
    DEBUG.debug("blocked process IDs: ", blockedPids);
  return;
}

function _makeMailbox(pid) {
  if (!_mailboxes[pid]) {
    _mailboxes[pid] = [];
  }
  return;
}

function _freshProcess() {
  _maxPid++;
  var clientPid = "cl_" + _maxPid;
  return clientPid;
}

function _spawnWithMessages(childPid, f, messages) {
  _mailboxes[childPid] = messages;

  setZeroTimeout(function () {
    DEBUG.debug("launched process #" + childPid);
    _current_pid = childPid;
    f(_makeCont(function () { delete _mailboxes[childPid] }))
  });
  return { _clientPid : childPid, _clientId : _client_id }
}

function _registerMobileKey(state, serverKey) {
  var clientKey = '_key' + _get_fresh_node_key();
  state.mobileKeys[serverKey] = clientKey;
  return clientKey;
}

function _lookupMobileKey(state, serverKey) {
  return state.mobileKeys[serverKey];
}

/* Spawn only makes sense on a client if the loc matches.
 * If a client wants to spawn on the server, it should make an RPC
 * request, or send a message to server-sided code.
 * It also makes no sense for a client to spawn a process on another client. */
function check_loc_spawnable(loc) {
  if ("_clientSpawnLoc" in loc) {
    if (loc["_clientSpawnLoc"] === _client_id) {
      return true;
    } else {
        DEBUG.assert(false, "Cannot spawn a process on another client"); return;
    }
  } else if ("_serverSpawnLoc" in loc) {
      DEBUG.assert(false, "Cannot spawn process on server from client"); return;
  } else {
      DEBUG.assert(false, "Invalid spawn location " + loc); return;
  }
}

function _spawnAt(f, loc) {
  check_loc_spawnable(loc);
  var childPid = _freshProcess();
  return _spawnWithMessages(childPid, f, []);
}

function spawnAt(f, loc, kappa) {
  return _applyCont(kappa,_spawnAt(f, loc));
}

// TODO: implement this properly
var _spawnAngelAt = _spawnAt;
var _spawnAngel = _spawn;
var spawnAngel = spawn;
function _spawn(f) { return _spawnAt(f, _here); }
function spawn(f, kappa) { return spawnAt(f, _here, kappa); }

function _spawnWrapper(env) {  // necessary wrapper for server->client calls
  return spawn;
}

function spawnWait(f, kappa) {
  // f is a zero-argument CPS function
  var parentPid = _current_pid
  var childPid = _freshProcess();
  _mailboxes[childPid] = [];

  DEBUG.debug("launched process #", childPid);
  _current_pid = childPid;

  return f(_makeCont(function(v) {_current_pid = parentPid; delete _mailboxes[childPid]; return _applyCont(kappa,v);}));
}

function _self() {
  return { _clientId : _client_id, _clientPid : _current_pid};
}

function self(kappa) {
  return _applyCont(kappa,_self());
}


function _here() {
  return { _clientSpawnLoc: _client_id };
}

function here(kappa) {
  return _applyCont(kappa,_here());
}


var there = here
var _there = _here

function _haveMail() {
  return _mailboxes[_self()._clientPid].length != 0;
}
var haveMail = LINKS.kify(_haveMail);

var _sched_pause = 0;

function _wakeup(pid) {
  if (_blocked_procs[pid]) {
    DEBUG.debug("Waking up ", pid);
    var proc = _blocked_procs[pid];
    delete _blocked_procs[pid];
    return setZeroTimeout(proc);
  } else {
    // already awake?
    DEBUG.debug("Tried to wake up ", pid, ", but it is already awake");
    return;
  }
}

/* Send for mailboxes -- adds a message to a local mailbox */

function _is_valid_client_pid(pid) {
  return (("_clientId" in pid) && ("_clientPid" in pid));
}

function _is_valid_server_pid(pid) {
  return ("_serverPid" in pid);
}

function get_client_id(pid) {
  return pid["_clientId"];
}

function get_process_id(pid) {
  if (_is_valid_client_pid(pid)) {
    return pid["_clientPid"];
  } else if (_is_valid_server_pid(pid)) {
    return pid["_serverPid"];
  } else {
    DEBUG.assert(false, "Invalid PID in get_process_id");
    return;
  }
}

function _Send(pid, msg) {
  //  _dump(_mailboxes)
  /* First, check to see whether we are sending to a well-formed PID */
  var valid_server_pid = _is_valid_server_pid(pid);
  var valid_client_pid = _is_valid_client_pid(pid);
  DEBUG.assert(
    valid_server_pid || valid_client_pid,
    "Malformed PID in _Send: neither client nor server PID"
  );

  if (valid_server_pid) {
    WEBSOCKET.sendRemoteServerMessage(pid._serverPid, msg);
  } else if (valid_client_pid) {
    if (get_client_id(pid) == _client_id) {
      // Local send
      DEBUG.debug("sending message ", msg, " to pid ", pid._clientPid);
      var client_pid = pid._clientPid;
      LINKS.deliverMessage(client_pid, msg);
    } else {
      // Remote send
      WEBSOCKET.sendRemoteClientMessage(get_client_id(pid), pid._clientPid, msg);
    }
  }
  //_dumpSchedStatus();
  return;
}

function Send(pid, msg, kappa) {
  return _applyCont(kappa, _Send(pid, msg));
}

function _SendWrapper(env) {  // necessary wrapper for server->client calls
  return Send;
}

function _dictlength(x) {
  var length = 0;
  for (let prop in x) {
    length++;
  }
  return length;
}

function _block_proc(pid, its_cont) {
  DEBUG.debug("Blocking ", pid);
  _blocked_procs[pid] = its_cont;
  // discard stack
  return;
}

// recv
//   recv is an unusual library function that may capture the
//   continuation; hence there is no _recv form (direct-style).
function recv(kappa) {
  DEBUG.assert(
    arguments.length == 1,
    'recv received ' + arguments.length + ' arguments, expected 1'
  );
  //DEBUG.assert(_current_pid != _mainPid,
  //           "Cannot call recv() in main process.");
  DEBUG.assert(
    _mailboxes[_current_pid],
    "Process " + _current_pid + " seems not to have been created."
  );
  if ( _mailboxes[_current_pid].length > 0) {
    let msg = _mailboxes[_current_pid].pop();
    DEBUG.debug("received message '"+ JSON.stringify(msg) +"'");
    return _applyCont(kappa, msg);
  } else {
    var current_pid = _current_pid;
    return _block_proc(
      current_pid,
      function () {
        _current_pid = current_pid;
        DEBUG.debug("scheduled process ", current_pid);
        return recv(kappa);
      }
    );
    // DEBUG.debug("blocked: "+current_pid)
  }
}

function _recvWrapper(env) {  // necessary wrapper for server->client calls
  return recv;
}

// SESSIONS

let _nextAP = 0
let _nextChannel = 0

/* Channel state */
// Moving this to the model used on the server.
//
// The buffers table maps channel IDs to lists of messages.
const _buffers = {}; // port |-> message list

// The _chan_blocked table is a partial mapping from channel IDs to
// processes, which are blocked waiting on a result.
const _chan_blocked = {}; // port |-> process ID

// Receive endpoints which have been received, but for which we do not yet have
// a lost message response.
// port |-> message list (buffer which was sent with the EP)
const _pending_endpoints = {};

// Messages which have been received after sending a delegation request,
// but before a request to get lost messages.
// port |-> message list
const _lost_messages = {};

// Messages which have been received without an associated buffer, and are
// not in _lost_messages.
// This happens when delegation has started, but we have not yet received
// the channel endpoint / lost messages.
// port |-> message list
const _orphan_messages = {};


// Channels which have been cancelled
const _cancelled_endpoints = new Set();

function isEndpointCancelled(chan) {
  return _cancelled_endpoints.has(chan);
}

function addCancelledEndpoint(chan) {
  _cancelled_endpoints.add(chan);
}


function addMessageToDict(dict, port, msg) {
  buf = [];
  if (port in dict) {
    buf = dict[port];
  }
  buf.unshift(msg);
  dict[port] = buf;
  return;
}

function addOrphanMessage(port, msg) {
  return addMessageToDict(_orphan_messages, port, msg);
}

function addLostMessage(port, msg) {
  return addMessageToDict(_lost_messages, port, msg);
}

// Access point innards
var BALANCED = 0
var ACCEPTING = 1
var REQUESTING = 2

// Access point: { ap_state : access point state code; pending : request list }
function freshAPID() {
  _nextAP++;
  return "clAP_" + _nextAP;
}

// Channel port names need to be globally unique, so add our client ID.
function freshChannelID() {
  _nextChannel++;
  return "clCh_" + _client_id + "_" + _nextChannel;
}

function freshChannel() {
  var id_ep1 = freshChannelID();
  var id_ep2 = freshChannelID();
  _buffers[id_ep1] = [];
  _buffers[id_ep2] = [];
  return { _sessEP1: id_ep1, _sessEP2: id_ep2 };
}

function newWithID(apid) {
  DEBUG.debug("Creating new access point ", apid);
  _aps[apid] = {ap_state: BALANCED, pending: []};
  return {_clientAPID: apid, _clientId: _client_id};
}

function _new() {
  var apid = freshAPID();
  return newWithID(apid);
}

function _is_valid_client_ap(pid) {
  return (("_clientId" in pid) && ("_clientAPID" in pid));
}

function _is_valid_server_ap(pid) {
  return ("_serverAPID" in pid);
}

function get_client_id_from_ap(pid) {
  return pid["_clientId"];
}

function get_apid_from_ap(apid) {
  return apid["_clientAPID"];
}

function get_server_apid_from_ap(apid) {
  return apid["_serverAPID"];
}

function makeFlippedChan(ch) {
  return { _sessEP1 : ch._sessEP2, _sessEP2 : ch._sessEP1 };
}

// Wakes up the process blocked on the given channel endpoint.
function wakeupFromChan(chEP) {
  DEBUG.assert(
    _chan_blocked[chEP] != undefined,
    "wakeupFromChan shouldn't be called on an undefined process!"
  );
  _wakeup(_chan_blocked[chEP]);
  delete _chan_blocked[chEP];
  return;
}

// Block the process until the response has been received from a remote
// AP.
function blockUntilAPResponse(kappa) {
  var current_pid = _current_pid;
  return _block_proc(
    _current_pid,
    function () {
      _current_pid = current_pid;
      // Grab the returned channel EP out of the _returned_channels table,
      // and continue
      DEBUG.assert(_returned_channels[_current_pid] != undefined,
        "resuming process after remote accept, but no channel available!");
      var chan = _returned_channels[_current_pid];
      delete _returned_channels[_current_pid];
      return _applyCont(kappa, chan);
    }
  );
}

// Block the process until a matching process has been found on a local AP.
function blockUntilLocalMatch(ch, kappa) {
  var current_pid = _current_pid;
  return _block_proc(
    _current_pid,
    function () {
      _current_pid = current_pid;
      return _applyCont(kappa,ch);
    }
  );
}

// Do an accept on a local access point. localAPID: the key into the aps table.
function localAccept(localAPID, kappa) {
  // This should explicitly not happen -- all server-created APs residing on
  // this client should be serialised in state dumps provided in realpages
  // delivery / RPC returns
  DEBUG.assert(_aps[localAPID] != undefined, "Attempting to accept on an undefined AP!");
  // If there's a requester, then pop the requester, create the other end of the channel, and
  // wake up the requester.
  var ap = _aps[localAPID];

  function makeAndBlock() {
    var new_ch = freshChannel();
    var our_ep = new_ch._sessEP1;
    ap.pending.unshift(new_ch);
    _chan_blocked[our_ep] = _current_pid;
    return blockUntilLocalMatch(new_ch, kappa);
  }
  switch (ap.ap_state) {
    case BALANCED:
      ap.ap_state = ACCEPTING;
      _aps[localAPID] = ap;
      return makeAndBlock();
    case ACCEPTING:
      return makeAndBlock();
    case REQUESTING:
      DEBUG.assert(ap.pending.length > 0,
        "Accepting on a requesting endpoint with no pending requests!");
      var top = ap.pending.pop();
      if (ap.pending.length == 0) {
        DEBUG.debug("Changing state to balanced");
        ap.ap_state = BALANCED;
      }

      _aps[localAPID] = ap;

      // Unblock other end of the channel
      var other_ep = top._sessEP2;
      wakeupFromChan(other_ep);
      // Pass the channel to the continuation
      return _applyCont(kappa,top);
   default:
      DEBUG.assert(false, "Invalid access point state! " + ap.ap_state);
      return;
  }
}

function remoteAccept(remote_apid, kappa) {
  // Accept remotely
  WEBSOCKET.sendRemoteAPAccept(_current_pid, remote_apid);
  return blockUntilAPResponse(kappa);
}

function accept(ap, kappa) {
  // Firstly, check the type of accept. Local, or remote? Server, or client?
  if (_is_valid_server_ap(ap)) {
    return remoteAccept(get_server_apid_from_ap(ap), kappa);
  } else if (_is_valid_client_ap(ap)) {
    if (get_client_id_from_ap(ap) != _client_id) {
      DEBUG.assert(false, "alas, accepting on a remote client AP is not yet supported");
      return;
    } else {
      return localAccept(get_apid_from_ap(ap), kappa);
    }
  } else {
    DEBUG.assert(false, "invalid access point ID in accept! " + JSON.stringify(ap));
    return;
  }
}


// Do a request on a local access point. localAPID: the key into the aps table.
function localRequest(localAPID, kappa) {
  // This should explicitly not happen -- all server-created APs residing on
  // this client should be serialised in state dumps provided in realpages
  // delivery / RPC returns
  DEBUG.assert(_aps[localAPID] != undefined, "Attempting to request on an undefined AP!");
  DEBUG.debug("Request called on local AP ", localAPID);

  // If there's a requester, then pop the requester, create the other end of the channel, and
  // wake up the requester.
  var ap = _aps[localAPID];

  function makeAndBlock() {
    var new_ch = freshChannel();
    var flipped_chan = makeFlippedChan(new_ch)
    var our_ep = flipped_chan._sessEP1;
    ap.pending.unshift(new_ch);
    _chan_blocked[our_ep] = _current_pid;
    return blockUntilLocalMatch(flipped_chan, kappa);
  }

  switch (ap.ap_state) {
    case BALANCED:
      ap.ap_state = REQUESTING;
      _aps[localAPID] = ap;
      return makeAndBlock();
    case REQUESTING :
      return makeAndBlock();
    case ACCEPTING:
      DEBUG.assert(ap.pending.length > 0,
        "Requesting on an accepting endpoint with no pending requests!");
      var top = ap.pending.pop();
      if (ap.pending.length === 0) {
        DEBUG.debug("Changing state to balanced");
        ap.ap_state = BALANCED;
      }

      _aps[localAPID] = ap;

      // Unblock other end of the channel
      var other_ep = top._sessEP1;
      wakeupFromChan(other_ep);

      // Pass our end of channel (flipped version of pending) to the continuation
      return _applyCont(kappa,makeFlippedChan(top));
   default:
      DEBUG.assert(false, "Invalid access point state! " + ap.ap_state);
      return;
  }
}

function remoteRequest(remote_apid, kappa) {
  // Request remotely
  WEBSOCKET.sendRemoteAPRequest(_current_pid, remote_apid);
  return blockUntilAPResponse(kappa);
}

function request(ap, kappa) {
  if (_is_valid_server_ap(ap)) {
    return remoteRequest(get_server_apid_from_ap(ap), kappa);
  } else if (_is_valid_client_ap(ap)) {
    if (get_client_id_from_ap(ap) != _client_id) {
      DEBUG.assert(false, "alas, requesting from a remote client AP is not yet supported");
      return;
    } else {
      return localRequest(get_apid_from_ap(ap), kappa);
    }
  } else {
    DEBUG.assert(false, "invalid access point ID in request! " + JSON.stringify(ap));
    return;
  }
}

function canReceiveMessage(v) {
  // Check whether all contained sessions are in _buffers (i.e. are committed)
  var contained_sessions = getContainedSessions(v);
  for (var i = 0; i < contained_sessions.length; i++) {
    if (!(contained_sessions[i]._sessEP2 in _buffers)) {
      return false;
    }
  }
  return true;
}

function deliverSessionMessage(epid, v) {
  // There are two circumstances under which we will get a message for which
  // we don't have a buffer. The first is if we've started delegating an endpoint,
  // and a message has been sent between our request and the server processing the
  // delegation request. This is a "lost message", and will be retrieved by the server later.
  // The other circumstance is when someone has delegated a channel to us, and we are receiving
  // the messages for it, but before we have received the channel endpoint. This is an orphan
  // message; we store these and then add them to the buffer when we've got the endpoint.
  if (!(epid in _buffers)) {
    if (epid in _lost_messages) {
      DEBUG.debug("Storing lost message ", v, " for port ", epid);
      return addLostMessage(epid, v);
    } else {
      DEBUG.debug("Storing orphan message ", v, " for port ", epid);
      return addOrphanMessage(epid, v);
    }
  } else {
    _buffers[epid].unshift(v);
    if (canReceiveMessage(v) && (epid in _chan_blocked)) {
      _wakeup(_chan_blocked[epid]);
      delete _chan_blocked[epid];
    }
    return;
  }
}

// Adds the buffers of a set of delegated channels to the pending list if we
// need to wait for lost messages (i.e. message has been delegated from another
// client) or to _buffers if we can proceed right away
function migrateDelegatedSessions(delegated_chans, requires_lost_messages) {
  for (var i = 0; i < delegated_chans.length; i++) {
    var deleg_chan = delegated_chans[i].ep_id;
    var buf = delegated_chans[i].buf;
    if (requires_lost_messages) {
      _pending_endpoints[deleg_chan] = buf;
    } else {
      _buffers[deleg_chan] = buf;
    }
  }
  return;
}


// Handle all lost messages. Given a list of endpoint IDs,
// creates a mapping of the associated entries, and deletes them from
// the lost messages table.
// Finally, sends a response.
function handleGetLostMessages(remote_ep, ep_ids) {
  var ret = {};

  for (var i = 0; i < ep_ids.length; i++) {
    var ep_id = ep_ids[i];
    ret[ep_id] = _lost_messages[ep_id];
    delete _lost_messages[ep_id];
  }

  return WEBSOCKET.sendLostMessageResponse(remote_ep, ret);
}

// Given an endpoint ID and a list of lost messages:
//   - Stores  EPID |-> buffer + lost messages + orphan messages in _buffers
//     (note that JS queues grow at the front, so the order is reversed)
//   - Deletes EPID from _pending_endpoints
//   - Deletes EPID from _orphan_messages
function commitChannel(epid, lost_messages) {
  var orig_buf = _pending_endpoints[epid];
  var orphans = _orphan_messages[epid];
  if (orphans == undefined) {
    orphans = [];
  }
  var complete_buf = orphans.concat(lost_messages, orig_buf);
  _buffers[epid] = complete_buf;
  delete _pending_endpoints[epid];
  delete _orphan_messages[epid];
  return;
}

function handleLostMessages(blocked_ep, lost_message_table) {
  // Commit all channels
  for (var ep_id in lost_message_table) {
    commitChannel(ep_id, lost_message_table[ep_id]);
  }

  // Wake up the channel awaiting the message
  _wakeup(_chan_blocked[blocked_ep]);
  delete _chan_blocked[blocked_ep];
  return;
}


// Retrieves all sessions within the value.
function getContainedSessions(v) {
  if (v == null || v == undefined) { return []; }
  if (TYPES.isArray(v)) {
    var res = [];
    for (var i = 0; i < v.length; i++) {
      res = res.concat(getContainedSessions(v[i]));
    }
    return res;
  } else if(TYPES.isObject(v)) {
    if ("_sessEP1" in v) {
        return [v];
    }
    // We can treat variants and records in the same way
    var vals = Object.values(v);
    var res = [];
    for (var i = 0; i < vals.length; i++) {
      res = res.concat(getContainedSessions(vals[i]));
    }
    return res;
  } else if(v.hasOwnProperty("__closureEnv")) {
    return getContainedSessions(v.__closureEnv); // tee hee, dynamic typing is nice sometimes
  } else {
    return [];
  }
}

// This function expects a list of channel references in the form
// chan = { _sessEP1: <send port>, _sessEP2: <recv port> }.
// The function is side effecting.
//
// Output of the function:
//   [ { chan : chan, buffer : <list of values in the buffer for recv_port> } ]
//
// Side-effects:
//   * _buffers[<recv port>] will be deleted.
//   * _delegating[<recv port>] will be set to [].
function prepareDelegatedChannels(chans) {
  var res = [];
  for (var i = 0; i < chans.length; i++) {
    var cur_chan = chans[i];
    var cur_recv_ep = chans[i]._sessEP2;
    DEBUG.assert(cur_recv_ep in _buffers, "Trying to delegate channel without a buffer!");
    var cur_buf = _buffers[cur_recv_ep];
    // Delete buffer, create lost message buffer
    delete _buffers[cur_recv_ep];
    _lost_messages[cur_recv_ep] = [];
    res.unshift({ chan: cur_chan, buffer: cur_buf});
  }
  return res;
}

function _remoteSessionSend(v, c) {
  var delegated_sessions = getContainedSessions(v);
  var prepared_delegated_sessions = prepareDelegatedChannels(delegated_sessions);
  DEBUG.debug("Prepared delegated sessions:");
  DEBUG.debug(prepared_delegated_sessions);
  return WEBSOCKET.sendRemoteSessionMessage(c, prepared_delegated_sessions, v);
}

function _send(v, c) {
  const peer_ep = c._sessEP1;
  const local_ep = c._sessEP2;

  if (peer_ep in _buffers) {
    // If the send is in the local buffers table, we have hold of the
    // endpoint and can send locally.
    _buffers[peer_ep].unshift(v);
    _wakeup(_chan_blocked[peer_ep]);
    delete _chan_blocked[peer_ep];
    return c;
  } else {
    _remoteSessionSend(v, c);
    return c;
  }
}

/* Basic receive: Does not take into account exceptions */
function _default_receive(c, kappa) {
  var peer_ep = c._sessEP1;
  var local_ep = c._sessEP2;
  DEBUG.assert(local_ep in _buffers, "Trying to receive from nonexistent buffer!");
  var buf_len = _buffers[local_ep].length;
  if (buf_len > 0 && canReceiveMessage(_buffers[local_ep][buf_len - 1])) {
    var msg = _buffers[local_ep].pop();
    return _applyCont(kappa, {1:msg, 2:c});
  } else {
    var current_pid = _current_pid;
    _chan_blocked[local_ep] = current_pid;
    _block_proc(current_pid,
      function () {
        _current_pid = current_pid;
        receive(c, kappa);
      });
  }
}

/* Exception receive: takes an extra argument which is a continuation
 * to invoke if the exception cannot be raised. */
function _exn_receive(c, cancellation_thunk, kappa) {
  var peer_ep = c._sessEP1;
  var local_ep = c._sessEP2;
  DEBUG.assert(local_ep in _buffers, "Trying to receive from nonexistent buffer!");
  var buf_len = _buffers[local_ep].length;
  if (buf_len > 0 && canReceiveMessage(_buffers[local_ep][buf_len - 1])) {
    var msg = _buffers[local_ep].pop();
    // DEBUG.debug("Grabbed " + JSON.stringify(msg));
    return _applyCont(kappa, {1:msg, 2:c});
  } else {
    // If other endpoint is cancelled, raise an exception. Otherwise, block.
    if (isEndpointCancelled(peer_ep)) {
      _debug("Trying to receive from empty buffer where remote endpoint is cancelled. Clearing out continuation...");
      // Cancel the channel & carried channels
      _cancel(c);
      // Force cancellation thunk to cancel FVs in the continuation.
      return cancellation_thunk(kappa);
    } else {
      var current_pid = _current_pid;
      _chan_blocked[local_ep] = current_pid;
      _block_proc(current_pid,
        function () {
          _current_pid = current_pid;
          receive(c, cancellation_thunk, kappa);
        });
    }
  }
}

// TODO: implement link
function link(c, d, kappa) {
  throw "link not implemented on the client yet"
}


// SCHEDULER

var _yieldCount = 0;
var _yieldGranularity = 60;
var _callCount = 0;

function _Continuation(v) { this.v = v; return; }

var _theContinuation = new _Continuation(LINKS._removeCGIArgs);


// yield: give up control for another "thread" to work.
// if we're running in an event handler then don't yield (but
// do throw away the stack periodically instead).
function _yield(f) {
  ++_yieldCount;
  if (_yieldCount == _yieldGranularity) {
    _yieldCount = 0;
    if (_handlingEvent) {
      _theContinuation.v = f; throw _theContinuation;
    } else {
      var current_pid = _current_pid;
        return setZeroTimeout(function() { _current_pid = current_pid; return f(); });
    }
  } else {
    return f();
  }
}

function _yieldCont_Default(k, arg) {
  ++_yieldCount;
  if (_yieldCount == _yieldGranularity) {
    _yieldCount = 0;
    if (_handlingEvent) {
      _theContinuation.v = function () { return k(arg); }; throw _theContinuation;
    } else {
      var current_pid = _current_pid;
      return setZeroTimeout(function () { _current_pid = current_pid; return k(arg); });
    }
  } else {
    return k(arg);
  }
}

function _applyCont_Default(k, arg) { return k(arg); }

function _yieldCont_HO(ks, arg) {
  var k = _hd(ks);
  var ks = _tl(ks);
  ++_yieldCount;
  if (_yieldCount == _yieldGranularity) {
    _yieldCount = 0;
    if (_handlingEvent) {
      _theContinuation.v = function () { return k(arg, ks); }; throw _theContinuation;
    } else {
      var current_pid = _current_pid;
      return setZeroTimeout(function () { _current_pid = current_pid; return k(arg, ks); });
    }
  } else {
    return k(arg, ks);
  }
}

function _applyCont_HO(ks, arg) {
  var k = _hd(ks);
  var ks = _tl(ks);

  return k(arg, ks);
}

// FFI
//
// want to find g such that:
//   CPS(g f a) = \k.CPS(a) (\x.k (f x))
// now:
//   CPS(g f a) =_def \k.CPS(a) (\x.g (\h.h k x) f)
// so we just need that:
//   g (\h.h k x) f = k (f x)
//
// setting g = \k.\f.k(\k.\x.k (f x)) solves the
// equation
//
// and LINKS.kify =_def \k.\x.k (f x)
// so let g =_def \k.\f.k (LINKS.kify(f))
//
// let callForeign = g
//
// callForeign allows us to call JS functions from within Links
// using the syntax: callForeign(f)(args)

// [DEACTIVATED]
//function callForeign(kappa) {
//  return function (f) {
//    return kappa (LINKS.kify(f));
//  };
//}

// [DEACTIVATED]
// like callForeign, except it takes
// two arguments: an object and a method
//function callForeignMethod(kappa) {
//  return function (obj, method) {
//    return kappa (LINKS.kifyMethod(obj, method));
//  };
//}

function _print(...str) {
  console.info(...str);
  return 0;
}

var print = LINKS.kify(_print);

// // [DUBIOUS FUNCTIONS]
// //   Should elementByID and attribute be here?
// function elementById(id, kappa) {
//     var elem = document.getElementById(id);
//     if (elem != null) kappa({'_label':'Some', '_value':[elem]});
//     else kappa({_label:'None', '_value':({})});
// }

function _attribute(xml, attr) {
  // FIXME: we need to straighten out the XML/XMLitem distinction.
  //  if (xml.length == 0 ) { return ({_label:'None', '_value':({})});}
  //   obj = xml[0];
  obj = xml;
  if (obj == undefined) {
     return ({_label:'None', '_value':({})});
  }

  //  Take note!!!
  if (attr == 'class') attr = 'className';

  var val = obj[attr];

  if (val == undefined) return({_label:'None', '_value':({})});
  else return({'_label':'Some', '_value':val});
}
var attribute = LINKS.kify(attribute);

function _is_integer(s) {
  return s.match(/^[0-9]+$/) != null;
}
const is_integer = LINKS.kify(_is_integer);

function _objectType(obj) {
  obj = obj[0];
  return(typeof(obj) == 'object' ? obj.constructor : typeof(obj))
}

var objectType = LINKS.kify(_objectType);

// childNodes
//   This is badly named
//    - what is its intended semantics?
//    - what should it be called?
//    - should it even be here?
//
// [BUG]: This expects a DomNode, while the server version expects Xml.
function _childNodes(elem) {
  DEBUG.assert(false, "childNodes is not implemented properly");
  var result = [];
  for (var i=0; i<elem[0].childNodes.length; i++) {
    result.push(elem[0].childNodes[i].cloneNode(true));
  }
  return(result);
}

function childNodes(elem, kappa) {
  return _applyCont(kappa,_childNodes(elem));
}

function _textContent (node) {
  try { return (node.innerHTML) }
  catch (e) { return ("") }
}

function textContent (node, kappa) {
  return _applyCont(kappa,_textContent(node));
}

function _reifyK() {
  return LINKS.unimpl("reifyK");
}

function sleep(duration, kappa) {
  var current_pid = _current_pid;
  return setTimeout(function() { _current_pid = current_pid; return _applyCont(kappa,{}); }, duration);
}

// include a js file
function _include(script_filename) {
  var html_doc = document.getElementsByTagName('head').item(0);
  var js = document.createElement('script');
  js.setAttribute('language', 'javascript');
  js.setAttribute('type', 'text/javascript');
  js.setAttribute('src', script_filename);
  html_doc.appendChild(js);
  return false;
}
// should do something more sensible for including extra javascript
// code (add a setting in js.ml, say)
//_include("extras.js")


function _chartest(r) {
  return function (c) {return r.test(c._c);};
}

var _isAlpha = _chartest(/[a-zA-Z]/);
var _isAlnum = _chartest(/[a-zA-Z0-9]/);
var _isLower = _chartest(/[a-z]/);
var _isUpper = _chartest(/[A-Z]/);
var _isDigit = _chartest(/[0-9]/);
var _isXDigit= _chartest(/[0-9a-fA-F]/);
var _isBlank = _chartest(/[ \t]/);

var isAlpha = LINKS.kify(_isAlpha);
var isAlnum = LINKS.kify(_isAlnum);
var isLower = LINKS.kify(_isLower);
var isUpper = LINKS.kify(_isUpper);
var isDigit = LINKS.kify(_isDigit);
var isXDigit= LINKS.kify(_isXDigit);
var isBlank = LINKS.kify(_isBlank);

function _chr(c) { return { _c: String.fromCharCode(c) } };
var chr = LINKS.kify(_chr);
function _ord(c) { return c._c.charCodeAt(0); }
var ord = LINKS.kify(_ord);

function _toUpper(c) {
  var c = c._c;
  DEBUG.assert(c.length == 1, "_toUpper only operates on single characters");
  return {_c:c.toUpperCase()};
}

function _toLower(c) {
  var c = c._c;
  DEBUG.assert(c.length == 1, "_toLower only operates on single characters");
  return {_c:c.toLowerCase()};
}

var toUpper = LINKS.kify(_toUpper);
var toLower = LINKS.kify(_toLower);

var _sqrt = Math.sqrt; var sqrt = LINKS.kify(_sqrt);
var _floor = Math.floor; var floor = LINKS.kify(_floor);
var _ceiling = Math.ceil; var ceiling = LINKS.kify(_ceiling);
var _tan = Math.tan; var tan = LINKS.kify(_tan);
var _sin = Math.sin; var sin = LINKS.kify(_sin);
var _cos = Math.cos; var cos = LINKS.kify(_cos);
var _log = Math.log; var log = LINKS.kify(_log);

function _makeCgiEnvironment() {
  var env = [];

  var i = 0;
  for(name in cgiEnv) {
    env[i] = {'1':name, '2':cgiEnv[name]};
    ++i;
  }

  cgiEnv = env;
}

function _environment() {
  return cgiEnv;
}
var environment = LINKS.kify(_environment);

function _redirect(url) {
  window.location = url;
}
var redirect = LINKS.kify(_redirect);

var QUIRKS = function () {
  return {
    // BEGIN code from quirksmode.org
    createCookie: function (name, value, days) {
      if (days) {
        var date = new Date(); // makes garbage
        date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
        var expires = "; expires=" + date.toGMTString();
      } else {
        var expires = "";
      }
      document.cookie = name + "=" + value + expires + "; path=/";
      return;
    },

    readCookie: function (name) {
      var nameEQ = name + "=";
      var ca = document.cookie.split(';');
      for (var i = 0; i < ca.length; i++) {
        var c = ca[i];
        while (c.charAt(0) == ' ') c = c.substring(1, c.length);
        if (c.indexOf(nameEQ) == 0) {
          return c.substring(nameEQ.length, c.length);
        }
      }
      return null;
    },

    eraseCookie : function (name) {
      return createCookie(name,"",-1);
    }
    // END code from quirksmode.org
  };
}();

function _setCookie(cookieName, value) {
  QUIRKS.createCookie(
    cookieName,
    value,
    10000
  );
  return CONSTANTS.UNIT;
}
var setCookie = LINKS.kify(_setCookie);

function _getCookie(cookieName) {
  return QUIRKS.readCookie(cookieName);
}
var getCookie = LINKS.kify(_getCookie);

function _random() {return Math.random();}
var random = LINKS.kify(_random);

//
//
// LINKS GAME LIBRARY
//
//

function _jsSetInterval(fn, interval) {
  window.setInterval(function () { return fn(_idy); }, interval);
  return;
}
function jsSetInterval(fn, interval, kappa) {
  _jsSetInterval(fn, interval);
  return _applyCont(kappa,{});
}

// NOTE: requestAnimationFrame can also take a callback that has one argument
function _jsRequestAnimationFrame(fn) {
  window.requestAnimationFrame(function () { fn(_idy) });// || window.webkitRequestAnimationFrame(function () { fn(_idy) });
  return;
}
function jsRequestAnimationFrame(fn, kappa) {
  _jsRequestAnimationFrame(fn);
  return _applyCont(kappa,{});
}

function _jsSave(ctx) {
  ctx.save();
  return;
}
function jsSave(ctx, kappa) {
  _jsSave(ctx);
  return _applyCont(kappa,{});
}

function _jsRestore(ctx) {
  ctx.restore();
  return;
}
function jsRestore(ctx, kappa) {
  _jsRestore(ctx);
  return _applyCont(kappa,{});
}

function _jsSetOnKeyDown(node, fn) {
  // note: node has to exist in the document, otherwise we get a JavaScript error
  node.addEventListener('keydown', function(e) { fn(e, _idy) }, true);
  return;
}
function jsSetOnKeyDown(node, fn, kappa) {
  _jsSetOnKeyDown(node, fn);
  return _applyCont(kappa,{});
}

function _jsSetOnEvent(node, event, fn, capture) {
  node.addEventListener(event, function(e) { fn(e, _idy) }, capture);
  return;
}
function jsSetOnEvent(node, event, fn, capture, kappa) {
  _jsSetOnEvent(node, event, fn, capture);
  return _applyCont(kappa,{});
}

function _jsSetOnLoad(fn) {
  window.addEventListener('load', function(e) { fn(e, _idy) }, false);
  return;
}
function jsSetOnLoad(fn, kappa) {
  _jsSetOnEvent(fn);
  return _applyCont(kappa,{});
}

var globalObjects = {};

function _jsSaveGlobalObject(name, obj) {
  globalObjects[name] = obj;
  return;
}
function jsSaveGlobalObject(name, obj, kappa) {
  _jsSaveGlobalObject(name, obj);
  return _applyCont(kappa,{});
}

function _jsLoadGlobalObject(name) {
  return globalObjects[name];
}
function jsLoadGlobalObject(name) {
  _jsSaveGlobalObject(name);
  return _applyCont(kappa,{});
}

function _jsGetContext2D(node) {
  return node.getContext('2d');
}
function jsGetContext2D(node, kappa) {
  _jsGetContext2D(node);
  return _applyCont(kappa,{});
}

function _jsFillText(ctx, text, x, y) {
  return ctx.fillText(text, x, y);
}
function jsFillText(ctx, text, x, y, kappa) {
  _jsFillText(ctx, text, x, y);
  return _applyCont(kappa,{});
}

function _jsCanvasFont(ctx, font) {
  ctx.font = font;
  return;
}
function jsCanvasFont(ctx, font, kappa) {
  _jsCanvasFont(ctx, font);
  return _applyCont(kappa,{});
}

function _jsDrawImage(ctx, node, x, y) {
  return ctx.drawImage(node, x, y);
}
function jsDrawImage(ctx, node, x, y, kappa) {
  _jsDrawImage(ctx, node, x, y);
  return _applyCont(kappa,{});
}

function _jsFillRect(ctx, x, y, width, height) {
  return ctx.fillRect(x, y, width, height);
}
function jsFillRect(ctx, x, y, width, height, kappa) {
  _jsFillRect(ctx, x, y, width, height);
  return _applyCont(kappa,{});
}

function _jsFillCircle(ctx, x, y, radius) {
  ctx.beginPath();
  ctx.arc(x, y, radius, 0, 2 * Math.PI, true);
  ctx.fill();
  return ctx.closePath();
}
function jsFillCircle(ctx, x, y, radius, kappa) {
  _jsFillCircle(ctx, x, y, radius);
  return _applyCont(kappa,{});
}

function _jsFill(ctx) {
  return ctx.fill();
}
var jsFill = _jsFill;

function _jsBeginPath(ctx) {
  return ctx.beginPath();
}
function jsBeginPath(ctx, kappa) {
  _jsBeginPath(ctx);
  return _applyCont(kappa,{});
}

function _jsClosePath(ctx) {
  return ctx.closePath();
}
var jsClosePath = _jsClosePath;

function _jsArc(ctx, x, y, radius, startAngle, endAngle, clockwise) {
  return ctx.arc(x, y, radius, startAngle, endAngle, clockwise);
}
var jsArc = _jsArc;

function _jsStrokeStyle(ctx, style) {
  ctx.strokeStyle = style;
  return;
}
function jsStrokeStyle(ctx, style, kappa) {
  _jsStrokeStyle(ctx, style);
  return _applyCont(kappa,{});
}

function _jsStroke(ctx) {
  return ctx.stroke();
}
function jsStroke(ctx, kappa) {
  _jsStroke(ctx);
  return _applyCont(kappa,{});
}

function _jsMoveTo(ctx, x, y) {
  return ctx.moveTo(x, y);
}
function jsMoveTo(ctx, x, y, kappa) {
  _jsMoveTo(ctx, x, y);
  return _applyCont(kappa,{});
}

function _jsLineTo(ctx, x, y) {
  return ctx.lineTo(x, y);
}
function jsLineTo(ctx, x, y, kappa) {
  _jsLineTo(ctx, x, y);
  return _applyCont(kappa,{});
}

function _jsLineWidth(ctx, width) {
  ctx.lineWidth = width;
  return;
}
var jsLineWidth = _jsLineWidth;

function _jsScale(ctx, x, y) {
  return ctx.scale(x, y);
}
function jsScale(ctx, x, y, kappa) {
  _jsScale(ctx, x, y);
  return _applyCont(kappa,{});
}

function _jsTranslate(ctx, x, y) {
  return ctx.translate(x, y);
}
function jsTranslate(ctx, x, y, kappa) {
  _jsTranslate(ctx, x, y);
  return _applyCont(kappa,{});
}

function _jsSetFillColor(ctx, color) {
  ctx.fillStyle = color;
  return;
}
function jsSetFillColor(ctx, color, kappa) {
  _jsSetFillColor(ctx, color);
  return _applyCont(kappa,{});
}

function _jsClearRect(ctx, x, y, width, height) {
  return ctx.clearRect(x, y, width, height);
}
function jsClearRect(ctx, x, y, width, height, kappa) {
  _jsClearRect(ctx, x, y, width, height);
  return _applyCont(kappa,{});
}

function _jsCanvasWidth(ctx) {
  return ctx.canvas.width;
}
var jsCanvasWidth = _jsCanvasWidth;

function _jsCanvasHeight(ctx) {
  return ctx.canvas.height;
}
var jsCanvasHeight = _jsCanvasHeight;

function _debugGetStats(what) {
  if (what == "yieldGranularity")
    return _yieldGranularity;
  else if (what == "yieldCount")
    return _yieldCount;
  else if (what == "yieldContCalls")
    return _yieldContCalls;
  else if (what == "yieldCalls")
    return _yieldCalls;
  else if (what == "callingTimeout")
    return _callingTimeout;
  else return undefined;
}
var debugGetStats = _debugGetStats;

function _jsSaveCanvas(canvas, node, mime) {
  var imageData = canvas.toDataURL(mime);//.replace("image/png", "image/octet-stream");;
  node.href = imageData; // window.location.
  return;
}
var jsSaveCanvas = _jsSaveCanvas;

function _debugChromiumGC() {
  if (window.gc) {
    return window.gc();
  } else {
    var msg = "Error. In order to use debugChromiumGC() invoke chromium like this: chromium --js-flags='--expose_gc'. Application terminated.";
    alert(msg);
    throw new Error(msg);
  }
}
var debugChromiumGC = _debugChromiumGC;

//
// EQUALITY
//

// "if you could create String Objects like new String("test"), re-use
// those and use those in the comparisons, that would be even faster,
// because the JS engine would only need to do a pointer-comparison"
function _stringEq(l, r) {
  return l === r;
}
var stringEq = _stringEq;

function _intEq(l, r) {
  return l === r;
}
var intEq = _intEq;

function _floatEq(l, r) {
  return l === r;
}
var floatEq = _floatEq;

function _floatNotEq(l, r) {
  return l !== r;
}
var floatNotEq = _floatNotEq;

function _objectEq(l, r) {
  return l._label === r._label && (l._value === r._value || (l._value === {} && r._value === {}));
}
var objectEq = _objectEq;

/**
 * Create XML rep from name, attributes and children
 *
 * @param {string} name
 * @param {{1: string, 2: string}[]} attrs
 * @param {any} children
 * @returns {[ string, string, Object, any ]}
 */
function _makeXml (name, attrs, children) {
  const attrObj = { };
  attrs.forEach(function (attr) {
    attrObj[attr['1']] = attr['2'];
    return;
  });

  return {
    type: 'ELEMENT',
    tagName: name,
    attrs: attrObj,
    children: children,
  };
}
const makeXml = LINKS.kify(_makeXml);

/**
 * Checks whether a string contains a character
 *
 * @param {string} str
 * @param {string} chr
 * @returns {boolean}
 */
function _strContains (str, chr) {
  return (str.indexOf(chr) > -1);
}

const strContains = LINKS.kify(_strContains);

/**
 * Converts Xml to its variant type equivalent
 *
 * @param {{ namespace?: string, tagName?: string, text?: string, attrs?: Object, children?: any[] }[]} xml
 * @returns {{ _label: string, _value: Object }[]}
 */
function _xmlToVariant (xml) {
  DEBUG.assert(TYPES.isArray(xml), 'xmlToVariant expects Xml (array of XmlItem)');
  return xml.map(_xmlItemToVariant);
}
const xmlToVariant = LINKS.kify(_xmlToVariant);

/**
 * Converts a variant version of Xml to an Xml representation
 *
 * @param {{ _label: string, _value: Object }[]} variants
 * @returns {{ namespace?: string, tagName?: string, text?: string, attrs?: Object, children?: any[] }[]}
 */
function _variantToXml (variants) {
  DEBUG.assert(TYPES.isArray(variants), 'variantToXml expects an array');
  return variants.map(_variantToXmlItem);
}
const variantToXml = LINKS.kify(_variantToXml);

/**
 * Converts a single XmlItem to its variant type equivalent
 *
 * @param {{ namespace?: string, tagName?: string, text?: string, attrs?: Object, children?: linked list }} xmlItem
 * @returns {{ _label: string, _value: Object }}
 */
function _xmlItemToVariant (xmlItem) {
  DEBUG.assert(
    xmlItem.type === 'ELEMENT' || xmlItem.type === 'TEXT',
    'Non-XmlItem passed to xmlItemToVariant'
  );
  if (xmlItem.type === 'ELEMENT') {
    DEBUG.assert(
      xmlItem.tagName && xmlItem.children && xmlItem.attrs,
      'Malformed element passed to xmlItemToVariant'
    );
    const attrs = LINKEDLIST.mapFromArray(Object.keys(xmlItem.attrs),function (name) {
      const splitIndex = name.indexOf(':');
      if (splitIndex > -1) {
        const ns = name.slice(0, splitIndex);
        const nm = name.slice(splitIndex + 1);
        return {
          '_label': 'NsAttr',
          '_value': {
            1: ns,
            2: nm,
            3: xmlItem.attrs[name],
          },
        };
      } else {
        return {
          '_label': 'Attr',
          '_value': {
            1: name,
            2: xmlItem.attrs[name],
          },
        };
      }
    });

    const children = LINKEDLIST.map(xmlItem.children,_xmlItemToVariant);

    if (xmlItem.namespace) {
      return {
        '_label': 'NsNode',
        '_value': {
          1: xmlItem.namespace,
          2: xmlItem.tagName,
          3: LINKEDLIST.append(attrs,children),
        }
      };
    } else {
      return {
        '_label': 'Node',
        '_value': {
          1: xmlItem.tagName,
          2: LINKEDLIST.append(attrs,children),
        }
      };
    }
  } else {
    return {
      '_label': 'Text',
      '_value': xmlItem.text || '',
    };
  }
}
const xmlItemToVariant = LINKS.kify(_xmlItemToVariant);

/**
 * Converts a variant version of an XmlItem to that XmlItem
 *
 * @param {{ _label: string, _value: Object }} variant
 * @returns {{ namespace?: string, tagName?: string, text?: string, attrs?: Object, children?: any[] }}
 */
function _variantToXmlItem (variant) {
  DEBUG.assert(TYPES.isObject(variant), 'variantToXmlItem expects an object');
  DEBUG.assert(variant['_label'], 'variantToXmlItem expects a variant type object');
  DEBUG.assert(variant['_value'], 'Malformed variant passed to variantToXmlItem');

  const attrs = {};

  var valueAsArray = LINKEDLIST.toArray(variant['_value']);
  if (variant['_label'] === 'Node' || variant['_label'] === 'NsNode') {
    valueAsArray[2]
      .filter(function (c) { return (c['_label'] === 'Attr'); })
      .forEach(function (a) { attrs[a['_value'][1]] = a['_value'][2]; return; });
      valueAsArray[2]
      .filter(function (c) { return (c['_label'] === 'NsAttr'); })
      .forEach(function (a) { attrs[a['_value'][1] + ':' + a['_value'][2]] = a['_value'][3]; return; });
  }

  switch (variant['_label']) {
    case 'Text':
      return {
        type: 'TEXT',
        text: valueAsArray[1],
      };
    case 'Node':
      return {
        type: 'ELEMENT',
        tagName: valueAsArray[1],
        attrs: attrs,
        children:
        LINKEDLIST.map(LINKEDLIST.filter(valueAsArray[2],function (c) { return (c['_label'] === 'Node' || c['_label'] === 'NsNode'); }),_variantToXmlItem)
      };
    case 'NsNode':
      return {
        type: 'ELEMENT',
        namespace: valueAsArray[1],
        tagName: valueAsArray[2],
        attrs: attrs,
        children:
        LINKEDLIST.map(LINKEDLIST.filter(valueAsArray[2],function (c) { return (c['_label'] === 'Node' || c['_label'] === 'NsNode');}),_variantToXmlItem)
      };
    case 'Attr':
    case 'NsAttr':
      throw new Error('Cannot construct detached attribute');
    default:
      throw new Error('Unknown Variant passed to variantToXmlItem');
  }
}
const variantToXmlItem = LINKS.kify(_variantToXmlItem);
