'use strict';
/* Magic constant -- used by the runtime to determine whether jslib has been loaded. */
window._JSLIB = true;

/* LIST MANIPULATING FUNCTIONS */
const _$List = (function(){
  return Object.freeze({
    Nil: null,
    isNil: function(n) {return n === null},
    Cons: function(x,xs) {_$Debug.assert(_$List.isList(xs), "Second argument must be linked list"); return {_head: x, _tail: xs}},
    isList: function(v) {
      return _$List.isNil(v) || v.hasOwnProperty("_head") && v.hasOwnProperty("_tail");
    },
    forEach: function(list, f) {
      while(!_$List.isNil(list)) {
        let cur = list._head;
        f(cur);
        list = list._tail;
      }
    },
    map: function(list, f) {
      let cur = _$List.singleton(null);
      const res = cur;
      while(!_$List.isNil(list)) {
        cur._tail = _$List.Cons(f(list._head),Nil);
        cur = cur.tail;
        list = list._tail;
      }
      return res._tail;
    },
    mapFromArray: function(array, f) {
      let cur = _$List.singleton(null);
      const res = cur;
      for(let i = 0; i < array.length; i++) {
        cur._tail = _$List.Cons(f(array[i]),Nil);
        cur = cur.tail;
        list = list._tail;
      }
      return res._tail;
    },
    toArray: function(list) {
      const res = [];
      let cur = list;
      while(!_$List.isNil(cur)) {
        res[res.length] = cur._head;
        cur = cur._tail;
      }
      return res;
    },
    snoc: function(xs, x) { return _$List.append(xs, _$List.singleton(x)); },
    singleton: function(head) { return _$List.Cons(head,Nil); },
    lsFromArray: function(arr) {
      let out = Nil;
      for (let i = arr.length - 1; i >= 0; --i) {
        out = _$List.Cons(arr[i], out);
      }
      return out;
    },
    take: function (n, xs) {
      let arr = [];
      while (!_$List.isNil(xs) && n > 0) {
        arr.push(xs._head);
        xs = xs._tail;
        --n;
      }
      return _$List.lsFromArray(arr);
    },
    drop: function(n, xs) {
      while (!_$List.isNil(xs) && n > 0) {
        xs = xs._tail;
        --n;
      }
      return xs;
    },
    length: function (xs) {
      let out = 0;
      while (!_$List.isNil(xs)) {
        out += 1;
        xs = xs._tail;
      }
      return out;
    },
    head: function(v) {
      _$Debug.assert(_$Types.isList(v), "Not a linked list");
      return _$List.isNil(v) ? _error('head') : v._head;
    },
    tail: function(v) {
      _$Debug.assert(_$Types.isList(v), "Not a linked list");
      return _$List.isNil(v) ? _error('tail') : v._tail;
    },
    last: function(xs) {
      if (_$List.isNil(xs)) {
        return _error('last');
      }
      let out = xs._head;
      while (!_$List.isNil(xs)) {
        out = xs._head;
        xs = xs._tail;
      }
      return out;
    },
    revAppend: function(xs, ys) {
      let out = ys;
      while (!_$List.isNil(xs)) {
        out = _$List.Cons(_$List.head(xs), out);
        xs = _$List.tail(xs);
      }
      return out;
    },
    append: function(xs, ys) {
      _$Debug.assert(_$Types.isList(xs) && _$Types.isList(ys), "Not linked lists");
      if (_$List.isNil(xs)) {
        return ys;
      }
      const origxs = xs;
      const rootEl = _Cons(xs._head, Nil);
      let curr = rootEl;
      xs = xs._tail;
      while (!_$List.isNil(xs)) {
        curr._tail = _Cons(xs._head, Nil);
        xs = xs._tail;
        curr = curr._tail;
      }
      curr._tail = ys;
      return rootEl;
    },
    at: function(xs, i) {
      let out = null;
      while (!_$List.isNil(xs) && i >= 0) {
        out = xs._head;
        xs = xs._tail;
        --i;
      }
      // FIXME should check i / index out of bounds here
      return out;
    },
    empty: function(xs) {
      return _$List.isNil(xs);
    },
    zip: function(xs, ys) {
      let arr = [];
      while (!_$List.isNil(xs) && !_$List.isNil(ys)) {
        arr.push({ "1": xs._head, "2": ys._head });
        xs = xs._tail;
        ys = ys._tail;
      }
      return _$List.lsFromArray(arr);
    },
    range:  function(a, b) {
      let lst = Nil;
      if (a <= b) {
        do {
          lst = _$List.Cons(b, lst);
        } while (b-- > a);
      }
      return lst;
    },
    replicate: function(n, item) {
      let out = Nil;
      while (n > 0) {
        out = _$List.Cons(item, out);
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
      return _$List.lsFromArray(arr);
    },
  // FIXME: max and min rely on '<' and '>', which
  // may not do the right thing for non-primitive types
  // (of course, we really want something like type classes
  // in order to be able to handle this kind of situation
  // more robustly)
    min: function(xs) {
      let currentMin = _$List.head(xs);
      while (!_$List.isNil(xs)) {
        if (xs._head < currentMin) currentMin = xs._head;
        xs = xs._tail;
      }
      return currentMin;
    },
    max: function(xs) {
      let currentMax = _$List.head(xs);
      while (!_$List.isNil(xs)) {
        if (xs._head > currentMax) currentMax = xs._head;
        xs = xs._tail;
      }
      return currentmax;
    },
    some: function(f, xs) {
      let ptr = xs;
      let found = false;
      while ( (found || _$List.isNil(ptr)) === false ) {
          found = f(ptr._head);
          ptr = ptr._tail;
      }
      return found;
    }
  });
}());

/* Core functions */

// Support events in firefox, which doesn't support window.event.
let event = undefined;

// Set up optimised setZeroTimeout
(function() {
  const timeouts = [];

  const messageName = "0TMsg";

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
const _$Types = {
  isUnit: function (val) { return (_$Types.isObject(val) && val.constructor === Object && Object.keys(val).length === 0); },
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
  isList: _$List.isList,
  isCharlist: function (val) { return (_$Types.isArray(val) && !val.some(function (c) { return !_$Types.isString(c) })); },
  isEvent: function (val) { return (val instanceof Event); },
  isTextnode: function (val) { return (_$Types.isXmlNode(val) && val.nodeType === document.TEXT_NODE); },
  isUndefined: function (val) { return (typeof val === 'undefined'); },
  isNull: function (val) { return (val === null); },
  isFunction: function (val) { return (typeof val === 'function'); },
  isDateTime: function (val) {
      return val.hasOwnProperty("_type") &&
        (val._type == "timestamp" && val.hasOwnProperty("_value")) ||
         val._type == "infinity" || val._type == "-infinity"
  },
  isChannel: function (val) { val.hasOwnProperty("_sessEP1") && val.hasOwnProperty("_sessEP2") },
  getType: function (val) {
    if (_$Types.isNumber (val))    return 'number';
    else if (_$Types.isString (val))      return 'string';
    else if (_$Types.isBoolean (val))     return 'boolean';
    else if (_$Types.isTextnode (val))    return 'textnode';
    else if (_$Types.isXmlnode (val))     return 'xmlnode';
    else if (_$Types.isArray (val))       return 'array';
    else if (_$Types.isList (val))  return 'linkedlist';
    else if (_$Types.isEvent (val))       return 'event';
    else if (_$Types.isUndefined (val))   return 'undefined';
    else if (_$Types.isNull (val))        return 'null';
    else if (typeof(val) == 'object' && 'constructor' in val) return new String(val.constructor); // makes garbage
    else return 'Unknown Type';
  }
};

const _$Debug = Object.freeze({
  debug: function (...msg) { (DEBUGGING && console.debug(...msg)); return; },
  show: function (any) { return (_$Types.isXmlNode(any)) ? _$Debug.xmldump(any) : _$Links.stringify(any); },
  xmldump: function (xml) { return (new XMLSerializer()).serializeToString(xml); },
  assert: function (condition, message) {
    // TODO(dhil): condition should be a thunk rather than a
    // computed boolean. Currently when debugging is disabled we
    // perform needless computation.
    if (DEBUGGING && !Boolean(condition)) {
      throw new Error(`Assertion failed: ${message}`);
    }
    return true;
  },
});

const _$Constants = Object.freeze({
  AJAX: Object.freeze({
    XHR_STATUS_IS_LOADED: 2,
    XHR_STATUS_IS_COMPLETE: 4,
    HTTP_STATUS_OK: 200,
  }),
  UNIT: Object.freeze({ }),
  NO_PROCESS: -99,
});

let _maxPid = 0;             // the highest process id allocated so far
let _mainPid = "MAIN";       // the process id of the main process
let _current_pid = _mainPid; // the process id of the currently active process
let _client_id = undefined;  // the unique ID given to this client
let _socket = undefined;     // reference to the websocket used to communicate with the server
let _handlingEvent = false;
let _aps = [];                // list of active client access points
let _returned_channels = {};  // channels that have been returned from an AP request.
                              // blocked PID |-> returned channel EP.
let _closureTable = {};

/* Functions for handling the websocket connection to the server. */
const _$Websocket = {
  buffer : [],
  is_connected : false,

  make_uri : function(path) {
    let loc = window.location;
    // Remove leading and trailing slashes if necessary
    if (path.length > 0) {
      let startIdx = (path.charAt(0) == '/') ? 1 : 0;
      let endIdx = (path.charAt(path.length - 1) == '/') ? path.length - 1 : path.length;
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
      _$Websocket.connect(_$Websocket.make_uri(state.ws_conn_url));
    } else {
      _$Debug.debug("No ws_conn_url in JSON state; not connecting to websocket server");
    }
    return;
  },

  connect: function (ws_uri) {
    _$Debug.assert(
      _client_id != undefined,
      "Trying to start websocket connection with undefined client ID"
    );

    _$Debug.debug("Connecting to websocket at address ", ws_uri);
    _socket = new WebSocket(ws_uri);

    /* Set up all of the event handlers */
    _socket.onopen = function (evt) {
      _$Websocket.onOpen(evt);
    };

    _socket.onclose = function (evt) {
      _$Websocket.onClose(evt);
    };

    _socket.onerror = function (evt) {
      _$Websocket.onError(evt);
    };

    _socket.onmessage = function (evt) {
      _$Websocket.onMessage(evt);
    }
    return;
  },

  onOpen: function (evt) {
    _$Debug.debug("Successfully opened websocket connection.");
    _$Websocket.is_connected = true;
    _$Websocket.drain_buffer();
    return;
  },

  onClose: function (evt) {
    // TODO: Fancier handling of errors here.
    // Should also make these prints as opposed to debugs?
    _$Debug.debug("Lost connection to the server. Please refresh the page.");
    _$Websocket.is_connected = false;
    return;
  },

  onError: function (evt) {
    _$Debug.debug("Error encountered when connecting to server. Please refresh the page.");
    _$Websocket.is_connected = false;
    return;
  },

  // TODO(dhil): I have a feeling that we may need to call
  // `resolveServerValue` on more "messages" below.
  onMessage : function(evt) {
    const js_parsed = JSON.parse(evt.data);
    _$Debug.debug("Received message ", js_parsed || evt.data);
    if (_$Types.isObject(js_parsed)) {
      if ("opcode" in js_parsed) {
        switch (js_parsed.opcode) {
        case "MESSAGE_DELIVERY": {
            _$Debug.debug("In message delivery case");
            let local_pid = js_parsed.dest_pid;
             // TODO(dhil): We should probably pass the parsed JSON
             // state here, but it is not available in this scope. A
             // refactoring of this code is warranted.
            let msg = _$Links.resolveServerValue(null, js_parsed.val);
            _$Links.deliverMessage(local_pid, msg);
            break;
        }
        case "AP_RESPONSE": {
            let blocked_pid = js_parsed.blocked_pid;
            let chan = js_parsed.chan;
            _returned_channels[blocked_pid] = chan;
            _buffers[chan._sessEP2] = [];
            _wakeup(blocked_pid);
            break;
        }
        case "SESSION_MESSAGE_DELIVERY": {
            let ep_id = js_parsed.ep_id;
            // TODO(dhil): We should probably pass the parsed JSON
            // state here, but it is not available in this scope. A
            // refactoring of this code is warranted.
            let message = _$Links.resolveServerValue(null, js_parsed.msg);
            let delegated_chans = js_parsed.deleg_chans;
            let requires_lost_messages = js_parsed.requires_lost_messages;
            migrateDelegatedSessions(delegated_chans, requires_lost_messages);
            deliverSessionMessage(ep_id, message);
            break;
        }
        case "GET_LOST_MESSAGES": {
            let remote_ep = js_parsed.carrier_ep;
            let ep_ids = js_parsed.ep_ids;
            handleGetLostMessages(remote_ep, ep_ids);
            break;
        }
        case "DELIVER_LOST_MESSAGES": {
            let ep_id = js_parsed.ep_id;
            let lost_message_table = js_parsed.lost_messages;
            handleLostMessages(ep_id, lost_message_table);
            break;
        }
        case "CHANNEL_CANCELLATION": {
            let notify_ep = js_parsed.notify_ep;
            let cancelled_ep = js_parsed.cancelled_ep;
            handleChannelCancellation(notify_ep, cancelled_ep);
            break;
        }
          default:
            _$Debug.debug("Unhandled message: ", evt.data);
            break;
        }
      }
    }
    return;
  },

  serialise_and_send: function (msg) {
    _socket.send(_$Links.stringify(msg));
    return;
  },

  drain_buffer: function () {
    while (_$Websocket.buffer.length > 0) {
      let msg = _$Websocket.buffer.pop();
      _$Debug.debug("Sending buffered message: ", msg);
      _$Websocket.serialise_and_send(msg);
    }
    return;
  },

  try_send: function (msg) {
    // If we're connected, send along. Otherwise add to the buffer.
    if (_$Websocket.is_connected) {
      _socket.send(_$Links.stringify(msg));
    } else {
      _$Debug.debug("No connection yet; buffering message ", msg);
      _$Websocket.buffer.unshift(msg);
    }
    return;
  },

  sendRemoteClientMessage: function (destClientId, destPid, msg) {
    _$Debug.debug(
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
    _$Websocket.try_send(to_send_json);
    return;
  },

  sendRemoteServerMessage: function (destServerPid, msg) {
    _$Debug.debug(
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
    _$Websocket.try_send(to_send_json);
    return;
  },

  sendRemoteAPRequest: function (current_pid, remote_apid) {
    _$Debug.debug("Requesting a connection from remote AP ", remote_apid);
    const to_send_json = {
      opcode: "SERVER_AP_REQUEST",
      blockedClientPid: current_pid,
      serverAPID: remote_apid,
    };
    _$Websocket.try_send(to_send_json);
    return;
  },

  sendRemoteAPAccept : function(current_pid, remote_apid) {
    _$Debug.debug("Accepting a connection from remote AP ", remote_apid);
    const to_send_json = {
      opcode: "SERVER_AP_ACCEPT",
      blockedClientPid: current_pid,
      serverAPID: remote_apid,
    };
    _$Websocket.try_send(to_send_json);
    return;
  },

  sendRemoteSessionMessage: function (channel, delegated_sessions, val) {
    let remote_ep = channel._sessEP1;
    _$Debug.debug("Sending value ", val, " to remote endpoint ", remote_ep);
    const to_send_json = {
      opcode: "REMOTE_SESSION_SEND",
      remoteEP: remote_ep,
      delegatedSessions: delegated_sessions,
      msg: val,
    };
    _$Websocket.try_send(to_send_json);
    return;
  },

  sendLostMessageResponse: function (ep_id, lost_message_table) {
    _$Debug.debug(
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
    _$Websocket.try_send(to_send_json);
  },

  sendRemoteCancellation : function(notify_ep, cancelled_ep) {
    _$Debug.debug("Sending remote cancellation. Notify ep ", notify_ep,
                ", cancelled ep ", cancelled_ep);
    const to_send_json =
      { opcode: "CHANNEL_CANCELLATION", notify_ep: notify_ep,
        cancelled_ep: cancelled_ep };
    _$Websocket.try_send(to_send_json);
  }
};


/** A package of functions used internally, not callable from Links code. */
const _$Links = new (function() {

  let _formkey = null;

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
   */
  function _remoteContinue (kappa, continuation, mailbox) {
    return _makeCont(function (res) {
      _$Debug.debug('Continuing at server with value', res, 'and continuation', continuation);
      const request = new XMLHttpRequest();
      const rootUrl = _removeCGIArgs(location.href);

      request.open('POST', rootUrl);

      request.onreadystatechange = remoteCallHandler(kappa, request);

      request.setRequestHeader(
        'Content-Type',
        'application/x-www-form-urlencoded'
      );

      request.pid = _current_pid;

      const resultJSON = _$Links.stringify(res);

      return request.send(
        "__continuation=" + continuation +
        "&__result=" + _$Links.base64encode(resultJSON) +
        "&__client_id=" + _$Links.base64encode(_client_id)
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
    _$Debug.debug("Setting client ID to ", clientId);
    _client_id = clientId;

    // Register event handlers
    for (let i in handlers) {
      handlers[i].eventHandlers = resolveServerValue(state, handlers[i].eventHandlers);
      _registerMobileEventHandlers(handlers[i].clientKey, handlers[i].eventHandlers);
    }

    // Resolve and create mobile access points
    // Needs to be done before processes, since processes may (will!) reference
    for (let i in aps) {
      let ap = aps[i];
      newWithID(ap);
    }

    // Resolve and spawn the mobile processes
    for (let i in processes) {
      processes[i] = resolveServerValue(state, processes[i]);
      let p = processes[i];
      _spawnWithMessages(p.pid, p.process, p.messages);
    }

    for (let i in buffers) {
      let entry = buffers[i];
      let id = entry.buf_id;
      let buf = entry.values;
      for (let val in buf) {
        // TODO(dhil): Resolution of buffers currently depends on the
        // fact that resolveServerValue mutates `val` internally.
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
    for (let i in handlers) {
      handlers[i].clientKey = _registerMobileKey(state, handlers[i].key);
    }

    // register event handlers
    for (let i in handlers) {
      handlers[i].eventHandlers = resolveServerValue(state, handlers[i].eventHandlers);
      _registerMobileEventHandlers(handlers[i].clientKey, handlers[i].eventHandlers);
    }

    // resolve and spawn the mobile processes
    for (let i in processes) {
      processes[i] = resolveServerValue(state, processes[i]);
      let p = processes[i];
      _spawnWithMessages(p.pid, p.process, p.messages);
    }
    return;
  }


   // Server value resolution
   class ServerValueResolutionYield extends Error {
     constructor(k) {
       super("resolveServerValueCPS yielding");
       this.k = k;
     }
   }

   let _resolveDepth = 0;
   const _resolveDepthMax = 1000;

  /**
   * Resolve function references in the object `obj` and return the
   * updated `obj`. The `obj` is specified as records { function:f,
   * environment:e }, where the environment is optional. If an
   * environment is specified, we assume that the function denoted by
   * f is actually a wrapper and that f(e) is the desired function.
   * Without an environment, f itself denotes the desired function, a
   * standard CPS compiled Links function. This is recursive, so each
   * object in `obj` also has its functions resolved.
   *
   * @param {any} state
   * @param {any} obj
   * @return {any} obj
   */
   // TODO(dhil): This function internally calls a CPS function inside
   // a trampoline in order to not cause a stack overflow on deeply
   // nested objects. Preferably, we should defunctionalise the CPS
   // function in order to obtain a iterative version that is more in
   // line with the rest of jslib. Another point worth making is that
   // the function mutates the given object and returns
   // it. Ultimately, I think we should return a new object.
   function resolveServerValue(state, obj) {
     let k = function() { return resolveServerValueCPS(state, obj, function(x) { return x; }) };
     while (true) {
       try {
         return k();
       } catch (e) {
         if (e instanceof ServerValueResolutionYield) {
           k = e.k;
         } else {
           throw e;
         }
       }
     }
   }

   function resolveServerValueCPS(state, obj, k) {
     // Applies a continuation, bounces on the trampoline if necessary.
     function applyK(k, arg) {
       _resolveDepth++;
       if (_resolveDepth >= _resolveDepthMax) {
         _resolveDepth = 0;
         throw new ServerValueResolutionYield(function() { return k(arg) });
       }
       return k(arg);
     }
     // Applies a function, bounces on the trampoline if necessary.
     function _yield(f) {
       _resolveDepth++;
       if (_resolveDepth >= _resolveDepthMax) {
         _resolveDepth = 0;
         throw new ServerValueResolutionYield(f);
       }
       return f();
     }

     if (obj._tag == null && obj.key != null) {
       obj.key = _lookupMobileKey(state, obj.key)
       return applyK(k, obj);
      }

      let tag = obj._tag;
      delete obj._tag;

      switch (tag) {
      case "Bool":
      case "Float":
      case "Int":
      case "String":
      case "Database":
      case "Table":
          return applyK(k, obj._value);
      case "XML":
          return _yield(function() {
              return resolveServerValueCPS(state, obj._value, function(xml) {
                  return applyK(k, xml);
              });
          });
      case "Text":
          return applyK(k, obj);
      case "NsNode":
          return _yield(function() {
              return resolveServerValueCPS(state, obj.children, function(children) {
                  obj.children = children;
                  return applyK(k, obj);
              });
          });
      case "DateTime":
          /* DateTimes are serialised via a UTC UNIX timestamp.
           * If not infinity / negative infinity, we need to replace the "value"
           * field with a JS date object. This will be turned back into a timestamp
           * before sending back to the server.
           */
          if (obj._type == "timestamp") {
            // Annoyingly, the date constructor has to be in local
            // time (and is converted to UTC under the hood).
            const tzOffset = new Date().getTimezoneOffset() * -1 * 60 * 1000;
            obj._value = new Date(obj._value * 1000 + tzOffset);
          }
          return applyK(k, obj);
      case "Char":
      case "ClientDomRef":
      case "ClientAccessPoint":
      case "ServerAccessPoint":
      case "ClientPid":
      case "ServerPid":
      case "SessionChannel":
      case "ServerSpawnLoc":
      case "ClientSpawnLoc":
          return applyK(k, obj);
      case "List":
          if (obj._head == null && obj._tail == null) return applyK(k, _$List.Nil);
          return _yield(function() {
              return resolveServerValueCPS(state, obj._head, function(head) {
                  obj._head = head;
                  return _yield(function () {
                      return resolveServerValueCPS(state, obj._tail, function(tail) {
                          obj._tail = tail;
                          return applyK(k, obj);
                      })
                  });
              });
          });
      case "Record": {
          let chain0 = function(obj) {
              return applyK(k, obj);
          };
          function resolveComponent(i, k) {
              return function(obj) {
                  return _yield(function() {
                      return resolveServerValueCPS(state, obj[i], function(ith) {
                          obj[i] = ith;
                          return applyK(k, obj);
                      });
                  });
              };
          };

          // resolveComponent builds the continuation in bottom-up
          // fashion, so in order to resolve the object in-order at
          // runtime construct the continuation from the keys in reverse
          // order.
          let keys = [];
          for (let key in obj) {
              keys.push(key);
          }
          let g = function(obj) {
              return applyK(k, obj);
          };
          while (keys.length > 0) {
              g = resolveComponent(keys.pop(), g);
          }
          return g(obj);
      }
      case "Variant":
          return _yield(function () {
              return resolveServerValueCPS(state, obj._value, function(value) {
                  return applyK(k, {'_label': obj._label, '_value': value});
              });
          });
      case "FunctionPtr":
      case "ClientFunction":
          if (obj.environment == null) {
              let f = eval(obj.func);
              f.location = obj.location;
              f.func = obj.func;
              return applyK(k, f);
          } else {
              return _yield(function() {
                  return resolveServerValueCPS(state, obj.environment, function(env) {
                      let f = partialApply(eval(obj.func),env);
                      f.location = obj.location;
                      f.func = obj.func;
                      return applyK(k, f);
                  });
              });
          }
      case "Process": {
          function resolveMessage(i, k) {
              return function(obj) {
                  return _yield(function() {
                      return resolveServerValueCPS(state, obj.messages[i], function(ith) {
                          obj.messages[i] = ith;
                          return applyK(k, obj);
                      });
                  });
              };
          }

          // resolveMessage builds the continuation in bottom-up
          // fashion, so in order to resolve the object in-order at
          // runtime construct the continuation from the indices in
          // reverse order.
          let indices = [];
          for (let index in obj.messages) {
              indices.push(index);
          }
          let g = function(obj) {
              return applyK(k, obj);
          };
          while (indices.length > 0) {
              g = resolveMessage(indices.pop(), g);
          }

          return _yield(function() {
              return resolveServerValueCPS(state, obj.process, function(process) {
                  obj.process = process;
                  obj.pid     = obj.pid;
                  return g(obj);
              });
          });
      }
      default:
          throw "Unrecognised tag " + tag + " for object \"" + JSON.stringify(obj) + "\"";
      }
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
   * NOTE: letiables defined within this function could shadow
   * the Links function we're trying to execute. Hence all local
   * vars are prefixed with underscore. Beware also of package variables
   * above shadowing.
   *
   * @param {any} kappa
   * @param {any} callPackage
   */
  function _invokeClientCall (kappa, callPackage) {
    _$Debug.debug('Invoking client call to ', callPackage.__name);
    _$Debug.debug('arguments: ', callPackage.__args);
    _$Debug.debug('arguments: ', callPackage.__args);

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
      if (request.readyState == _$Constants.AJAX.XHR_STATUS_IS_COMPLETE && !request.finished) {
        _current_pid = _$Constants.NO_PROCESS;

        // The 'finished' field guards against the callback being called more
        // than once on the same request object, an anomaly observed by EEKC.
        request.finished = true;

        _$Debug.debug("Server response: ", _$Links.base64decode(request.responseText));

        const serverResponse = _$Links.parseB64Safe(request.responseText);

        if (!serverResponse) {
          throw new Error("Fatal error: nonsense returned from server.");
        }

        // Any state that we need for resolving values
        // (currently just a mapping between server and client pids)
        const state = { mobileKeys: { } };

        if (serverResponse.content.hasOwnProperty("error")) {
          const gripe = serverResponse.content.error;
          throw new Error("Fatal error: call to server returned an error. Details: " + gripe);
        }

        resolveMobileState(
          state,
          serverResponse.content.state.processes,
          serverResponse.content.state.handlers
        );

        // Check whether we are bouncing the trampoline with a client call
        // or continuing with a final result.
        // TBD: Would be more elegant to use JS constructors instead of
        // using a signal member like __continuation.
        const callPackageOrServerValue = serverResponse.content.value;

        if ((callPackageOrServerValue instanceof Object) && ('__continuation' in callPackageOrServerValue)) {
          // callPackageOrServerValue is a call package.
          // Bouncing the trampoline

          _$Debug.debug("Client function name, before evaluation, is ", callPackageOrServerValue.__name);

          _current_pid = request.pid;
          // TODO(dhil): The call package should probably be tagged
          // such that we can supply it to resolveServerValue.
          // Resolve arguments.
          let args = callPackageOrServerValue.__args;
          for (let i = 0; i < args.length; i++) {
            args[i] = resolveServerValue(state, args[i]);
          }
          // TODO(dhil): The following is redundant at the moment, but
          // this is subject to change if/when we make
          // resolveServerValue purely functional.
          callPackageOrServerValue.__args = args;
          return _invokeClientCall(kappa, callPackageOrServerValue);
        } else {
          // callPackageOrServerValue is a server value
          const serverValue = resolveServerValue(state, callPackageOrServerValue);
          _$Debug.debug("Client continuing after remote server call, value ", serverValue);
          // it's the final result: return it.
          _$Debug.debug("Server response decoded: ", serverValue);
          return _applyCont(kappa,serverValue);
        }
      }
      return;
    }
  }

  let nextFuncID = 0;

  // Used to emulate DOMRef serialisation
  let domRefId = 0;
  let domRefs = { };

  function storeDomRef(ref) {
    const id = domRefId++;
    domRefs[id] = ref;
    return { _domRefKey : id };
  }


  function replacer (key, value) {
    _$Debug.debug("In replacer with key: ", key);
    _$Debug.debug("typeof value: ", typeof value);
    _$Debug.debug("value: ", value);

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
    } else if (_$Types.isDateTime(value)) {
      // If date is set, then we need to serialise the UTC timestamp
      if (value._type == "timestamp") {
        const ts = Math.floor(value._value.getTime() / 1000);
        return { _type: "timestamp", _value: ts };
      } else {
        return value;
      }
    }
    return value;
  }

  const _$Links = {
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
    gt: undefined,
    lt: undefined,
    gte: undefined,
    lte: undefined,
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
      _$Debug.assert(
        _$Types.isList(xmlForest),
        '_$Links.XmlToDomNodes expected a linked list, but got ',
        xmlForest
      );
      let domNodeArray = [];
      _$List.forEach(xmlForest,function(xmlNode){
        domNodeArray.push(_$Links.singleXmlToDomNodes(xmlNode))
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
      let children = Nil;
      _$List.forEach(body,function(e) {
        if(_$Types.isList(e)) {
          children = _$List.append(children,e);
        } else {
          children = _$List.snoc(children,e);
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
      _$List.forEach(labels, function(l) { return ls.add(l); });

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
      return _$Debug.debug(pid, ' now has ', _mailboxes[pid].length, ' message(s)');
    },

    // Remote calls

    remoteCall: function (kappa) {
      return function (name, env, args) {
        _$Debug.debug("Making remote call to: ", name);
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
        const argsJSON = _$Links.stringify(args);

        // TODO: get rid of env - this should be handled by closure conversion

        if (!env) {
          env = { };
        }

        const envJSON = _$Links.stringify(env);

        // request.funcs = _compose(argsJSON.funcs, envJSON.funcs);

        let argString =
          "__name=" + _$Links.base64encode(name) +
          "&__args=" + _$Links.base64encode(argsJSON) +
          "&__env=" + _$Links.base64encode(envJSON) +
          "&__client_id=" + _$Links.base64encode(_client_id)

        for (let i = 0; i < cgiEnv.length; ++i) {
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
      let containingForm = null;

      // find the containing form
      for (let i = 0; i < forms.length; ++i) {
        const key = forms[i].getAttribute('key');
        if (key === formkey) {
          containingForm = forms[i];
          break;
        }
      }

      _$Debug.assert(Boolean(containingForm), "Form does not exist!")

      // find the input value
      const xs = document.getElementsByName(name);
      for(let i = 0; i < xs.length; ++i) {
        const node = xs[i];
        while(node) {
          if(node == containingForm) {
              return xs[i].value;
          }
          node = node.parentNode;
        }
      }

      return _$Debug.assert(false, "Form element with name '" + name +"' does not exist!");
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
      let down = 1;
      let right = 2;
      let up = 3;

      f(root);
      if(!root.firstChild)
        return;
      let node = root.firstChild;
      let direction = down;
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
              let temp = handlers[event];
              temp(e);
              e.stopPropagation();
              return e.preventDefault();
            }, false);
          });
        }
      }

      return _$Links.appDom(node, activate);
    },

    stringify: function (v) {
      _$Debug.debug("stringifying: ", v);
      const t = JSON.stringify(v, replacer);
      _$Debug.debug("stringified: ", t);
      if (typeof t === 'string') {
        return t;
      }
      throw new Error("Internal error: unable to JSONize " + v);
    },

    parseB64: function (text) { return { content: JSON.parse(_$Links.base64decode(text)) }; },
    parseB64Safe: function (text) { return _$Links.parseB64(text.replace('\n', '')); },
  };


  _$Links.singleXmlToDomNodes = function (xmlObj) {
    _$Debug.assert(
      _isXmlItem(xmlObj),
      '_$Links.singleXmlToDomNodes expected a XmlItem, but got ' + xmlObj
    );

    const type = xmlObj.type;
    switch (type) {
      case 'ELEMENT':
        const tagName = xmlObj.tagName;
        const attributes = xmlObj.attrs;
        const children = xmlObj.children !== undefined ? _$List.toArray(xmlObj.children) : [ ];
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
          _$List.forEach(xmlObj.children,function(c) {
            node.appendChild(_$Links.singleXmlToDomNodes(c));
          });
        }

        return node;
      case 'TEXT':
        return document.createTextNode(xmlObj.text || '');
      default:
        return null;
    }
  };

  _$Links.eq = function(l, r) {
    // It is possible to compare two function objects in JavaScript,
    // so we need to explicitly check whether l or r are functions in
    // order respects the equality semantics of the server-side.
    if (typeof(l) === 'function' || typeof(r) === 'function')
        throw ("Comparing " + l + " with " + r + " which either does not make sense or isn't implemented.");

    // Fast path.
    if (l === r) return true;

    // Bools, strings, ints, and floats are implemented as native
    // JavaScript bools, strings, ints, and floats respectively and
    // their equality ditto. So if l and r were identical the fast
    // path would have been triggered.

    // TODO(dhil): Potential micro-optimisation: if we assume the
    // program is type-correct, then we need only recompute the type
    // of either `l` or `r`.
    if (_$Types.isNumber(l) && _$Types.isNumber(r)
        || _$Types.isString(l) && _$Types.isString(r)
        || _$Types.isBoolean(l) && _$Types.isBoolean(r)) return false;

    // Object equality is tricky. The fast path only covers
    // referential equal objects. However, the equality semantics for
    // records, variants, etc is structural. So if the fast path
    // wasn't triggered then we need to iterate through the objects. A
    // potential performance optimisation could be to limit the depth
    // of equality. TODO(dhil): This implementation uses naive
    // recursion and will stack overflow on large objects.
    if (_$Types.isList(l) && _$Types.isList(r)) {
        while(!_$List.isNil(l) && !_$List.isNil(r)) {
            if (!_$Links.eq(_$List.head(l), _$List.head(r))) return false;
            l = _$List.tail(l);
            r = _$List.tail(r);
        }
        return _$List.isNil(l) && _$List.isNil(r);
    }

    if (_$Types.isUnit(l) && _$Types.isUnit(r)) return true;

    if (_$Types.isArray(l) && _$Types.isArray(r)) {
      if (l.length !== r.length) return false;

      for (let i = 0; i < l.length; ++i)
        if (!_$Links.eq(l[i], r[i])) return false;

      return true;
    }

    if(typeof(l) === 'object' && l !== null &&
       typeof(r) === 'object' && r !== null) {
      if (l.constructor !== r.constructor) return false;

      // DODGEYNESS:
      //   - it isn't clear that structural equality is always the same as
      //   referential equality for DOM nodes

      for (let p in l)
        if (!_$Links.eq(l[p], r[p])) return false;

      for(let p in r)
        if (!_$Links.eq(l[p], r[p])) return false;

      return true;
    }

    throw ("Comparing " + l + " with " + r + " which either does not make sense or isn't implemented.");
  };

  _$Links.gt = function(l, r) {
    if (typeof(l) === 'function' || typeof(r) === 'function')
        throw ("Comparing " + l + " with " + r + " which either does not make sense or isn't implemented.");

    // Fast path.
    // Some care is required as JavaScript has a wacky relational
    // semantics on objects.
    if (typeof l !== 'object' && typeof r !== 'object' && l > r) return true;

    if (_$Types.isNumber(l) && _$Types.isNumber(r)
        || _$Types.isString(l) && _$Types.isString(r)
        || _$Types.isBoolean(l) && _$Types.isBoolean(r)) return false;

    if (_$Types.isList(l) && _$Types.isList(r)) {
        while(!_$List.isNil(l) && !_$List.isNil(r)) {
            if (!_$Links.gt(_$List.head(l), _$List.head(r))) return false;
            l = _$List.tail(l);
            r = _$List.tail(r);
        }
        return !_$List.isNil(l) && _$List.isNil(r);
    }

    if (_$Types.isUnit(l) && _$Types.isUnit(r)) return false;

    if (_$Types.isArray(l) && _$Types.isArray(r)) {
        let i = 0;
        while (i < l.length && i < r.length) {
            if (!_$Links.gt(l[i], r[i])) return false;
            i++;
        }
        return l.length > r.length;
    }

    if (typeof(l) === 'object' && l !== null &&
        typeof(r) === 'object' && r !== null) {

      for (let p in l)
        if (!_$Links.gt(l[p], r[p])) return false;

      for(let p in r)
        if (!_$Links.gt(l[p], r[p])) return false;

      return true;
    }

    throw ("Comparing " + l + " with " + r + " which either does not make sense or isn't implemented.");
  };

  _$Links.lt = function(l, r) {
    if (typeof(l) === 'function' || typeof(r) === 'function')
        throw ("Comparing " + l + " with " + r + " which either does not make sense or isn't implemented.");

    // Fast path.
    if (typeof l !== 'object' && typeof r !== 'object' && l < r) return true;

    if (_$Types.isNumber(l) && _$Types.isNumber(r)
        || _$Types.isString(l) && _$Types.isString(r)
        || _$Types.isBoolean(l) && _$Types.isBoolean(r)) return false;

    if (_$Types.isList(l) && _$Types.isList(r)) {
        while(!_$List.isNil(l) && !_$List.isNil(r)) {
            if (!_$Links.lt(_$List.head(l), _$List.head(r))) return false;
            l = _$List.tail(l);
            r = _$List.tail(r);
        }
        return _$List.isNil(l) && !_$List.isNil(r);
    }

    if (_$Types.isUnit(l) && _$Types.isUnit(r)) return false;

    if (_$Types.isArray(l) && _$Types.isArray(r)) {
        let i = 0;
        while (i < l.length && i < r.length) {
            if (!_$Links.gt(l[i], r[i])) return false;
            i++;
        }
        return l.length < r.length;
    }

    if (typeof(l) === 'object' && l !== null &&
        typeof(r) === 'object' && r !== null) {

      for (let p in l)
        if (!_$Links.lt(l[p], r[p])) return false;

      for(let p in r)
        if (!_$Links.lt(l[p], r[p])) return false;

      return true;
    }

    throw ("Comparing " + l + " with " + r + " which either does not make sense or isn't implemented.");
  };

  _$Links.lte = function(l, r) {
    if (typeof(l) === 'function' || typeof(r) === 'function')
        throw ("Comparing " + l + " with " + r + " which either does not make sense or isn't implemented.");

    // Fast path.
    if (typeof l !== 'object' && typeof r !== 'object' && l <= r) return true;

    if (_$Types.isNumber(l) && _$Types.isNumber(r)
        || _$Types.isString(l) && _$Types.isString(r)
        || _$Types.isBoolean(l) && _$Types.isBoolean(r)) return false;

    if (_$Types.isList(l) && _$Types.isList(r)) {
        while(!_$List.isNil(l) && !_$List.isNil(r)) {
            if (!_$Links.lte(_$List.head(l), _$List.head(r))) return false;
            l = _$List.tail(l);
            r = _$List.tail(r);
        }
        return _$List.isNil(l) && _$List.isNil(r)
            || _$List.isNil(l) && !_$List.isNil(r);
    }

    if (_$Types.isUnit(l) && _$Types.isUnit(r)) return true;

    if (_$Types.isArray(l) && _$Types.isArray(r)) {
        let i = 0;
        while (i < l.length && i < r.length) {
            if (!_$Links.lte(l[i], r[i])) return false;
            i++;
        }
        return l.length === r.length || l.length < r.length;
    }

    if (typeof(l) === 'object' && l !== null &&
        typeof(r) === 'object' && r !== null) {

      for (let p in l)
        if (!_$Links.lte(l[p], r[p])) return false;

      for(let p in r)
        if (!_$Links.lte(l[p], r[p])) return false;

      return true;
    }

    throw ("Comparing " + l + " with " + r + " which either does not make sense or isn't implemented.");
  };

  _$Links.gte = function(l, r) {
    if (typeof(l) === 'function' || typeof(r) === 'function')
        throw ("Comparing " + l + " with " + r + " which either does not make sense or isn't implemented.");

    // Fast path.
    if (typeof l !== 'object' && typeof r !== 'object' && l >= r) return true;

    if (_$Types.isNumber(l) && _$Types.isNumber(r)
        || _$Types.isString(l) && _$Types.isString(r)
        || _$Types.isBoolean(l) && _$Types.isBoolean(r)) return false;

    if (_$Types.isList(l) && _$Types.isList(r)) {
        while(!_$List.isNil(l) && !_$List.isNil(r)) {
            if (!_$Links.gte(_$List.head(l), _$List.head(r))) return false;
            l = _$List.tail(l);
            r = _$List.tail(r);
        }
        return _$List.isNil(l) && _$List.isNil(r)
            || !_$List.isNil(l) && _$List.isNil(r);
    }

    if (_$Types.isUnit(l) && _$Types.isUnit(r)) return true;

    if (_$Types.isArray(l) && _$Types.isArray(r)) {
        let i = 0;
        while (i < l.length && i < r.length) {
            if (!_$Links.gte(l[i], r[i])) return false;
            i++;
        }
        return l.length === r.length || l.length > r.length;
    }

    if (typeof(l) === 'object' && l !== null &&
        typeof(r) === 'object' && r !== null) {

      for (let p in l)
        if (!_$Links.gte(l[p], r[p])) return false;

      for(let p in r)
        if (!_$Links.gte(l[p], r[p])) return false;

      return true;
    }

    throw ("Comparing " + l + " with " + r + " which either does not make sense or isn't implemented.");
  };

  _$Links.resolveServerValue = resolveServerValue;

  return _$Links;
})();

const _debug = _$Debug.debug;
const debug = _$Links.kify(_$Debug.debug);

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
      const regexes = _$List.toArray(expr._value);
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
const tilde = _$Links.kify(_tilde);

const _intToString = function (x) { return String(x); }
const _stringToInt = function (x) { return parseInt(x); }
const _intToFloat = Number;
const _floatToInt = Math.floor;
const _floatToString = function (x) { return String(x); }
const _stringToFloat = function (x) { return parseFloat(x); }

const intToString = _$Links.kify(_intToString);
const stringToInt = _$Links.kify(_stringToInt);
const intToFloat = _$Links.kify(_intToFloat);
const floatToInt = _$Links.kify(_floatToInt);
const floatToString = _$Links.kify(_floatToString);
const stringToFloat = _$Links.kify(_stringToFloat);

function _not(x) { return !x; } // should be inlined
const _Concat = _$List.append;
const _empty = _$List.empty;
const _Cons = _$List.Cons;
const _hd = _$List.head;
const _tl = _$List.tail;
const _$$hd = _$List.head;
const _$$tl = _$List.tail;
const _length = _$List.length;
const _take = _$List.take;
const _drop = _$List.drop;
const _max = _$List.max;
const _min = _$List.min;
const _singleton = _$List.singleton;



const not    = _$Links.kify(_not);
const Nil    = _$List.Nil;
const Cons   = _$Links.kify(_$List.Cons);
const Concat = _$Links.kify(_$List.Concat);
const empty  = _$Links.kify(_$List.empty);
const hd     = _$Links.kify(_$List.head);
const tl     = _$Links.kify(_$List.tail);
const length = _$Links.kify(_$List.length);
const take   = _$Links.kify(_$List.take);
const drop   = _$Links.kify(_$List.drop);
const max    = _$Links.kify(_$List.max);
const min    = _$Links.kify(_$List.min);



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
  let cs = Nil;
  for (let i = s.length-1; i >= 0; --i) {
    cs = _$List.Cons({ _c: s.charAt(i) },cs)
  }
  return cs;
}

function _implode(cs) {
  let s = "";
  _$List.forEach(cs,function(e) {
    s += e._c;
  });
  return s;
}

const charAt  = _$Links.kify(_charAt);
const strsub  = _$Links.kify(_strsub);
const strlen  = _$Links.kify(_strlen);
const explode = _$Links.kify(_explode);
const implode = _$Links.kify(_implode);

function _debugObj(obj) {
  if (obj == undefined) {
    _$Debug.debug(obj, " : undefined");
  } else {
    _$Debug.debug(obj, " : ", typeof(obj), (typeof(obj) == 'object' ? obj.constructor : ''));
  }
  return [];
}
let debugObj = _$Links.kify(_debugObj);

function _dump(obj) {
  console.info(obj);
}

let dump = _$Links.kify(_dump);

function _show(obj) {
  if (_$Types.isDateTime(obj)) {
      return showLocalDate(obj);
  } else {
      return JSON.stringify(obj, null, 2);
  }
}

let show = _$Links.kify(_show);

function _negate(x) { return -x; }
let negate = _$Links.kify(_negate);

let _negatef = _negate;
let negatef = negate;

function _error(msg) {
  console.error(msg);
  throw new Error("Error: " + msg);
}

let error = _$Links.kify(_error);

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
  let parent = beforeNode.parentNode;
  let nodes = _$Links.XmlToDomNodes(insertXml);
  for (let i=0; i < nodes.length; i++) {
    parent.insertBefore(nodes[i], beforeNode);
    _$Links.activateHandlers(nodes[i]);
  }
  return _$Constants.UNIT;
}

//appendChildXml : xml -> domRef -> ()
function _appendChildren(appendXml, parentNode) {
  let nodes = _$Links.XmlToDomNodes(appendXml);
  for (let i = 0; i < nodes.length; i++) {
    parentNode.appendChild(nodes[i]);
    _$Links.activateHandlers(nodes[i]);
  }
  return _$Constants.UNIT;
}

//removeNode : domRef -> ()
function _removeNode(nodeRef) {
  if (nodeRef.parentNode) {
    nodeRef.parentNode.removeChild(nodeRef);
  } else {
    throw new Error("Cannot remove DOM root node");
  }

  return _$Constants.UNIT;
}

function _cloneNode(nodeRef, deep) {
  return nodeRef.cloneNoe(deep);
}

//replaceNode : (xml, domRef) -> ()
function _replaceNode(withXml, replaceNode) {
  _insertBefore(withXml, replaceNode);
  _removeNode(replaceNode);
  return _$Constants.UNIT;
}

//replaceDocument : xml -> ()
let replaceDocument = _$Links.kify(_replaceDocument);


// WARNING: insertBeforeRef MOVES a DOM node
//insertBeforeRef : domRef -> domRef -> ()
function _domInsertBeforeRef(insertNode, beforeNode) {
  let parent = beforeNode.parentNode;
  parent.insertBefore(insertNode, beforeNode)
  _$Links.activateHandlers(insertNode);
  return _$Constants.UNIT;
}

//appendChildRef : domRef -> domRef -> ()
function _domAppendChildRef(appendNode, parentNode) {
  parentNode.appendChild(appendNode);
  _$Links.activateHandlers(appendNode);
  return _$Constants.UNIT;
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

let insertBefore = _$Links.kify(_insertBefore);
let appendChildren = _$Links.kify(_appendChildren);
let replaceNode = _$Links.kify(_replaceNode);

let domInsertBeforeRef = _$Links.kify(_domInsertBeforeRef);
let domAppendChildRef = _$Links.kify(_domAppendChildRef);
let removeNode = _$Links.kify(_removeNode);
let cloneNode = _$Links.kify(_cloneNode);

let getDocumentNode = _$Links.kify(_getDocumentNode);
let getNodeById = _$Links.kify(_getNodeById);
let isNull = _$Links.kify(_isNull);


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
  const element = document.getElementById(id);
  _$Debug.assert(element != null, "invalid input node (id " + id + ")");
  _$Debug.assert(element.value != undefined, "invalid input value in id " + id)
  if ((element.type !== "radio" && element.type !== "checkbox") || element.checked) {
    return element.value;
  } else {
    return "";
  }
}

const getInputValue = _$Links.kify(_getInputValue);

// getRadioGroupValue : [String] -> String
function _getRadioGroupValue(ids) {
  let ptr = ids;
  while ( _$List.isNil(ptr) === false ) {
    const element = document.getElementById(ptr._head);
    if (element.checked) {
      return element.value;
    }
    ptr = ptr._tail;
  }
  return "";
}

let getRadioGroupValue = _$Links.kify(_getRadioGroupValue);

//getValue : domRef -> xml
//    NOTE: this is recursive.
function _getValue(nodeRef) {
    function toDomNode(nodeRef) {
        if (nodeRef.nodeType == document.TEXT_NODE) {
            return {
                type: 'TEXT',
                text: _nodeTextContent(nodeRef)
            };
        } else if (nodeRef.nodeType == document.ELEMENT_NODE ) {
            let children = Nil;
            const numChildren = nodeRef.childNodes.length;
            for (let i = numChildren - 1; 0 <= i; i--) {
                children = _$List.Cons(toDomNode(nodeRef.childNodes[i]), children);
            }

            let attrs = { };
            for (let i = 0; i < nodeRef.attributes.length; i++) {
                attrs[nodeRef.attributes[i].name] = nodeRef.attributes[i].value;
            }

            _$Debug.assert(
                !_$List.some(function (e) { return e.type !== 'ELEMENT' && e.type !== 'TEXT'; }, children),
                'Invalid children constructed in _getValue'
            );

            return {
                type: 'ELEMENT',
                tagName: nodeRef.tagName,
                attrs: attrs,
                children: children,
            };
        } else {
            throw new Error("Unknown node type " + nodeRef.nodeType + " in GetXml");
        }
    }
    return _$List.Cons(toDomNode(nodeRef), _$List.Nil);
}
const getValue = _$Links.kify(_getValue);

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
  return _$Constants.UNIT;
}

function _domSetAttributeFromRef(nodeRef, attr, value) {
  nodeRef.setAttribute(attr, value);
  return _$Constants.UNIT;
}

function _domRemoveAttributeFromRef(nodeRef, attr) {
  nodeRef.removeAttribute(attr);
  return _$Constants.UNIT;
}

function _domSetStyleAttrFromRef(nodeRef, attr, value) {
  nodeRef.style[attr] = value;
  return _$Constants.UNIT;
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
  return _$List.lsFromArray(nodeRef.children);
}

function _domGetNodeValueFromRef(node) {
  return node.value;
}

function _domSetAnchor(anchorRef) {
  window.location.hash = anchorRef;
  return _$Constants.UNIT;
}

function _domGetAnchor() {
  if (window.location.hash) {
    return window.location.hash;
  } else {
    return "";
  }
}

const domGetNodeValueFromRef = _$Links.kify(_domGetNodeValueFromRef)
const domGetChildrenFromRef = _$Links.kify(_domGetChildrenFromRef);
const domGetTagNameFromRef = _$Links.kify(_domGetTagNameFromRef);
const domHasAttribute = _$Links.kify(_domHasAttribute);
const domGetAttributeFromRef = _$Links.kify(_domGetAttributeFromRef);
const domSetAttributeFromRef = _$Links.kify(_domSetAttributeFromRef);
const domRemoveAttributeFromRef = _$Links.kify(_domRemoveAttributeFromRef);
const domSetStyleAttrFromRef = _$Links.kify(_domSetStyleAttrFromRef);
const domGetStyleAttrFromRef = _$Links.kify(_domGetStyleAttrFromRef);
const domGetPropertyFromRef = _$Links.kify(_domGetPropertyFromRef);
const domSetPropertyFromRef = _$Links.kify(_domSetPropertyFromRef);
const domSetAnchor = _$Links.kify(_domSetAnchor);
const domGetAnchor = _$Links.kify(_domGetAnchor);

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
const parentNode = _$Links.kify(_parentNode);
const firstChild = _$Links.kify(_firstChild);
const nextSibling = _$Links.kify(_nextSibling);


// useful DOM operations

//swapNodes : (domRef, domRef) -> ()
function _swapNodes(x, y) {
  _$Debug.assert(
    x.parentNode != null && y.parentNode != null,
    "cannot swap root nodes"
  );
  _$Debug.assert(x.parentNode != y, "cannot swap a node with its parent");
  _$Debug.assert(y.parentNode != x, "cannot swap a node with its parent");

  let xNextSibling = x.nextSibling;
  let yNextSibling = y.nextSibling;
  let xParent = x.parentNode;
  let yParent = y.parentNode;

  if (xNextSibling != y) {
    xParent.insertBefore(y, xNextSibling);
  }

  if (yNextSibling != x) {
    yParent.insertBefore(x, yNextSibling);
  }

  return _$Constants.UNIT;
}
let swapNodes = _$Links.kify(_swapNodes);

/**
 * domReplaceChildren : xml -> domRef -> ()
 * @param {Xml} xml
 * @param {DomNode} parent
 * @returns {()}
 */
function _domReplaceChildren(xml, parent) {
  const newNodes = _$Links.XmlToDomNodes(xml);

  parent.innerHTML = '';

  newNodes.forEach(function (node) { return _domAppendChildRef(node, parent) });
  return _$Constants.UNIT;
}

let domReplaceChildren = _$Links.kify(_domReplaceChildren);

// for server generated event handlers
function _registerMobileEventHandlers(key, handlers) {
  _$List.forEach(handlers, function(h){
    let event = h[1];
    let handler =_wrapEventHandler(h[2]);
    if (!_eventHandlers[key]) {
      _eventHandlers[key] = {};
    }
    _eventHandlers[key][event] = handler;
  });
  return key;
}

function _registerEventHandlers(handlers) {
  let key = '_key' + _get_fresh_node_key();

  _$List.forEach(handlers, function(h) {
    let event = h[1];
    let handler = _wrapEventHandler(h[2]);
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

const getTarget = _$Links.kify(_getTarget)
const getTargetValue = _$Links.kify(_getTargetValue)
const getPageX = _$Links.kify(_getPageX)
const getPageY =_$Links.kify(_getPageY)
const getFromElement = _$Links.kify(_getFromElement)
const getToElement = _$Links.kify(_getToElement)
const getTime = _$Links.kify(_getTime)
const getCharCode = _$Links.kify(_getCharCode)

function innerHTML(x) { return x.innerHTML }

// (IE) hack
let _cssText = "";

// TBD: put these _isXml functions in the DEBUG module?

function _isXmlItem(obj) {
  if (!obj || !obj.type) {
    return false;
  }

  if (obj.type === 'ELEMENT') {

    if(!_$Types.isList(obj.children)) {
      return false;
    }
    let childrenOkay = true;
    _$List.forEach(obj.children, function(child) {
      childrenOkay &= _isXmlItem(child)
    });
    if(!childrenOkay)
      return false;


    return Boolean(
      obj.tagName &&
      _$Types.isString(obj.tagName) &&

      obj.attrs &&
      Object.keys(obj.attrs)
        .map(function (key) { return _$Types.isString(key) && _$Types.isString(obj.attrs[key]); })
        .reduce(function (a, b) { return a && b; }, true)
    );
  }

  if (obj.type === 'TEXT') {
    return _$Types.isString(obj.text);
  }

  return false;
}

function _isXml(obj) {
  if (!_$Types.isList(obj)) return false;
  let childrenOkay = true;
  _$List.forEach(obj, function(c) {
    childrenOkay &= _isXmlItem(c);
  });
}

function _stringToXml(s) {
//  DEBUG.assert(_$Types.isArray(s), "_stringToXml Expected an array, got: " + s);
  let res = _Cons(  { type: 'TEXT',text: s},Nil);
  return res;
}

function _intToXml(i) {
  return _stringToXml(_intToString(i));
}
function _floatToXml(f) {
  return _stringToXml(_floatToString(f));
}

const stringToXml = _$Links.kify(_stringToXml);
const intToXml = _$Links.kify(_intToXml);

// not in library.ml yet
const floatToXml = _$Links.kify(_floatToXml);


function _getTagName(xml) {
  let first = _$List.head(xml);
  if (first.type !== "ELEMENT") {
    throw new Error("getTagName() applied to non-element node");
  }
  return first.tagName;
}

function _getNamespace (xml) {
  let first = _$List.head(xml);
  if (first.type !== 'ELEMENT') {
    throw new Error('getNameSpace applied to non-element node');
  }
  return first.namespace || '';
}

function _itemChildNodes(xmlitem) {
  if (xmlitem.type !== "ELEMENT") {
    throw new Error("itemChildNodes() applied to non-element node");
  }
  return xmlitem.children || [ ];
}

function _itemTextContent(xmlitem) {
  if (xmlitem.type !== "TEXT") {
    throw new Error("itemTextContent() applied to non-text node");
  }
  return xmlitem.text || '';
}

function _getAttributes(xml) {
  let first = _$List.head(xml);
  if (first.type !== "ELEMENT") {
    throw new Error("getAttributes() applied to non-element node");
  }

  return _$List.lsFromArray(Object.keys(first.attrs).map(function (key) {
    return {
      1: key,
      2: first.attrs[key],
    };
  }));
}

function _hasAttribute(xml, attrName) {
  let first = _$List.head(xml);
  if (first.type !== "ELEMENT") {
    throw new Error("hasAttribute() applied to non-element node");
  }

  return Boolean(first.attrs[attrName]);
}

function _getAttribute(xml, attrName) {
  let first = _$List.head(xml);
  if (first.type !== "ELEMENT") {
    throw new Error("getAttribute() applied to non-element node");
  }
  return first.attrs[attrName];
}

// addAttributes : (Xml, [(String, String)]) -> Xml
function _addAttributes(xml, attrs) {
  let first = _$List.head(xml);
  if(first.type != "ELEMENT") {
    throw new Error("addAttributes() applied to non-element node");
  }

  const xmlItem = JSON.parse(JSON.stringify(first));

  _$List.forEach(attrs, function(a) {
    xmlItem.attrs[a[1]] = a[2];
  });
  return _$List.singleton( xmlItem );
}

const getTagName = _$Links.kify(_getTagName);
const getAttributes = _$Links.kify(_getAttributes);
const hasAttribute = _$Links.kify(_hasAttribute);
const getAttribute = _$Links.kify(_getAttribute);
const itemChildNodes = _$Links.kify(_itemChildNodes);
const itemTextContent = _$Links.kify(_itemTextContent);

function _fail(str) {
  return _alert("Internal error: " + str);
}


function _isElementNode(node) {
  return (node != null && node.nodeType == document.ELEMENT_NODE);
}

// DomNode -> bool
let isElementNode = _$Links.kify(_isElementNode);

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

  let styleElement = document.getElementsByTagName('style')[0]
  if(!styleElement || !styleElement.styleSheet)
    throw ("style element doesn't have a style sheet")

  styleElement.styleSheet.cssText = _cssText;
  _cssText = null;
  return;
}

// time in seconds since the beginning of 1970
function _clientTime () { return Math.floor(Date.now()/1000); }
let clientTime = _$Links.kify(_clientTime);

// time in milliseconds since the beginning of 1970
function _clientTimeMilliseconds () { return Date.now(); }
let clientTimeMilliseconds = _$Links.kify(_clientTimeMilliseconds);

let _pageTimer;

function _startTimer() {
  _pageTimer = _clientTime();
  return;
}
function _stopTimer() {
  _pageTimer = _clientTime() - _pageTimer;
  _$Debug.debug("Page drawn in ", _pageTimer, "ms");
  return;
}

/// focus stuff
let _focused = null;
function _focus() {
  if (_focused) {
    let y = document.getElementById(_focused);
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
  _$Debug.assert(tree != null, "No argument given to _replaceDocument");
  let firstChild = _$List.head(tree);
  _$Debug.assert(firstChild != null, "Null tree passed to _replaceDocument");
  _$Debug.assert(
    firstChild.type === "ELEMENT",
    "New document value was not an XML element (it was non-XML or was an XML text node)."
  );
  tree = _$Links.XmlToDomNodes(tree);

  // save here
  let _saved_fieldvals = [];
  let inputFields = document.getElementsByTagName("input");
  for (let i = 0; i < inputFields.length; i++) {
     let current = inputFields[i];
     if (current.id != null && current.id != "") { // only store fields with an id!
       _saved_fieldvals.push({'field' : current.id, 'value' : current.value});
     }
  }

  // delete the DOM except for the html tag and the body
  // (IE) IE doesn't allow these tags to be deleted
  let d = document.documentElement;
  let body;
  while (d.hasChildNodes()) {
    if (isElementWithTag(d.firstChild, 'body')) {
      body = d.firstChild;
      let bodyLength = body.childNodes.length;
      while (body.hasChildNodes()) {
        body.removeChild(body.firstChild);
      }
      break; // (IE) no more nodes allowed after the body
    }
    d.removeChild(d.firstChild);
  }

  // insert new dom nodes
  for (let p = tree[0].firstChild; p != null; p = p.nextSibling) {
    if (isElementWithTag(p, 'body')) {
     // insert body nodes inside the existing body node
      for (let q = p.firstChild; q != null; q = q.nextSibling) {
        let it = q.cloneNode(true);
        body.appendChild(it);
        _$Links.activateHandlers(it);
      }
      break; // (IE) no more nodes allowed after the body
    }
    let it = p.cloneNode(true);
    d.insertBefore(it, body);
    _$Links.activateHandlers(it);
  }

  // (IE) hack to activate a style element in IE
  _activateStyleElement();

  // restore here
  for (let i = 0; i < _saved_fieldvals.length; i++) {
     let current = _saved_fieldvals[i];
     let elem = document.getElementById(current.field);
     if (elem) {
        elem.value = current.value;
     }
  }

  _focus();

  return _$Constants.UNIT;
}

function _start(page) {
  _stopTimer();
  return _replaceDocument(page);
//  renderPage(page, _replaceDocument)
}

function _startRealPage() {
  const parsedState = JSON.parse(_jsonState);
  let state = _$Links.resolveJsonState(parsedState);
  _initVars(state); // resolve JSONized values for toplevel let bindings received from the server
  _$Links.activateJsonState(state, parsedState); // register event handlers + spawn processes
  _$Links.activateHandlers(_getDocumentNode());
  // Create a websocket connection
  return _$Websocket.connect_if_required(parsedState);
}

// generate a fresh key for each node
let _node_key = 0;
function _get_fresh_node_key() {
  return _node_key++;
}

let _eventHandlers = {};

// Wrap an event handler in a function that sets the main process id
// at the beginning of the handler and unsets it at the end.
function _wrapEventHandler(handler) {
  return function (event) {
    // set process id here
    let active_pid = _current_pid;
    _current_pid = _mainPid;
    _handlingEvent = true;

    let _cont = function () { return handler(event, _idy); }
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
   let key = '_key' + _get_fresh_node_key();

  for (let i = 0; i < actions.length; i++) {
    let action = actions[i];
      // FIXME: Shouldn't we need to clone the actions[i] objs?

    //_$Debug.debug("adding " + action.evName + " to object " + key);
    if (!_eventHandlers[key])
      _eventHandlers[key] = [];
    _eventHandlers[key][action.evName] = _wrapEventHandler(action.handler);
  }

  return key; // returns the ID to give to the elt
}




// db functions (via remote calls)
// TODO!

const javascript = true;

// identity: a "toplevel" continuation
//
// (this needn't actually return a value as _yield and _yieldcont
// don't actually return values)
//
// function _idy(x) { return; }

function _efferr(z, ks) { return _error("Unhandled operation `" + z._label + "'.") }

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

  _$Debug.assert(_$Types.isChannel(c), "Cancelling non-channel");

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
        _$Websocket.sendRemoteCancellation(peer_ep, local_ep);
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

        return _yieldCont(_$List.revAppend(s, ks), op_arg);
    }
}

// Called "fun" in the CPS paper
function _make_resumption (s) {
  return function (x, ks) {
    return _yieldCont(_$List.revAppend(s, ks), x);
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
//  let x;
//  f(function (v) {x = v});
//  return x
// }
//
// Something like this version could be made to work...
// ...but clearly it breaks concurrency as it stands
//
// function _run(f) {
//   _handlingEvent = true;
//   let x;
//   let cont = function () {return f(function (v) {x = v})};
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

let _mailboxes = {0:[]};
let _blocked_procs = {};
//let _suspended_procs = [];

function _dumpSchedStatus() {
  _$Debug.debug("--------\nMailbox status:");
  for (let i in _mailboxes) {
    if (_mailboxes[i].length > 0)
      _$Debug.debug("pid ", i, ": ", _mailboxes[i].length, " msgs waiting");
  }
  let blockedPids = "";
  for (let i in _blocked_procs) {
    if (blockedPids != "") blockedPids += ", ";
    blockedPids += i
  }
  if (blockedPids != "")
    _$Debug.debug("blocked process IDs: ", blockedPids);
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
  let clientPid = "cl_" + _maxPid;
  return clientPid;
}

function _spawnWithMessages(childPid, f, messages) {
  _mailboxes[childPid] = messages;

  setZeroTimeout(function () {
    _$Debug.debug("launched process #" + childPid);
    _current_pid = childPid;
    f(_makeCont(function () { delete _mailboxes[childPid] }))
  });
  return { _clientPid : childPid, _clientId : _client_id }
}

function _registerMobileKey(state, serverKey) {
  let clientKey = '_key' + _get_fresh_node_key();
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
        _$Debug.assert(false, "Cannot spawn a process on another client"); return;
    }
  } else if ("_serverSpawnLoc" in loc) {
      _$Debug.assert(false, "Cannot spawn process on server from client"); return;
  } else {
      _$Debug.assert(false, "Invalid spawn location " + loc); return;
  }
}

function _spawnAt(loc, f) {
  check_loc_spawnable(loc);
  let childPid = _freshProcess();
  return _spawnWithMessages(childPid, f, []);
}

function spawnAt(loc, f, kappa) {
  return _applyCont(kappa,_spawnAt(loc, f));
}

// TODO: implement this properly
let _spawnAngelAt = _spawnAt;
let _spawnAngel = _spawn;
let spawnAngel = spawn;
function _spawn(f) { return _spawnAt(_here(), f); }
function spawn(f, kappa) { return spawnAt(_here(), f, kappa); }

function _spawnWrapper(env) {  // necessary wrapper for server->client calls
  return spawn;
}

function spawnWait(f, kappa) {
  // f is a zero-argument CPS function
  let parentPid = _current_pid
  let childPid = _freshProcess();
  _mailboxes[childPid] = [];

  _$Debug.debug("launched process #", childPid);
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


let there = here
let _there = _here

function _haveMail() {
  return _mailboxes[_self()._clientPid].length != 0;
}
let haveMail = _$Links.kify(_haveMail);

let _sched_pause = 0;

function _wakeup(pid) {
  if (_blocked_procs[pid]) {
    _$Debug.debug("Waking up ", pid);
    let proc = _blocked_procs[pid];
    delete _blocked_procs[pid];
    return setZeroTimeout(proc);
  } else {
    // already awake?
    _$Debug.debug("Tried to wake up ", pid, ", but it is already awake");
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
    _$Debug.assert(false, "Invalid PID in get_process_id");
    return;
  }
}

function _Send(pid, msg) {
  //  _dump(_mailboxes)
  /* First, check to see whether we are sending to a well-formed PID */
  let valid_server_pid = _is_valid_server_pid(pid);
  let valid_client_pid = _is_valid_client_pid(pid);
  _$Debug.assert(
    valid_server_pid || valid_client_pid,
    "Malformed PID in _Send: neither client nor server PID"
  );

  if (valid_server_pid) {
    _$Websocket.sendRemoteServerMessage(pid._serverPid, msg);
  } else if (valid_client_pid) {
    if (get_client_id(pid) == _client_id) {
      // Local send
      _$Debug.debug("sending message ", msg, " to pid ", pid._clientPid);
      let client_pid = pid._clientPid;
      _$Links.deliverMessage(client_pid, msg);
    } else {
      // Remote send
      _$Websocket.sendRemoteClientMessage(get_client_id(pid), pid._clientPid, msg);
    }
  }
  //_dumpSchedStatus();
  return;
}

function Send(pid, msg, kappa) {
  return _applyCont(kappa, _Send(pid, msg));
}

function _dictlength(x) {
  let length = 0;
  for (let prop in x) {
    length++;
  }
  return length;
}

function _block_proc(pid, its_cont) {
  _$Debug.debug("Blocking ", pid);
  _blocked_procs[pid] = its_cont;
  // discard stack
  return;
}

// recv
//   recv is an unusual library function that may capture the
//   continuation; hence there is no _recv form (direct-style).
function recv(kappa) {
  _$Debug.assert(
    arguments.length == 1,
    'recv received ' + arguments.length + ' arguments, expected 1'
  );
  //_$Debug.assert(_current_pid != _mainPid,
  //           "Cannot call recv() in main process.");
  _$Debug.assert(
    _mailboxes[_current_pid],
    "Process " + _current_pid + " seems not to have been created."
  );
  if ( _mailboxes[_current_pid].length > 0) {
    let msg = _mailboxes[_current_pid].pop();
    _$Debug.debug("received message '"+ JSON.stringify(msg) +"'");
    return _applyCont(kappa, msg);
  } else {
    let current_pid = _current_pid;
    return _block_proc(
      current_pid,
      function () {
        _current_pid = current_pid;
        _$Debug.debug("scheduled process ", current_pid);
        return recv(kappa);
      }
    );
    // _$Debug.debug("blocked: "+current_pid)
  }
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
const BALANCED = 0
const ACCEPTING = 1
const REQUESTING = 2

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
  let id_ep1 = freshChannelID();
  let id_ep2 = freshChannelID();
  _buffers[id_ep1] = [];
  _buffers[id_ep2] = [];
  return { _sessEP1: id_ep1, _sessEP2: id_ep2 };
}

function newWithID(apid) {
  _$Debug.debug("Creating new access point ", apid);
  _aps[apid] = {ap_state: BALANCED, pending: []};
  return {_clientAPID: apid, _clientId: _client_id};
}

function _new() {
  let apid = freshAPID();
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
  _$Debug.assert(
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
  let current_pid = _current_pid;
  return _block_proc(
    _current_pid,
    function () {
      _current_pid = current_pid;
      // Grab the returned channel EP out of the _returned_channels table,
      // and continue
      _$Debug.assert(_returned_channels[_current_pid] != undefined,
        "resuming process after remote accept, but no channel available!");
      let chan = _returned_channels[_current_pid];
      delete _returned_channels[_current_pid];
      return _applyCont(kappa, chan);
    }
  );
}

// Block the process until a matching process has been found on a local AP.
function blockUntilLocalMatch(ch, kappa) {
  let current_pid = _current_pid;
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
  _$Debug.assert(_aps[localAPID] != undefined, "Attempting to accept on an undefined AP!");
  // If there's a requester, then pop the requester, create the other end of the channel, and
  // wake up the requester.
  let ap = _aps[localAPID];

  function makeAndBlock() {
    let new_ch = freshChannel();
    let our_ep = new_ch._sessEP1;
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
      _$Debug.assert(ap.pending.length > 0,
        "Accepting on a requesting endpoint with no pending requests!");
      let top = ap.pending.pop();
      if (ap.pending.length == 0) {
        _$Debug.debug("Changing state to balanced");
        ap.ap_state = BALANCED;
      }

      _aps[localAPID] = ap;

      // Unblock other end of the channel
      let other_ep = top._sessEP2;
      wakeupFromChan(other_ep);
      // Pass the channel to the continuation
      return _applyCont(kappa,top);
   default:
      _$Debug.assert(false, "Invalid access point state! " + ap.ap_state);
      return;
  }
}

function remoteAccept(remote_apid, kappa) {
  // Accept remotely
  _$Websocket.sendRemoteAPAccept(_current_pid, remote_apid);
  return blockUntilAPResponse(kappa);
}

function accept(ap, kappa) {
  // Firstly, check the type of accept. Local, or remote? Server, or client?
  if (_is_valid_server_ap(ap)) {
    return remoteAccept(get_server_apid_from_ap(ap), kappa);
  } else if (_is_valid_client_ap(ap)) {
    if (get_client_id_from_ap(ap) != _client_id) {
      _$Debug.assert(false, "alas, accepting on a remote client AP is not yet supported");
      return;
    } else {
      return localAccept(get_apid_from_ap(ap), kappa);
    }
  } else {
    _$Debug.assert(false, "invalid access point ID in accept! " + JSON.stringify(ap));
    return;
  }
}


// Do a request on a local access point. localAPID: the key into the aps table.
function localRequest(localAPID, kappa) {
  // This should explicitly not happen -- all server-created APs residing on
  // this client should be serialised in state dumps provided in realpages
  // delivery / RPC returns
  _$Debug.assert(_aps[localAPID] != undefined, "Attempting to request on an undefined AP!");
  _$Debug.debug("Request called on local AP ", localAPID);

  // If there's a requester, then pop the requester, create the other end of the channel, and
  // wake up the requester.
  let ap = _aps[localAPID];

  function makeAndBlock() {
    let new_ch = freshChannel();
    let flipped_chan = makeFlippedChan(new_ch)
    let our_ep = flipped_chan._sessEP1;
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
      _$Debug.assert(ap.pending.length > 0,
        "Requesting on an accepting endpoint with no pending requests!");
      let top = ap.pending.pop();
      if (ap.pending.length === 0) {
        _$Debug.debug("Changing state to balanced");
        ap.ap_state = BALANCED;
      }

      _aps[localAPID] = ap;

      // Unblock other end of the channel
      let other_ep = top._sessEP1;
      wakeupFromChan(other_ep);

      // Pass our end of channel (flipped version of pending) to the continuation
      return _applyCont(kappa,makeFlippedChan(top));
   default:
      _$Debug.assert(false, "Invalid access point state! " + ap.ap_state);
      return;
  }
}

function remoteRequest(remote_apid, kappa) {
  // Request remotely
  _$Websocket.sendRemoteAPRequest(_current_pid, remote_apid);
  return blockUntilAPResponse(kappa);
}

function request(ap, kappa) {
  if (_is_valid_server_ap(ap)) {
    return remoteRequest(get_server_apid_from_ap(ap), kappa);
  } else if (_is_valid_client_ap(ap)) {
    if (get_client_id_from_ap(ap) != _client_id) {
      _$Debug.assert(false, "alas, requesting from a remote client AP is not yet supported");
      return;
    } else {
      return localRequest(get_apid_from_ap(ap), kappa);
    }
  } else {
    _$Debug.assert(false, "invalid access point ID in request! " + JSON.stringify(ap));
    return;
  }
}

function canReceiveMessage(v) {
  // Check whether all contained sessions are in _buffers (i.e. are committed)
  let contained_sessions = getContainedSessions(v);
  for (let i = 0; i < contained_sessions.length; i++) {
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
      _$Debug.debug("Storing lost message ", v, " for port ", epid);
      return addLostMessage(epid, v);
    } else {
      _$Debug.debug("Storing orphan message ", v, " for port ", epid);
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
  for (let i = 0; i < delegated_chans.length; i++) {
    let deleg_chan = delegated_chans[i].ep_id;
    let buf = delegated_chans[i].buf;
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
  let ret = {};

  for (let i = 0; i < ep_ids.length; i++) {
    let ep_id = ep_ids[i];
    ret[ep_id] = _lost_messages[ep_id];
    delete _lost_messages[ep_id];
  }

  return _$Websocket.sendLostMessageResponse(remote_ep, ret);
}

// Given an endpoint ID and a list of lost messages:
//   - Stores  EPID |-> buffer + lost messages + orphan messages in _buffers
//     (note that JS queues grow at the front, so the order is reversed)
//   - Deletes EPID from _pending_endpoints
//   - Deletes EPID from _orphan_messages
function commitChannel(epid, lost_messages) {
  let orig_buf = _pending_endpoints[epid];
  let orphans = _orphan_messages[epid];
  if (orphans == undefined) {
    orphans = [];
  }
  let complete_buf = orphans.concat(lost_messages, orig_buf);
  _buffers[epid] = complete_buf;
  delete _pending_endpoints[epid];
  delete _orphan_messages[epid];
  return;
}

function handleLostMessages(blocked_ep, lost_message_table) {
  // Commit all channels
  for (let ep_id in lost_message_table) {
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
  if (_$Types.isArray(v)) {
    let res = [];
    for (let i = 0; i < v.length; i++) {
      res = res.concat(getContainedSessions(v[i]));
    }
    return res;
  } else if(_$Types.isObject(v)) {
    if (_$Types.isChannel(v)) {
        return [v];
    }
    // We can treat variants and records in the same way
    let vals = Object.values(v);
    let res = [];
    for (let i = 0; i < vals.length; i++) {
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
  let res = [];
  for (let i = 0; i < chans.length; i++) {
    let cur_chan = chans[i];
    let cur_recv_ep = chans[i]._sessEP2;
    _$Debug.assert(cur_recv_ep in _buffers, "Trying to delegate channel without a buffer!");
    let cur_buf = _buffers[cur_recv_ep];
    // Delete buffer, create lost message buffer
    delete _buffers[cur_recv_ep];
    _lost_messages[cur_recv_ep] = [];
    res.unshift({ chan: cur_chan, buffer: cur_buf});
  }
  return res;
}

function _remoteSessionSend(v, c) {
  let delegated_sessions = getContainedSessions(v);
  let prepared_delegated_sessions = prepareDelegatedChannels(delegated_sessions);
  _$Debug.debug("Prepared delegated sessions:");
  _$Debug.debug(prepared_delegated_sessions);
  return _$Websocket.sendRemoteSessionMessage(c, prepared_delegated_sessions, v);
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
  let peer_ep = c._sessEP1;
  let local_ep = c._sessEP2;
  _$Debug.assert(local_ep in _buffers, "Trying to receive from nonexistent buffer!");
  let buf_len = _buffers[local_ep].length;
  if (buf_len > 0 && canReceiveMessage(_buffers[local_ep][buf_len - 1])) {
    let msg = _buffers[local_ep].pop();
    return _applyCont(kappa, {1:msg, 2:c});
  } else {
    let current_pid = _current_pid;
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
  let peer_ep = c._sessEP1;
  let local_ep = c._sessEP2;
  _$Debug.assert(local_ep in _buffers, "Trying to receive from nonexistent buffer!");
  let buf_len = _buffers[local_ep].length;
  if (buf_len > 0 && canReceiveMessage(_buffers[local_ep][buf_len - 1])) {
    let msg = _buffers[local_ep].pop();
    // _$Debug.debug("Grabbed " + JSON.stringify(msg));
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
      let current_pid = _current_pid;
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

function _close(c) {
  const local_ep = c._sessEP2;
  delete _buffers[local_ep];
  return {};
}

// SCHEDULER

let _yieldCount = 0;
let _yieldGranularity = 60;
let _callCount = 0;

function _Continuation(v) { this.v = v; return; }

let _theContinuation = new _Continuation(_$Links._removeCGIArgs);


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
      let current_pid = _current_pid;
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
      let current_pid = _current_pid;
      return setZeroTimeout(function () { _current_pid = current_pid; return k(arg); });
    }
  } else {
    return k(arg);
  }
}

function _applyCont_Default(k, arg) { return k(arg); }

function _yieldCont_HO(ks, arg) {
  let k = _hd(ks);
  ks = _tl(ks);
  ++_yieldCount;
  if (_yieldCount == _yieldGranularity) {
    _yieldCount = 0;
    if (_handlingEvent) {
      _theContinuation.v = function () { return k(arg, ks); }; throw _theContinuation;
    } else {
      let current_pid = _current_pid;
      return setZeroTimeout(function () { _current_pid = current_pid; return k(arg, ks); });
    }
  } else {
    return k(arg, ks);
  }
}

function _applyCont_HO(ks, arg) {
  let k = _hd(ks);
  ks = _tl(ks);

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
// and _$Links.kify =_def \k.\x.k (f x)
// so let g =_def \k.\f.k (_$Links.kify(f))
//
// let callForeign = g
//
// callForeign allows us to call JS functions from within Links
// using the syntax: callForeign(f)(args)

// [DEACTIVATED]
//function callForeign(kappa) {
//  return function (f) {
//    return kappa (_$Links.kify(f));
//  };
//}

// [DEACTIVATED]
// like callForeign, except it takes
// two arguments: an object and a method
//function callForeignMethod(kappa) {
//  return function (obj, method) {
//    return kappa (_$Links.kifyMethod(obj, method));
//  };
//}

function _print(...str) {
  console.info(...str);
  return 0;
}

const print = _$Links.kify(_print);

// // [DUBIOUS FUNCTIONS]
// //   Should elementByID and attribute be here?
// function elementById(id, kappa) {
//     let elem = document.getElementById(id);
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

  let val = obj[attr];

  if (val == undefined) return({_label:'None', '_value':({})});
  else return({'_label':'Some', '_value':val});
}
const attribute = _$Links.kify(_attribute);

function _is_integer(s) {
  return s.match(/^[0-9]+$/) != null;
}
const is_integer = _$Links.kify(_is_integer);

function _objectType(obj) {
  obj = obj[0];
  return(typeof(obj) == 'object' ? obj.constructor : typeof(obj))
}

const objectType = _$Links.kify(_objectType);

function _textContent (node) {
  try { return (node.innerHTML) }
  catch (e) { return ("") }
}

function textContent (node, kappa) {
  return _applyCont(kappa,_textContent(node));
}

function _reifyK() {
  return _$Links.unimpl("reifyK");
}

function sleep(duration, kappa) {
  let current_pid = _current_pid;
  return setTimeout(function() { _current_pid = current_pid; return _applyCont(kappa,{}); }, duration);
}

// include a js file
function _include(script_filename) {
  const html_doc = document.getElementsByTagName('head').item(0);
  const js = document.createElement('script');
  js.setAttribute('language', 'javascript');
  js.setAttribute('type', 'text/javascript');
  js.setAttribute('src', script_filename);
  html_doc.appendChild(js);
  return false;
}
// should do something more sensible for including extra javascript
// code (add a setting in js.ml, say)
//_include("extras.js")


function _chr(c) { return { _c: String.fromCharCode(c) } };
const chr = _$Links.kify(_chr);
function _ord(c) { return c._c.charCodeAt(0); }
const ord = _$Links.kify(_ord);

const _sqrt = Math.sqrt; const sqrt = _$Links.kify(_sqrt);
const _floor = Math.floor; const floor = _$Links.kify(_floor);
const _ceiling = Math.ceil; const ceiling = _$Links.kify(_ceiling);
const _tan = Math.tan; const tan = _$Links.kify(_tan);
const _sin = Math.sin; const sin = _$Links.kify(_sin);
const _cos = Math.cos; const cos = _$Links.kify(_cos);
const _log = Math.log; const log = _$Links.kify(_log);
const _log10 = Math.log10; const log10 = _$Links.kify(_log10);
const _exp = Math.exp; const exp = _$Links.kify(_exp);

function _makeCgiEnvironment() {
  let env = [];

  let i = 0;
  for(name in cgiEnv) {
    env[i] = {'1':name, '2':cgiEnv[name]};
    ++i;
  }

  cgiEnv = env;
}

function _environment() {
  return cgiEnv;
}
let environment = _$Links.kify(_environment);

function _redirect(url) {
  window.location = url;
}
const redirect = _$Links.kify(_redirect);

let QUIRKS = function () {
  return {
    // BEGIN code from quirksmode.org
    createCookie: function (name, value, days) {
      if (days) {
        let date = new Date(); // makes garbage
        date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
        let expires = "; expires=" + date.toGMTString();
      } else {
        let expires = "";
      }
      document.cookie = name + "=" + value + expires + "; path=/";
      return;
    },

    readCookie: function (name) {
      let nameEQ = name + "=";
      let ca = document.cookie.split(';');
      for (let i = 0; i < ca.length; i++) {
        let c = ca[i];
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
  return _$Constants.UNIT;
}
let setCookie = _$Links.kify(_setCookie);

function _getCookie(cookieName) {
  return QUIRKS.readCookie(cookieName);
}
let getCookie = _$Links.kify(_getCookie);

function _random() {return Math.random();}
let random = _$Links.kify(_random);

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

function _jsSetWindowEvent(event, fn, capture) {
  window.addEventListener(event, function(e) { fn(e, _idy) }, capture);
  return _$Constants.UNIT;
}

const jsSetWindowEvent = _$Links.kify(_jsSetWindowEvent);

function _jsSetOnLoad(fn) {
  window.addEventListener('load', function(e) { fn(e, _idy) }, false);
  return;
}
function jsSetOnLoad(fn, kappa) {
  _jsSetOnEvent(fn);
  return _applyCont(kappa,{});
}

let globalObjects = {};

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
const jsFill = _jsFill;

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
const jsClosePath = _jsClosePath;

function _jsArc(ctx, x, y, radius, startAngle, endAngle, clockwise) {
  return ctx.arc(x, y, radius, startAngle, endAngle, clockwise);
}
const jsArc = _jsArc;

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
let jsLineWidth = _jsLineWidth;

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
const jsCanvasWidth = _jsCanvasWidth;

function _jsCanvasHeight(ctx) {
  return ctx.canvas.height;
}
const jsCanvasHeight = _jsCanvasHeight;

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
let debugGetStats = _debugGetStats;

function _jsSaveCanvas(canvas, node, mime) {
  const imageData = canvas.toDataURL(mime);//.replace("image/png", "image/octet-stream");;
  node.href = imageData; // window.location.
  return;
}
let jsSaveCanvas = _jsSaveCanvas;

function _debugChromiumGC() {
  if (window.gc) {
    return window.gc();
  } else {
    let msg = "Error. In order to use debugChromiumGC() invoke chromium like this: chromium --js-flags='--expose_gc'. Application terminated.";
    alert(msg);
    throw new Error(msg);
  }
}
const debugChromiumGC = _debugChromiumGC;

//
// EQUALITY
//

// "if you could create String Objects like new String("test"), re-use
// those and use those in the comparisons, that would be even faster,
// because the JS engine would only need to do a pointer-comparison"
function _stringEq(l, r) {
  return l === r;
}
const stringEq = _stringEq; // TODO(dhil): these definitions appear
                            // broken to me. They are missing a
                            // _$Links.kify.

function _intEq(l, r) {
  return l === r;
}
const intEq = _intEq;

function _floatEq(l, r) {
  return l === r;
}
const floatEq = _floatEq;

function _floatNotEq(l, r) {
  return l !== r;
}
const floatNotEq = _floatNotEq;

function _objectEq(l, r) {
  return l._label === r._label && (l._value === r._value || (l._value === {} && r._value === {}));
}
const objectEq = _objectEq;

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
const makeXml = _$Links.kify(_makeXml);

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

const strContains = _$Links.kify(_strContains);

/**
 * Converts Xml to its variant type equivalent
 *
 * @param {{ namespace?: string, tagName?: string, text?: string, attrs?: Object, children?: any[] }[]} xml
 * @returns {{ _label: string, _value: Object }[]}
 */
function _xmlToVariant (xml) {
  _$Debug.assert(_$Types.isArray(xml), 'xmlToVariant expects Xml (array of XmlItem)');
  return xml.map(_xmlItemToVariant);
}
const xmlToVariant = _$Links.kify(_xmlToVariant);

/**
 * Converts a variant version of Xml to an Xml representation
 *
 * @param {{ _label: string, _value: Object }[]} variants
 * @returns {{ namespace?: string, tagName?: string, text?: string, attrs?: Object, children?: any[] }[]}
 */
function _variantToXml (variants) {
  _$Debug.assert(_$Types.isArray(variants), 'variantToXml expects an array');
  return variants.map(_variantToXmlItem);
}
const variantToXml = _$Links.kify(_variantToXml);

/**
 * Converts a single XmlItem to its variant type equivalent
 *
 * @param {{ namespace?: string, tagName?: string, text?: string, attrs?: Object, children?: linked list }} xmlItem
 * @returns {{ _label: string, _value: Object }}
 */
function _xmlItemToVariant (xmlItem) {
  _$Debug.assert(
    xmlItem.type === 'ELEMENT' || xmlItem.type === 'TEXT',
    'Non-XmlItem passed to xmlItemToVariant'
  );
  if (xmlItem.type === 'ELEMENT') {
    _$Debug.assert(
      xmlItem.tagName && xmlItem.children && xmlItem.attrs,
      'Malformed element passed to xmlItemToVariant'
    );
    const attrs = _$List.mapFromArray(Object.keys(xmlItem.attrs),function (name) {
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

    const children = _$List.map(xmlItem.children,_xmlItemToVariant);

    if (xmlItem.namespace) {
      return {
        '_label': 'NsNode',
        '_value': {
          1: xmlItem.namespace,
          2: xmlItem.tagName,
          3: _$List.append(attrs,children),
        }
      };
    } else {
      return {
        '_label': 'Node',
        '_value': {
          1: xmlItem.tagName,
          2: _$List.append(attrs,children),
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
const xmlItemToVariant = _$Links.kify(_xmlItemToVariant);

/**
 * Converts a variant version of an XmlItem to that XmlItem
 *
 * @param {{ _label: string, _value: Object }} variant
 * @returns {{ namespace?: string, tagName?: string, text?: string, attrs?: Object, children?: any[] }}
 */
function _variantToXmlItem (variant) {
  _$Debug.assert(_$Types.isObject(variant), 'variantToXmlItem expects an object');
  _$Debug.assert(variant['_label'], 'variantToXmlItem expects a variant type object');
  _$Debug.assert(variant['_value'], 'Malformed variant passed to variantToXmlItem');

  const attrs = {};

  let valueAsArray = _$List.toArray(variant['_value']);
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
        _$List.map(_$List.filter(valueAsArray[2],function (c) { return (c['_label'] === 'Node' || c['_label'] === 'NsNode'); }),_variantToXmlItem)
      };
    case 'NsNode':
      return {
        type: 'ELEMENT',
        namespace: valueAsArray[1],
        tagName: valueAsArray[2],
        attrs: attrs,
        children:
        _$List.map(_$List.filter(valueAsArray[2],function (c) { return (c['_label'] === 'Node' || c['_label'] === 'NsNode');}),_variantToXmlItem)
      };
    case 'Attr':
    case 'NsAttr':
      throw new Error('Cannot construct detached attribute');
    default:
      throw new Error('Unknown Variant passed to variantToXmlItem');
  }
}
const variantToXmlItem = _$Links.kify(_variantToXmlItem);


// Date / time functions

/* Internal */
function shiftOffset(date, offset) {
    // Date is a UTC timestamp. Offset is the number of hours offset from UTC.
    // We want to construct a new UNIX timestamp based on offset.
  const addedOffset =
        offset
        * 60 // minutes
        * 60 // seconds
        * 1000; // milliseconds (JS)
  // Since the constructor takes a *local* time timestamp, we also need to
  // adjust for the time zone the date is being constructed in.
  const tzOffset = new Date().getTimezoneOffset() * -1 * 60 * 1000;
  return new Date(date.getTime() + tzOffset + offset);
}

function projectDate(dt) {
    if (dt._type == "timestamp") {
        _$Debug.assert(dt._value instanceof Date);
        return dt._value;
    } else {
        throw new Error('Unable to project date component from infinity / -infinity');
    }
}

/* Constants */
const forever =  { _type: "infinity" } ;
const beginningOfTime = { _type: "-infinity" };

/* API (direct style) */
function _dateToInt(dt) {
  if (dt._type != "timestamp") {
    throw new Error('Unable to get UNIX timestamp for infinity / -infinity')
  }
  const tzOffset = new Date().getTimezoneOffset() * -1 * 60 * 1000;
  return Math.floor((dt.getTime() - tzOffset) / 1000);
}

function _intToDate(ts) {
  const tzOffset = new Date().getTimezoneOffset() * -1 * 60 * 1000;
  return new Date(ts * 1000 + tzOffset);
}

function _dateYear(dt, offset) {
    // TODO: Need to check whether it's UTCFullYear or just fullYear here
    // (I think it's UTCFullYear though)
    return shiftOffset(projectDate(dt), offset).getUTCFullYear();
}

function _dateMonth(dt, offset) {
    return shiftOffset(projectDate(dt), offset).getUTCMonth();
}

function _dateDay(dt, offset) {
    return shiftOffset(projectDate(dt), offset).getUTCDay();
}

function _dateHours(dt, offset) {
    return shiftOffset(projectDate(dt), offset).getUTCHours();
}

function _dateMinutes(dt) {
    return projectDate(dt).getMinutes();
}

function _dateSeconds(dt) {
    return projectDate(dt).getSeconds();
}

function _dateMilliseconds(dt) {
    return projectDate(dt).getMilliseconds();
}
function _parseDate(str) {
    // We're fortunate that JS accepts our internal date representation
    // without any need for parsing.
    if (str == "infinity") {
        return forever;
    } else if (str == "-infinity") {
        return beginningOfTime;
    } else {
        return { _type: "timestamp", _value: new Date(str) }
    }
}

function _now() {
    return { _type: "timestamp", _value: new Date() };
}

function _utcOffset(dt) {
    return ((new Date().getTimezoneOffset()) * -1) / 60
}

function _showUTC(dt) {
    switch (dt._type) {
        case "infinity":
        case "-infinity":
            return dt._type;
        case "timestamp":
            const ts = dt._value;
            const offset = ts.getTimezoneOffset() * -1 / 60;
            let offsetSign = "+";
            if (offset < 0) {
                offsetSign = "-";
            }
            const month = (ts.getUTCMonth() + 1).toString().padStart(2, '0');
            const day = ts.getUTCDate().toString().padStart(2, '0');
            const hours = ts.getUTCHours().toString().padStart(2, '0');
            const minutes = ts.getMinutes().toString().padStart(2, '0');
            const seconds = ts.getSeconds().toString().padStart(2, '0');
            return `${ts.getUTCFullYear()}-${month}-${day} ${hours}:${minutes}:${seconds}+0`
        default:
            throw new Error("Invalid DateTime type");
    }
}

function showLocalDate(dt) {
    switch (dt._type) {
        case "infinity":
        case "-infinity":
            return dt._type;
        case "timestamp":
            const ts = dt._value;
            const offset = ts.getTimezoneOffset() * -1 / 60;
            let offsetSign = "+";
            if (offset < 0) {
                offsetSign = "-";
            }
            const month = ts.getMonth().toString().padStart(2, '0');
            const day = ts.getDate().toString().padStart(2, '0');
            const hours = ts.getHours().toString().padStart(2, '0');
            const minutes = ts.getMinutes().toString().padStart(2, '0');
            const seconds = ts.getSeconds().toString().padStart(2, '0');
            return `${ts.getFullYear()}-${month}-${day} ${hours}:${minutes}:${seconds}${offsetSign}${offset}`
        default:
            throw new Error("Invalid DateTime type");
    }
}

// CPS functions
const dateToInt = _$Links.kify(_dateToInt);
const intToDate = _$Links.kify(_intToDate);
const dateYear = _$Links.kify(_dateYear);
const dateMonth = _$Links.kify(_dateMonth);
const dateDay = _$Links.kify(_dateDay);
const dateHours = _$Links.kify(_dateHours);
const dateMinutes = _$Links.kify(_dateMinutes);
const dateSeconds = _$Links.kify(_dateSeconds);
const dateMilliseconds = _$Links.kify(_dateMilliseconds);
const parseDate = _$Links.kify(_parseDate);
const now = _$Links.kify(_now);
const utcOffset = _$Links.kify(_utcOffset);
const showUTC = _$Links.kify(_showUTC);


/* Temporal Query Accessors */
function _ttData(x) { x["!data"] }
function _ttFrom(x) { x["!from"] }
function _ttTo(x)   { x["!to"] }
function _vtData(x) { x["!data"] }
function _vtFrom(x) { x["!from"] }
function _vtTo(x)   { x["!to"] }
const ttData = _$Links.kify(_ttData);
const ttFrom = _$Links.kify(_ttFrom);
const ttTo = _$Links.kify(_ttTo);
const vtData = _$Links.kify(_vtData);
const vtFrom = _$Links.kify(_vtFrom);
const vtTo = _$Links.kify(_vtTo);


// The following definitions are for backwards compatibility. They
// should eventually be removed.
const LINKS = _$Links;
const TYPES = _$Types;
const CONSTANTS = _$Constants;
const WEBSOCKET = _$Websocket;
