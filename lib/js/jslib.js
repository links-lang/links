// set up optimised setZeroTimeout
(function() {
	var timeouts = [];
	//~ var offset = 0;

	var messageName = "0TMsg";

	function setZeroTimeout(fn) {
		timeouts.push(fn);
		window.postMessage(messageName, "*");
	}

	function handleMessage(event) {
		if (event.source == window && event.data == messageName) {
			event.stopPropagation();
			//~ if (timeouts.length > offset) { //length > 0) {
			if (timeouts.length > 0) {

				//~ timeouts[offset]();
//~
				//~ offset = offset + 1;
				//~ // increment the offset and remove the free space if necessary
				//~ if (offset * 2 >= timeouts.length){
				  //~ timeouts = timeouts.slice(offset);
				  //~ offset = 0;
				//~ }

				timeouts.shift()();
			}
		}
	}

	window.addEventListener("message", handleMessage, true);

	window.setZeroTimeout = setZeroTimeout;
})();

function setZeroTimeout(f) {
  //setTimeout(f, 0)
  window.setZeroTimeout(f)
}

// Links js runtime.

// [HACK]
//   import third party APIs into here
//   - jslib is typed as an empty open row
//   - foreign calls are untyped
var jslib = function () {
  return {
    event: YAHOO.util.Event
  };
}();

var _dwindow = DEBUGGING ? open('', 'debugwindow','width=550,height=800,toolbar=0,scrollbars=yes') : null;

// (IE) these aren't defined in IE
document.ELEMENT_NODE = 1;
document.ATTRIBUTE_NODE = 2;
document.TEXT_NODE = 3;
document.CDATA_SECTION_NODE = 4;
document.ENTITY_REFERENCE_NODE = 5;
document.ENTITY_NODE = 6;
document.PROCESSING_INSTRUCTION_NODE = 7;
document.COMMENT_NODE = 8;
document.DOCUMENT_NODE = 9;
document.DOCUMENT_TYPE_NODE = 10;
document.DOCUMENT_FRAGMENT_NODE = 11;
document.NOTATION_NODE = 12;

var DEBUG = function () {
  // FIXME: These dynamic type-checking functions are useful for more than
  //   debugging; promote them to some other module

  function is_instance(value, type, constructor) {
    return value != undefined
        && (typeof value == type || value instanceof Object
                  && value.constructor == constructor)
  }
  // [xmldump]
  function xmldump(xml) {
    return (new XMLSerializer()).serializeToString(xml) // makes garbage
  }
  // [xmldump2] Another way of dumping XML
  // (IE) the xml property is supposed to work for IE. It
  // doesn't appear to work for DOM nodes created
  // by the global document object, but does work
  // for nodes created by some other document
  // (other documents are created with
  //  new ActiveXObject('Msxml2.DOMDocument.3.0'))...
  //
  // ...it seems that IE has two different kinds of
  // DOM node object: a JScript one, and an ActiveX
  // one. The JScript one is the one used by
  // the DOM for the current document, and does not
  // support serialization.
  //
  // Perhaps we should implement our own serializer
  // at some point if we really need it.
  function xmldump2 (xmlNode) {
    var text = false;
    try {
      // Firefox
      var serializer = new XMLSerializer(); // makes garbage
      text = serializer.serializeToString(xmlNode);
    }
    catch (e) {
      try {
        // IE
        if(xmlNode.nodeType == document.ELEMENT_NODE) {
          text = "ELEMENT" //xmlNode.outerHTML;
        } else if(xmlNode.nodeType == document.TEXT_NODE) {
          text = xmlNode.nodeValue
        } else {
          throw ("can only xmldump element and text nodes in IE")
        }
      }
      catch (e) {}
    }
    return text;
    //   return (new XMLSerializer()).serializeToString(xml)
  }

  // [is_xmlnode]
  //   IE:
  //     The class "Node" isn't defined in IE, so we need an alternative way
  //     of detecting xml nodes
  //   BUG:
  //     Need to do something similar for events.
  var is_xmlnode;
  try {
    null instanceof Node;
    is_xmlnode = function(value) {
      return is_instance(value, -1, Node)
    }
  } catch(e) {
    // in IE resort to testing whether the object has a 'nodeType' field
    is_xmlnode = function(value) {
      return value.nodeType;
    }
  }
  return {
  assert_noisy : DEBUGGING ? function(value, message) {
    if (value != true) {
      try {
        throw new Error() // throw an exception so that we can retrieve the
                          // stack (via the 'stack' property)
      }
      catch (e) {
        var msg = "<b>ASSERTION FAILED!</b> in function "
                    + arguments.callee.caller + "<br/><b>Stack</b>: " + e.stack;
        _debug(msg);
        throw new Error("assertion failed (check debug output): " + message);
      }
    }
  } : function(value, message) { return; } ,
  is_unit : function (x) {
    if (typeof(x) != 'object') return false;
    if (x.constructor != Object) return false;
    for (var i in x) { return false }; return true;
  },
  assert : function(c, msg) {
    if (!c) {
      _debug(msg);
      throw("LINKS ASSERTION FAILED: " + msg);
    }
  },
  is_object : function(value) {
    return value != undefined && (value instanceof Object)
  },
  is_number : function(value) {
    return is_instance(value, 'number', Number);
  },
  is_string: function(value) {
    return is_instance(value, 'string', String);
  },
  is_boolean : function(value) {
    return is_instance(value, 'boolean', Boolean);
  },
  is_xmlnode : is_xmlnode,
  is_array : function(value) {
    return is_instance(value, -1, Array);
  },
  is_charlist: function(value) {
    return(DEBUG.is_array(value) && DEBUG.is_string(value[0]));
  },
  is_event : function(value) {
    return is_instance(value, -1, Event)
  },
  is_textnode : function(value) {
    return DEBUG.is_xmlnode(value) && value.nodeType == document.TEXT_NODE;
  },
  is_undefined : function(value) {
    return value == undefined && String(value) == 'undefined';
  },
  is_null : function(value) {
    return value == null && String(value) == 'null';
  },
  is_function : function(value) {
    return is_instance(value, 'function', Function);
  },
  type : function(value) {
     if      (DEBUG.is_number (value))    return 'number';
     else if (DEBUG.is_string (value))    return 'string';
     else if (DEBUG.is_boolean (value))   return 'boolean';
     else if (DEBUG.is_textnode (value))  return 'textnode';
     else if (DEBUG.is_xmlnode (value))   return 'xmlnode';
     else if (DEBUG.is_array (value))     return 'array';
     else if (DEBUG.is_event (value))     return 'event';
     else if (DEBUG.is_undefined (value)) return 'undefined';
     else if (DEBUG.is_null (value))      return 'null';
     else if (typeof(value) == 'object' && 'constructor' in value)
                                          return new String(constructor); // makes garbage
     else return 'UNKNOWN TYPE'
  },
  // Could also add type-predicates for Array, Error, Function, Date, etc.
  show : function(value) {
     if (DEBUG.is_xmlnode(value)) return xmldump(value)
     else return LINKS.stringify(value)
  }
  }
}();

var AJAX = function() {
  // (IE) XMLHttpRequest is an ActiveXObject in IE
  return {
    isLoaded : 2,
    isComplete : 4,

  newRequest : function() {
    var http_request;
    if(window.XMLHttpRequest) { // native XMLHttpRequest object (FireFox, etc.)
      try {
        http_request = new XMLHttpRequest(); // makes garbage
      } catch(e) {
        throw ("Failed to create (native) XMLHttpRequest");
      }
    } else if(window.ActiveXObject) { //IE
      try {
          http_request = new ActiveXObject("Msxml2.XMLHTTP"); // makes garbage
      } catch(e) {
        try {
          http_request = new ActiveXObject("Microsoft.XMLHTTP"); // makes garbage
        } catch(e) {
          throw ("Failed to create (ActiveX) XMLHttpRequest object");
        }
      }
    }
    return http_request;
  }
  };
}();


/* Functions for handling the websocket connection to the server. */
var WEBSOCKET = function() {
  return {
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

    connect_if_required : function(state) {
      if ("ws_conn_url" in state) {
        WEBSOCKET.connect(WEBSOCKET.make_uri(state.ws_conn_url));
      } else {
         _debug("No ws_conn_url in JSON state; not connecting to websocket server");
      }
    },

    connect : function(ws_uri) {
      DEBUG.assert(_client_id != undefined,
                   "Trying to start websocket connection with undefined client ID");

      _debug("Connecting to websocket at address " + ws_uri);
      _socket = new WebSocket(ws_uri);

      /* Set up all of the event handlers */
      _socket.onopen = function(evt) {
        WEBSOCKET.onOpen(evt);
      };

      _socket.onclose = function(evt) {
        WEBSOCKET.onClose(evt);
      };

      _socket.onerror = function(evt) {
        WEBSOCKET.onError(evt);
      };

      _socket.onmessage = function(evt) {
        WEBSOCKET.onMessage(evt);
      }

    },

    onOpen : function(evt) {
      _debug("Successfully opened websocket connection.");
      WEBSOCKET.is_connected = true;
      WEBSOCKET.drain_buffer();
    },

    onClose : function(evt) {
      // TODO: Fancier handling of errors here.
      // Should also make these prints as opposed to debugs?
      _debug("Lost connection to the server. Please refresh the page.");
      WEBSOCKET.is_connected = false;
    },

    onError : function(evt) {
      _debug("Error encountered when connecting to server. Please refresh the page.");
      WEBSOCKET.is_connected = false;
    },

    onMessage : function(evt) {
      _debug("Received message " + evt.data);
      var js_parsed = JSON.parse(evt.data);
      if (DEBUG.is_object(js_parsed)) {
        if ("opcode" in js_parsed) {
          switch (js_parsed.opcode) {
            case "MESSAGE_DELIVERY":
              _debug("In message delivery case");
              var local_pid = js_parsed.dest_pid;
              var msg = js_parsed.val;
              LINKS.deliverMessage(local_pid, msg);
              break;
            case "AP_RESPONSE":
              var blocked_pid = js_parsed.blocked_pid;
              var chan = js_parsed.chan;
              _returned_channels[blocked_pid] = chan;
              _wakeup(blocked_pid);
              break;
            case "SESSION_MESSAGE_DELIVERY":
              var ep_id = js_parsed.ep_id;
              var msg = js_parsed.msg;
              deliverSessionMessage(ep_id, msg);
              break;
            default:
              _debug("Unhandled message: " + JSON.stringify(evt.data));
              break;
          }
        }
      }
    },

    serialise_and_send : function(msg) {
      _socket.send(JSON.stringify(msg));
    },

    drain_buffer : function() {
      while (WEBSOCKET.buffer.length > 0) {
        var msg = WEBSOCKET.buffer.pop();
        _debug("Sending buffered message: " + JSON.stringify(msg));
        WEBSOCKET.serialise_and_send(msg);
      }
    },

    try_send : function(msg) {
      // If we're connected, send along. Otherwise add to the buffer.
      if (WEBSOCKET.is_connected) {
        _socket.send(JSON.stringify(msg));
      } else {
        _debug("No connection yet; buffering message " + JSON.stringify(msg));
        WEBSOCKET.buffer.unshift(msg);
      }
    },

    sendRemoteClientMessage : function(destClientId, destPid, msg) {
      _debug("Sending message bound for client " + destClientId +
             ", process " + destPid + ", msg: " + JSON.stringify(msg));

      var to_send_json =
        { opcode: "CLIENT_TO_CLIENT", destClientId: destClientId,
          destPid : destPid, msg: msg};
      WEBSOCKET.try_send(to_send_json);
    },

    sendRemoteServerMessage : function(destServerPid, msg) {
      _debug("Sending message bound for server process " + JSON.stringify(destServerPid) + ", msg: "
             + JSON.stringify(msg));

      var to_send_json =
        { opcode: "CLIENT_TO_SERVER", destPid: destServerPid, msg: msg};
      WEBSOCKET.try_send(to_send_json);
    },

    sendRemoteAPRequest : function(current_pid, remote_apid) {
      _debug("Requesting a connection from remote AP " + JSON.stringify(remote_apid));
      var to_send_json =
        { opcode: "SERVER_AP_REQUEST", blockedClientPid: current_pid, serverAPID: remote_apid};
      WEBSOCKET.try_send(to_send_json);
    },

    sendRemoteAPAccept : function(current_pid, remote_apid) {
      _debug("Accepting a connection from remote AP " + JSON.stringify(remote_apid));
      var to_send_json =
        { opcode: "SERVER_AP_ACCEPT", blockedClientPid: current_pid, serverAPID: remote_apid};
      WEBSOCKET.try_send(to_send_json);
    },

    sendRemoteSessionMessage : function (channel, delegated_sessions, val) {
      var remote_ep = channel._sessEP1;
      _debug("Sending value " + (JSON.stringify(val)) + " to remote endpoint " + remote_ep);
      var to_send_json =
        { opcode: "REMOTE_SESSION_SEND", remoteEP: remote_ep,
          delegatedSessions: delegated_sessions, msg: val };
      WEBSOCKET.try_send(to_send_json);
    }
  }

}();


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

var LINKS = function() {

  // A package of functions used internally, not callable from Links code.

  // BEGIN private LINKS vars/methods

  var _formkey = null;

  var _removeCGIArgs = function(str) {
    return str.replace(/\?.*/, "");
  }

  // Continue a thread at server after a client call has finished.
  // _kappa is our local (client) continuation, for use when the
  // server is really finished, and _continuation is the server-side
  // continuation which the server asked client to invoke it with.
  var _remoteContinue = function (_kappa, _continuation, _mailbox,
                                  _synch) {
    return function (_res) {
      _debug("Continuing at server with value \"" + _res + "\" and continuation " +
             _continuation);
      var _request = AJAX.newRequest();
      var _rootURL = _removeCGIArgs(location.href);
      _request.open('POST', _rootURL, !_synch);
      if (!_synch)
        _request.onreadystatechange = _remoteCallHandler(_kappa, _request, _synch);
      _request.setRequestHeader('Content-Type',
                                'application/x-www-form-urlencoded');

      _request.pid = _current_pid;

      var _resultJSON = LINKS.stringify(_res);
      var _mailboxJSON = LINKS.stringify(_mailbox);
      _request.send("__continuation=" + _continuation +
                    "&__result=" + LINKS.base64encode(_resultJSON) +
                    "&__client_id=" + LINKS.base64encode(_client_id)
                   );
      if (_synch) {
        _remoteCallHandler(_kappa, _request, _synch)();
      }
    }
  }

  // resolve the JSON state for a top-level client program
  function _resolveJsonState(state, handlers) {
    for (var i in handlers) {
      var h = handlers[i];
      h.clientKey = _registerMobileKey(state, h.key);

      // update nodes with the client keys
      var nodes = document.querySelectorAll("[key=\"" + h.key + "\"]");
      for (var j = 0; j < nodes.length; ++j)
        nodes[j].setAttribute("key", h.clientKey);
    }
  }

  // register event handlers and spawn processes captured by the JSON
  // state for a top-level client program
  function _activateJsonState(state, client_id, processes, handlers, aps) {
    // set client ID
    _debug("Setting client ID to " + client_id);
    _client_id = client_id;
    // register event handlers
    for (var i in handlers) {
      var h = handlers[i];
      var hs = eval(h.eventHandlers);
      resolveServerValue(state, hs);
      _registerMobileEventHandlers(h.clientKey, hs);
    }

    // resolve and create mobile access points
    // needs to be done before processes, since processes may (will!) reference
    for (var i in aps) {
      var ap = aps[i];
      newWithID(ap);
    }

    // resolve and spawn the mobile processes
    for (var i in processes) {
      var p = processes[i];
      resolveServerValue(state, p);
      _spawnWithMessages(p.pid, p.process, p.messages);
    }
  }

  // resolve, spawn, and register, serialised client processes
  // received from the server
  //
  // it is important to do this is two stages as the process and message
  // values may themselves reference the mobile processes which must
  // have been registered
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
  }


  // Resolve function references in the object _obj, specified as records
  // {function:f, environment:e}, where the environment is optional. If
  // an environment is specified, we assume that the function denoted by
  // f is actually a wrapper and that f(e) is the desired function.
  // Without an environment, f itself denotes the desired function, a
  // standard CPS compiled Links function. This is recursive, so each
  // object in _obj also has its functions resolved.
  function resolveServerValue(state, _obj) {
    if (_obj instanceof Object) {
      for (var i in _obj) {
         resolveServerValue(state, _obj[i]);
         if (_obj[i].func) {
           // _debug("resolving " + _obj[i].func);
           // _debug(eval(_obj[i].func));
           // _debug("in environment " + _obj[i].environment);
           // _debug(eval(_obj[i].environment));

           var f;

           if (!DEBUG.is_object(_obj[i].environment)) {
             //_debug("Note: environmentless function resolved");
             f = eval(_obj[i].func);
           } else {
             f = partialApply(eval(_obj[i].func), eval(_obj[i].environment));
           }
           f.location = _obj[i].location; // This may be set to 'server' by the server serializer.
           f.func = _obj[i].func;
//           f.environment = _obj[i].environment;

           _obj[i] = f;
         } else if (_obj[i].key) {
           _obj[i].key = _lookupMobileKey(state, _obj[i].key)
         // } else if (_obj[i].eventHandlers) {
         //   var hs = eval(_obj[i].eventHandlers);
         //   // is the following line necessary?
         //   resolveServerValue(state, hs);
         //   delete _obj[i].eventHandlers;
         //   _obj[i].key = _registerEventHandlers(hs);
         }
         // Removed: PID resolution.
      }
    }
  }

  // ___append: return a new array with the elements of xs followed by the
  // single item x
  function ___append(xs, x) {
    var out = [];
    for (var i = 0; i < xs.length; i++) {
      out[i] = xs[i];
    }
    out[i] = x;
    return out;
  }

  // Perform a client call as specified in _callPackage, then re-invoke
  // the server using _remoteContinue
  // NOTE: variables defined within this function could shadow
  // the Links function we're trying to execute. Hence all local
  // vars are prefixed with underscore. Beware also of package variables
  // above shadowing.
  var _invokeClientCall = function (_kappa, _callPackage, _synch) {
    _debug('Invoking client call to ' + _callPackage.__name + '.');
    _debug('arguments: ' + _callPackage.__args);
    _debug('arguments: ' + LINKS.stringify(_callPackage.__args));

    //   FIXME: the eval is redundant, because done in
    //   _remoteCallHandler; also this name may actually be a
    //   closure-table reference, expecting "request" to be defined.
    var _f = eval(_callPackage.__name);

    var args = _callPackage.__args;
    var k = _remoteContinue(_kappa, _callPackage.__continuation,
                           _mailboxes[_current_pid] || [],
                           _synch);

    _yield(function () {return _f.apply(_f, ___append(args, k))});
  }

  // _remoteCallHandler is the trampoline that tunnels symmetrical
  //   client-server calls over the request/response link.
  var _remoteCallHandler = function (kappa, request, synch) {
    return function() {
      if (request.readyState == AJAX.isComplete && !request.finished) {
       _current_pid = -99;        // We're in no process until kappa is invoked.
       // The 'finished' field guards against the callback being called more
       // than once on the same request object, an anomaly observed by EEKC.
       request.finished = true;

       _debug("Server response: " + LINKS.base64decode(request.responseText));

       var serverResponse = LINKS.parseB64Safe(request.responseText);
       if (!serverResponse) throw "Fatal error: nonsense returned from server.";

       // any state that we need for resolving values
       // (currently just a mapping between server and client pids)
       var state = {mobileKeys: {}};

       resolveMobileState(state, serverResponse.content.state.processes, serverResponse.content.state.handlers);

       var box = {content: serverResponse.content.value};
       resolveServerValue(state, box);
       var serverValue = box.content;

       _debug("Server response decoded: "); _dump(serverValue);

       // Check whether we are bouncing the trampoline with a client call
       //   or continuing with a final result.
       // TBD: Would be more elegant to use JS constructors instead of
       //   using a signal member like __continuation.

       if ((serverValue instanceof Object)
           && ('__continuation' in serverValue)) {
          // Bouncing the trampoline

          _debug("Client function name, before evaluation, is " +
                 serverValue.__name);

          _current_pid = request.pid;
          _invokeClientCall(kappa, serverValue, synch);
        } else {
          _debug("Client continuing after remote server call, value " +
                 serverValue);
          // it's the final result: return it.

          kappa(serverValue);
        }
      }
    }
  };

  var nextFuncID = 0;

  function replacer(key, value) {
      _debug("In replacer with key: " + key);
      _debug("typeof value: " + typeof value);
      _debug("value: " + value);
      if (typeof value === 'function') {
        // _debug("replacing function");
        if (value.location == 'server') {
          return {_serverFunc:value.func, _env:value.environment};
        }
        var id = nextFuncID++;
        _closureTable[id] = function (_env) { return value };
        return {_closureTable:id};
      }
      // SL: HACK for sending XML to the server
      else if (key !== '_xml' &&
                 typeof value === 'object' &&
                 value instanceof Array &&
                   (value.length == 2 && value[0] == 'TEXT' ||
                    value.length == 4 && value[0] == 'ELEMENT')) {
        return {_xml:value}
      }
      return value;
  };

  //// BEGIN public LINKS methods.
  var LINKS = {
    resolveJsonState : function (s) {
      var state = {mobileKeys: {}};
      _resolveJsonState(state, s.handlers);
      return state;
    },

    activateJsonState : function (state, s) {
      _activateJsonState(state, s.client_id, s.processes, s.handlers, s.access_points);
    },

    resolveValue : function (state, v) {
      resolveServerValue(state, v);
    },

    // JS uses UCS2 internally.
    // The (un)escape / URI nonsense converts back and forth between UCS2 and UTF-8
    // The btoa / atob methods convert back and forth between UTF-8 and base 64.
    base64encode : function (s) {
      return window.btoa(unescape(encodeURIComponent(s)));
    },

    base64decode : function (s) {
      return decodeURIComponent(escape(window.atob(s)));
    },

    unimpl : function(name) {
      throw "Fatal error: function '" + name + "' not available on client."
    },

    // _continuationize
    //   Turns a direct-style js function into a continuationized one under the
    //   Links calling conventions. "trivial" means it cannot call back to a
    //   Links function, and that the scheduler can safely be suspended
    //   while it runs.
    kify : function kify(f) {
      return function () {
         // Nota bene: arguments is not a real array, hence no .pop()
         var kappa = arguments[arguments.length-1];
         var args = Array.prototype.slice.call(arguments, 0, arguments.length-1);
         return kappa(f.apply(f, args));
      };
    },
    //function _kifyBinaryCurried(f) {
    //  return function () {
    //    var args1 = arguments[0];
    //    var kappa = arguments[1];
    //    return kappa(
    //      function () {
    //        var args2 = arguments[0];
    //        var kappa = arguments[1];
    //        return kappa(f.apply(f, [args1]).apply(f, [args2]));
    //      }
    //    );
    //  }
    //}
    //
    //function _kifyMethod(obj, method) {
    //  return function (kappa) {
    //    return function () {
    //      return kappa(method.apply(obj, arguments));
    //    };
    //  };
    //}

    //function _makeUrlArgs() {
    //  var result = '';
    //  for (var i = 0; i < arguments.length; i++) {
    //    if (typeof(arguments[i][1]) != 'function') {
    //      DEBUG.assert(typeof(arguments[i][1]) != 'function',
    //                   "Cannot marshal function value (" +
    //                   arguments[i][0] + ") to send to the server");
    //      var name = arguments[i][0];
    //      var val = arguments[i][1];
    //      result += '&' + name + '=' + escape(val);
    //    }
    //  }
    //  return result;
    //}

    // [mapStrCat]
    // function mapStrCat(f, glue, l) {
    //   if (l.length <= 0) return '';
    //   temp = f(l[0]);
    //   for (var i = 1; i < l.length; i++) {
    //     temp += glue + f(l[i]);
    //   }
    //   return temp;
    // }

    isEmpty : function(list) { return (list.length == 0); },

    eq : undefined,

    jsStrConcat : function(s1, s2) { return s1 + s2; },

    //  _concat(a, b)
    //     Concatenate two lists
    concat : function (l,r) {  return l.concat(r); },

    // _concatList(xss)
    //    Concatenate all the lists in xss
    concatList : function(list) {
      return [].concat.apply([],list);
    },

    // NOTE: accum is replaced by concatMap in prelude.
    // accumAux : undefined,

    //  accum(f, i)
    //    concatMap: apply f to every element of `list'
    //    and concatenate the results.
    // accum : function(fn, list, kappa) {
    //   DEBUG.assert(DEBUG.is_array(list),
    //                ("source for list comprehension was not a list, it was "+
    //                 DEBUG.type(list)));
    //   LINKS.accumAux(fn, list, 0, [], kappa);
    // },

    //// LINKS.singleXmlToDomNodes
    ////   (NOTE: this is recursive)
    singleXmlToDomNodes: undefined,

    map : function(f, list) {
      var result = [];
      for (var i = 0; i < list.length; i++) {
        result[i] = f(list[i]);
        //delete list[i];
      }
      //list = null;
      return result;
    },

    XmlToDomNodes : function (xmlForest) {
      DEBUG.assert(DEBUG.is_array(xmlForest),
                   'LINKS.XmlToDomNodes expected an array, but got ' +
                   xmlForest);
      return LINKS.map(LINKS.singleXmlToDomNodes(null), xmlForest);
    },

    /// XML
    //  XML(tag, attrs, children)
    //    create a DOM node with
    //    element tag name `tag'
    //          attributes `attrs' (a dictionary)
    //            children `children' (a sequence of DOM nodes)
    // XML : (string, (string, [char]) map, Xml, cont) -> Xml
    XML : function (tag, attr, body) {
      return [["ELEMENT", tag, attr, [].concat.apply([],body)]]
    },

    // yucky CPS XML function used by the old JS compiler
    XMLk : function (tag, attr, body, kappa) {
      kappa([["ELEMENT", tag, attr, [].concat.apply([],body)]])
    },

    // Records

    //   _union(r, s)
    //   compute the union of dictionaries r and s
    //   precondition: r and s are disjoint
    union : function(r, s) {
      var result = {};
      for (var label in r) {
         result[label] = r[label];
      }
      for (var label in s) {
        result[label] = s[label];
      }
      return result;
    },


    // _project(object, name)
    // project a field of a record
    project : function(object, name) { return object[name]; },

    // _erase(object, name)
    // erase a field of a record
    //
    // Unfortunately we can't elide erase otherwise equality will break.
    erase : function (object, names) {
      var result = {};
      for (fld in object) {
        var copy = true;
        for (name in names) {
          if (fld == name) {
            copy = false;
            break;
          }
        }
        if (copy)
          result[fld] = object[fld];
      }
      return result;
    },

    vrntLbl : function (object) { return object['_label']},
    vrntVal : function (object) { return object['_value']},

    deliverMessage : function(pid, msg) {
        if (!_mailboxes[pid]) {
          _makeMailbox(pid);
        }
        _mailboxes[pid].unshift(msg);
        _wakeup(pid);
        _debug(pid + ' now has ' + _mailboxes[pid].length + ' message(s)');
      },

    //// Remote calls

    remoteCall : function(kappa) {
     DEBUG.assert_noisy(DEBUG.is_function(kappa),
       "remoteCall given non-function as continuation");
     return function(name, env, arguments) {
       _debug ("Making remote call to: " + name);
       var synchronous = false;
       // synchronous XMLHttpRequest is deprecated. Do we ever really need it?
       //
       //var synchronous = (_current_pid == _mainPid); || _handlingEvent;
       var current_pid = _current_pid;

       // setpid_kappa: Re-establish the process identifier and continue
       // with kappa.
       var setpid_kappa = function (response) {
         _current_pid = current_pid;
         kappa(response)
       }

       var request = AJAX.newRequest();

       // Posting to location.href works in both Firefox and IE
       // (unlike posting to '#', which IE mistakenly urlencodes as %23)
       request.open('POST', location.href, !synchronous);
       request.setRequestHeader('Content-Type',
                                'application/x-www-form-urlencoded');

       // TBD: make request.funcs a parameter of remotecallhandler
       // instead of stuffing it in the request.
       if (!synchronous)
         request.onreadystatechange =
                _remoteCallHandler(setpid_kappa, request, synchronous);

       request.pid = _current_pid;
       var argsJSON = LINKS.stringify(arguments);

       // TODO: get rid of env - this should be handled by closure conversion

       if (!env) env = {};
       var envJSON = LINKS.stringify(env);
       // request.funcs = _compose(argsJSON.funcs, envJSON.funcs);

       var argString =
         "__name=" + LINKS.base64encode(name) +
         "&__args=" + LINKS.base64encode(argsJSON) +
         "&__env=" + LINKS.base64encode(envJSON) +
         "&__client_id=" + LINKS.base64encode(_client_id)

       for (var i = 0; i < cgiEnv.length; ++i) {
         argString = argString + "&" + cgiEnv[i][1] + "=" + cgiEnv[i][2];
       };

       request.send(argString);

       if (synchronous)
         return _remoteCallHandler(setpid_kappa, request, synchronous)();
     }
    },

    // fieldVal
    //   return the input value for the
    //   input field whose name is 'name' in the current form
    //   (identified by _formkey)
    fieldVal : function(name) {
      var forms = document.getElementsByTagName('form');
      var containingForm = null;

      // find the containing form
      for (var i = 0; i < forms.length; ++i) {
        var key = forms[i].getAttribute('key');
        if(key == _formkey) {
          containingForm = forms[i];
          break;
        }
      }

      if(!containingForm) {
        DEBUG.assert(false, "Form does not exist!")
      }

      // find the input value
      var xs = document.getElementsByName(name);
      for(var i = 0; i < xs.length; ++i) {
        var node = xs[i];
        while(node) {
          if(node == containingForm) {
             return xs[i].value;
          }
          node = node.parentNode;
        }
      }

      DEBUG.assert(false,
                   "Form element with name '" + name +"' does not exist!");
    },

    // appDom
    //   apply f to every node in the DOM tree rooted at root
    //
    //   NOTE:
    //   appDom is deliberately defined non-recursively as
    //   JavaScript implementations have very ropey support
    //   for recursive functions.
    //
    //   It is implemented as a state machine that traverses
    //   the tree.
    appDom : function (root, f) {
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
    },

    // activateHandlers
    //   bind all the handlers registered to this key to this DOM node
     activateHandlers : function (node) {
      if(!isElement(node))
        return;

      function activate(node) {
        //_debug("node: "+node+"("+node.childNodes.length+")")
        if(!isElement(node))
          return;

        var key = node.getAttribute('key');
        if(key != null) {
          var hs = _eventHandlers[key];
          for(var lAttrName in hs) {
            listenerElem = lAttrName.match(/page$/) ? document.documentElement : node;
            var handlerName = lAttrName.replace(/page$/, "");
            var eventName = handlerName.replace(/^on/, "");
            // _debug("installing event handler "+eventName+"; for node: "+key);
            jslib.event.addListener(listenerElem, eventName,
              function (key, name){
                return function (e) {
                  // _alert("firing event: "+name+ " on elem with key=" + key);
                  // TBD: clone the event record.
                  _formkey = key;
                  _eventHandlers[key][name](e);
                  // make sure this event isn't handled by anyone else
                  jslib.event.stopEvent(e);
      	          return false;
                }
              }(key, lAttrName)
            );
          }
        }
      }
      LINKS.appDom(node, activate);
    },

    // stringify: JFATHER.stringify,
    // stringifyB64: JFATHER.stringifyB64
    // parseB64: JFATHER.parseB64,
    // parseB64Safe: JFATHER.parseB64Safe

    stringify: function (v) {
        _debug("stringifying: " + JSON.stringify(v));
        var t = JSON.stringify(v, replacer);
        _debug("stringified: " + t);
        if (typeof t == 'string') {
          return t;
        }
        throw("Internal error: unable to JSONize " + v);
    },

    stringifyB64: function(v) {
        return LINKS.b64encode(LINKS.stringify(v));
    },


    parseB64: function(text) {
        var s = LINKS.base64decode(text);
        var v = JSON.parse(s);
        return {content: v};
    },

    parseB64Safe: function(text) {
        return LINKS.parseB64(text.replace('\n', ''));
    }


   //// END of non-recursive LINKS methods.
  };

  //// BEGIN recursive LINKS methods.
  //LINKS.accumAux = function (fn, list, i, result, kappa) {
  //    if (i >= list.length) kappa(result)
  //    else {
  //      h = list[i];
  //      _debug("calling " + fn + " with " + h);
  //      _yield(fn, [h],
  //             function(himg) {
  //               LINKS.accumAux(fn, list, i+1, result.concat(himg), kappa)
  //             } );
  //    }
  //  };

  LINKS.singleXmlToDomNodes = function (namespace) {return function (xmlObj) {
    DEBUG.assert(DEBUG.is_array(xmlObj),
                 'LINKS.singleXmlToDomNodes expected an array, but got ' + xmlObj);
    if (xmlObj[0] == "ELEMENT") {
      var tag = xmlObj[1];
      var attrs = xmlObj[2];
      var body = xmlObj[3];

      var node = null;
      if(attrs['xmlns'])
        namespace = attrs['xmlns'];

      // HACK: this allows us to provide some support for content such as SVG
      if (namespace) {
        node = document.createElementNS(namespace, tag);
      } else {
        node = document.createElement(tag);
      }

      // (IE) IE doesn't allow children to be appended to a style element
      if (isElementWithTag(node, "style") &&
          (node.styleSheet || (""+node.styleSheet == "null")))
      {
        //if(_cssText)
        //  throw ("only one style element allowed by IE")

        node.type = 'text/css';

        var cssText = "";
        for (var i = 0; i < body.length; ++i) {
          if(body[i][0] == "TEXT") {
            cssText += body[i][1]
          } else if(body[i][0] == "ELEMENT") {
            throw "element node " + xmlObj[0] +
                  " in style element (LINKS.singleXmlToDomNodes: IE style hack)"
          } else {
            throw "unknown XML node " + xmlObj[0] +
                  " in LINKS.singleXmlToDomNodes (IE style hack)"
          }
        }
        // [HACK]
        //   the cssText has to be stored in a global as it
        //   cannot be set until the node has actually been installed in the DOM!
        _cssText = cssText;
        return node;
      }

      for (var name in attrs) {
        if(name == 'style' && node.style) {
          // (IE) preserve style attributes in IE
          node.style.cssText = attrs['style'];
        } else {
          node.setAttribute(name, attrs[name]);
        }
      }
      for (var i = 0; i < body.length; i++) {
        var child = LINKS.singleXmlToDomNodes(namespace)(body[i]);
        node.appendChild(child);
      }
      return node;
    } else if (xmlObj[0] == "TEXT"){
      return document.createTextNode(xmlObj[1]);
    } else {
      throw "unknown XML node " + xmlObj[0] + " in LINKS.singleXmlToDomNodes"
    }

  }};

  LINKS.eq = function(l,r) {
    if (l == r)
      return true;

    if (l == null)
      return (r == null);
    else if (r == null)
      return false;

    if (DEBUG.is_unit(l) && DEBUG.is_unit(r))
      return true;

    if (DEBUG.is_array(l) && l != null &&
        DEBUG.is_array(r) && r != null) {
      if (l.length != r.length)
        return false;

      for (var i = 0; i < l.length; ++i) {
        if (!LINKS.eq(l[i], r[i])) return false;
      }

      return true;
    }
    else if(typeof(l) == 'object' && l != undefined && l != null &&
            typeof(r) == 'object' && r != undefined && r != null) {
      if(l.constructor != r.constructor)
        return false;

      // DODGEYNESS:
      //   - it isn't clear that structural equality is always the same as
      //   referential equality for DOM nodes
      return LINKS.eqAux(l, r);
    }
    return false;
  };

  // supposedly this prevented LINKS.eq to be optimized,
  // because the p in for(p in x) is non-local -
  // so we put it in an auxiliary function
  LINKS.eqAux = function(l,r) {
      for(p in l) {
        if(!LINKS.eq(l[p], r[p])) {
          return false;
        }
      }
      for(p in r) {
        if(!LINKS.eq(l[p], r[p]))
          return false;
      }
      return true;
  };

  return LINKS;
} ();

function _debug(msg) {
   if (DEBUGGING) {
     if(DEBUG.is_charlist(msg))
       _dwindow.document.write('<b>' + _current_pid + '</b> : ' + msg + '<br/>');
     else
       _dwindow.document.write('<b>' + _current_pid + '</b> : ' + msg + '<br/>');
   _dwindow.scroll(0, _dwindow.scrollMaxY);
   }
}

var debug = LINKS.kify(_debug);

function _alertDialog(msg) {
//   DEBUG.assert(DEBUG.is_charlist(msg), "_alertDialog expected charlist, got: "
//                  + msg);
  return (alert(msg));
}
var alertDialog = LINKS.kify(_alertDialog);

function _tilde(s, regex) {
//    _debug("string: " + s);
    var r = Regex.compile(regex);
//    _debug("compiled regex: " + r);
    return (new RegExp(r)).test(s); // makes garbage
}
var tilde = LINKS.kify(_tilde);

var _intToString = function (x) { return String(x) }
var _stringToInt = function (x) { return parseInt(x) }
var _intToFloat = Number;
var _floatToInt = Math.floor;
var _floatToString = function (x) { return String(x) }
var _stringToFloat = function (x) { return parseFloat(x) }

var intToString = LINKS.kify(_intToString);
var stringToInt = LINKS.kify(_stringToInt);
var intToFloat = LINKS.kify(_intToFloat);
var floatToInt = LINKS.kify(_floatToInt);
var floatToString = LINKS.kify(_floatToString);
var stringToFloat = LINKS.kify(_stringToFloat);

function _Concat(xs, ys) { return LINKS.concat(xs, ys); }
function _Cons(x, xs) { return _Concat([x], xs); }

function _not(x) { return !x; } // should be inlined
function _empty(list) { return (list.length == 0); }
function _hd(list) { return list[0]; }
function _tl(list) { return list.slice(1); } // makes garbage

function _length(list) { return list.length }
function _take(n, list) { return list.slice(0, n) } // makes garbage
function _drop(n, list) { return list.slice(n) } // makes garbage

// FIXME: _max and _min rely on '<' and '>', which
// may not do the right thing for non-primitive types
// (of course, we really want something like type classes
// in order to be able to handle this kind of situation
// more robustly)

function _max(list) {
  if(list.length == 0)
    return {'_label':'None'}
  else {
    var x = list[0];
    for(i = 1; i < list.length; i++) {
      if (list[i] > x)
        x = list[i];
    }
    return {'_label':'Some', '_value':x}
  }
}

function _min(list) {
  if(list.length == 0)
    return {'_label':'None'}
  else {
    var x = list[0];
    for(i = 1; i < list.length; i++) {
      if (list[i] < x)
        x = list[i];
    }
    return {'_label':'Some', '_value':x}
  }
}

var Nil    = [];
var Cons   = LINKS.kify(_Cons);
var Concat = LINKS.kify(_Concat);

var not    = LINKS.kify(_not);
var empty  = LINKS.kify(_empty);
var hd     = LINKS.kify(_hd);
var tl     = LINKS.kify(_tl);

var length = LINKS.kify(_length);
var take   = LINKS.kify(_take);
var drop   = LINKS.kify(_drop);
var max    = LINKS.kify(_max);
var min    = LINKS.kify(_min);

function _charAt(s, i) {
  return {_c:s.charAt(i)}
}

function _strlen(s) {
  return s.length;
}

function _strsub(s, start, len) {
  return s.substr(start, len);
}

function _explode(s) {
  var cs = [];
  for (var i = 0; i < s.length; ++i) {
    cs.push({_c:s.charAt(i)});
  }
  return cs;
}

function _implode(cs) {
  DEBUG.assert(DEBUG.is_array(cs),
               "_implode expected an array, got: " + cs);
  var s = "";
  for (var i in cs) {
    s += cs[i]._c;
  }
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
    _debug(obj + " : undefined");
  } else {
    _debug(obj + " : " + typeof(obj) + ' ' +
          (typeof(obj) == 'object' ? obj.constructor : ''));
  }
  return [];
}
var debugObj = LINKS.kify(_debugObj);

function _dump(obj) {
  if (obj == undefined)
    _debug(obj + " : undefined");
  else {
    _debug("==TYPE== " + typeof(obj) + " " +
      (typeof(obj) == 'object' ? obj.constructor : ""));
    if (typeof(obj) == 'object') {
      for (var i in obj) {
        try {
          _debug(i + "=" + obj[i]);
        } catch (e) {
          _debug(i + " (died)");
        }
      }
    } else
       _debug(obj);
  }
}
var dump = LINKS.kify(_dump);

function _negate(x) { return -x }
var negate = LINKS.kify(_negate);

var _negatef = _negate;
var negatef = negate;

function _error(msg) {
  alert(msg);
  throw ("Error: " + msg);
}

var error = LINKS.kify(_error);

// partialApply : ((a0, a1, ..., an) -> b, a0) -> (a1, ..., an) -> b
// the partialApply function is used to construct closures
function partialApply(f, x) {
    return function () {
        f.apply(this, [x].concat(Array.prototype.slice.call(arguments)))
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
  return {}
}

//appendChildXml : xml -> domRef -> ()
function _appendChildren(appendXml, parentNode) {
  var nodes = LINKS.XmlToDomNodes(appendXml);
  for (var i=0; i < nodes.length; i++) {
    parentNode.appendChild(nodes[i]);
    LINKS.activateHandlers(nodes[i]);
  }
  return {}
}

//removeNode : domRef -> ()
function _removeNode(nodeRef) {
  if(nodeRef.parentNode)
    nodeRef.parentNode.removeChild(nodeRef);
  else
    throw ("Cannot remove DOM root node");

  return {}
}

function _cloneNode(nodeRef, deep) {
  return nodeRef.cloneNoe(deep);
}

//replaceNode : (xml, domRef) -> ()
function _replaceNode(withXml, replaceNode) {
  _insertBefore(withXml, replaceNode);
  _removeNode(replaceNode);
  return {}
}

//replaceDocument : xml -> ()
var replaceDocument = LINKS.kify(_replaceDocument);


// WARNING: insertBeforeRef MOVES a DOM node
//insertBeforeRef : domRef -> domRef -> ()
function _domInsertBeforeRef(insertNode, beforeNode) {
  var parent = beforeNode.parentNode;
  parent.insertBefore(insertNode, beforeNode)
  LINKS.activateHandlers(insertNode);
  return {}
}

//appendChildRef : domRef -> domRef -> ()
function _domAppendChildRef(appendNode, parentNode) {
  parentNode.appendChild(appendNode);
  LINKS.activateHandlers(appendNode);
  return {}
}

//getDocRef : () -> domRef
function _getDocumentNode() {
  return document.documentElement;
}

//getRefById : string -> domRef
function _getNodeById(id) {
//   DEBUG.assert_noisy(DEBUG.is_array(id),
//                      "_getNodeById Expected an array, got: " + id);
  ref = document.getElementById(id);

//  if (!ref) _alert("element " + id + " does not exist");
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


//// XML datatype manipulation.
// The Xml representation in JavaScript looks like this:
//   [["ELEMENT", String, [(String, [Char])], Xml]]
//   [["TEXT", String]]

// (IE)
//   IE calls node.textContent "node.data"
function _nodeTextContent(node) {
  if(node.textContent)
    return node.textContent;
  else
    return node.data;
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
    return [["TEXT", _nodeTextContent(nodeRef)]]
  } else if (nodeRef.nodeType == document.ELEMENT_NODE ) {
    var children = [];
    for (var i=0; i < nodeRef.childNodes.length; i++) {
      children = children.concat(_getValue(nodeRef.childNodes[i]));
    }
    var attrs = {};
    for (var i=0; i < nodeRef.attributes.length; i++) {
      attrs[nodeRef.attributes[i].name] =
        nodeRef.attributes[i].value
    }
    for (var i=0; i < children.length; i++) {
      DEBUG.assert(children[i][0] == "ELEMENT" || children[i][0] == "TEXT",
                   "Invalid children list constructed in DOMNodeToXML")
    }
    var result = [["ELEMENT", nodeRef.tagName, attrs, children]];
    return result;
  } else {
    throw("Unknown node type " + nodeRef.nodeType + " in GetXml")
  }
}
getValue = LINKS.kify(_getValue);

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
  return nodeRef.setAttribute(attr,
                              value);
}

function _domRemoveAttributeFromRef(nodeRef, attr) {
  nodeRef.removeAttribute(attr);
  return null;
}

function _domSetStyleAttrFromRef(nodeRef, attr, value) {
  return nodeRef.style[attr] = value;
}

function _domGetStyleAttrFromRef(nodeRef, attr) {
  return nodeRef.style[attr];
}

function _domGetNodeValueFromRef(node) {
  return node.value;
}

function _domSetAnchor(anchorRef) {
  window.location.hash = anchorRef;
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
  DEBUG.assert(x.parentNode != null && y.parentNode != null,
               "cannot swap root nodes");
  DEBUG.assert(x.parentNode != y, "cannot swap a node with its parent");
  DEBUG.assert(y.parentNode != x, "cannot swap a node with its parent");

  var xNextSibling = x.nextSibling;
  var yNextSibling = y.nextSibling;
  var xParent = x.parentNode;
  var yParent = y.parentNode;

  if(xNextSibling != y)
    xParent.insertBefore(y, xNextSibling);

  if(yNextSibling != x)
    yParent.insertBefore(x, yNextSibling);

  return {}
}
var swapNodes = LINKS.kify(_swapNodes);

//replaceChildren : xml -> domRef -> ()
function _replaceChildren(xml, parent) {
  newNodes = LINKS.XmlToDomNodes(xml);

  // OPTIMISATION:
  // innerHTML isn't officially part of the DOM API
  parent.innerHTML = "";

// unoptimised version
//
//  while (parent.hasChildNodes()) {
//    parent.removeChild(parent.firstChild);
//  }

  for(i = 0; i < newNodes.length; i++)
    _domAppendChildRef(newNodes[i], parent);

  return {}
}

var replaceChildren = LINKS.kify(_replaceChildren);

// for server generated event handlers
function _registerMobileEventHandlers(key, handlers) {
  for (var i = 0; i < handlers.length; i++) {
     var event = handlers[i][1];
     var handler =_wrapEventHandler(handlers[i][2]);
     if (!_eventHandlers[key]) {
       _eventHandlers[key] = {};
     }
     _eventHandlers[key][event] = handler;
  }
  return key;
}

function _registerEventHandlers(handlers) {
  var key = '_key' + _get_fresh_node_key();

  for (var i = 0; i < handlers.length; i++) {
     var event = handlers[i][1];
     var handler = _wrapEventHandler(handlers[i][2]);
     if (!_eventHandlers[key]) {
       _eventHandlers[key] = {};
     }
     _eventHandlers[key][event] = handler;
  }
  return key;
}

function _getTarget(event) { return YAHOO.util.Event.getTarget(event, false) }
function _getTargetValue(event) {
  return _getTarget(event).value;
}
function _getTargetElement(event) {
  return YAHOO.util.Event.getTarget(event, true)
}
function _getPageX(event) { return YAHOO.util.Event.getPageX(event) }
function _getPageY(event) { return YAHOO.util.Event.getPageY(event) }
function _getFromElement(event) {
  if(event.type == "mouseover")
    return YAHOO.util.Event.getRelatedTarget(event);
  else if(event.type == "mouseout")
    return YAHOO.util.Event.getTarget(event);
  else
    throw ("Can only get the from element for mouseover and mouseout events");
}
function _getToElement(event) {
  if(event.type == "mouseover")
    return YAHOO.util.Event.getTarget(event);
  else if(event.type == "mouseout")
    return YAHOO.util.Event.getRelatedTarget(event);
  else
    throw ("Can only get the to element for mouseover and mouseout events");
}
function _getTime(event) { return YAHOO.util.Event.getTime(event) }
function _getCharCode(event) { return YAHOO.util.Event.getCharCode(event) }

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
  if (!DEBUG.is_array(obj)) return false;
  if (obj[0] == "ELEMENT" && obj.length == 4) {
    if (!DEBUG.is_string(obj[1])) return false;
    for (i in obj[2]) {
      // TBD: check that attrs are attrs
    }
    for (i in obj[3]) {
      if (!_isXmlItem(obj[3][i])) return false;
      return true;
    }
  } else if (obj[0] != "TEXT" && obj.length == 2) {
    return (DEBUG.is_string(obj[1]));
  } else return false;
}

function _isXml(obj) {
  if (!DEBUG.is_array(obj)) return false;
  for (i in obj) {
    if (!_isXmlItem(obj[i]))
      return false;
  }
}

function _stringToXml(s) {
//  DEBUG.assert(DEBUG.is_array(s), "_stringToXml Expected an array, got: " + s);
  return [["TEXT", s]];
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
  if (xml[0][0] != "ELEMENT")
    throw "getTagName() applied to non-element node";
  return xml[0][1];
}

function _getChildNodes(xml) {
  if (xml[0][0] != "ELEMENT")
    throw "getChildNodes() applied to non-element node";
  return xml[0][3];
}

function _getTextContent(xml) {
  if (xml[0][0] != "TEXT")
    throw "getTextContent() applied to non-text node";
  return xml[0][1];
}

function _getAttributes(xml) {
  if (xml[0][0] != "ELEMENT")
    throw "getAttributes() applied to non-element node";
  var attrs = [];
  for (name in xml[0][2])
    attrs.push({'1':name, '2':xml[0][2][name]})
  return attrs;
}

function _hasAttribute(xml, attrName) {
  if (xml[0][0] != "ELEMENT")
    throw "hasAttribute() applied to non-element node";

  // silly js idiom (this can't be eta-reduced)
  if (xml[0][2][attrName]) return true;
  else return false;
}

function _getAttribute(xml, attrName) {
  if (xml[0][0] != "ELEMENT")
    throw "getAttribute() applied to non-element node";
  return xml[0][2][attrName];
}

// addAttributes : (Xml, [(String, String)]) -> Xml
function _addAttributes(xml, attrs) {
  var xml = xml[0];
  if(xml[0] != "ELEMENT")
    throw "addAttributes() applied to non-element node";

  // copy xml
  var newXml = ["ELEMENT", xml[1], {}, xml[3]];
  for (var name in xml[2])
    newXml[2][name] = xml[2][name];

  // update attributes
  for (var i in attrs) {
    var name = attrs[i][1];
    var value = attrs[i][2];
    newXml[2][name] = value;
  }

  return [newXml];
}

getTagName = LINKS.kify(_getTagName);
getAttributes = LINKS.kify(_getAttributes);
hasAttribute = LINKS.kify(_hasAttribute);
getAttribute = LINKS.kify(_getAttribute);
getChildNodes = LINKS.kify(_getChildNodes);
getTextContent = LINKS.kify(_getTextContent);

function _fail(str) {
  _alert("Internal error: " + str);
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
}

// time in seconds since the beginning of 1970
function _clientTime () { return Date.now(); }
var clientTime = LINKS.kify(_clientTime);

function _dateToLinksDate(d) {
  return {year:d.getFullYear(), month:d.getMonth(), day:d.getDate(),
        hours:d.getHours(), minutes:d.getMinutes(), seconds:d.getSeconds()};
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
}
function _stopTimer() {
  _pageTimer = _clientTime() - _pageTimer;
  _debug("Page drawn in " + _pageTimer + "ms");
}

/// focus stuff
var _focused = null;
function _focus() {
  if (_focused) {
      var y = document.getElementById(_focused);
      if (y) { y.focus(); }
  }
}

// Page update

//  _replaceDocument(tree)
//    Replace the current page with `tree'.
function _replaceDocument(tree) {
  DEBUG.assert(tree != null, "No argument given to _replaceDocument");
  DEBUG.assert(tree[0] != null, "Null tree passed to _replaceDocument");
  DEBUG.assert(tree[0][0] == "ELEMENT",
               "New document value was not an XML element (it was non-XML or was an XML text node).")
  tree = LINKS.XmlToDomNodes(tree);

  // save here
  var _saved_fieldvals = [];
  var inputFields = document.getElementsByTagName("input");
  for (var i = 0; i < inputFields.length; i++) {
     var current = inputFields[i];
     if(current.id != null && current.id != "") // only store fields with an id!
       _saved_fieldvals.push({'field' : current.id, 'value' : current.value});
  }

  // delete the DOM except for the html tag and the body
  // (IE) IE doesn't allow these tags to be deleted
  var d = document.documentElement;
  var body;
  while(d.hasChildNodes()) {
    if(isElementWithTag(d.firstChild, 'body')) {
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
    if(isElementWithTag(p, 'body')) {
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

  return {};
}

function _start(page) {
  _stopTimer();
  _replaceDocument(page)
//  renderPage(page, _replaceDocument)
}

function _startRealPage() {
  var state = LINKS.resolveJsonState(_jsonState);
  _initVars(state); // resolve JSONized values for toplevel let bindings received from the server
  LINKS.activateJsonState(state, _jsonState); // register event handlers + spawn processes
  LINKS.activateHandlers(_getDocumentNode());
  // Create a websocket connection
  WEBSOCKET.connect_if_required(_jsonState);
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
  return function(event) {
    // set process id here
    var active_pid = _current_pid;
    _current_pid = _mainPid;
    _handlingEvent = true;

    var _cont = function () { handler(event, _idy) }
    // A trampoline for use while handling events.
    // Since we don't yield to the browser event loop in event
    // handlers, we quickly run out of stack. To avoid that we
    // throw away the stack periodically by throwing an
    // exception containing the current continuation, which we
    // invoke when the exception is caught.
    for (;;) {
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
    return false;
  }
}

// SL: I think this function is no longer used
function _registerFormEventHandlers(actions) {
   var key = '_key' + _get_fresh_node_key();

   for (var i = 0; i < actions.length; i++) {
     var action = actions[i];
        // FIXME: Shouldn't we need to clone the actions[i] objs?

     //_debug("adding " + action.evName + " to object " + key);
     if (!_eventHandlers[key])
       _eventHandlers[key] = [];
     _eventHandlers[key][action.evName] = _wrapEventHandler(action.handler);
   }

   return (key); // returns the ID to give to the elt
}




// db functions (via remote calls)
// TODO!

var javascript = true;

// identity: a "toplevel" continuation
//
// (this needn't actually return a value as _yield and _yieldcont
// don't actually return values)
//
function _idy(x) { return; }


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
  _debug("--------\nMailbox status:");
  for (var i in _mailboxes) {
    if (_mailboxes[i].length > 0)
      _debug("&nbsp; pid " + i + ": " + _mailboxes[i].length + " msgs waiting");
  }
  var blockedPids = "";
  for (var i in _blocked_procs) {
    if (blockedPids != "") blockedPids += ", ";
    blockedPids += i
  }
  if (blockedPids != "")
    _debug("&nbsp; blocked process IDs: " + blockedPids + ".");
}

function _makeMailbox(pid) {
  if (!_mailboxes[pid])
    _mailboxes[pid] = [];
}

function _freshProcess() {
  _maxPid++;
  var clientPid = "cl_" + _maxPid;
  return clientPid;
}

function _spawnWithMessages(childPid, f, messages) {
  _mailboxes[childPid] = messages;
  setZeroTimeout(function () {
               _debug("launched process #" + childPid);
               _current_pid = childPid;
               f(function () { delete _mailboxes[childPid] })});
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
      DEBUG.assert(false, "Cannot spawn a process on another client");
    }
  } else if ("_serverSpawnLoc" in loc) {
    DEBUG.assert(false, "Cannot spawn process on server from client");
  } else {
    DEBUG.assert(false, "Invalid spawn location " + loc);
  }
}

function _spawnAt(f, loc) {
  check_loc_spawnable(loc);
  var childPid = _freshProcess();
  return _spawnWithMessages(childPid, f, []);
}

function spawnAt(f, loc, kappa) {
   kappa(_spawnAt(f, loc));
}

// TODO: implement this properly
var _spawnAngel = _spawn;
var spawnAngel = spawn;
function _spawn(f) { _spawn(f, _here) }
function spawn(f, kappa) { spawn(f, _here, kappa) }

function _spawnWrapper(env) {  // necessary wrapper for server->client calls
  return spawn;
}

function spawnWait(f, kappa) {
  // f is a zero-argument CPS function
  var parentPid = _current_pid
  var childPid = _freshProcess();
  _mailboxes[childPid] = [];

  _debug("launched process #" + childPid);
  _current_pid = childPid;

  f(function(v) {_current_pid = parentPid; delete _mailboxes[childPid]; kappa(v)});
}

function _self() {
     return _current_pid;
}

function self(kappa) {
     return kappa(_self())
}


function _here() {
  return { _clientSpawnLoc: _client_id };
}

function here(kappa) {
  return kappa(_here());
}

function there(kappa) {
  throw "'there' not defined on the client"
}

function _there() {
  throw "'there' not defined on the client"
}

function _haveMail() {
  return _mailboxes[_self()].length != 0;
}
var haveMail = LINKS.kify(_haveMail);

var _sched_pause = 0;

function _wakeup(pid) {
  if (_blocked_procs[pid]) {
    _debug("Waking up " + pid);
    var proc = _blocked_procs[pid];
    delete _blocked_procs[pid];
    setZeroTimeout(proc);
  }
  else {
    // already awake?
    _debug("Tried to wake up " + pid + ", but it is already awake");
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
  }
}

function _Send(pid, msg) {
//  _dump(_mailboxes)
  /* First, check to see whether we are sending to a well-formed PID */
  var valid_server_pid = _is_valid_server_pid(pid);
  var valid_client_pid = _is_valid_client_pid(pid);
  DEBUG.assert(valid_server_pid || valid_client_pid,
               "Malformed PID in _Send: neither client nor server PID");

  if (valid_server_pid) {
    WEBSOCKET.sendRemoteServerMessage(pid._serverPid, msg);
  } else if (valid_client_pid) {
    if (get_client_id(pid) == _client_id) {
      // Local send
      _debug("sending message '" + msg._label + "' to pid " + pid._clientPid);
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
  kappa(_send(pid, msg));
}

function _SendWrapper(env) {  // necessary wrapper for server->client calls
  return Send;
}

function _dictlength(x) {
  var length = 0;
  for (var prop in x) {
    length++;
  }
 return length;
}

function _block_proc(pid, its_cont) {
  _blocked_procs[pid] = its_cont;
  // discard stack
}

// recv
//   recv is an unusual library function that may capture the
//   continuation; hence there is no _recv form (direct-style).
function recv(kappa) {
  DEBUG.assert(arguments.length == 1,
             ('recv received '+arguments.length+ ' arguments, expected 1'));
  //DEBUG.assert(_current_pid != _mainPid,
  //           "Cannot call recv() in main process.");
  DEBUG.assert(_mailboxes[_current_pid],
             "Process " + _current_pid + " seems not to have been created.")
  if ( _mailboxes[_current_pid].length > 0) {
    msg = _mailboxes[_current_pid].pop();
    _debug("received message '"+ msg._label +"'");
    kappa(msg);
  } else {
    var current_pid = _current_pid;
    _block_proc(current_pid,
                function () {
                    _current_pid = current_pid;
                    _debug("scheduled process " + current_pid);
                    recv(kappa);
                });
    // _debug("blocked: "+current_pid)
  }
}

function _recvWrapper(env) {  // necessary wrapper for server->client calls
  return recv;
}

// SESSIONS

_nextAP = 0
_nextChannel = 0

/* Channel state */
// Moving this to the model used on the server.
//
// The buffers table maps channel IDs to lists of messages.
_buffers = {} // port |-> message list

// The _chan_blocked table is a partial mapping from channel IDs to
// processes, which are blocked waiting on a result.
_chan_blocked = [] // port |-> process ID

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
  _debug("Creating new access point " + apid);
  _aps[apid] = {ap_state: BALANCED, pending: []};
  return {_clientAPID: apid, _clientID: _client_id};
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
  _wakeup(_chan_blocked[chEP]);
  delete _chan_blocked[chEP];
}

// Block the process until the response has been received from a remote
// AP.
function blockUntilAPResponse(kappa) {
  _block_proc(
    _current_pid,
    function () {
      // Grab the returned channel EP out of the _returned_channels table,
      // and continue
      DEBUG.assert(_returned_channels[_current_pid] != undefined,
        "resuming process after remote accept, but no channel available!");
      var chan = _returned_channels[_current_pid];
      delete _returned_channels[_current_pid];
      var id_ep2 = chan._sessEP2;
      _buffers[id_ep2] = [];
      kappa(chan);
    }
  );
}

// Block the process until a matching process has been found on a local AP.
function blockUntilLocalMatch(ch, kappa) {
  _block_proc(
    _current_pid,
    function () {
      kappa(ch);
    });
}

// Do an accept on a local access point. localAPID: the key into the aps table.
function localAccept(localAPID, kappa) {
  // This should explicitly not happen -- all server-created APs residing on
  // this client should be serialised in state dumps provided in realpages
  // delivery / RPC returns
  DEBUG.assert(_aps[localAPID] != undefined, "Attempting to accept on an undefined AP!");
  _debug("Accept called on local AP " + localAPID);
  // If there's a requester, then pop the requester, create the other end of the channel, and
  // wake up the requester.
  var ap = _aps[localAPID];

  function makeAndBlock() {
      var new_ch = freshChannel();
      var our_ep = new_ch._sessEP1;
      ap.pending.unshift(new_ch);
      _chan_blocked[our_ep] = _current_pid;
      blockUntilLocalMatch(new_ch, kappa);
  }
  switch (ap.ap_state) {
    case BALANCED:
      makeAndBlock();
      ap.ap_state = ACCEPTING;
      break;
    case ACCEPTING:
      makeAndBlock();
      break;
    case REQUESTING:
      DEBUG.assert(ap.pending.length > 0,
        "Accepting on a requesting endpoint with no pending requests!");
      _debug("Length before pop: " + ap.pending.length);
      var top = ap.pending.pop();
      _debug("Length after pop: " + ap.pending.length);
      if (ap.pending.length == 0) {
        _debug("Changing state to balanced");
        ap.ap_state = BALANCED;
      }
      // Unblock other end of the channel
      var other_ep = top._sessEP2;
      wakeupFromChan(other_ep);
      // Pass the channel to the continuation
      kappa(top);
      break;
   default:
     DEBUG.assert(false, "Invalid access point state! " + ap.ap_state);
  }
  _aps[localAPID] = ap;

}

function remoteAccept(remote_apid, kappa) {
  // Accept remotely
  WEBSOCKET.sendRemoteAPAccept(_current_pid, remote_apid);
  blockUntilAPResponse(kappa);
}

function accept(ap, kappa) {
  // Firstly, check the type of accept. Local, or remote? Server, or client?
  if (_is_valid_server_ap(ap)) {
    remoteAccept(get_server_apid_from_ap(ap), kappa);
  } else if (_is_valid_client_ap(ap)) {
    if (get_client_id_from_ap(ap) != _client_id) {
      DEBUG.assert(false, "alas, accepting on a remote client AP is not yet supported");
    } else {
      localAccept(get_apid_from_ap(ap), kappa);
    }
  } else {
    DEBUG.assert(false, "invalid access point ID in accept! " + JSON.stringify(ap));
  }
}


// Do a request on a local access point. localAPID: the key into the aps table.
function localRequest(localAPID, kappa) {
  // This should explicitly not happen -- all server-created APs residing on
  // this client should be serialised in state dumps provided in realpages
  // delivery / RPC returns
  DEBUG.assert(_aps[localAPID] != undefined, "Attempting to accept on an undefined AP!");
  _debug("Accept called on local AP " + localAPID);

  // If there's a requester, then pop the requester, create the other end of the channel, and
  // wake up the requester.
  var ap = _aps[localAPID];

  function makeAndBlock() {
      var new_ch = freshChannel();
      var flipped_chan = makeFlippedChan(new_ch)
      var our_ep = flipped_chan._sessEP1;
      ap.pending.unshift(new_ch);
      _chan_blocked[our_ep] = _current_pid;
      blockUntilLocalMatch(flipped_chan, kappa);
  }

  switch (ap.ap_state) {
    case BALANCED:
      makeAndBlock();
      ap.ap_state = REQUESTING;
      break;
    case REQUESTING :
      makeAndBlock();
      break;
    case ACCEPTING:
      DEBUG.assert(ap.pending.length > 0,
        "Requesting on an accepting endpoint with no pending requests!");
      _debug("Length before pop: " + ap.pending.length);
      var top = ap.pending.pop();
      _debug("Length after pop: " + ap.pending.length);
      if (ap.pending.length == 0) {
        _debug("Changing state to balanced");
        ap.ap_state = BALANCED;
      }

      // Unblock other end of the channel
      var other_ep = top._sessEP1;
      wakeupFromChan(other_ep);

      // Pass our end of channel (flipped version of pending) to the continuation
      kappa(makeFlippedChan(top));
      break;
   default:
     DEBUG.assert(false, "Invalid access point state! " + ap.ap_state);
  }
  _aps[localAPID] = ap;
}

function remoteRequest(remote_apid, kappa) {
  // Request remotely
  WEBSOCKET.sendRemoteAPRequest(_current_pid, remote_apid);
  blockUntilAPResponse(kappa);
}

function request(ap, kappa) {
  if (_is_valid_server_ap(ap)) {
    remoteRequest(get_server_apid_from_ap(ap), kappa);
  } else if (_is_valid_client_ap(ap)) {
    if (get_client_id_from_ap(ap) != _client_id) {
      DEBUG.assert(false, "alas, requesting from a remote client AP is not yet supported");
    } else {
      localRequest(get_apid_from_ap(ap), kappa);
    }
  } else {
    DEBUG.assert(false, "invalid access point ID in request! " + JSON.stringify(ap));
  }
}


function deliverSessionMessage(epid, v) {
  DEBUG.assert(epid in _buffers,
    "Trying to deliver message to nonexistent buffer " + epid + "!");
  _buffers[epid].unshift(v);
  _wakeup(_chan_blocked[epid]);
  delete _chan_blocked[epid];
}

// Retrieves all sessions within the value.
function getDelegatedSessions(v) {
  if ("_sessEP1" in v) {
    return [v];
  // Arrays
  } else if (DEBUG.is_array(v)) {
    var res = [];
    for (var i = 0; i < v.length; i++) {
      res = res.concat(getDelegatedSessions(v[i]));
    }
    return res;
  } else if(DEBUG.is_object(v)) {
    // We can treat variants and records in the same way
    var vals = Object.values(v);
    var res = [];
    for (var i = 0; i < vals.length; i++) {
      res = res.concat(getDelegatedSessions(vals[i]));
    }
    return res;
  }
}

// This function expects a list of channel references in the form
// { _sessEP1: <send port>, _sessEP2: <recv port> }.
// The function is side effecting.
//
// Output of the function:
//   [ { chan : { _sessEP1: <send port>, _sessEP2: <recv port> }, buffer : <list of values in the buffer for recv_port> } ]
//
// Side-effects:
//   * _buffers[<recv port>] will be deleted.
//   * _delegating[<recv port>] will be set to [].
function prepareDelegatedChannels(chans) {
  var res = [];
  for (var i = 0; i < chans.length; i++) {
    var cur_chan = chans[i];
    var cur_recv_ep = chans[i]._sessEP2;
    DEBUG.assert(_sessEP2 in buffers, "Trying to delegate channel without a buffer!");
    var cur_buf = _buffers[_sessEP2];
    delete _buffers[_sessEP2];
    _delegating[_sessEP2] = [];
    res.unshift({ chan: cur_chan, buffer: cur_buf});
  }
  return res;
}

function _remoteSessionSend(v, c) {
  // TODO: Will need to check v to see if it is a session channel, and if so, will
  // need to do the delegation protocol.
  var delegated_sessions = getDelegatedSessions(v);
  var prepared_delegated_sessions = prepareDelegatedChannels(v);
  WEBSOCKET.sendRemoteSessionMessage(c, prepared_delegated_sessions, v);
}

function _send(v, c) {
  // console.log("Giving " + v + " to channel " + c.channel + "-" + c.direction);
  var sendEP = c._sessEP1;
  if (sendEP in _buffers) {
    // If the send is in the local buffers table, we have hold of the
    // endpoint and can send locally.
    _buffers[sendEP].unshift(v);
    _wakeup(_chan_blocked[sendEP]);
    delete _chan_blocked[sendEP];
    return c;
  } else {
    _remoteSessionSend(v, c);
    return c;
  }
}

function receive(c, kappa) {
  // console.log("Grabbing from channel " + c.channel + "-" + c.direction);
  var recvEP = c._sessEP2;
  DEBUG.assert(recvEP in _buffers, "Trying to receive from nonexistent buffer!");
  if (_buffers[recvEP].length > 0) {
    var msg = _buffers[recvEP].pop();
    // console.log("Grabbed " + msg);
    return (kappa({1:msg, 2:c}));
  } else {
    var current_pid = _current_pid;
    _chan_blocked[recvEP] = current_pid;
    _block_proc(current_pid,
      function () {
        _current_pid = current_pid;
        receive(c, kappa);
      });
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

function _Continuation(v) { this.v = v }

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
			setZeroTimeout(function() { _current_pid = current_pid; return f()});
		}
	} else {
		return f();
	}
}

function _yieldCont(k, arg) {
	++_yieldCount;
	if (_yieldCount == _yieldGranularity) {
		_yieldCount = 0;
		if (_handlingEvent) {
			_theContinuation.v = function () { k(arg) }; throw _theContinuation;
		} else {
			var current_pid = _current_pid;
			setZeroTimeout(function() { _current_pid = current_pid; k(arg) });
		}
	} else {
		return k(arg);
	}
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

function _print(str) {
  alert(str);
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
is_integer = LINKS.kify(_is_integer);

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
  kappa(_childNodes(elem));
}

function _textContent (node) {
  try { return (node.innerHTML) }
  catch (e) { return ("") }
}

function textContent (node, kappa) {
  kappa(_textContent(node));
}

function _reifyK() {
  LINKS.unimpl("reifyK");
}

function sleep(duration, kappa) {
  var current_pid = _current_pid;
  setTimeout(function() { _current_pid = current_pid; kappa({}); }, duration);
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

function _chr(c) {{_c:String.fromCharCode(c)}};
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
   createCookie : function(name,value,days) {
     if (days) {
       var date = new Date(); // makes garbage
       date.setTime(date.getTime()+(days*24*60*60*1000));
       var expires = "; expires="+date.toGMTString();
     } else var expires = "";
     document.cookie = name+"="+value+expires+"; path=/";
   },

    readCookie : function(name) {
      var nameEQ = name + "=";
      var ca = document.cookie.split(';');
      for(var i=0;i < ca.length;i++) {
        var c = ca[i];
        while (c.charAt(0)==' ') c = c.substring(1,c.length);
        if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
     }
     return null;
   },

    eraseCookie : function (name) {
      createCookie(name,"",-1);
    }
// END code from quirksmode.org
  }
} ();

function _setCookie(cookieName, value) {
  QUIRKS.createCookie(cookieName,
                      value,
                      10000);
  return {};
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
	window.setInterval(function () { fn(_idy) }, interval);
	return;
}
function jsSetInterval(fn, interval, kappa) {
    _jsSetInterval(fn, interval);
    kappa({});
}

// NOTE: requestAnimationFrame can also take a callback that has one argument
function _jsRequestAnimationFrame(fn) {
	window.requestAnimationFrame(function () { fn(_idy) });// || window.webkitRequestAnimationFrame(function () { fn(_idy) });
	return;
}
function jsRequestAnimationFrame(fn, kappa) {
    _jsRequestAnimationFrame(fn);
    kappa({});
}

function _jsSave(ctx) {
	ctx.save();
	return;
}
function jsSave(ctx, kappa) {
    _jsSave(ctx);
    kappa({});
}

function _jsRestore(ctx) {
	ctx.restore();
	return;
}
function jsRestore(ctx, kappa) {
    _jsRestore(ctx);
    kappa({});
}

function _jsSetOnKeyDown(node, fn) {
	// note: node has to exist in the document, otherwise we get a JavaScript error
	node.addEventListener('keydown', function(e) { fn(e, _idy) }, true);
	return;
}
function jsSetOnKeyDown(node, fn, kappa) {
    _jsSetOnKeyDown(node, fn);
    kappa({});
}

function _jsSetOnEvent(node, event, fn, capture) {
	node.addEventListener(event, function(e) { fn(e, _idy) }, capture);
	return;
}
function jsSetOnEvent(node, event, fn, capture, kappa) {
    _jsSetOnEvent(node, event, fn, capture);
    kappa({});
}

function _jsSetOnLoad(fn) {
	window.addEventListener('load', function(e) { fn(e, _idy) }, false);
	return;
}
function jsSetOnLoad(fn, kappa) {
    _jsSetOnEvent(fn);
    kappa({});
}

var globalObjects = {};

function _jsSaveGlobalObject(name, obj) {
	globalObjects[name] = obj;
	return;
}
function jsSaveGlobalObject(name, obj, kappa) {
    _jsSaveGlobalObject(name, obj);
    kappa({});
}

function _jsLoadGlobalObject(name) {
	return globalObjects[name];
}
function jsLoadGlobalObject(name) {
    _jsSaveGlobalObject(name);
    kappa({});
}

function _jsGetContext2D(node) {
	return node.getContext('2d');
}
function jsGetContext2D(node, kappa) {
	_jsGetContext2D(node);
	kappa({});
}

function _jsFillText(ctx, text, x, y) {
	ctx.fillText(text, x, y);
}
function jsFillText(ctx, text, x, y, kappa) {
	_jsFillText(ctx, text, x, y);
	kappa({});
}

function _jsCanvasFont(ctx, font) {
	ctx.font = font;
}
function jsCanvasFont(ctx, font, kappa) {
	_jsCanvasFont(ctx, font);
	kappa({});
}

function _jsDrawImage(ctx, node, x, y) {
	ctx.drawImage(node, x, y);
}
function jsDrawImage(ctx, node, x, y, kappa) {
	_jsDrawImage(ctx, node, x, y);
	kappa({});
}

function _jsFillRect(ctx, x, y, width, height) {
	ctx.fillRect(x, y, width, height);
}
function jsFillRect(ctx, x, y, width, height, kappa) {
	_jsFillRect(ctx, x, y, width, height);
	kappa({});
}

function _jsFillCircle(ctx, x, y, radius) {
	ctx.beginPath();
	ctx.arc(x, y, radius, 0, 2 * Math.PI, true);
	ctx.fill();
	ctx.closePath();
}
function jsFillCircle(ctx, x, y, radius, kappa) {
	_jsFillCircle(ctx, x, y, radius);
	kappa({});
}

function _jsFill(ctx) {
	ctx.fill();
}
var jsFill = _jsFill;

function _jsBeginPath(ctx) {
    ctx.beginPath();
}
function jsBeginPath(ctx, kappa) {
	_jsBeginPath(ctx);
	kappa({});
}

function _jsClosePath(ctx) {
	ctx.closePath();
}
var jsClosePath = _jsClosePath;

function _jsArc(ctx, x, y, radius, startAngle, endAngle, clockwise) {
	ctx.arc(x, y, radius, startAngle, endAngle, clockwise);
}
var jsArc = _jsArc;

function _jsStrokeStyle(ctx, style) {
    ctx.strokeStyle = style;
}
function jsStrokeStyle(ctx, style, kappa) {
	_jsStrokeStyle(ctx, style);
	kappa({});
}

function _jsStroke(ctx) {
    ctx.stroke();
}
function jsStroke(ctx, kappa) {
	_jsStroke(ctx);
	kappa({});
}

function _jsMoveTo(ctx, x, y) {
    ctx.moveTo(x, y);
}
function jsMoveTo(ctx, x, y, kappa) {
	_jsMoveTo(ctx, x, y);
	kappa({});
}

function _jsLineTo(ctx, x, y) {
    ctx.lineTo(x, y);
}
function jsLineTo(ctx, x, y, kappa) {
	_jsLineTo(ctx, x, y);
	kappa({});
}

function _jsLineWidth(ctx, width) {
	ctx.lineWidth = width;
}
var jsLineWidth = _jsLineWidth;

function _jsScale(ctx, x, y) {
    ctx.scale(x, y);
}
function jsScale(ctx, x, y, kappa) {
	_jsScale(ctx, x, y);
	kappa({});
}

function _jsTranslate(ctx, x, y) {
    ctx.translate(x, y);
}
function jsTranslate(ctx, x, y, kappa) {
	_jsTranslate(ctx, x, y);
	kappa({});
}

function _jsSetFillColor(ctx, color) {
	ctx.fillStyle = color;
}
function jsSetFillColor(ctx, color, kappa) {
	_jsSetFillColor(ctx, color);
	kappa({});
}

function _jsClearRect(ctx, x, y, width, height) {
	ctx.clearRect(x, y, width, height);
}
function jsClearRect(ctx, x, y, width, height, kappa) {
	_jsClearRect(ctx, x, y, width, height);
	kappa({});
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
}
var jsSaveCanvas = _jsSaveCanvas;

function _debugChromiumGC() {
	if (window.gc) window.gc()
	else {
		var msg = "Error. In order to use debugChromiumGC() invoke chromium like this: chromium --js-flags='--expose_gc'. Application terminated.";
        alert(msg);
		throw new Error(msg);
	}
}
var debugChromiumGC = _debugChromiumGC;

//
// LIST MANIPULATING FUNCTIONS
//

var lsNil = null;

function _lsNilF() {
	return null;
}
var lsNilF = _lsNilF;

function _lsCons(head, tail) { return { _head: head, _tail: tail }; }
var lsCons = _lsCons;

function _lsSnoc(xs, x) {
  _lsAppend(xs, _lsSingleton(x))
}

function _lsSingleton(head) { return { _head: head, _tail: lsNil }; }

function _lsFromArray(arr) {
	var out = lsNil;
	for (var i = arr.length - 1; i >= 0; --i) {
		out = _lsCons(arr[i], out);
	}
	return out;
}

function _lsTake(n, xs) {
	var arr = [];
	while (xs !== null && n > 0) {
		arr.push(xs._head);
		xs = xs._tail;
		--n;
	}
	return _lsFromArray(arr);
}
var lsTake = _lsTake;

function _lsDrop(n, xs) {
	while (xs !== null && n > 0) {
		xs = xs._tail;
		--n;
	}
	return xs;
}
var lsDrop = _lsDrop;

function _lsLength(xs) {
	var out = 0;
	while (xs !== null) {
		out += 1;
		xs = xs._tail;
	}
	return out;
}
var lsLength = _lsLength;


function _lsHead(v) { return v === null ? _error('head') : v._head; } // inline?
function _lsTail(v) { return v === null ? _error('tail') : v._tail; }
var lsHead = _lsHead;
var lsTail = _lsTail;

function _lsLast(xs) {
	if (xs === null) { _error('last'); }
	var out = xs._head;
	while (xs !== null) {
		out = xs._head;
		xs = xs._tail;
	}
	return out;
}
var lsLast = _lsLast;

//~ function _lsMap(f, xs) {
	//~ //return lsNil;
	//~ var arr = [];
	//~ while (xs !== null) {
		//~ arr.push(_yield(f, [xs._head], _idy)); // f doesn't return
		//~ xs = xs._tail;
	//~ }
	//~ return _lsFromArray(arr);
//~ }
//~ function lsMap(f, xs, kappa) {
    //~ kappa(_lsMap(f, xs));
//~ }
//~
//~ function _lsMapIgnore(f, xs) {
	//~ //return;
	//~ while (xs._label !==  0) {
		//~ f(xs._head, _idy);
		//~ xs = xs._tail;
	//~ }
	//~ return;
//~ }
//~ function lsMapIgnore(f, xs, kappa) {
    //~ _lsMapIgnore(f, xs);
    //~ kappa({});
//~ }

function _lsAppend(xs, ys) {
	if (xs === null) { return ys; }
	var rootEl = _lsCons(xs._head, lsNil);
	var curr = rootEl;
	xs = xs._tail;
	while (xs !== null) {
		curr._tail = _lsCons(xs._head, lsNil);
		xs = xs._tail;
		curr = curr._tail;
	}
	curr._tail = ys;

	return rootEl;
}
var lsAppend = _lsAppend;

function _lsAt(xs, i) {
	var out;
	while (xs !== null && i >= 0) {
		out = xs._head;
		xs = xs._tail;
		--i;
	}
	// should check i here
	return out;
}
var lsAt = _lsAt;

function _lsEmpty(xs) {
	return xs === null;
}
var lsEmpty = _lsEmpty;

function _lsZip(xs, ys) {
	var arr = [];
	while (xs !== null && ys !== null) {
		arr.push({ "1": xs._head, "2": ys._head }); // { ctor:"_Tuple2", _0:x, _1:y }
		xs = xs._tail;
		ys = ys._tail;
	}
	return _lsFromArray(arr);
}
var lsZip = _lsZip;

//~ function _lsFilter(p, xs) {
	//~ //return lsNil;
	//~ var arr = [];
	//~ while (xs !== null) {
		//~ if (p(xs._head)) { arr.push(xs._head); }
		//~ xs = xs._tail;
	//~ }
	//~ return _lsFromArray(arr);
//~ }
//~ function lsFilter(p, xs, kappa) {
	//~ kappa(_lsFilter(p, xs))
//~ }

//~ function _lsConcatMap(f, xs) {
	//~ if (xs._label === 0) return lsNil;
	//~ lsAppend(f(xs._head), _lsConcatMap(f, l._tail))
//~
	//~ var arr = [];
	//~ while (xs !== null) {
		//~ arr.push(f(xs._head));
		//~ xs = xs._tail;
	//~ }
//~
	//~ return _lsFromArray(arr);
//~ }

function _lsRange(a, b) {
	var lst = lsNil;
	if (a <= b)
		do { lst = _lsCons(b, lst) } while (b-- > a);
	return lst;
}
var lsRange = _lsRange;

function _lsReplicate(n, item) {
    var out = lsNil;
	while (n > 0) {
		out = _lsCons(item, out);
		--n;
	}
	return out;
}
var lsReplicate = _lsReplicate;

function _lsRepeat(n, x) { // faster than _lsReplicate? not really?
	var arr = [];
	var pattern = [x];
	while (n > 0) {
		if (n & 1) arr = arr.concat(pattern);
		n >>= 1, pattern = pattern.concat(pattern);
	}
	return _lsFromArray(arr);
}

function _ls(arr) {
	var out = lsNil;
	for (var i = arr.length - 1; i >= 0; --i) {
		out = _lsCons(arr[i], out);
	}
	return out;
}
var ls = _ls;

function _lsMinimum(xs) {
	var currentMin = _lsHead(xs);
	while (xs !== null) {
		if (xs._head < currentMin) currentMin = xs._head;
		xs = xs._tail;
	}

	return currentMin;
}

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
