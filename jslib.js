// Links js runtime.

var DEBUGGING = true;
var _dwindow = DEBUGGING ? open('', 'debugwindow','width=550,height=800,toolbar=0,scrollbars=yes') : null;

// [IE] these aren't defined in IE
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


// [IE] Node isn't defined in IE
/*
Node.prototype.swapNode = function (node) {
    var nextSibling = this.nextSibling;
    var parentNode = this.parentNode;
    node.parentNode.replaceChild(this, node);
    parentNode.insertBefore(node, nextSibling);
 }
*/

_swapNode = function (me, node) {
  var nextSibling = me.nextSibling;
  var parentNode = me.parentNode;
  node.parentNode.replaceChild(me, node);
  parentNode.insertBefore(node, nextSibling);
}

var DEBUG = function () {
 function is_instance(value, type, constructor) {
    return value != undefined
        && (typeof value == type || value instanceof Object && value.constructor == constructor)
 }
 function xmldump(xml) {
   return (new XMLSerializer()).serializeToString(xml)
 }
 return {
 assert : function(value) {
   if (value != true) {
     try {  
        throw new Error() // throw an exception so that we can retrieve the stack (via the 'stack' property)
     }
     catch (e) {  
       var msg = "<b>ASSERTION FAILED!</b> in function " + arguments.callee.caller + "<br/><b>Stack</b>: " + e.stack;
       alert(msg.replace('@', '<br/><b>=> </b> ', 'g'));
       throw new Error("assertion failed (check debug output)");
    }
   }
 },
 is_number : function(value) {
  return is_instance(value, 'number', Number);
 },
 is_string : function(value) {
   return is_instance(value, 'string', String);
 },
 is_boolean : function(value) {
   return is_instance(value, 'boolean', Boolean);
 },
 is_xmlnode : function(value) {
   return is_instance(value, -1, Node)
 },
 is_array : function(value) {
   return is_instance(value, -1, Array);
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
    else if (typeof(value) == 'object' && 'constructor' in value) return new String(constructor);
    else return 'UNKNOWN TYPE'
 },
 // could also add type-predicates for Array, Error, Function, Date, etc.
 show : function(value) {
    if (DEBUG.is_xmlnode(value)) return xmldump(value)
    else return JSON.stringify(value)
 }//,
  // [IE] superflous comments aren't allowed
}
}();

function _debug(msg) {
   if (DEBUGGING) {
     _dwindow.document.write('<b>' + _current_pid + '</b> : ' + msg + '<br/>');
   }
}
_alert = _debug;

// utilities (NOT CPS'd)
function isEmpty(list) { return (list.length == 0); }
function isarray(obj) {
  return typeof(obj) == 'object' && obj.constructor == Array
}

// library functions (CPS'd)
function _continuationize(f) {
    return function (kappa) {
        return function () {
            return kappa(f.apply(f, arguments));
        };
    };
}
var float_of_int = _continuationize(Number);
var string_of_float = _continuationize(String);
var int_of_string = _continuationize(parseInt);
var string_of_int = _continuationize(String);
//function not(x) { return !x; }
function not(kappa) { return function(x) { kappa( !x ); } }
function empty(kappa) { return function(list) { kappa(list.length == 0); } }
function hd(kappa) { return function(list) { kappa(list[0]); } }
function tl(kappa) { return function(list) { kappa(list.slice(1)); } }

// library functions (NOT CPS'd)
function _minus(l)  { return function (r) { return l -  r ; }}
function _plus(l)   { return function (r) { return l +  r ; }}
function _times(l)  { return function (r) { return l *  r ; }}
function _divide(l) { return function (r) { return l /  r ; }}
function _eq(l,r)     { 
  if (typeof(l) == 'object' && l.constructor == Array && 
      typeof(r) == 'object' && r.constructor == Array) {
    return l.length == 0 && r.length == 0
  }
  else return (l == r)
 }
function _le(l,r)     {   return l <= r ; }
function _ge(l,r)     {   return l >= r ; }
function _gt(l,r)     {   return l >  r ; }

// Links-level debugging routines
function debug(kappa) {
  return function(msg) {
     _debug(msg);
     return kappa();
  }
}

function debugObj(kappa) {
  return function(obj) {
     if (obj == undefined) {
       alert(obj + " : undefined");
     } else {
       alert(obj + " : " + typeof(obj) + (typeof(obj) == 'object' ? obj.constructor : ''));
     }
     return kappa();
  }
}

dump = _continuationize(
  function (obj) {
    if (obj == undefined)
      _debug(obj + " : undefined");
    else
      _debug("==TYPE== " + typeof(obj) + " " + 
        (typeof(obj) == 'object' ? obj.constructor : ""));
      for (var i in obj) {
        try {
          _debug(i + "=" + obj[i]);
        } catch (e) {
          _debug(i + " (died)");
        } 
      }
  }
);

function error(kappa) {
  return function(msg) {
    alert(msg);
    throw ("Error: " + msg);
  }
}

/// ???
function _xmldump(xml) {
   return (new XMLSerializer()).serializeToString(xml)
}

function _applyChanges(changes) {
  for (var i in changes) {
    var change = changes[i];
    if (change.label == 'ReplaceElement') {
    var element = document.getElementById(change.value.id);
    if (!element) _alert("element " + change.value.id + " does not exist");
          element.parentNode.replaceChild(change.value.replacement[0], element);
    }
    else if (change.label == 'AppendChild') {
        var element = document.getElementById(change.value.id);
        if (!element) _alert("element " + change.value.id + " does not exist");
        element.appendChild(change.value.replacement[0]);
    }  
    else if (change.label == 'SwapNodes') {
          var one = change.value.first;
          var two = change.value.second;
          _swapNode(one, two);
    }  
    else if (change.label == 'Document') {
          _start(change.value);
    }  
  }
}

function see(x) { return x.innerHTML }

function cmap(f, glue, l) {
  if (l.length <= 0) return '';
  temp = f(l[0]);
  for (var i = 1; i < l.length; i++) {
    temp += glue + f(l[i]);
  }
  return temp;
}

//  _concat(a, b)
//     concatenate two sequences: either strings or lists
function _concat () {
  if (typeof(arguments[0]) == 'string') {
    var rv = '';
    for (var i = 0; i < arguments.length; i++) {
      rv += arguments[i];
    }
    return rv;
  }
  else {
    if (arguments.length == 0) return [];
    var rv = arguments[0];
    for (var i = 1; i < arguments.length; i++) {
      rv = rv.concat(arguments[i]);
    }
    return rv;
  }
}

//  _accum(f, i)
//    concatMap: apply f to every element of `list'
//    and concatenate the results.
function _accumAux(kappa) {
  return function(fn, list, result) {
    if (isEmpty(list)) kappa(result)
    else {
      // instead of cps hd/tl, should use js ops directly
      hd(function(x) {
           fn(function(ximg) {
                tl(function (rest) {
                     _accumAux(kappa)(fn, rest, result.concat(ximg))
                   })(list)
              } )(x);
         })(list)
    }
  }
}

function _accum(kappa) {
  return function(fn, list) {
    _accumAux(kappa)(fn, list, []);
  }
}

// function _constructHandlerCall (the_id, eventName) {
//   '_eventHandlers['+the_id+']['+eventName+']; return false';
// }

// function _activateHandler(node, the_id, eventName) {
//   var e = 'node.'+eventName+' = '+'\'_eventHandlers['+the_id+']['+eventName+']; return false\'';
// //'+_constructHandlerCall(the_id, eventName);
//   _alert("activating handler: "+e);
//   e();
// }

/// XML
//  _XML(tag, attrs, children)
//    create a DOM node with name `tag'
//                       and attributes `attrs' (a dictionary)
//                       and children `children' (a sequence of DOM 
//                                                nodes and strings)    

function _XML(kappa) {
  return function(tag, attrs, body) { 
   var node = document.createElement(tag);
   _alert("node: "+node.nodeName);
   for (name in attrs) {
      _alert("attribute name: "+name+"; value: "+attrs[name]);
      node.setAttribute(name, attrs[name]);
   }
   _alert("boo");

   var the_id = node.getAttribute('id');
   if(the_id != null) {
     var hs = _eventHandlers[the_id];
     for(eventName in hs) {
       function h(e) {
         _alert("an event!");
         //_eventHandlers[the_id][eventName](e);
       }


       switch (eventName) {
         case 'onsubmit':
           _alert("Event handler: "+the_id+", "+eventName);
           node.onsubmit = h;
           break;
         default:
           _alert("Event not supported in jslib.js yet: "+eventName);
       }
       //_activateHandler(node, the_id, eventName);
     }
   }

   for (var i = 0; i < body.length; i++) {
     var child = body[i];
     if (typeof(child) == 'string') {
       node.appendChild(document.createTextNode(child));
     } else {
       for (var j = 0; j < child.length; j++) {
          node.appendChild(child[j]);
       }
     }
   }
   kappa([node]);
  }
}

/// Records
//   _extend(record, tag, value)
//    extend a record (dictionary) with a new field (label `tag'; value `value').
//    Don't update the old record.
function _extend(object, label, value) {
  var copy = {};
  for (var name in object) {
     copy[name] = object[name];
  }
  copy[label] = value;
  return copy;
}

//   _project(object, name)
//    project a field of a record
function _project(object, name) { return object[name]; }

function _vrntLbl(object) { return object['label']}
function _vrntVal(object) { return object['value']}

function _fail(str) {
  _alert("Internal error: " + str);
}


function isElement(node) {
  return (node.nodeType == document.ELEMENT_NODE);
}

function isElementWithTag(node, tag) {
  return (isElement(node) && (node.tagName.toLowerCase() == 'body'));
}


function _activateHandlers(node) {
  if(!isElement(node))
    return;

  var the_id = node.getAttribute('id');
  if(the_id != null) {
    var hs = _eventHandlers[the_id];
    for(eventName in hs) {
      function h(e) {
        _alert("an event!");
        _eventHandlers[the_id][eventName](e);
	return false;
      }

      switch (eventName) {
        case 'onsubmit':
          _alert("Event handler: "+the_id+", "+eventName);
          node.onsubmit = h;
          break;
        default:
          _alert("Event not supported in jslib.js yet: "+eventName);
      }
      //_activateHandler(node, the_id, eventName);
    }
  }
}


// Page update

//  _start(tree)
//    Replace the current page with `tree'.
function _start(tree) {
  // save here
  var _saved_fieldvals = [];
  var inputFields = document.getElementsByTagName("input");
  for (var i = 0; i < inputFields.length; i++) { 
     var current = inputFields[i];
     _saved_fieldvals.push({'field' : current.id, 'value' : current.value});
  }

  // delete the DOM except for the html tag and the body
  // [IE] IE doesn't allow these tags to be deleted
  var d = document.documentElement;
  var body;
  while(d.hasChildNodes()) {
    if(isElementWithTag(d.firstChild, 'body')) {
      body = d.firstChild;
      var bodyLength = body.childNodes.length;
      while (body.hasChildNodes()) {
        body.removeChild(body.firstChild);
      }
      break; // [IE] no more nodes allowed after the body
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
        _activateHandlers(it);
      }
      break; // [IE] no more nodes allowed after the body
    }
    _alert("inserting node: "+p.nodeName);
    var it = p.cloneNode(true);
    d.insertBefore(it, body);
    _activateHandlers(it);
  }


  // restore here
  for (var i = 0; i < _saved_fieldvals.length; i++) { 
     var current = _saved_fieldvals[i];
     var elem = document.getElementById(current.field);
     if (elem) {
        elem.value = current.value; 
     }
  }
  
  // hmm.
  _focus();
  _alert(time() - start_time + ' milliseconds');

}

// //  _registerFormEventHandlers(id, [action1, action2, ...])
// //    Register an event handler.
// //    We need to register it, and invoke it through the registry, 
// //    because attributes like onClick are merely strings, whereas we 
// //    want to associate proper closures.
// //    `id' should be either 0 or the real ID of the element to which 
// //    you're attaching the handler. If it is 0, a new ID will be
// //    generated and returned.

var _id = 0;

function _registerFormEventHandlers(id, actions) {
   the_id = id != 0 ? id : _id++;
   for (var i = 0; i < actions.length; i++) {
     var action = actions[i];
        // FIXME: clone this ??

//      if (!_eventHandlers[action.evName])
//        _eventHandlers[action.evName] = [];
//      _eventHandlers[action.evName][the_id] = action.handler;
     if (!_eventHandlers[the_id])
       _eventHandlers[the_id] = [];
     _eventHandlers[the_id][action.evName] = action.handler;
   }
   return the_id;
}
var _eventHandlers = {};

function enxml(kappa) { return function (body) {
  return kappa([document.createTextNode(body)]);
}}
var javascript = true;



//// Remote calls

/// serialization
// TODO!

/// calls.
// TODO!
// Base64 encoding and decoding (from www.farfarfar.com)
var _charBase64 = new Array(
    'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
    'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
    'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
    'w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/'
);

function _base64encode(str)
{
  var out = "";
  var i = 0;

  var len = str.length;

  do
  {
    var chr1 = str.charCodeAt(i++);
    var chr2 = str.charCodeAt(i++);
    var chr3 = str.charCodeAt(i++);

    var enc3 = ((chr2 & 0x0F) << 2) | (chr3 >> 6);

    out += _charBase64[chr1 >> 2] + _charBase64[((chr1 & 0x03) << 4) | (chr2 >> 4)];

    if (isNaN(chr2))
        out += '==';
    else if (isNaN(chr3))
        out += _charBase64[enc3] + '=';
    else
        out += _charBase64[enc3] + _charBase64[chr3 & 0x3F];
  } while (i < len);

  return out;
}

var _indexBase64 = new Array(
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,62, -1,-1,-1,63,
    52,53,54,55, 56,57,58,59, 60,61,-1,-1, -1,-1,-1,-1,
    -1, 0, 1, 2,  3, 4, 5, 6,  7, 8, 9,10, 11,12,13,14,
    15,16,17,18, 19,20,21,22, 23,24,25,-1, -1,-1,-1,-1,
    -1,26,27,28, 29,30,31,32, 33,34,35,36, 37,38,39,40,
    41,42,43,44, 45,46,47,48, 49,50,51,-1, -1,-1,-1,-1,
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1
);


function _base64decode(str)
{
  var out = "";
  var i = 0;
  var len = str.length;

  do
  {
    var enc1 = _indexBase64[str.charCodeAt(i++)];
    var enc2 = _indexBase64[str.charCodeAt(i++)];
    var enc3 = _indexBase64[str.charCodeAt(i++)];
    var enc4 = _indexBase64[str.charCodeAt(i++)];

    out += String.fromCharCode((enc1 << 2) | (enc2 >> 4));

    if (enc3 != -1)
        out += String.fromCharCode(((enc2 & 15) << 4) | (enc3 >> 2));
    if (enc4 != -1)
        out += String.fromCharCode(((enc3 & 3) << 6) | enc4);
  } while (i < len);

  return out;
}


// Convert a one-indexed tuple ({"1" : v1, "2" : v2 ... })
// to a list.
function _tuple_as_list(tuple) {
  var rv = [];
  for (var i = 1; ; i++) {
     var elem = tuple[i];
     if (elem == undefined) return rv;
     rv.push(elem);
  }
}

var isLoaded = 2;
var isComplete = 4;

// [IE] XMLHttpRequest is an ActiveXObject in IE
function _newXMLHttpRequest() {
  var http_request;
  if(window.XMLHttpRequest) { // native XMLHttpRequest object (FireFox, etc.)
    try {
      http_request = new XMLHttpRequest();
    } catch(e) {
      throw ("Failed to create (native) XMLHttpRequest");
    }
  } else if(window.ActiveXObject) { //IE
    try {
        http_request = new ActiveXObject("Msxml2.XMLHTTP");
    } catch(e) {
      try {
        http_request = new ActiveXObject("Microsoft.XMLHTTP");
      } catch(e) {
        throw ("Failed to create (ActiveX) XMLHttpRequest object");
      }
    }
  }
  return http_request;
}

function _remoteCallHandler(kappa, request) {
  return function() {
    if (request.readyState == isComplete) {
     // FIXME: this case apparently triggers if we leave the page.
     var serverResponse = JSON.parse(_base64decode(request.responseText.replace('\n', '')));
     if ((serverResponse instanceof Object) && ('__continuation' in serverResponse)) {
        // it's a continuation
        var _res = null;
        var cont = function(r) { res = r; }
        // FIXME: does window[] necessarily look up functions?
        var f = window[serverResponse.__name](cont);
        f.apply(f, _tuple_as_list(serverResponse.__arg));
        var request_ = _newXMLHttpRequest();
        request_.open('POST', location.href, true);
        request_.onreadystatechange = _remoteCallHandler(kappa, request_);
        request_.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
        request_.send("__continuation=" + serverResponse.__continuation + "&__result=" + _base64encode(JSON.stringify(_res)));
      }
      else {
        // it's the final result: return it.
        kappa(serverResponse);
      }
    }
  }
}

function _remoteCall(kappa) {
  return function(name, arguments) {

    _alert("starting xmlhttprequest");

    var request = _newXMLHttpRequest();
    asynch = true;

    // [IE] IE mistakenly urlencodes # as %23
    // whereas Firefox leaves it plain
    //
    // so we use location.href instead
    // which seems to work in both
    request.open('POST', location.href, asynch);
    request.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
    request.onreadystatechange = _remoteCallHandler(kappa, request);

    request.send("__name=" + _base64encode(name) + 
                 "&__args=" + _base64encode(JSON.stringify(arguments)));

  }
} 

// db functions (via remote calls)
// TODO!

/// focus stuff
var _focused = null;
function _val(x) { 
    var y = document.getElementById(x);
    if (y) {return y.value} else { return "boogers"}
}
function _focus() {
  if (_focused) {
      var y = document.getElementById(_focused);
      if (y) { y.focus(); }
  }

}

var javascript = true;

// identity: a "toplevel" continuation
function _idy(x) {
  return x;
}

var _maxPid = 0;
var _current_pid = 0;

function spawn(kCurry) {
  return function (f) {
    kCurry(function (kappa) {
      return function (arg) {
        var childPid = ++_maxPid;
        _mailboxes[childPid] = [];
        setTimeout(function () { 
                     _current_pid = childPid;
                     f(_applyChanges)(arg) 
                   }, 200);
        kappa(childPid);
      };
    });
  }
}


var _mailboxes = {0:[]};
var _blocked_procs = {};
//var _suspended_procs = [];

function self(kappa) { 
  return function(unit) {
     return kappa(_current_pid) 
  }
}

var sched_pause = 10;

function _wakeup(pid) {
  if (_blocked_procs[pid]) {
    var proc = _blocked_procs[pid];
    delete _blocked_procs[pid];
    setTimeout(proc, sched_pause);
  }
  else {
  }
}

function send(kCurry) {
  return function(pid) {
   kCurry(function (kappa) {
      return function (msg) {
        _mailboxes[pid].push(msg);
        _wakeup(pid);
        kappa(1);
      }
    })
  }
}

function _dictlength(x) {
  var length = 0;
  for (var prop in x) { 
    length++;
  }
 return length;
}


function _block_proc(pid, its_cont) {
  // FIXME: const?? is this OK?
  var current_pid = _current_pid;
  _blocked_procs[pid] = function () { _current_pid = current_pid;  its_cont() };
  // discard stack
}

function recv(kappa) {
  return function() {
    if ( _mailboxes[_current_pid].length > 0) {
      msg = _mailboxes[_current_pid].pop()
      kappa(msg);
    } else {
      _block_proc(_current_pid, function () { recv(kappa)() });
    }
  }
}

function _scheduler() {
  
}

var _yieldCount = 0;
var _yieldGranularity = 100;

// yield: give up control for another "thread" to work
function _yield(my_cont) {
  ++_yieldCount;
  if ((_yieldCount % _yieldGranularity) == 0) {
    var current_pid = _current_pid;
    setTimeout((function() { _current_pid = current_pid; my_cont()}),
               sched_pause);
  }
  else {
    my_cont();
  }
}

// _call: using this makes compiled js code easier to read.
function _call(f, args, kappa) {
  _yield(function() { it = f(kappa); it.apply(it, args) });
}


function print(kappa) {
  return function(str) {
    _alert(str);
    kappa(0);
  }
}

function elementById(kappa) {
   return function(id) {
      var elem = document.getElementById(id);
      if (elem != null) kappa({'label':'Some', 'value':[elem]});
      else kappa({label:'None', 'value':({})});
   }
}

function attribute(kappa) {
   return function(xml, attr) {
   // FIXME: we need to straighten out the XML/XMLitem distinction.
//     if (xml.length == 0 ) { return kappa({label:'None', 'value':({})});}
//      obj = xml[0];
      obj = xml;
      if (obj == undefined) {
         return kappa({label:'None', 'value':({})}); 
      }

      //  Take note!!!
      if (attr == 'class') attr = 'className';

      var val = obj[attr];

      if (val == undefined) kappa({label:'None', 'value':({})});
      else kappa({'label':'Some', 'value':val});
   }
}

function is_integer(s) {
  return s.match(/^[0-9]+$/) != null;
}
is_integer = _continuationize(is_integer);

function objectType(kappa) {
  return function(obj) {
    obj = obj[0];
    kappa(typeof(obj) == 'object' ? obj.constructor : typeof(obj))
  }
}

function childNodes(kappa) {
   return function(elem) {
      var result = [];
      for (var i=0; i<elem[0].childNodes.length; i++) {
        result.push(elem[0].childNodes[i].cloneNode(true));
//        result.push(elem[0].childNodes[i]);
      }
      kappa(result);
   }
}

function time () { return  (new Date()).getTime() }

var start_time = time();

function isDefined(kappa) {
  return function(obj) {
    kappa(obj == undefined);
  }
}

textContent = _continuationize(function (node) {
  try { return node.innerHTML } catch (e) { return "" }
});

function _domproc(k) {
  return function (arg) {
    function continuation(msg) {
      _applyChanges([msg]);
      _call(recv, [], continuation);
    }
    _alert("dom waiting");
    _call(recv, [], continuation);
  }
}

var dom = null;

function setDomPid(pid) { 
            dom = pid 
          }

spawn(function (partiallyAppliedSpawn) { 
        partiallyAppliedSpawn(setDomPid)([]) 
})(_domproc)

