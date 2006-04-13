// Links js runtime.

var DEBUGGING = false;
var __dwindow = DEBUGGING ? open('', 'debugwindow','width=550,height=800,toolbar=0,scrollbars=yes') : null;

 Node.prototype.swapNode = function (node) {
    var nextSibling = this.nextSibling;
    var parentNode = this.parentNode;
    node.parentNode.replaceChild(this, node);
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
 },
}
}();

function __debug(msg) {
   if (DEBUGGING) {
     __dwindow.document.write('<b>' + __current_pid + '</b> : ' + msg + '<br/>');
   }
}
__alert = __debug;

// utilities (NOT CPS'd)
function isEmpty(list) { return (list.length == 0); }
function isarray(obj) {
  return typeof(obj) == 'object' && obj.constructor == Array
}

// library functions (CPS'd)
function __continuationize(f) {
    return function (kappa) {
        return function () {
            return kappa(f.apply(f, arguments));
        };
    };
}
var float_of_int = __continuationize(Number);
var string_of_float = __continuationize(String);
var int_of_string = __continuationize(parseInt);
var string_of_int = __continuationize(String);
//function not(x) { return !x; }
function not(kappa) { return function(x) { kappa( !x ); } }
function empty(kappa) { return function(list) { kappa(list.length == 0); } }
function hd(kappa) { return function(list) { kappa(list[0]); } }
function tl(kappa) { return function(list) { kappa(list.slice(1)); } }

// library functions (NOT CPS'd)
function __minus(l)  { return function (r) { return l -  r ; }}
function __plus(l)   { return function (r) { return l +  r ; }}
function __times(l)  { return function (r) { return l *  r ; }}
function __divide(l) { return function (r) { return l /  r ; }}
function __eq(l,r)     { 
  if (typeof(l) == 'object' && l.constructor == Array && 
      typeof(r) == 'object' && r.constructor == Array) {
    return l.length == 0 && r.length == 0
  }
  else return (l == r)
 }
function __le(l,r)     {   return l <= r ; }
function __ge(l,r)     {   return l >= r ; }
function __gt(l,r)     {   return l >  r ; }

// Links-level debugging routines
function debug(kappa) {
  return function(msg) {
     __debug(msg);
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

dump = __continuationize(
  function (obj) {
    if (obj == undefined)
      __debug(obj + " : undefined");
    else
      __debug("==TYPE== " + typeof(obj) + " " + 
        (typeof(obj) == 'object' ? obj.constructor : ""));
      for (var i in obj) {
        try {
          __debug(i + "=" + obj[i]);
        } catch (e) {
          __debug(i + " (died)");
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
function __xmldump(xml) {
   return (new XMLSerializer()).serializeToString(xml)
}

function __applyChanges(changes) {
  for (var i in changes) {
    var change = changes[i];
    if (change.label == 'ReplaceElement') {
    var element = document.getElementById(change.value.id);
    if (!element) __alert("element " + change.value.id + " does not exist");
          element.parentNode.replaceChild(change.value.replacement[0], element);
    }
    else if (change.label == 'AppendChild') {
        var element = document.getElementById(change.value.id);
        if (!element) __alert("element " + change.value.id + " does not exist");
        element.appendChild(change.value.replacement[0]);
    }  
    else if (change.label == 'SwapNodes') {
          var one = change.value.first;
          var two = change.value.second;
          one.swapNode(two);
    }  
    else if (change.label == 'Document') {
          __start(change.value);
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

//  __concat(a, b)
//     concatenate two sequences: either strings or lists
function __concat () {
  if (typeof(arguments[0]) == 'string') {
    var rv = '';
    for (var i = 0; i < arguments.length; i++) {
      rv += arguments[i];
    }
    return rv;
  }
  else {
//    __debug("concatenating " + arguments.length + " lists");
    if (arguments.length == 0) return [];
    var rv = arguments[0];
    for (var i = 1; i < arguments.length; i++) {
//      __debug("concatenating " + cmap(see, ", ", rv) + 
//              " with " + cmap(see, ", ", arguments[i]));
      rv = rv.concat(arguments[i]);
    }
    return rv;
  }
}

//  __accum(f, i)
//    concatMap: apply f to every element of `list'
//    and concatenate the results.
function __accumAux(kappa) {
  return function(fn, list, result) {
    if (isEmpty(list)) kappa(result)
    else {
      // instead of cps hd/tl, should use js ops directly
      hd(function(x) {
           fn(function(ximg) {
                tl(function (rest) {
                     __accumAux(kappa)(fn, rest, result.concat(ximg))
                   })(list)
              } )(x);
         })(list)
    }
  }
}

function __accum(kappa) {
  return function(fn, list) {
    __accumAux(kappa)(fn, list, []);
  }
}

/// XML
//  __XML(tag, attrs, children)
//    create a DOM node with name `tag'
//                       and attributes `attrs' (a dictionary)
//                       and children `children' (a sequence of DOM 
//                       nodes and strings)    

function __XML(kappa) {
  return function(tag, attrs, body) { 
   var node = document.createElement(tag);
   for (name in attrs) {
      node.setAttribute(name, attrs[name]);
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
//   __extend(record, tag, value)
//    extend a record (dictionary) with a new field (label `tag'; value `value').
//    Don't update the old record.
function __extend(object, label, value) {
  var copy = {};
  for (var name in object) {
     copy[name] = object[name];
  }
  copy[label] = value;
  return copy;
}

//   __project(object, name)
//    project a field of a record
function __project(object, name) { return object[name]; }

function __vrntLbl(object) { return object['label']}
function __vrntVal(object) { return object['value']}

function __fail(str) {
  __alert("Internal error: " + str);
}

// Page update

//  __start(tree)
//    Replace the current page with `tree'.
function __start(tree) {
  // save here
  var __saved_fieldvals = [];
  var inputFields = document.getElementsByTagName("input");
  for (var i = 0; i < inputFields.length; i++) { 
     var current = inputFields[i];
     __saved_fieldvals.push({'field' : current.id, 'value' : current.value});
  }
  var d = document.documentElement;
  var l = d.childNodes.length;
  for (var i = 0; i < l; i++) {
     d.removeChild( d.childNodes[0] );
  }
  d.appendChild( tree[0] );
  // restore here
  for (var i = 0; i < __saved_fieldvals.length; i++) { 
     var current = __saved_fieldvals[i];
     var elem = document.getElementById(current.field);
     if (elem) {
        elem.value = current.value; 
     }
  }
  
  // hmm.
  __focus();
  __alert(time() - start_time + ' milliseconds');

}

// //  __registerFormEventHandlers(id, [action1, action2, ...])
// //    Register an event handler.
// //    We need to register it, and invoke it through the registry, 
// //    because attributes like onClick are merely strings, whereas we 
// //    want to associate proper closures.
// //    `id' should be either 0 or the real ID of the element to which 
// //    you're attaching the handler. If it is 0, a new ID will be
// //    generated and returned.

var __id = 0;

function __registerFormEventHandlers(id, actions) {
   the_id = id != 0 ? id : __id++;
   for (var i = 0; i < actions.length; i++) {
     var action = actions[i];
        // FIXME: clone this ??

     if (!__eventHandlers[action.evName])
       __eventHandlers[action.evName] = [];
     __eventHandlers[action.evName][the_id] = action.handler;
   }
   return the_id;
}
var __eventHandlers = {};

function enxml(kappa) { return function (body) { return kappa([document.createTextNode(body)]); }}
var javascript = true;



//// Remote calls

/// serialization
// TODO!

/// calls.
// TODO!
// Base64 encoding and decoding (from www.farfarfar.com)
var __charBase64 = new Array(
    'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
    'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
    'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
    'w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/'
);

function __base64encode(str)
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

    out += __charBase64[chr1 >> 2] + __charBase64[((chr1 & 0x03) << 4) | (chr2 >> 4)];

    if (isNaN(chr2))
        out += '==';
    else if (isNaN(chr3))
        out += __charBase64[enc3] + '=';
    else
        out += __charBase64[enc3] + __charBase64[chr3 & 0x3F];
  } while (i < len);

  return out;
}

var __indexBase64 = new Array(
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


function __base64decode(str)
{
  var out = "";
  var i = 0;
  var len = str.length;

  do
  {
    var enc1 = __indexBase64[str.charCodeAt(i++)];
    var enc2 = __indexBase64[str.charCodeAt(i++)];
    var enc3 = __indexBase64[str.charCodeAt(i++)];
    var enc4 = __indexBase64[str.charCodeAt(i++)];

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
function __tuple_as_list(tuple) {
  var rv = [];
  for (var i = 1; ; i++) {
     var elem = tuple[i];
     if (elem == undefined) return rv;
     rv.push(elem);
  }
}

var isLoaded = 2;
var isComplete = 4;

function __remoteCallHandler(kappa, request) {
  return function() {
    if (request.readyState == isComplete) {
     // TBD: this apparently triggers if we leave the page.
     var serverResponse = JSON.parse(__base64decode(request.responseText.replace('\n', '')));
     if ((serverResponse instanceof Object) && ('__continuation' in serverResponse)) {
        // it's a continuation
        var _res = null;
        var cont = function(r) { res = r; }
        var f = window[serverResponse.__name](cont);
        f.apply(f, __tuple_as_list(serverResponse.__arg));
//        __alert('result : ' + _res)
        var request_ = new XMLHttpRequest();
        request_.open('POST', '#', true);
        request_.onreadystatechange = __remoteCallHandler(kappa, request_);
        request_.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
        request_.send("__continuation=" + serverResponse.__continuation + "&__result=" + __base64encode(JSON.stringify(_res)));
      }
      else {
        // it's the final result: return it.
        kappa(serverResponse);
      }
    }
  }
}

function __remoteCall(kappa) {
  return function(name, arguments) {

    var request = new XMLHttpRequest();
    asynch = true;
    request.open("POST", "#", asynch);
    request.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
    request.onreadystatechange = __remoteCallHandler(kappa, request);

    request.send("__name=" + __base64encode(name) + 
                 "&__args=" + __base64encode(JSON.stringify(arguments)));

  }
} 

// db functions (via remote calls)
// TODO!

/// focus stuff
var __focused = null;
function __val(x) { 
    var y = document.getElementById(x);
    if (y) {return y.value} else { return "boogers"}
}
function __focus() {
  if (__focused) {
      var y = document.getElementById(__focused);
      if (y) { y.focus(); }
  }

}

var javascript = true;

// identity: a "toplevel" continuation
function __idy(x) {
  return x;
}

var __maxPid = 0;
var __current_pid = 0;

function spawn(kCurry) {
  return function (f) {
    kCurry(function (kappa) {
      return function (arg) {
        var childPid = ++__maxPid;
        __mailboxes[childPid] = [];
        setTimeout(function () { 
                     __current_pid = childPid;
                     f(__applyChanges)(arg) 
                   }, 200);
        kappa(childPid);
      };
    });
  }
}


var __mailboxes = {0:[]};
var __blocked_procs = {};
//var __suspended_procs = [];

function self(kappa) { 
  return function(unit) {
     return kappa(__current_pid) 
  }
}

var sched_pause = 10;

function __wakeup(pid) {
  if (__blocked_procs[pid]) {
    var proc = __blocked_procs[pid];
    delete __blocked_procs[pid];
    setTimeout(proc, sched_pause);
  }
  else {
  }
}

function send(kCurry) {
  return function(pid) {
   kCurry(function (kappa) {
      return function (msg) {
        __mailboxes[pid].push(msg);
        __wakeup(pid);
        kappa(1);
      }
    })
  }
}

function __dictlength(x) {
  var length = 0;
  for (var prop in x) { 
    length++;
  }
 return length;
}


function __block_proc(pid, its_cont) {
  // FIXME: const?? is this OK?
  var current_pid = __current_pid;
  __blocked_procs[pid] = function () { __current_pid = current_pid;  its_cont() };
  // discard stack
}

function recv(kappa) {
  return function() {
    if ( __mailboxes[__current_pid].length > 0) {
      msg = __mailboxes[__current_pid].pop()
      kappa(msg);
    } else {
      __block_proc(__current_pid, function () { recv(kappa)() });
    }
  }
}

function __scheduler() {
  
}

var __yieldCount = 0;
var __yieldGranularity = 100;

// yield: give up control for another "thread" to work
function __yield(my_cont) {
  ++__yieldCount;
  if ((__yieldCount % __yieldGranularity) == 0) {
    var current_pid = __current_pid;
    setTimeout((function() { __current_pid = current_pid; my_cont()}),
               sched_pause);
  }
  else {
    my_cont();
  }
}

// __call: using this would make compiled js code easier to read.
function __call(f, args, kappa) {
  __yield(function() { it = f(kappa); it.apply(it, args) });
}


function print(kappa) {
  return function(str) {
    __alert(str);
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
is_integer = __continuationize(is_integer);

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

textContent = __continuationize(function (node) {
  try { return node.innerHTML } catch (e) { return "" }
});

function __domproc(k) {
  return function (arg) {
    function continuation(msg) {
      __applyChanges([msg]);
      __call(recv, [], continuation);
    }
    __alert("dom waiting");
    __call(recv, [], continuation);
  }
}

var dom = null;

function setDomPid(pid) { 
            dom = pid 
          }

spawn(function (partiallyAppliedSpawn) { 
        partiallyAppliedSpawn(setDomPid)([]) 
})(__domproc)

