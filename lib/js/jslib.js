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
    return (new XMLSerializer()).serializeToString(xml)
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
      var serializer = new XMLSerializer();
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
  assert_noisy : function(value, message) {
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
  },
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
                                          return new String(constructor);
     else return 'UNKNOWN TYPE'
  },
  // Could also add type-predicates for Array, Error, Function, Date, etc.
  show : function(value) {
     if (DEBUG.is_xmlnode(value)) return xmldump(value)
     else return JSON.stringify(value)
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
  };
}();

var _maxPid = 0;             // the highest process id allocated so far 
var _mainPid = -1;           // the process id of the main process
var _current_pid = _mainPid; // the process id of the currently active process
var _handlingEvent = false;

var LINKS = function() {

  // A package of functions used internally, not callable from Links code.

  // BEGIN private LINKS vars/methods

  var _formkey = null;
    
  var _removeCGIArgs = function(str) {
    return str.replace(/\?.*/, "");
  }
    
  var _invokeClientCall = function (_kappa, _callPackage) {
    _debug('Invoking client call to ' + _callPackage.__name + '.');
    // FIXME: variables defined within this function can shadow
    // the Links function we're trying to execute.
    var _f = eval(_callPackage.__name);
    _yield(_f, _callPackage.__args,
           function (r) {_remoteCallContinue(_kappa, _callPackage, r)});
  }

  // continue a remote call after a server --> client call has finished
  var _remoteCallContinue = function (_kappa, _callPackage, _res) {  
    // Continue the computation on the server.
    var request_ = AJAX.newRequest();
    var _rootURL = _removeCGIArgs(location.href);
    //  _debug("POST URL is " + _rootURL);
    // FIXME: Need to determine async flag in same way as original call.
    request_.open('POST', _rootURL, true);
    request_.onreadystatechange = _remoteCallHandler(_kappa, request_);
    request_.setRequestHeader('Content-Type',
                              'application/x-www-form-urlencoded');
    // _debug("Sending " + JSON.stringify(_res)+ " to " + 
    //        _callPackage.__continuation);
    request_.send("__continuation=" + _callPackage.__continuation + 
                  "&__result=" + _base64encode(JSON.stringify(_res)));
  }
    
  var _remoteCallHandler = function (kappa, request) {
    return function() {
      if (request.readyState == AJAX.isComplete && !request.finished) {
       // I observed cases where this was being invoked twice with
       // isComplete readyState, so I added the finished flag. Is
       // this a bug in XHR, or in our code? --Ezra 10/07
       request.finished = true;
       var serverResponse = JSON.parseB64Safe(request.responseText);
  
       if ((serverResponse instanceof Object)
           && ('__continuation' in serverResponse)) {
          // it's a call to a client function
          _invokeClientCall(kappa, serverResponse);
        } else {
          // it's the final result: return it.
          kappa(serverResponse);
        }
      }
    }
  };

  //// BEGIN public LINKS methods.
  var LINKS = {

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
         var args = [];
         for (var i = 0; i < arguments.length-1; i++) {
           args[i] = arguments[i];
         }
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

    stringToCharlist : function(str) {
      var result = [];
      // NOTE:
      //   IE does not support the form
      //     for (var i in s) { ... }
      //   when s is a string, nor does it support s[i].
      for (var i = 0; i < str.length; ++i) {
        result.push(str.charAt(i));
      }
      return result;
    },

    charlistToString : function (chlist) {
      DEBUG.assert(DEBUG.is_array(chlist), 
                   "_charlistToString Expected an array, got: " + chlist);
      var str = "";
      for (var i in chlist) {
        str += chlist[i]
      }
      return str;
    },

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
        result[i] = f(list[i])
      }
      return result;
    },
    
    XmlToDomNodes : function (xmlForest) {
      DEBUG.assert(DEBUG.is_array(xmlForest),
                   'LINKS.XmlToDomNodes expected an array, but got ' + 
                   xmlForest);
      return LINKS.map(LINKS.singleXmlToDomNodes, xmlForest);
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

    //// Remote calls

    remoteCall : function(kappa) {
     DEBUG.assert_noisy(DEBUG.is_function(kappa),
       "remoteCall given non-function as continuation");
     return function(name, arguments) {
       _debug ("Making remote call to: " + name);
       var synchronous = (_current_pid == _mainPid) || _handlingEvent;
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
       if (!synchronous)
         request.onreadystatechange = _remoteCallHandler(setpid_kappa, request);
   
       request.send("__name=" + _base64encode(name) + 
                    "&__args=" + _base64encode(JSON.stringify(arguments)));
       if (synchronous)
         return _remoteCallHandler(setpid_kappa, request)();
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
             return $chl(xs[i].value);
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
    //    _debug("down");
      var node = root.firstChild;
      var direction = down;
      while(node != root) {
        switch(direction) {
          case down:
            f(node);
            if(node.firstChild) {
    //          _debug("down");
              node = node.firstChild;
              direction = down;
            } else {
              direction = right;
            }
            break;
          case right:
            if(node.nextSibling) {
    //          _debug("right");
              node = node.nextSibling;
              direction = down;
            } else {
              direction = up;
            }
            break;
          case up:
    //        _debug("up");
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

  LINKS.singleXmlToDomNodes = function (xmlObj) {
    DEBUG.assert(DEBUG.is_array(xmlObj),
                 'LINKS.singleXmlToDomNodes expected an array, but got ' + xmlObj);
    if (xmlObj[0] == "ELEMENT") {
      var tag = xmlObj[1];
      var attrs = xmlObj[2];
      var body = xmlObj[3];
      var node = document.createElement(tag);
  
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
          node.style.cssText = $str(attrs['style']);
        } else {
          node.setAttribute(name, $str(attrs[name]));
        }
      }
      for (var i = 0; i < body.length; i++) {
        var child = LINKS.singleXmlToDomNodes(body[i]);
        node.appendChild(child);
      }
      return node;
    } else if (xmlObj[0] == "TEXT"){
      return document.createTextNode(xmlObj[1]);
    } else {
      throw "unknown XML node " + xmlObj[0] + " in LINKS.singleXmlToDomNodes"
    }

  };

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

      for (var i in l) {
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
    }
    else {
      return (l == r);
    }
  };
  LINKS . $str = LINKS.charlistToString;
  LINKS . $chl = LINKS.stringToCharlist;
  return LINKS;
} ();

var $str = LINKS.charlistToString;
var $chl = LINKS.stringToCharlist;

function _debug(msg) {
   if (DEBUGGING) {
     if(DEBUG.is_charlist(msg))
       _dwindow.document.write('<b>' + _current_pid + '</b> : ' + $str(msg) + '<br/>');
     else
       _dwindow.document.write('<b>' + _current_pid + '</b> : ' + msg + '<br/>');
   }
   _dwindow.scroll(0, _dwindow.scrollMaxY);
}

var debug = LINKS.kify(_debug);

function _alertDialog(msg) {
  DEBUG.assert(DEBUG.is_charlist(msg), "_alertDialog expected charlist, got: "
                 + msg);
  return (alert($str(msg)));
}
var alertDialog = LINKS.kify(_alertDialog);

function _tilde(s, regex) {
    var r = Regex.compile(regex);
//    _debug("compiled regex: " + r);
//    _debug("string: " + $str(s));
    return (new RegExp(r)).test($str(s));
}
var tilde = LINKS.kify(_tilde);

var _intToString = function (x) { return $chl(String(x)) }
var _stringToInt = function (x) { return parseInt($str(x)) }
var _intToFloat = Number;
var _floatToInt = Math.floor;
var _floatToString = function (x) { return $chl(String(x)) }
var _stringToFloat = function (x) { return parseFloat($str(x)) }

var intToString = LINKS.kify(_intToString);
var stringToInt = LINKS.kify(_stringToInt);
var intToFloat = LINKS.kify(_intToFloat);
var floatToInt = LINKS.kify(_floatToInt);
var floatToString = LINKS.kify(_floatToString);
var stringToFloat = LINKS.kify(_stringToFloat);

function _Concat(xs, ys) { return LINKS.concat(xs, ys); }
function _Cons(x, xs) { return _Concat([x], xs); }

function _not(x) { return !x; }
function _empty(list) { return (list.length == 0); }
function _hd(list) { return list[0]; }
function _tl(list) { return list.slice(1); }

function _length(list) { return list.length }
function _take(n, list) { return list.slice(0, n) }
function _drop(n, list) { return list.slice(n) }

function _max(list) {
  if(list.length == 0)
    return {'_label':'None'}
  else {
    var x = _hd(list);
    for (y in _tl(list)) {
      if (y > x)
        x = y;
    }
    return {'_label':'Some', '_value':x}
  }
}

function _min(list) {
  if(list.length == 0)
    return {'_label':'None'}
  else {
    var x = _hd(list);
    for (y in _tl(list)) {
      if (y < x)
        x = y;
    }
    return {'_label':'Some', '_value':x}
  }
}

function _abs(f) {
  return (function () {
    var kappa = arguments[arguments.length - 1];
    var args = [];
    for (var i = 0; i < arguments.length - 1; i++) {
       args[i+1] = arguments[i]; // nb: arguments array is 0-based; our 
                                 // tuples are 1-based.
    }
    return f(args, kappa);
  });
}

function _app(f) {
  return (function (tuple, kappa) {
     var args = [];
     for (var i = 1; i in tuple; i++) {
       args[i-1] = tuple[i]; // nb: arguments array is 0-based; our 
                             // tuples are 1-based.
     }
     args[i-1] = kappa;
     return f.apply(f, args);
  });
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
var abs    = _abs ; // LINKS.kify(_abs);
var app    = _app ; // LINKS.kify(_app);

//// Basic operators in direct-style
////   (perhaps move these into LINKS package)
function __minus(l,r)  { return l - r; }
function __plus(l,r)   { return l + r ; }
function __times(l,r)  { return l * r ; }
function __divide(l,r) { return l / r ; }
function _le(l,r)     {   return l <= r ; }
function _ge(l,r)     {   return l >= r ; }
function _gt(l,r)     {   return l >  r ; }

var _plus   = LINKS.kify(__plus);
var _times  = LINKS.kify(__times);
var _divide = LINKS.kify(__divide);
var _minus  = LINKS.kify(__minus);
var _hyphen = _minus;
var _star   = _times;
var _slash  = _divide;

var _plus_fullstop   = _plus;
var _hyphen_fullstop = _minus;
var _star_fullstop   = _times;
var _slash_fullstop  = _divide;

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
  var msg = $str(msg);
  alert(msg);
  throw ("Error: " + msg);
}

var error = LINKS.kify(_error);

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
  DEBUG.assert_noisy(DEBUG.is_array(id),
                     "_getNodeById Expected an array, got: " + id);
  var id = $str(id);
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
  var id = $str(id);
  var element = document.getElementById(id);
  DEBUG.assert(element != null, "invalid input node (id " + id + ")");
  DEBUG.assert(element.value != undefined, "invalid input value in id " + id)
  return $chl(element.value);
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
        $chl(nodeRef.attributes[i].value)
    }
    for (var i=0; i < children.length; i++) {
      DEBUG.assert(children[i][0] == "ELEMENT" || children[i][0] == "TEXT",
                   "Invalid children list constructed in DOMNodeToXML")
    }
    var result = [["ELEMENT", nodeRef.tagName, attrs, children]];
//    _debug("getValue is returning: ");
//    _dump(result);
    return result;
  } else {
    throw("Unknown node type " + nodeRef.nodeType + " in GetXml")
  }
}
getValue = LINKS.kify(_getValue);

// Accessors for DomRefs
function _domGetTagNameFromRef(nodeRef) {
  return $chl(nodeRef.nodeName);
}

function _domGetAttributeFromRef(nodeRef, attr) {
  var attr = $str(attr);
  if (attr == 'offsetTop') {
    return _intToString(nodeRef.offsetTop);
  } else if (attr == 'offsetLeft') {
    return _intToString(nodeRef.offsetLeft);
  }

  return $chl(nodeRef.getAttribute(attr));
}
function _domSetAttributeFromRef(nodeRef, attr, value) {
  return nodeRef.setAttribute($str(attr),
                              $str(value));
}

function _domSetStyleAttrFromRef(nodeRef, attr, value) {
  return nodeRef.style[$str(attr)] = $str(value);
}

function _domGetStyleAttrFromRef(nodeRef, attr) {
  return nodeRef.style[$str(attr)];
}

function _domGetNodeValueFromRef(node) {
  return $chl(node.value);
}
var domGetNodeValueFromRef = LINKS.kify(_domGetNodeValueFromRef)

var domGetTagNameFromRef = LINKS.kify(_domGetTagNameFromRef);
var domGetAttributeFromRef = LINKS.kify(_domGetAttributeFromRef);
var domSetAttributeFromRef = LINKS.kify(_domSetAttributeFromRef);
var domSetStyleAttrFromRef = LINKS.kify(_domSetStyleAttrFromRef);
var domGetStyleAttrFromRef = LINKS.kify(_domGetStyleAttrFromRef);

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

function _registerEventHandlers(handlers) {
// basically the same as _registerFormEventHandlers, except
// strings are Links strings (char lists) and records are pairs
  var key = '_key' + _get_fresh_node_key();

  for (var i = 0; i < handlers.length; i++) {
     var event = $str(handlers[i][1]);
     var handler =_wrapEventHandler(handlers[i][2]);
     if (!_eventHandlers[key]) {
       _eventHandlers[key] = [];
     }
     _eventHandlers[key][event] = handler;
  }
  return $chl(key);
}

function _getTarget(event) { return YAHOO.util.Event.getTarget(event, false) }
function _getTargetValue(event) { 
  return $chl(_getTarget(event).value);
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

function _stringifyB64(s) {
  return $chl(JSON.stringifyB64(s));
}
var stringifyB64 = LINKS.kify(_stringifyB64);


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
  DEBUG.assert(DEBUG.is_array(s), "_stringToXml Expected an array, got: " + s);
  return [["TEXT", $str(s)]];
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
  return $chl(xml[0][1]);
}

function _getChildNodes(xml) {
  if (xml[0][0] != "ELEMENT")
    throw "getChildNodes() applied to non-element node";
  return xml[0][3];
}

function _getTextContent(xml) {
  if (xml[0][0] != "TEXT")
    throw "getTextContent() applied to non-text node";
  return $chl(xml[0][1]);
}

function _getAttributes(xml) {
  if (xml[0][0] != "ELEMENT")
    throw "getAttributes() applied to non-element node";
  var attrs = [];
  for (name in xml[0][2])
    attrs.push({'1':$chl(name), '2':xml[0][2][name]})
  return attrs;
}

function _hasAttribute(xml, attrName) {
  if (xml[0][0] != "ELEMENT")
    throw "hasAttribute() applied to non-element node";

  // silly js idiom (this can't be eta-reduced)
  if (xml[0][2][$str(attrName)]) return true;
  else return false;
}

function _getAttribute(xml, attrName) {
  if (xml[0][0] != "ELEMENT")
    throw "getAttribute() applied to non-element node";
  return xml[0][2][$str(attrName)];
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
function _clientTime () { return (new Date()).getTime(); } 
var clientTime = LINKS.kify(_clientTime);

function _dateToLinksDate(d) {
  return {year:d.getFullYear(), month:d.getMonth(), day:d.getDate(),
        hours:d.getHours(), minutes:d.getMinutes(), seconds:d.getSeconds()};
}

function _linksDateToDate(d) {
  return new Date(d.year, d.month, d.day, d.hours, d.minutes, d.seconds);
}

// convert seconds since beginning of 1970 to a date
function _intToDate(t) {
  return _dateToLinksDate(new Date(t * 1000));
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

   return ($chl(key)); // returns the ID to give to the elt
}




// db functions (via remote calls)
// TODO!

var javascript = true;

// identity: a "toplevel" continuation
//
// [BUG]
//   this is a load of rubbish as _yield and _yieldCont don't actually return values
function _idy(x) { return x; }


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
  _debug("Mailbox status:");
  for (var i in _mailboxes) {
    if (_mailboxes[i].length > 0)
      _debug("&nbsp; pid " + i + ": " + _mailboxes[i].length + " msgs waiting");
  }
  var waitingPids = "";
  for (var i in _mailboxes) {
    waitingPids += i + ", "
  }
  _debug(" waiting processes: " + waitingPids + ".");
}

function _spawn(f) {
    // f is a zero-argument function
    var childPid = ++_maxPid;
    _mailboxes[childPid] = [];
    setTimeout(function () { 
                 _debug("launched process #" + childPid);
                 _current_pid = childPid;
                 f(function () { delete _mailboxes[childPid] }) 
               }, 200);
   return childPid;
}

function spawn(f, kappa) {
   kappa(_spawn(f));
}

function spawnWait(f, kappa) {
  // f is a zero-argument CPS function
  var parentPid = _current_pid
  var childPid = ++_maxPid;
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

function _haveMail() {
  return _mailboxes[_self()].length != 0;
}
var haveMail = LINKS.kify(_haveMail);

var _sched_pause = 0;

function _wakeup(pid) {
  if (_blocked_procs[pid]) {
    var proc = _blocked_procs[pid];
    delete _blocked_procs[pid];
    setTimeout(proc, _sched_pause);
  }
  else {
    // already awake?
  }
}

function _send(pid, msg) {
//  _debug("sending message '" + msg._label + "' to pid "+pid);
  if(_mailboxes[pid]) {
    _mailboxes[pid].unshift(msg);
    _wakeup(pid);
  }
  return {};  
}

function send(pid, msg, kappa) {
  kappa(_send(pid, msg));
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
  DEBUG.assert(_current_pid != _mainPid,
               "Cannot call recv() in main process.");
  DEBUG.assert(_mailboxes[_current_pid],
               "Process " + _current_pid + " seems not to have been created.")
  if ( _mailboxes[_current_pid].length > 0) {
    msg = _mailboxes[_current_pid].pop()
    //_debug("received message '"+ msg._label +"'");
    kappa(msg);
  } else {
    var current_pid = _current_pid;
    _block_proc(current_pid,
                function () {
                    _current_pid = current_pid;
                    //_debug("scheduled process " + current_pid);
                    recv(kappa);
                });
    //_debug("blocked: "+current_pid)
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

// SCHEDULER

var _yieldCount = 0;
var _yieldGranularity = 60;
var _callCount = 0;

function _Continuation(v) { this.v = v }

// yield: give up control for another "thread" to work.
// if we're running in an event handler then don't yield (but 
// do throw away the stack periodically instead).
function _yield(f, args, k) {
  DEBUG.assert_noisy(DEBUG.is_function(f),
                     "_yield: 1st arg expected a function, got: " + f);
  DEBUG.assert_noisy(DEBUG.is_function(k),
                     "_yield: 3rd arg expected a function, got: " + k);
  DEBUG.assert(DEBUG.is_array(args), 
               "_yield: 2nd arg expected an array, got: " + args);
//  var arguments = args.concat([k]);
  var argslen = args.length;
  var arguments = ___append(args, k);
//  DEBUG.assert(argslen + 1 == arguments.length,
//               'arglen check failed before yielding');
  ++_yieldCount;
//  _debug("yield count: "^_yieldCount);
  if ((_yieldCount % _yieldGranularity) == 0) {
    if (!_handlingEvent) {
      var current_pid = _current_pid;
      setTimeout((function() {
//                      DEBUG.assert(argslen + 1 == arguments.length,
//                                   'arglen check failed after yielding');

                      _current_pid = current_pid;
//                      _debug("yielding to " + current_pid);
                      return f.apply(f, ___append(args, k))}),
//                      f(a, k)}),
                 _sched_pause);
    }
    else {
      throw new _Continuation(function () { 
                                return f.apply(f, ___append(args, k));
                              });
    }
  } 
  else {
    return f.apply(f, ___append(args, k));
//    return f(a,k);
  }
}

function _yieldCont(k, arg) {
  DEBUG.assert_noisy(DEBUG.is_function(k),
                     "_yieldCont: expected a function, got: " + k);
  ++_yieldCount;
  if ((_yieldCount % _yieldGranularity) == 0) {
    if (!_handlingEvent) {
      var current_pid = _current_pid;
      setTimeout((function() {
                      _current_pid = current_pid;
                      k(arg) }),
                 _sched_pause);
    }
    else {
      throw new _Continuation(function () { k(arg) });
    }
  } 
  else {
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

function _toUpper(c) {
  DEBUG.assert(c.length == 1, "_toUpper only operates on single characters");
  return c.toUpperCase();
}

function _toLower(c) {
  DEBUG.assert(c.length == 1, "_toLower only operates on single characters");
  return c.toLowerCase();
}

var toUpper = LINKS.kify(_toUpper);
var toLower = LINKS.kify(_toLower);

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
_include("extras.js")


function _chartest(r) { 
    return function (c) {return r.test(c);};
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

var _chr = String.fromCharCode;
var chr = LINKS.kify(_chr);
function _ord(c) { return c.charCodeAt(0); }
var ord = LINKS.kify(_ord);

var _toLower = String.toLowerCase;
var toLower = LINKS.kify(_toLower);

var _toUpper = String.toUpperCase;
var toUpper = LINKS.kify(_toUpper);

var _sqrt = Math.sqrt; var sqrt = LINKS.kify(_sqrt);
var _floor = Math.floor; var floor = LINKS.kify(_floor);
var _ceiling = Math.ceil; var ceiling = LINKS.kify(_ceiling);
var _tan = Math.tan; var tan = LINKS.kify(_tan);
var _sin = Math.sin; var sin = LINKS.kify(_sin);
var _cos = Math.cos; var cos = LINKS.kify(_cos);
var _log = Math.log; var log = LINKS.kify(_log);

function _environment() {
  LINKS.unimpl("environment");
}
var environment = LINKS.kify(_environment);

function _redirect(url) {
  LINKS.unimpl("redirect");
}

var QUIRKS = function () {
 return {
// BEGIN code from quirksmode.org
   createCookie : function(name,value,days) {
     if (days) {
       var date = new Date();
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
  QUIRKS.createCookie($str(cookieName),
                      $str(value), 
                      10000);
  return {};
}
var setCookie = LINKS.kify(_setCookie);

function _getCookie(cookieName) {
  return $chl(QUIRKS.readCookie($str(cookieName)));
}
var getCookie = LINKS.kify(_getCookie);

function _random() {return Math.random();}
var random = LINKS.kify(_random);

