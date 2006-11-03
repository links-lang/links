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

var DEBUG = function () {
 function is_instance(value, type, constructor) {
    return value != undefined
        && (typeof value == type || value instanceof Object && value.constructor == constructor)
 }
 function xmldump(xml) {
   return (new XMLSerializer()).serializeToString(xml)
 }
 return {
 assert_noisy : function(value) {
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
 is_number : function(value) {
  return is_instance(value, 'number', Number);
 },
 is_string: function(value) {
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
  // [IE] superflous commas aren't allowed
}
}();

function _debug(msg) {
   if (DEBUGGING) {
     _dwindow.document.write('<b>' + _current_pid + '</b> : ' + msg + '<br/>');
   }
}
_alert = _debug;

function alertDialog(k) {
  return function (msg) {
    return k(alert(msg));
  }
}

// utilities (NOT CPS'd)
function isEmpty(list) { return (list.length == 0); }
function isarray(obj) {
  return typeof(obj) == 'object' && obj.constructor == Array
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

function _extractArgs(arguments, arity) {
  if(arity == 0)
    return [];
  else if(arity == 1)
    return [arguments[0]];
  else
    return _tuple_as_list(arguments[0]);
}

// _continuationize
//   Turns a "trivial" js function into a continuationized one under the
//   Links calling conventions. "trivial" means it cannot call back to a
//   Links function, and that the scheduler can safely be suspended
//   while it runs.
function _continuationize(f, arity) {
        return function () {
            // Nota bene: arguments is not a real array, hence no .pop() 
            var kappa = arguments[arguments.length-1];
            var args = [];
            for (var i = 0; i < arguments.length-1; i++) {
              args[i] = arguments[i];
            }
            return kappa(f.apply(f, _extractArgs(args, arity)));
        };
}
function _continuationizeBinaryCurried(f) {
      return function () {
        var args1 = arguments[0];
        var kappa = arguments[1];
        return kappa(
          function () {
              var args2 = arguments[0];
              var kappa = arguments[1];
              return kappa(f.apply(f, [args1]).apply(f, [args2]));
          }
        );
      }
}

function _continuationizeMethod(obj, method) {
    return function (kappa) {
        return function () {
            return kappa(method.apply(obj, arguments));
        };
    };
}
function _tilde(s) {
//  var obj = s;
//  alert(obj + " : " + typeof(obj) + (typeof(obj) == 'object' ? obj.constructor : ''));
  return function (regex) {
//    var obj = regex;
//    alert(obj + " : " + typeof(obj) + (typeof(obj) == 'object' ? obj.constructor : ''));
//    alert(Regex);
//    alert(Regex.compile);
    var r = Regex.compile(regex);
    return (new RegExp("^" + r + "$")).test(_charlistToString(s));
  }
}
var tilde = _continuationizeBinaryCurried(_tilde);

function _stringToCharlist(str) {
  var result = [];
  for (var i in str) {
    result.push(str[i]);
  }
  return result;
}

function _charlistToString(chlist) {
  DEBUG.assert(DEBUG.is_array(chlist));
  var str = "";
  for (var i in chlist) {
    str += chlist[i]
  }
  return str;
}

var _stringToInt = function (x) {return parseInt(_charlistToString(x)) }
var _intToFloat = Number
var _floatToString = function (x) { return _stringToCharlist(String(x)) }
var _intToString = function (x) { return _stringToCharlist(String(x)) }


var stringToInt = _continuationize(_stringToInt, 1);
var intToFloat = _continuationize(_intToFloat, 1);
var floatToString = _continuationize(_floatToString, 1);
var intToString = _continuationize(_intToString, 1);
//function not(x) { return !x; }

function _not(x) { return !x; }
function _empty(list) { return (list.length == 0); }
function _hd(list) { return list[0]; }
function _tl(list) { return list.slice(1); }

function _length(list) { list.length }
function _take(list, n) { list.slice(0, n) }
function _drop(list, n) { list.slice(n) }

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

var not = _continuationize(_not, 1);
var empty = _continuationize(_empty, 1);
var hd = _continuationize(_hd, 1);
var tl = _continuationize(_tl, 1);

var length = _continuationize(_length, 1);
var take = _continuationize(_take, 2);
var drop = _continuationize(_drop, 2);
var max = _continuationize(_max, 1);
var min = _continuationize(_min, 1);

// library functions (NOT CPS'd)
function _minus(l)  { return function (r) { return l -  r ; }}
function _plus(l)   { return function (r) { return l +  r ; }}
function _times(l)  { return function (r) { return l *  r ; }}
function _divide(l) { return function (r) { return l /  r ; }}
// function _eq(l,r)     {
//   if(l != null && l != undefined && r != null && r != undefined) {
//     if (typeof(l) == 'object' && l.constructor == Array && 
//         typeof(r) == 'object' && r.constructor == Array) {
//       return l.length == 0 && r.length == 0
//     }
//   }
//   else return (l == r)
//  }
function _eq(l,r)     {
  if(l == r) {
    return true;
  }
 
  if (l == r)
    return true;

  if (l == null) {
    return (r == null);
  } else if (r == null) return false;
  if (DEBUG.is_unit(l) && DEBUG.is_unit(r)) {
    return true;
  }
  if (typeof(l) == 'object' && l != undefined && l != null && l.constructor == Array && 
      typeof(r) == 'object' && r != undefined && r != null && r.constructor == Array) {
    if (l.length != r.length)
        return false;
    for (var i in l)
      if (!_eq(l[i], r[i])) return false;
    return true;
  }
  else if(typeof(l) == 'object' && l != undefined && l != null &&
          typeof(r) == 'object' && r != undefined && r != null) {
    if(l.constructor != r.constructor)
      return false;

    // [DODGEYNESS]
    //   - it isn't clear that structural equality is always the same as
    //   referential equality for DOM nodes
    //   - sometimes the following appears to return true when it shouldn't
    for(p in l) {
      if(!_eq(l.p, r.p)) {
        return false;
      }
    }
    for(p in r) {
      if(!_eq(l.p, r.p)) {
        return false;
      }
    }
    return true;
  }
  else return (l == r)
 }
function _le(l,r)     {   return l <= r ; }
function _ge(l,r)     {   return l >= r ; }
function _gt(l,r)     {   return l >  r ; }

// Links-level debugging routines
function debug(msg, kappa) {
   _debug(_charlistToString(msg));
   return kappa();
}

function _debugObj(obj) {
  if (obj == undefined) {
    alert(obj + " : undefined");
  } else {
    alert(obj + " : " + typeof(obj) + ' ' + 
          (typeof(obj) == 'object' ? obj.constructor : ''));
  }
  return [];
}

debugObj = _continuationize(_debugObj, 1);

function _dump(obj) {
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

dump = _continuationize(_dump, 1);

var negate = _continuationize(function (x) { return -x }, 1);
var negatef = negate;

function error(msg, kappa) {
    alert(_charlistToString(msg));
    throw ("Error: " + msg);
}

/// ???
// [IE] the xml property is supposed to work for IE. It
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
function _xmldump(xmlNode) {
  var text = false;
  try {
    // Firefox
    var serializer = new XMLSerializer();
    text = serializer.serializeToString(xmlNode);
  }
  catch (e) {
    try {
      // IE
      text = xmlNode.xml;
    }
    catch (e) {}
  }
  return text;
//   return (new XMLSerializer()).serializeToString(xml)
}

// DOM interaction

//insertBeforeXml : xml -> domRef -> ()
function _insertBefore(insertXml, beforeNode) {
  var parent = beforeNode.parentNode;
  var nodes = (_XmlToDomNodes(insertXml)).reverse();
  for (var i=0; i < nodes.length; i++)
    parent.insertBefore(nodes[i], beforeNode)
  return {}
}

//appendChildXml : xml -> domRef -> ()
function _appendChildren(appendXml, parentNode) {
  var nodes = _XmlToDomNodes(appendXml);
  for (var i=0; i < nodes.length; i++) 
    parentNode.appendChild(nodes[i]);
  return {}
}

//removeNodeRef : domRef -> ()
function _removeNode(nodeRef) {
  if(nodeRef.parentNode)
    nodeRef.parentNode.removeChild(nodeRef);
  else
    throw ("Cannot remove DOM root node");

  return {}
}

//replaceNode : (xml, domRef) -> ()
function _replaceNode(withXml, replaceNode) {
  _insertBefore(withXml, replaceNode);
  _removeNode(replaceNode);
  return {}
}

//replaceDocument : xml -> ()
var replaceDocument = _continuationize(_replaceDocument, 1);


// WARNING: insertBeforeRef MOVES a DOM node
//insertBeforeRef : domRef -> domRef -> ()
function _domInsertBeforeRef(insertNode, beforeNode) {
  var parent = beforeNode.parentNode;
  parent.insertBefore(insertNode, beforeNode)
  return {}
}

//appendChildRef : domRef -> domRef -> ()
function _domAppendChildRef(appendNode, parentNode) {
  parentNode.appendChild(appendNode);
  return {}
}

//getDocRef : () -> domRef 
function _getDocumentNode() {
  return document.documentElement;
}

//getRefById : string -> domRef
function _getNodeById(id) {
  var id = _charlistToString(id);
  ref = document.getElementById(id);
//  if (!ref) _alert("element " + id + " does not exist");
  return ref;
}

//isNullRef : domRef -> bool
function _isNull(node) {
  return node == null;
}

var insertBefore = _continuationize(_insertBefore, 2);
var appendChildren = _continuationize(_appendChildren, 2);
var replaceNode = _continuationize(_replaceNode, 2);

var domInsertBeforeRef = _continuationize(_domInsertBeforeRef, 2);
var domAppendChildRef = _continuationize(_domAppendChildRef, 2);
var removeNode = _continuationize(_removeNode, 1);

var getDocumentNode = _continuationize(_getDocumentNode, 1);
var getNodeById = _continuationize(_getNodeById, 1);
var isNull = _continuationize(_isNull, 1);


// Xml representation in JavaScript
//   [["ELEMENT", String, [(String, [Char])], Xml]]
//   [["TEXT", String]]

// [IE]
//   IE calls node.textContent "node.data"
function _textContent(node) {
  if(node.textContent)
    return node.textContent;
  else
    return node.data;
}

//getRepresentation : domRef -> xml
function _getValue(nodeRef) {
  if (nodeRef.nodeType == document.TEXT_NODE) {
    return [["TEXT", _textContent(nodeRef)]]
  } else if (nodeRef.nodeType == document.ELEMENT_NODE ) {
    var children = [];
    for (var i=0; i < nodeRef.childNodes.length; i++) {
      children = children.concat(_getValue(nodeRef.childNodes[i]));
    }
    var attrs = {};
    for (var i=0; i < nodeRef.attributes.length; i++) {
      attrs[nodeRef.attributes[i].name] = 
        _stringToCharlist(nodeRef.attributes[i].value)
    }
    for (var i=0; i < children.length; i++) {
      DEBUG.assert(children[i][0] == "ELEMENT" || children[i][0] == "TEXT",
                   "Invalid children list constructed in DOMNodeToXML")
    }
//    _debug("getValue is returning: ");
//    _dump(["ELEMENT", nodeRef.tagName, attrs, children]);
    return [["ELEMENT", nodeRef.tagName, attrs, children]]
  } else {
    throw("Unknown node type " + nodeRef.nodeType + " in GetXml")
  }
}

getValue = _continuationize(_getValue, 1);

// Accessors for DomRefs
function _domGetTagNameFromRef(nodeRef) {
  return _stringToCharlist(nodeRef.nodeName);
}

function _domGetAttributeFromRef(nodeRef, attr) {
  var attr = _charlistToString(attr);
  if (attr == 'offsetTop') {
    return _intToString(nodeRef.offsetTop);
  } else if (attr == 'offsetLeft') {
    return _intToString(nodeRef.offsetLeft);
  }

  return _stringToCharlist(nodeRef.getAttribute(attr));
}
function _domSetAttributeFromRef(nodeRef, attr, value) {
  return nodeRef.setAttribute(_charlistToString(attr), _charlistToString(value));
}

function _domSetStyleAttrFromRef(nodeRef, attr, value) {
  return nodeRef.style[_charlistToString(attr)] = _charlistToString(value);
}

function _domGetStyleAttrFromRef(nodeRef, attr) {
  return nodeRef.style[_charlistToString(attr)];
}

var domGetTagNameFromRef = _continuationize(_domGetTagNameFromRef, 1);
var domGetAttributeFromRef = _continuationize(_domGetAttributeFromRef, 2);
var domSetAttributeFromRef = _continuationize(_domSetAttributeFromRef, 2);
var domSetStyleAttrFromRef = _continuationize(_domSetStyleAttrFromRef, 2);
var domGetStyleAttrFromRef = _continuationize(_domGetStyleAttrFromRef, 2);

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
var parentNode = _continuationize(_parentNode, 1);
var firstChild = _continuationize(_firstChild, 1);
var nextSibling = _continuationize(_nextSibling, 1);


// useful DOM operations

//swapNodeRefs : (domRef, domRef) -> ()
_swapNodes = function (x, y) {
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

//replaceChildren : xml -> domRef -> ()
function _replaceChildren(xml, parent) {
  newNodes = _XmlToDomNodes(xml);
  while (parent.hasChildNodes()) {
    parent.removeChild(parent.firstChild);
  }
  for(i = 0; i < newNodes.length; i++)
    _domAppendChildRef(newNodes[i], parent);
  return {}
}

var swapNodes = _continuationize(_swapNodes, 2);
var replaceChildren = _continuationize(_replaceChildren, 2);


function _getTarget(event) { return YAHOO.util.Event.getTarget(event, false) }
function _getTargetValue(event) { return _stringToCharlist(_getTarget(event).value); }
function _getTargetElement(event) { return YAHOO.util.Event.getTarget(event, true) }
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

var getTarget = _continuationize(_getTarget, 1)
var getTargetValue = _continuationize(_getTargetValue, 1)
var getPageX = _continuationize(_getPageX, 1)
var getPageY =_continuationize(_getPageY, 1)
var getFromElement = _continuationize(_getFromElement, 1)
var getToElement = _continuationize(_getToElement, 1)
var getTime = _continuationize(_getTime, 1)
var getCharCode = _continuationize(_getCharCode, 1)

function innerHTML(x) { return x.innerHTML }

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
function _concat (l,r) {
//  if (typeof(l) == 'string') {
//    return l + r
//  }
//  else {
      return l.concat(r);
//  }
}

//  _accum(f, i)
//    concatMap: apply f to every element of `list'
//    and concatenate the results.
function _accumAux(fn, list, result, kappa) {
    if (isEmpty(list)) kappa(result)
    else {
      h = list[0];
      _yield(fn, h, function(himg) {
                       t = list.slice(1);
                       _accumAux(fn, t, result.concat(himg), kappa)
                     } );
    }
}

function _accum(kappa) {
  return function(fn, list) {
    DEBUG.assert(DEBUG.is_array(list), "source for list comprehension was not a list");
    _accumAux(fn, list, [], kappa);
  }
}

function _singleXmlToDomNodes(xmlObj) {
    if (xmlObj[0] == "ELEMENT") {
      var tag = xmlObj[1];
      var attrs = xmlObj[2];
      var body = xmlObj[3];
      var node = document.createElement(tag);
      for (var name in attrs) {
         node.setAttribute(name, _charlistToString(attrs[name]));
      }
      for (var i = 0; i < body.length; i++) {
        var child = _singleXmlToDomNodes(body[i]);
        node.appendChild(child);
      }
      return node;
    } else if (xmlObj[0] == "TEXT"){
      return document.createTextNode(xmlObj[1]);
    } else {
      throw "unknown XML node " + xmlObj[0] + " in _singleXmlToDomNodes"
    }
}

function _map(f, list) {
  var result = [];
  for (var i = 0; i < list.length; i++) {
    result[i] = f(list[i])
  }
  return result;
}

function _XmlToDomNodes(xmlForest) {
  return _map(_singleXmlToDomNodes, xmlForest);
}

function _concatList(list) {
  return [].concat.apply([],list);
}



/// XML
//  _XML(tag, attrs, children)
//    create a DOM node with name `tag'
//                       and attributes `attrs' (a dictionary)
//                       and children `children' (a sequence of DOM 
//                                                nodes)    
// _XML (string, (string, [char]) map, Xml, cont) -> Xml
function _XML(tag, attr, body, kappa) {
  kappa([["ELEMENT", tag, attr, [].concat.apply([],body)]])
}

function _stringToXml(s) {
//  _debug("Creating text node with " + _charlistToString(s));
  return [["TEXT", _charlistToString(s)]];
}
function _intToXml(i) {
  return _stringToXml(_intToString(i));
}
function _floatToXml(f) {
  return _stringToXml(_floatToString(f));
}

var stringToXml = _continuationize(_stringToXml, 1);
var intToXml = _continuationize(_intToXml, 1);

// not in library.ml yet
var floatToXml = _continuationize(_floatToXml, 1);


function _getTagName(xml) {
  if (xml[0][0] != "ELEMENT")
    throw "getTagName() applied to non-element node";
  return _stringToCharlist(xml[0][1]);
}

function _getChildNodes(xml) {
  if (xml[0][0] != "ELEMENT")
    throw "getChildNodes() applied to non-element node";
  return xml[0][3];
}

function _getTextContent(xml) {
  if (xml[0][0] != "TEXT")
    throw "getTextContent() applied to non-text node";
  return _stringToCharlist(xml[0][1]);
}

function _getAttributes(xml) {
  if (xml[0][0] != "ELEMENT")
    throw "getAttributes() applied to non-element node";
  var attrs;
  for (name in xml[0][2])
    attrs.push({'1':_stringToCharlist(name), '2':xml[0][2][name]})
  return attrs;
}

function _hasAttribute(xml, attrName) {
  if (xml[0][0] != "ELEMENT")
    throw "hasAttribute() applied to non-element node";

  // silly js idiom (this can't be eta-reduced)
  if (xml[0][2][_charlistToString(attrName)]) return true;
  else return false;
}

function _getAttribute(xml, attrName) {
  if (xml[0][0] != "ELEMENT")
    throw "getAttribute() applied to non-element node";
  return xml[0][2][_charlistToString(attrName)];
}



getTagName = _continuationize(_getTagName, 1);
getAttributes = _continuationize(_getAttributes,1 );
hasAttribute = _continuationize(_hasAttribute, 2);
getAttribute = _continuationize(_getAttribute, 2);
getChildNodes = _continuationize(_getChildNodes, 1);
getTextContent = _continuationize(_getTextContent, 1);

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

function _remove(object, name) {
  var result = {};
  for (fld in object) {
    if (fld != name)
      result[fld] = object[fld];
  }
  return result;
}

function _vrntLbl(object) { return object['_label']}
function _vrntVal(object) { return object['_value']}

function _fail(str) {
  _alert("Internal error: " + str);
}


function _isElementNode(node) {
  return (node != null && node.nodeType == document.ELEMENT_NODE);
}

// DomNode -> bool
var isElementNode = _continuationize(_isElementNode, 1);

function isElement(node) {
  return (node.nodeType == document.ELEMENT_NODE);
}

function isElementWithTag(node, tag) {
  return (isElement(node) && (node.tagName.toLowerCase() == 'body'));
}

// bind all the handlers registered to this key to this DOM node
function _activateHandlers(node) {
  if(!isElement(node))
    return;

  var key = node.getAttribute('key');
  if(key != null) {
    var hs = _eventHandlers[key];
    for(var lAttrName in hs) {
//      _debug("installing event handler "+eventName+"; for node: "+key);
      listenerElem = lAttrName.match(/page$/) ? document.documentElement : node;
      var handlerName = lAttrName.replace(/page$/, "");
      var eventName = handlerName.replace(/^on/, "");
      jslib.event.addListener(listenerElem, eventName,
	function (key, name){return function (e) {
//          _alert("firing event: "+name+ " on elem with key=" + key);
          // TBD: clone the event record.
//          _debug("registered handler is " + _eventHandlers[key][name]);
//          _dumpSchedStatus();
 	  _eventHandlers[key][name](e);
	  // make sure this event isn't handled by anyone else
          jslib.event.stopEvent(e);
	  return false;
      	}}(key, lAttrName)
        );
    }
  }

  // recurse!
  var child;
  for(child = node.firstChild; child != null; child = child.nextSibling) {
    _activateHandlers(child);
  }
}

function _time () {
  return  (new Date()).getTime();
} 

var _pageTimer;

function _startTimer() {
  _pageTimer = _time();
}
function _stopTimer() {
  _pageTimer = _time() - _pageTimer;
  _debug("Page drawn in " + _pageTimer + "ms");
}

// Page update

//  _replaceDocument(tree)
//    Replace the current page with `tree'.
function _replaceDocument(tree) {
  DEBUG.assert(tree != null, "Null tree passed to _replaceDocument");
  DEBUG.assert(tree[0][0] == "ELEMENT" || tree[0][0] == "TEXT",
               "Top-level expression was not XML")
  tree = _XmlToDomNodes(tree);

  // save here
  var _saved_fieldvals = [];
  var inputFields = document.getElementsByTagName("input");
  for (var i = 0; i < inputFields.length; i++) { 
     var current = inputFields[i];
     if(current.id != null && current.id != "") // only store fields with an id!
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

  return {};
}

var _start = function(page) {
  _stopTimer();
  if (!(DEBUG.is_unit(page)))
    _replaceDocument(page);
}

// generate a fresh key for each node
var _node_key = 0;
function _get_fresh_node_key() {
  return _node_key++;
}

var _eventHandlers = {};
// Wrap an event handler in a function that sets the main process id at the beginning of the handler
// and unsets it at the end.
function _wrapEventHandler(handler) {
  return function(event) {
    // set process id here
    var active_pid = _current_pid;
    _current_pid = _mainPid;
    _handlingEvent = true;

    var _cont = function () { handler(event) }
    // a trampoline, of sorts.
    // since we don't yield in event handlers we quickly run
    // out of stack.  To avoid that we throw away the stack
    // periodically by throwing an exception containing the
    // current continuation, which we invoke when the exception
    // is caught.
    for (;;) {
      try {
        var rv = _cont();
        
        // restore process id here.
        _handlingEvent = false;
        _current_pid = active_pid;
        return rv;
      }
      catch (e) {
        if (e instanceof ContinuationNonException) {
           _cont = e.v;
           continue;
        }
        else { throw e; }
      }
    }
  }
}

function _registerFormEventHandlers(actions) {
   var key = '_key' + _get_fresh_node_key();

   for (var i = 0; i < actions.length; i++) {
     var action = actions[i];
        // FIXME: clone this ??

     if (!_eventHandlers[key])
       _eventHandlers[key] = [];
     _eventHandlers[key][action.evName] = _wrapEventHandler(action.handler);
   }
   return (_stringToCharlist(key)); // HACKISH
}

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
        var res = null;
        var cont = function(r) {
          // [HACK]
          //   Shouldn't functions with no return value
          //   be returning unit rather than undefined?
          if(r == undefined)
            r = {};
          res = r;
        }
        var f = eval(serverResponse.__name);
        f.apply(f, [serverResponse.__arg, cont]);
        var request_ = _newXMLHttpRequest();
        request_.open('POST', location.href, true);
        request_.onreadystatechange = _remoteCallHandler(kappa, request_);
        request_.setRequestHeader('Content-Type',
                                  'application/x-www-form-urlencoded');
        request_.send("__continuation=" + serverResponse.__continuation + 
                      "&__result=" + _base64encode(JSON.stringify(res)));
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
    var in_main_process = _current_pid == _mainPid;  

    _debug("starting xmlhttprequest");

    var request = _newXMLHttpRequest();

    // Posting to location.href works in both Firefox and IE
    // (unlike posting to '#', which IE mistakenly urlencodes as %23)
    request.open('POST', location.href, !in_main_process);
    request.setRequestHeader('Content-Type','application/x-www-form-urlencoded');
    request.onreadystatechange = _remoteCallHandler(kappa, request);

    request.send("__name=" + _base64encode(name) + 
                 "&__args=" + _base64encode(JSON.stringify(arguments)));
    if (in_main_process) {
      return _remoteCallHandler(kappa, request)();
    }
  }
}

// db functions (via remote calls)
// TODO!

/// focus stuff
var _focused = null;
function _val(x) { 
    var y = document.getElementById(x);
    if (y) {return _stringToCharlist(y.value)} else { return "boogers"}
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

var _maxPid = 0; // the highest process id allocated so far 
var _mainPid = -1; // the process id of the main process
var _current_pid = _mainPid; // the process id of the currently active process
var _handlingEvent = false;


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

function spawn(f, kappa) {
    kappa(function (arg, kappa) {
    
        var childPid = ++_maxPid;
        _mailboxes[childPid] = [];
        setTimeout(function () { 
                     _debug("launched process #" + childPid);
                     _current_pid = childPid;
                     f(arg, function () { }) 
                   }, 200);
        kappa(childPid);

      });
}

function self(unit, kappa) {
     return kappa(_current_pid) 
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

function send(pid, kappa) {
   kappa(function (msg, kappa) {
        _mailboxes[pid].unshift(msg);
        _wakeup(pid);
        kappa(1); 
      }
    );
}

function _dictlength(x) {
  var length = 0;
  for (var prop in x) { 
    length++;
  }
 return length;
}


function _block_proc(pid, its_cont) {
  var current_pid = _current_pid;
  _blocked_procs[pid] = function () {
                            _current_pid = current_pid;
//                            _debug("scheduled process " + current_pid);
                            its_cont()
                        };
  // discard stack
}

function recv(_, kappa) {
    if ( _mailboxes[_current_pid].length > 0) {
      msg = _mailboxes[_current_pid].pop()
      kappa(msg);
    } else {
      var current_pid = _current_pid;
      _block_proc(_current_pid,
                  function () {
                      _current_pid = current_pid;
                      recv({}, kappa)   // TBD: remove this bogus arg
                  });
    }
}

// scheduler

var _yieldCount = 0;
var _yieldGranularity = 60;
var _callCount = 0;

function ContinuationNonException(v) { this.v = v }

// yield: give up control for another "thread" to work.
// if we're running in an event handler then don't yield (but 
// do throw away the stack periodically instead).
function _yield(f, a, k) {
  ++_yieldCount;
  if ((_yieldCount % _yieldGranularity) == 0) {
    if (!_handlingEvent) {
      var current_pid = _current_pid;
      setTimeout((function() { _current_pid = current_pid; f(a, k)}),
                 sched_pause);
    }
    else {
      throw new ContinuationNonException(function () { f(a,k) });
    }
  } 
  else {
    return f(a,k);
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
// and _continuationize =_def \k.\x.k (f x) 
// so let g =_def \k.\f.k (_continuationize(f)) 
//
// let callForeign = g
//
// callForeign allows us to call JS functions from within Links
// using the syntax: callForeign(f)(args)

// [DEACTIVATED]
//function callForeign(kappa) {
//  return function (f) {
//    return kappa (_continuationize(f));
//  };
//}

// [DEACTIVATED]
// like callForeign, except it takes
// two arguments: an object and a method
//function callForeignMethod(kappa) {
//  return function (obj, method) {
//    return kappa (_continuationizeMethod(obj, method));
//  };
//}

function print(kappa) {
  return function(str) {
    _alert(str);
    kappa(0);
  }
}




// [DUBIOUS FUNCTIONS]
//   Should elementByID and attribute be here?  
function elementById(kappa) {
   return function(id) {
      var elem = document.getElementById(id);
      if (elem != null) kappa({'_label':'Some', '_value':[elem]});
      else kappa({_label:'None', '_value':({})});
   }
}

function attribute(kappa) {
   return function(xml, attr) {
   // FIXME: we need to straighten out the XML/XMLitem distinction.
//     if (xml.length == 0 ) { return kappa({_label:'None', '_value':({})});}
//      obj = xml[0];
      obj = xml;
      if (obj == undefined) {
         return kappa({_label:'None', '_value':({})}); 
      }

      //  Take note!!!
      if (attr == 'class') attr = 'className';

      var val = obj[attr];

      if (val == undefined) kappa({_label:'None', '_value':({})});
      else kappa({'_label':'Some', '_value':val});
   }
}

function is_integer(s) {
  return s.match(/^[0-9]+$/) != null;
}
is_integer = _continuationize(is_integer, 1);

function objectType(kappa) {
  return function(obj) {
    obj = obj[0];
    kappa(typeof(obj) == 'object' ? obj.constructor : typeof(obj))
  }
}


// [BUG]
//   This is badly named
//    - what is its intended semantics?
//    - what should it be called?
//    - should it even be here?
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

function isDefined(kappa) {
  return function(obj) {
    kappa(obj == undefined);
  }
}

textContent = _continuationize(function (node) {
  try { return node.innerHTML } catch (e) { return "" }
}, 1);


function sleep(kappa) {
  return function(duration) {
      var current_pid = _current_pid;
      setTimeout((function() { _current_pid = current_pid; kappa(); }), duration * 1000);
  }
}


function _toUpper(c) {
  DEBUG.assert(c.length == 1, "_toUpper only operates on single characters");
  return c.toUpperCase();
}

function _toLower(c) {
  DEBUG.assert(c.length == 1, "_toLower only operates on single characters");
  return c.toLowerCase(c);
}


var toUpper = _continuationize(_toUpper, 1);
var toLower = _continuationize(_toLower, 1);

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

function _chartest(r) { return _continuationize(function (c) { return r.test(c) }, 1); }

var isAlpha = _chartest(/[a-zA-Z]/);
var isAlnum = _chartest(/[a-zA-Z0-9]/);
var isLower = _chartest(/[a-z]/);
var isUpper = _chartest(/[A-Z]/);
var isDigit = _chartest(/[0-9]/);
var isXDigit = _chartest(/[0-9a-fA-F]/);
var isBlank =  _chartest(/[ \t]/);

var chr = _continuationize(String.fromCharCode, 1);
var ord = _continuationize(function (c) { return c.charCodeAt(0); }, 1);

var floor = _continuationize(Math.floor, 1);
var ceiling = _continuationize(Math.ceil, 1);
var cos = _continuationize(Math.cos, 1);
var sin = _continuationize(Math.sin, 1);
var tan = _continuationize(Math.tan, 1);
var log = _continuationize(Math.log, 1);
var sqrt = _continuationize(Math.sqrt, 1);
