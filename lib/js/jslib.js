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
 assert2 : function(c, msg) {
   if (!c) {
     _debug(msg);
     throw(msg);
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
  // [IE] superflous commas aren't allowed
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

// library functions (CPS'd)
function _continuationize(f, arity) {
    return function (kappa) {
        return function () {
//	    _alert("Applying continuationized function: "+f);
//	    _alert("to args: "+arguments);
            return kappa(f.apply(f, _extractArgs(arguments, arity)));
        };
    };
}
function _continuationizeBinaryCurried(f) {
    return function (kappa) {
      return function () {
        var args1 = arguments;
        return kappa(function (kappa) {
          return function () {
              return kappa(f.apply(f, args1).apply(f, arguments));
          };
        });
      }
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
  var obj = s;
//  alert(obj + " : " + typeof(obj) + (typeof(obj) == 'object' ? obj.constructor : ''));
  return function (regex) {
    var obj = regex;
//    alert(obj + " : " + typeof(obj) + (typeof(obj) == 'object' ? obj.constructor : ''));
//    alert(Regex);
//    alert(Regex.compile);
    var r = Regex.compile(regex);
    return RegExp(r)(s) != null
  }
}
var tilde = _continuationizeBinaryCurried(_tilde);

var float_of_int = _continuationize(Number, 1);
var string_of_float = _continuationize(String, 1);
var int_of_string = _continuationize(parseInt, 1);
var string_of_int = _continuationize(String, 1);
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
  },
  1);

var negate = _continuationize(function (x) { return -x }, 1);

function error(kappa) {
  return function(msg) {
    alert(msg);
    throw ("Error: " + msg);
  }
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

function _applyChanges(changes) {
  for (var i in changes) {
    var change = changes[i];
    if (change.label == 'InsertBefore') {
      _domInsertBefore(change.value.insertXml, change.value.before);
    }
    else if (change.label == 'ReplaceNode') {
      _domReplaceNode(change.value.refNode, change.value.replacement);
    }
    else if (change.label == 'GetRefById') {
      return _domGetRefById(change.value.id);
    }
    else if (change.label == 'AppendChild') {
      _domAppendChild(change.value.newChild, change.value.parentNode);
    }
    else if (change.label == 'RemoveRef') {
      _domRemoveRef(change.value.nodeRef);
    }
    else if (change.label == 'InsertBeforeRef') {
      _domInsertBeforeRef(change.value.moveRef, change.value.beforeRef);
    }
    else if (change.label == 'AppendChildRef') {
      _domAppendChildRef(change.value.moveRef, change.value.parentRef);
    }
    else if (change.label == 'GetDocumentRef') {
      return _domGetDocumentRef();
    }
    else if (change.label == 'GetXml') {
      return _domGetXml(change.value.nodeRef);
    }
    // old-fashioned ops
    else if (change.label == 'AppendChild') {
        var element = document.getElementById(change.value.id);
        if (!element) _alert("element " + change.value.id + " does not exist");
	var replacement = _XmlToDomNodes(change.value.replacement);

        // FIXME: "replacement" is not a good name; nothing is being replaced.
        element.appendChild(replacement[0]);
    }  
    else if (change.label == 'SwapNodes') {
          var one = change.value.first;
          var two = change.value.second;
          _swapNode(one, two);
    }  
    else if (change.label == 'Document') {
          _start(change.value);
    } else {
       throw("Unknown dom operation " + change)
    }
  }
}


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

// DOM interaction

//domOp = _continuationize(_applyChanges);

//insertBeforeXml : xml -> domRef -> ()
function _domInsertBefore(insertXml, beforeNode) {
  var parent = beforeNode.parentNode;
  var newNode = _XmlToDomNodes(insertXml);
  parent.insertBefore(newNode[0], beforeNode)
}

//appendChildXml : xml -> domRef -> ()
function _domAppendChild(appendXml, parentNode) {
  var newNode = _XmlToDomNodes(appendXml);
  parentNode.appendChild(newNode[0]);
}

//removeNodeRef : domRef -> ()
function _domRemoveRef(nodeRef) {
  nodeRef.parentNode.removeChild(nodeRef);
}

//replaceNode : (xml, domRef) -> ()
function _domReplaceNode(withXml, replaceNode) {
  newNode = _XmlToDomNodes(withXml);
  replaceNode.parentNode.replaceChild(newNode[0], replaceNode);
}

//replaceDocument : xml -> ()
function _domReplaceDocument(withXML) {
  _start(withXML);
}
var domReplaceDocument = _continuationize(_domReplaceDocument, 1);


// WARNING: insertBeforeRef MOVES a DOM node
//insertBeforeRef : domRef -> domRef -> ()
function _domInsertBeforeRef(insertNode, beforeNode) {
  var parent = beforeNode.parentNode;
  parent.insertBefore(insertNode, beforeNode)
}

//appendChildRef : domRef -> domRef -> ()
function _domAppendChildRef(appendNode, parentNode) {
  parentNode.appendChild(appendNode);
}

//getDocRef : () -> domRef 
function _domGetDocumentRef() {
  return document.documentElement;
}

//getRefById : string -> domRef
function _domGetRefById(id) {
  ref = document.getElementById(id);
//  _debug("returning "+ ref + " for " + id);
  if (!ref) _alert("element " + id + " does not exist");
  return ref;
}

//isNullRef : domRef -> bool
function _domIsNullRef(node) {
  return node == null;
}

var domInsertBefore = _continuationize(_domInsertBefore, 2);
var domAppendChild =  _continuationize(_domAppendChild, 2);
var domReplaceNode = _continuationize(_domReplaceNode, 2);

var domInsertBeforeRef = _continuationize(_domInsertBeforeRef, 2);
var domAppendChildRef = _continuationize(_domAppendChildRef, 2);
var domRemoveRef = _continuationize(_domRemoveRef, 1);

var domGetDocumentRef = _continuationize(_domGetDocumentRef, 1);
var domGetRefById = _continuationize(_domGetRefById, 1);
var domIsNullRef = _continuationize(_domIsNullRef, 1);

//getRepresentation : domRef -> xml
function _domGetXml(nodeRef) {
//  _debug("arg of getRepresentation is ");
//  dump(_idy)(nodeRef);
  if (nodeRef.nodeType == Node.TEXT_NODE) {
    return [["TEXT", nodeRef.textContent]]
  } else if (nodeRef.nodeType == Node.ELEMENT_NODE ) {
    var children = [];
    for (var i=0; i < nodeRef.childNodes.length; i++) {
      children = children.concat(_domGetXml(nodeRef.childNodes[i]));
    }
    var attrs = {};
    for (var i=0; i < nodeRef.attributes.length; i++) {
      attrs[nodeRef.attributes[i].name] = nodeRef.attributes[i].value
    }
    for (var i=0; i < children.length; i++) {
      DEBUG.assert(children[i][0] == "ELEMENT" || children[i][0] == "TEXT",
                   "Invalid children list constructed in DOMNodeToXML")
    }
    return [["ELEMENT", nodeRef.tagName, attrs, children]];
  } else {
    throw("Unknown node type " + nodeRef.nodeType + " in GetXml")
  }
}

domGetXml = _continuationize(_domGetXml, 1);

// Accessors for DomRefs
function _domGetTagNameFromRef(nodeRef) {
  return nodeRef.nodeName;
}

function _domGetAttributeFromRef(nodeRef, attr) {
  return nodeRef.getAttribute(attr);
}
var domGetTagNameFromRef = _continuationize(_domGetTagNameFromRef, 1);
var domGetAttributeFromRef = _continuationize(_domGetAttributeFromRef, 2);

// basic dom navigation
function _domGetParentFromRef(nodeRef) {
  return nodeRef.parentNode;
}
function _domGetFirstChildFromRef(nodeRef) {
  return nodeRef.firstChild;
}
function _domGetNextSiblingFromRef(nodeRef) {
  return nodeRef.nextSibling;
}
var domGetParentFromRef = _continuationize(_domGetParentFromRef, 1);
var domGetFirstChildFromRef = _continuationize(_domGetFirstChildFromRef, 1);
var domGetNextSiblingFromRef = _continuationize(_domGetNextSiblingFromRef, 1);


// useful DOM operations

//swapNodeRefs : (domRef, domRef) -> ()
_domSwapNodeRefs = function (x, y) {
  var xNextSibling = x.nextSibling;
  var xParent = x.parentNode;
  y.parentNode.replaceChild(x, y);
  if(xNextSibling == null)
    xParent.appendChild(y);
  else
    xParent.insertBefore(y, xNextSibling);
}

//replaceChildren : xml -> domRef -> ()
function _domReplaceChildren(xml, parent) {
  newNodes = _XmlToDomNodes(xml);
  while (parent.hasChildNodes()) {
    parent.removeChild(parent.firstChild);
  }
  for(i = 0; i < newNodes.length; i++)
    _domAppendChildRef(newNodes[i], parent);
}

var domSwapNodeRefs = _continuationize(_domSwapNodeRefs, 2);
var domReplaceChildren = _continuationize(_domReplaceChildren, 2);


function _eventGetTarget(event) { return YAHOO.util.Event.getTarget(event, false) }
function _eventGetTargetValue(event) { return _eventGetTarget(event).value; }
function _eventGetTargetResolveTextNode(event) { return YAHOO.util.Event.getTarget(event, true) }
function _eventGetPageX(event) { return YAHOO.util.Event.getPageX(event) }
function _eventGetPageY(event) { return YAHOO.util.Event.getPageY(event) }
function _eventGetRelatedTarget(event) { return YAHOO.util.Event.getRelatedTarget(event) }
function _eventGetTime(event) { return YAHOO.util.Event.getTime(event) }
function _eventGetCharCode(event) { return YAHOO.util.Event.getCharCode(event) }

var eventGetTarget = _continuationize(_eventGetTarget, 1)
var eventGetTargetValue = _continuationize(_eventGetTargetValue, 1)
var eventGetPageX = _continuationize(_eventGetPageX, 1)
var eventGetPageY =_continuationize(_eventGetPageY, 1)
var eventGetRelatedTarget = _continuationize(_eventGetRelatedTarget, 1)
var eventGetTime = _continuationize(_eventGetTime, 1)
var eventGetCharCode = _continuationize(_eventGetCharCode, 1)

//replaceDocument : xml -> ()
function _domReplaceDocument(withXML) {
  _start(withXML);
}
var domReplaceDocumentXml = _continuationize(_domReplaceDocument, 1);

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
      h = list[0];
      _call(fn, [h], function(himg) {
                       t = list.slice(1);
                       _accumAux(kappa)(fn, t, result.concat(himg))
                     } );
    }
  }
}

function _accum(kappa) {
  return function(fn, list) {
    _accumAux(kappa)(fn, list, []);
  }
}

/// XML
//  _XML(tag, attrs, children)
//    create a DOM node with name `tag'
//                       and attributes `attrs' (a dictionary)
//                       and children `children' (a sequence of DOM 
//                                                nodes and strings)    

function _singleXmlToDomNodes(xmlObj) {
    if (xmlObj[0] == "ELEMENT") {
      var tag = xmlObj[1];
      var attrs = xmlObj[2];
      var body = xmlObj[3];
      var node = document.createElement(tag);
      for (name in attrs) {
         node.setAttribute(name, attrs[name]);
      }
      for (var i = 0; i < body.length; i++) {
        var child = _singleXmlToDomNodes(body[i]);
        node.appendChild(child);
      }
      return node;
    } else if (xmlObj[0] == "TEXT"){
      return document.createTextNode(xmlObj[1]);
    } else {
      throw "unknown XML node " + xmlObj[0] + " in _XmlToDomNodes"
    }
}

function _map(f) { return function (list) {
  var result = [];
  for (var i = 0; i < list.length; i++) {
    result = result.concat([f(list[i])]);

  }
  return result;
} }

function _XmlToDomNodes(xmlForest) {
  return _map(_singleXmlToDomNodes)(xmlForest);
}

function _concatList(list) {
  var result = [];
  for (i = 0; i < list.length; i++) {
    if (DEBUG.is_array(list[i])) {
      result = result.concat(list[i]);
    } else {
      throw("attempt to concat non-lists (" + typeof(list[i]) + ")");
    }
  }
  return result;
}

function _XML(kappa) { return function(tag, attr, body) {
  for (var i = 0; i < body.length; i++) {
    DEBUG.assert(body[i][0][0] == "TEXT" || body[i][0][0] == "ELEMENT");
  }
  var theBody = _concatList(body);
  if (theBody.length > 0 && 
      theBody[0][0] != "TEXT" && theBody[0][0] != "ELEMENT") 
    _debug("Complete failure in XML!");
  kappa([["ELEMENT", tag, attr, theBody]])
}}

function enxml(kappa) { return function (body) {
  kappa([["TEXT", body]]);
}}

function _getTagName(xml) {
  if (xml[0][0] != "ELEMENT")
    throw "getTagName() applied to non-element node";
  return xml[0][1];
}

function _getAttributes(xml) {
  if (xml[0][0] != "ELEMENT")
    throw "getAttributes() applied to non-element node";
  return xml[0][2];
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

function _getAttribute(xml, attrName) {
  if (xml[0][0] != "ELEMENT")
    throw "getAttributes() applied to non-element node";
  return xml[0][2][attrName];
}

getTagName = _continuationize(_getTagName, 1);
getAttributes = _continuationize(_getAttributes,1 );
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

// bind all the handlers registered to this key to this DOM node
function _activateHandlers(node) {
  if(!isElement(node))
    return;

  var key = node.getAttribute('key');
  if(key != null) {
    var hs = _eventHandlers[key];
    for(var eventName in hs) {
      _alert("registering event: "+eventName+"; for node: "+key);
      jslib.event.addListener(node, eventName.replace(/^on/, ""), 
	function (key, name){return function (e) {
          _alert("event firing: "+name);
 	  _eventHandlers[key][name](e);
	  // make sure this event isn't handled by anyone else
          jslib.event.stopEvent(e);
	  return false;
      	}}(key, eventName)
	);
    }
  }

  // recurse!
  var child;
  for(child = node.firstChild; child != null; child = child.nextSibling) {
    _activateHandlers(child);
  }
}


// Page update

//  _start(tree)
//    Replace the current page with `tree'.
//    tree is actually of type forest, with the proviso it must 
//    contain only one tree.
function _start(tree) {
  DEBUG.assert(tree.length == 1);
  DEBUG.assert(tree[0][0] == "ELEMENT" || tree[0][0] == "TEXT")
  tree = _XmlToDomNodes(tree);

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

// generate a fresh key for each node
var _node_key = 0;
function _get_fresh_node_key() {
  return _node_key++;
}

var _eventHandlers = {};
function _registerFormEventHandlers(actions) {
   var key = _get_fresh_node_key();
   for (var i = 0; i < actions.length; i++) {
     var action = actions[i];
        // FIXME: clone this ??

     if (!_eventHandlers[key])
       _eventHandlers[key] = [];
     _eventHandlers[key][action.evName] = action.handler;
   }
   return key;
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
        var _res = null;
        var cont = function(r) { res = r; }
        // FIXME: does window[] necessarily look up functions?
        var f = window[serverResponse.__name](cont);
// Not a tuple any more
//        f.apply(f, _tuple_as_list(serverResponse.__arg));
        f.apply(f, [serverResponse.__arg]);
        var request_ = _newXMLHttpRequest();
        request_.open('POST', location.href, true);
        request_.onreadystatechange = _remoteCallHandler(kappa, request_);
        request_.setRequestHeader('Content-Type',
                                  'application/x-www-form-urlencoded');
        request_.send("__continuation=" + serverResponse.__continuation + 
                      "&__result=" + _base64encode(JSON.stringify(_res)));
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

    _debug("starting xmlhttprequest");

    var request = _newXMLHttpRequest();
    asynch = true;

    // [IE] IE mistakenly urlencodes # as %23
    // whereas Firefox leaves it plain
    //
    // so we use location.href instead
    // which seems to work in both
    request.open('POST', location.href, asynch);
    request.setRequestHeader('Content-Type','application/x-www-form-urlencoded');
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

var _mailboxes = {0:[]};
var _blocked_procs = {};
//var _suspended_procs = [];

function spawn(kCurry) {
  return function (f) {
    kCurry(function (kappa) {
       return function (arg) {

        var childPid = ++_maxPid;
        _mailboxes[childPid] = [];
        setTimeout(function () { 
                     _debug("launched " + childPid);
                     _current_pid = childPid;
                     f(_applyChanges)(arg) 
                   }, 200);
        kappa(childPid);

      };
    });
  }
}

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
  _blocked_procs[pid] = function () { _current_pid = current_pid; _debug("scheduled " + current_pid); its_cont() };
  // discard stack
}

function recv(kappa) {
  return function() {
    if ( _mailboxes[_current_pid].length > 0) {
      msg = _mailboxes[_current_pid].pop()
      kappa(msg);
    } else {
      var current_pid = _current_pid;
      _block_proc(_current_pid, function () { _current_pid = current_pid; recv(kappa)() });
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
    setTimeout((function() { _current_pid = current_pid; _debug("scheduled (after yield) " + current_pid); my_cont()}),
               sched_pause);
  }
  else {
    my_cont();
  }
}

// _call: using this makes compiled js code easier to read.
function _call(f, args, kappa) {
  _yield(function() { //try {
                      it = f(kappa); it.apply(it, args) 
//                    }
//                    catch(e) {//_alert("failed to apply function: "+it);
//                              //_alert("to args: "+args); throw(e);
//                    }
        }
        );
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
is_integer = _continuationize(is_integer, 1);

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
v}

textContent = _continuationize(function (node) {
  try { return node.innerHTML } catch (e) { return "" }
}, 1);

// Startup actions

var dom = null;

function setDomPid(pid) { 
            dom = pid 
          }

spawn(function (partiallyAppliedSpawn) { 
        partiallyAppliedSpawn(setDomPid)([]) 
})(_domproc)

