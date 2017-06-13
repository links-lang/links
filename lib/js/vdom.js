
/////// STUFF FOR VIRTUAL DOM INTEGRATION
// Assumes virtual-dom.js has been loaded.

// Our current ****virtual-dom style**** representation of the VDom.
// Note that this isn't the Links representation -- you'll have to perform
// some processing to get it into virtual-dom form.
var currentVDom = undefined;
var rootNode = undefined;
var h = virtualDom.h;
var diff = virtualDom.diff;
var patch = virtualDom.patch;
var createElement = virtualDom.create;
var evtHandlerPid = undefined;

function toAttrsArray(attrs) {
  var attrsArr = [];
  for (var i=0; i < attrs.length; i++) {
    attrsArr[attrs[i]["1"]] = attrs[i]["2"];
  }
  return attrsArr;
}

// Input: Array of Links variants representing the "shapes" of event
// handlers we can get, along with the name of the event, and callback functions in CPS form to generate a message
// Output: Array of objects of the form { evtName : f }, where f is a direct-style callback which
// produces a message and dispatches it to the event handler process
function setupEvtHandlers(attrs, evtHandlers) {
  if (evtHandlers === undefined) { return; }

  function setupEvtHandler(handler) {
    // I'll do UnitHandler, and leave StringHandler to you...
    const variantTag = handler["_label"];
    if (variantTag == "UnitHandler") {
      // When this is invoked, we'll want to invoke the (Links) function
      // to produce the appropriate message, and then dispatch this to the event
      // handler process.
      const evtName = handler["_value"]["1"];
      const genMsgFn = handler["_value"]["2"];

      // Note that genMsgFn is in continuation-passing style. Therefore,
      // to invoke it, we need to pass it a function which it calls with
      // the result -- in this case, the message we need to dispatch.
      // res: result of the HOF we're invoking to get the message
      function cont(res) {
        // Use Send from jslib to send a message to the event handler process.
        // (This is equivalent to "res ! evtHandlerPid" in Links code itself)
        // Send will invoke the continuation kappa
        _Send(evtHandlerPid, res);
      }
      attrs[evtName] = function() { genMsgFn(cont) };
    } else {
      throw("Unsupported event handler form");
    }
  }

  for (let i = 0; i < evtHandlers.length; i++) {
    setupEvtHandler(evtHandlers[i]);
  }
}


function jsonToVtree(jsonContent) {
  if (jsonContent["_label"] == "DocTagNode") {
    var treeArr = [];
    var tagContent = jsonContent["_value"];
    var attrs = toAttrsArray(tagContent["attrs"]);
    setupEvtHandlers(attrs, tagContent["eventHandlers"]);
    var children = tagContent["children"];
    _debug("Attributes of " + tagContent["tagName"] + ": " + JSON.stringify(tagContent["attrs"]));
    for (var i = 0; i < children.length; i++) {
      treeArr.push(jsonToVtree(children[i]));
    }
    return h(tagContent["tagName"], attrs, treeArr);
  }
  if (jsonContent["_label"] == "DocTextNode") {
    return [String(jsonContent["_value"])];
  }
}

function _runDom(str, doc, pid) {
  _debug("in runDom. string: " + str);
  _debug("in runDom. doc: " + JSON.stringify(doc));
  evtHandlerPid = pid;
  currentVDom = jsonToVtree(doc);
  rootNode = createElement(currentVDom);
  document.body.appendChild(rootNode);
}

function _updateDom(doc) {
  _debug("in updateDom. doc: " + JSON.stringify(doc));
  var newTree = jsonToVtree(doc);
  var patches = diff(currentVDom, newTree);
  currentVDom = newTree;
  rootNode = patch(rootNode, patches);
}

// Magic, don't worry about these
var runDom = LINKS.kify(_runDom);
var updateDom = LINKS.kify(_updateDom);

