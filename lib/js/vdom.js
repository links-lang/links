
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
var inputText = "";

var keyboardEvents = ["oninput", "onkeyup", "onkeydown", "onkeypress"];
var focusEvents = ["onfocus"];

function toAttrsArray(attrs) {
  var attrsArr = [];
  for (var i=0; i < attrs.length; i++) {
    _debug(attrs[i]["1"] + " : " + attrs[i]["2"]);
    attrsArr[attrs[i]["1"]] = attrs[i]["2"];
  }
  return attrsArr;
}

function sleep(milliseconds) {
  var start = new Date().getTime();
  for (var i = 0; i < 1e7; i++) {
    if ((new Date().getTime() - start) > milliseconds){
      break;
    }
  }
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
    const evtName = handler["_value"]["1"];
    const genMsgFn = handler["_value"]["2"];
    function cont(res) {
      _Send(evtHandlerPid, res);
    }

    if (variantTag == "VirtualDom.UnitHandler") {
      _debug(evtName);
      attrs[evtName] = function() { genMsgFn(cont) };
    }
    else if (variantTag == "VirtualDom.StringHandler") {

      if (evtName == "mouseListener") {
        attrs["onmousemove"] = function(event) { 
          var inputText = event.clientX.toString() + " " + event.clientY.toString();
          genMsgFn(inputText, cont) 
        };
      }

      else if (evtName == "keycode") {
        attrs["onkeyup"] = function(event) {
          var keycode = event.keyCode.toString();
          genMsgFn(keycode, cont)
        };
      }

      else if (keyboardEvents.indexOf(evtName) != -1) {
        if (!attrs["id"]) {
            throw("Element with StringHandler event requires id attribute")
        }
        if (document.getElementById(attrs["id"]) != null) {
          inputText = document.getElementById(attrs["id"]).value;
        }  else {
          inputText = "";
        }
        attrs[evtName] = function() { 
          if (document.getElementById(attrs["id"]) != null) {
            var inputText = document.getElementById(attrs["id"]).value;
          } else {
            var inputText = "";
          }
          genMsgFn(inputText, cont) };
      }
    } else {
      throw("Unsupported event handler form");
    }
  }

  for (let i = 0; i < evtHandlers.length; i++) {
    setupEvtHandler(evtHandlers[i]);
  }
}


function jsonToVtree(jsonContent) {
  if (jsonContent["_label"] == "VirtualDom.DocTagNode") {
    var treeArr = [];
    var tagContent = jsonContent["_value"];
    var attrs = toAttrsArray(tagContent["attrs"]);
    setupEvtHandlers(attrs, tagContent["eventHandlers"]);
    var children = tagContent["children"];
    for (var i = 0; i < children.length; i++) {
      treeArr.push(jsonToVtree(children[i]));
    }
    return h(tagContent["tagName"], attrs, treeArr);
  }
  if (jsonContent["_label"] == "VirtualDom.DocTextNode") {
    return [String(jsonContent["_value"])];
  }
}

function _runDom(str, doc, pid) {
  evtHandlerPid = pid;
  currentVDom = jsonToVtree(doc);
  rootNode = createElement(currentVDom);
  document.getElementById(str).appendChild(rootNode);
}

function _updateDom(doc) {
  var newTree = jsonToVtree(doc);
  var patches = diff(currentVDom, newTree);
  currentVDom = newTree;
  rootNode = patch(rootNode, patches);
}

// Magic, don't worry about these
var runDom = LINKS.kify(_runDom);
var updateDom = LINKS.kify(_updateDom);

