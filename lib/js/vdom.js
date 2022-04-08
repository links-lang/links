/*
 * Virtual DOM integration.
 */
// Assumes virtual-dom.js has been loaded.

// Our current ****virtual-dom style**** representation of the VDom.

/* Global variables. */

const VDom = (function() {

  // loaded: This module should only be loaded once per page; it is an error
  // to load the page more than once. "loaded" is set to true when "runDom" is
  // called for the first time.
  let loaded = false;

  // Current VDOM representation of the page. Initially undefined.
  let currentVDom = undefined;

  // Event handler AP, for event dispatch.
  var evtHandlerAP = undefined;
  // Running subscription ID.
  var currentSubID = 0;

  // Root VDom node.
  var rootNode = undefined;

  // Quick-access functions from the virtualDom JS module.
  const h = virtualDom.h;
  const diff = virtualDom.diff;
  const patch = virtualDom.patch;
  const createElement = virtualDom.create;

  // Globals for subscriptions
  let animFrameKey = null; // Key for current animation frame handler
  let currentSubscriptions =
    {windowEvents: {}, intervalEvents: {}, animationFrameHandler: null};
  const windowEvents = {}; // Event name |-> { key: Key, handler: function }
  const intervalEvents = {}; // Key |-> Interval reference


  // Running element ID.
  let elementID = 0;

  let widgets = {};

  function genElementID() {
    const ret = elementID;
    elementID++;
    return "elem_" + ret.toString();
  }

  /* genEventHandler: Takes a target, a Links variant type of `EventHandler`,
   * and returns a dictionary of the form { evtName |-> [fn] }, where `fn` is a JS
   * callback which dispatches a message to the event handler process.
   *
   * Inputs: target: { type: "window" } | { type: "element", elementID: id }
   *         handler: Links EventHandler
   * Output: { evtName |-> [fn] }
   */
  function genEventHandler(target, evtHandler) {
    const evtTag = evtHandler["_label"];
    const handler = evtHandler["_value"];

    /* Some event handlers unconditionally dispatch the message.
     * Others may choose not to dispatch a message upon invocation.
     * These two helpers encode these two modes of dispatch. */
    const unconditionalDispatch = _$K.make(dispatchMessage);
    const conditionalDispatch =
      _$K.make(function(fnRes) {
          if(fnRes["_label"] == "Just") {
            dispatchMessage(fnRes["_value"]);
          }
      });

    if (evtTag == "UnitHandler") {
      const evtName = handler["1"];
      const genMsgFn = handler["2"];
      const retEvents = {};
      retEvents[evtName] = [function() { genMsgFn(unconditionalDispatch) }];
      return retEvents;
    } else if (evtTag == "PropertyHandler") {
      const evtName =  handler["1"];
      const propName = handler["2"];
      const genMsgFn = handler["3"];

      const fn =
        function() {
          let propVal = "";
          if (target.type == "element") {
            if (document.getElementById(target.elementID) != null) {
              const pv = document.getElementById(target.elementID)[propName];
              if(pv != undefined) {
                propVal = pv;
              }
          } else if (target.type == "window") {
            const pv = window[propName];
            if(pv != undefined) {
              propVal = pv;
            }
          }
          genMsgFn(propVal, conditionalDispatch)
        }
      };
      const ret = {};
      ret[evtName] = [fn];
      return ret;
    } else if (evtTag == "MouseEventHandler") {
      // Serendipitously, the way Links records are stored coincides
      // with the way JS records are stored -- so we don't need to do
      // any explicit marshalling! (I think.)
      const evtName = handler["1"];
      const genMsgFn = handler["2"]
      const retEvents = {};
      const fn =
        function(evt) {
          genMsgFn(evt, conditionalDispatch)
        };
      retEvents[evtName] = [fn];
      return retEvents;
    } else if (evtTag == "KeyboardEventHandler") {
      const evtName = handler["1"];
      const genMsgFn = handler["2"]
      const retEvents = {};
      const fn =
        function(evt) {
          genMsgFn(evt, conditionalDispatch)
        };
      retEvents[evtName] = [fn];
      return retEvents;
    } else {
      throw("Unsupported event handler " + evtTag);
    }
  }


  /*
   * evalSubscription: Takes a Links variant of type `Sub`
   * and returns an object detailing the new event handlers.
   *
   * Argument: `subscription`: a Links variant of type `Sub`
   * Result: { windowEvents: { eventName |-> {key: unique key, handler: handler} },
   *           intervalEvents: { key |-> {interval: int, handler: callback} },
   *           animationFrameHandler: either `null` or { key: unique key, handler: handler } }
   *
   * The "unique key" is the concatenation of all of the (sorted) Links
   * function names for the callbacks. This means that we only need to set / unset
   * event handlers when there is a change, and also supports duplicate invocations
   * of the same function per event.
   */
  function evalSubscription(subscription) {

    // First, evaluate the subscription to a point where we have a list of
    // callbacks for each event, and a list of animation frame handlers.
    // We can then batch later.
    // Result: { windowEvents: { eventName: [callback] },
    //           intervalEvents: { key |-> { interval: int, handler: callback } },
    //           animationFrameHandlers: [callback] }
    function evalSubscriptionInner(subscription) {
      const tag = subscription["_label"];
      const val = subscription["_value"];

      function makeRes(windowEvents, intervalEvents, animationFrames) {
        return { windowEvents: windowEvents,
            intervalEvents: intervalEvents,
            animationFrameHandlers: animationFrames };
      }

      if (tag == "SubEmpty") {
        return makeRes({}, {}, []);
      } else if (tag == "SubAppend") {
        const res1 = evalSubscriptionInner(val["1"]);
        const res2 = evalSubscriptionInner(val["2"]);

        // Merge the window events
        const mergedWindowEvents = {};
        Object.keys(res1.windowEvents).forEach((evtName) => {
          if (evtName in res2.windowEvents) {
            mergedWindowEvents[evtName] =
              res1.windowEvents[evtName].concat(res2.windowEvents[evtName]);
          } else {
            mergedWindowEvents[evtName] = res1.windowEvents[evtName];
          }
        });

        const res1Keys = new Set(Object.keys(res1.windowEvents));
        Object.keys(res2.windowEvents)
          .filter((evtName) => !res1Keys.has(evtName))
          .forEach((evtName) => {
              mergedWindowEvents[evtName] = res2.windowEvents[evtName];
            });
        return makeRes(
          mergedWindowEvents,
          Object.assign(res1.intervalEvents, res2.intervalEvents),
          res1.animationFrameHandlers.concat(res2.animationFrameHandlers));
      } else if (tag == "SubEventHandler") {
        return makeRes(genEventHandler({type: "window"}, val), {}, []);
      } else if (tag == "SubInterval") {
        const interval = val["1"];
        const handler = val["2"];
        const key = "subInterval_" + interval + "_" + handler.name;
        const newInterval = {};
        newInterval[key] = { interval: interval, handler: handler };
        return makeRes({}, newInterval, []);
      } else if (tag == "SubAnimationFrame") {
        const genMsgFn = val;
        const fn =
          function(ts) {
            genMsgFn(ts, _$K.make(dispatchMessage));
          };
        return makeRes({}, {}, [fn]);
      } else {
        throw("Unsupported subscription type: " + tag)
      }
    }

    const res = evalSubscriptionInner(subscription);
    // windowEvents: Batched window events, complete with unique key comprising
    // the different Links function names, and the callback which invokes all of
    // them.
    const windowEvents = {};
    Object.keys(res.windowEvents).forEach(function(eventName) {
      // Key: names of all functions
      let key = "subsKey_" + eventName + "_" +
        res.windowEvents[eventName]
          .map((fn) => fn.name)
          .sort()
          .join("");

      _debug("Event subscription key (" + eventName + "): " + key);

      windowEvents[eventName] =
      { key: key,
        handler:
          function(evt) {
            res.windowEvents[eventName].forEach(function(f) { f(evt); })
          }
      };
    });

    let animationFrameHandler = null;
    if (res.animationFrameHandlers.length > 0) {
      let afkey = "subsKeyAnim_" +
        res.animationFrameHandlers
          .map((fn) => fn.name)
          .sort()
          .join("");
      _debug("AF subscription key: " + afkey);

      animationFrameHandler =
      { key: afkey,
        handler: function(timestamp) {
          res.animationFrameHandlers.forEach((f) => f(timestamp));
        }
      }
    }

    return { windowEvents: windowEvents,
             intervalEvents: res.intervalEvents,
             animationFrameHandler: animationFrameHandler };
  }

  const SubscriptionCommand =
    Object.freeze({
      UNSET_INTERVAL: "UNSET_INTERVAL",
      SET_INTERVAL: "SET_INTERVAL",
      ANIMATION_FRAME: "ANIMATION_FRAME",
      UNSET_WINDOW_EVENT: "UNSET_WINDOW_EVENT",
      SET_WINDOW_EVENT: "SET_WINDOW_EVENT"});

  /* Diffs subscriptions, producing a list of commands to enact on the DOM.
   * Inputs: subscription datatypes produced by `evalSubscription`.
   * Output: A small list of instructions:
   *  [
   *      { command: SubscriptionCommand.UNSET_INTERVAL, key: unique interval key }
   *    | { command: SubscriptionCommand.SET_INTERVAL, key: UIK, interval: int, handler: callback function }
   *    | { command: SubscriptionCommand.ANIMATION_FRAME, key: unique key, handler: callback function }
   *    | { command: SubscriptionCommand.UNSET_WINDOW_EVENT, eventName: event name }
   *    | { command: SubscriptionCommand.SET_WINDOW_EVENT, eventName: event name,
   *          key: unique event key, handler: fn }
   *  ]
   * */
  function diffSubscriptions(oldSubscriptions, newSubscriptions) {
    const commands = [];

    function setWindowEvent(eventName, key, handler) {
      commands.push({ command: SubscriptionCommand.SET_WINDOW_EVENT,
        eventName: eventName, key: key, handler: handler });
    }

    function unsetWindowEvent(eventName) {
      commands.push({ command: SubscriptionCommand.UNSET_WINDOW_EVENT,
        eventName: eventName });
    }

    function setNewInterval(key, interval, handler) {
      commands.push({ command: SubscriptionCommand.SET_INTERVAL,
        key: key, interval: interval, handler: handler });
    }

    function unsetOldInterval(key) {
      commands.push({ command: SubscriptionCommand.UNSET_INTERVAL, key: key });
    }

    // First, handle window event diffs
    Object.keys(newSubscriptions.windowEvents).forEach((evtName) => {
      const key = newSubscriptions.windowEvents[evtName].key;
      const handler = newSubscriptions.windowEvents[evtName].handler;
      // Check whether we have an event handler for evtName
      if (evtName in windowEvents) {
        // Only update if key has changed.
        if (windowEvents[evtName].key != key) {
          unsetWindowEvent(evtName);
          setWindowEvent(evtName, key, handler);
        }
      } else {
        // If not, we need to setup the new event handler
        setWindowEvent(evtName, key, handler);
      }
    });
    // -- We also need to remove any event handlers which appear in
    //    oldSubscriptions but not newSubscriptions
    const oldEvts = Object.keys(oldSubscriptions.windowEvents);
    const newEvtSet = new Set(Object.keys(newSubscriptions.windowEvents));
    oldEvts.filter((evt) => !newEvtSet.has(evt))
           .forEach((evt) => unsetWindowEvent(evt));

    // Second, handle interval event diffs
    Object.keys(newSubscriptions.intervalEvents).forEach((key) => {
      const interval = newSubscriptions.intervalEvents[key].interval;
      const handler = newSubscriptions.intervalEvents[key].handler;
      if (!(key in oldSubscriptions.intervalEvents)) {
        setNewInterval(key, interval, handler);
      }
    });

    const oldIntervals = Object.keys(oldSubscriptions.intervalEvents);
    const newIntervalSet = new Set(Object.keys(newSubscriptions.intervalEvents));
    oldIntervals.filter((key) => !newIntervalSet.has(key))
                .forEach((key) => unsetOldInterval(key));

    // Third, add a command to do an animation frame, if necessary.
    const afh = newSubscriptions.animationFrameHandler;
    if (afh != null) {
      commands.push(
        { command: SubscriptionCommand.ANIMATION_FRAME,
          key: afh.key,
          handler: afh.handler });
    }

    return commands;
  }

  /* Interprets the commands produced by `diffSubscriptions`. */
  function interpretSubscriptionCommands(cmds) {
    cmds.forEach((cmd) => {
      if (cmd.command == SubscriptionCommand.UNSET_INTERVAL) {
        const key = cmd.key;
        if (key in intervalEvents) {
          clearInterval(intervalEvents[key]);
          delete intervalEvents[key];
        } else {
          _debug("WARN: Tried to delete invalid interval with key " + key);
        }
      } else if (cmd.command == SubscriptionCommand.SET_INTERVAL) {
        const cont = _$K.make(dispatchMessage);
        intervalEvents[cmd.key] = setInterval(function () { cmd.handler(cont) }, cmd.interval);
      } else if (cmd.command == SubscriptionCommand.ANIMATION_FRAME) {
        const cont = _$K.make(dispatchMessage);
        function animFrameHandler(timestamp) {
            cmd.handler(timestamp, cont);
            // If the key is the same, great, reschedule the handler
            if (animFrameKey == cmd.key) {
              window.requestAnimationFrame(animFrameHandler);
            } else {
              _debug("Not rescheduling frame handler for key " + cmd.key);
            }
        }

        // Re-installing an existing frame handler is a no-op.
        // Otherwise, update the key and request an initial animation frame
        if (animFrameKey != cmd.key) {
          animFrameKey = cmd.key;
          window.requestAnimationFrame(animFrameHandler);
        }
      } else if (cmd.command == SubscriptionCommand.UNSET_WINDOW_EVENT) {
        if (eventName in windowEvents) {
          window.removeEventListener(windowEvents[eventName].handler);
        } else {
          _debug("WARN: Tried to delete nonexistent event handler for " + eventName);
        }
      } else if (cmd.command == SubscriptionCommand.SET_WINDOW_EVENT) {
        const cont = _$K.make(dispatchMessage);
        const callback = function(evt) { cmd.handler(evt, cont) };
        window.addEventListener(cmd.eventName, callback);
        windowEvents[cmd.eventName] = { key: cmd.key, handler: callback };
      } else {
        throw("Invalid subscription command: " + cmd.command);
      }
    });
  }

  function updateSubscriptions(subs) {
    const oldSubs = currentSubscriptions;
    // First, interpret the new subscriptions.
    const newSubs = evalSubscription(subs);
    // Second, diff the subscriptions against the old ones.
    const diffCmds = diffSubscriptions(oldSubs, newSubs);
    // Third, enact the side-effects generated by the diff.
    interpretSubscriptionCommands(diffCmds);
    // Set the new subscriptions as the current subscriptions, and we're done.
    currentSubscriptions = newSubs;
  }

  /*
   * Dispatches a message to the event handler.
  */
  function dispatchMessage(msg) {
    // Need session communication rather than mailbox communication
    // since we don't have linear effects yet.
    //   1. Request on AP, set in global variable
    //   2. Send message on returned channel
    //   3. Close buffer
    _spawn(function() {
        request(evtHandlerAP, _$K.make(function(c) {
          _send(msg, c, _$K.make(function(c) { close(c) }));
        }))
    });
  }

  /* evalEventHandlers: Generates JS event handlers from Links event handlers
   * for a given element.
   *
   * Note that we are taking an *array* of Links event handlers for the element
   * as an input, as opposed to a single EH, in order to batch them correctly.
   *
   * Input:
   *   elementID: ID of the element to which the event handlers will be attached.
   *   evtHandler: JS Array of Links variants of type
   *
   * Output: Array of objects of the form { evtName : f }, where f is a
   * direct-style callback which produces a message and dispatches it to
   * the event handler process.
   */
  function setupEventHandlers(elementID, evtHandlers) {
    if (evtHandlers === undefined || evtHandlers === null) { return; }
    // Event name |-> [Callback] mapping
    const eventCallbacks = {};
    function addCallback(evtName, callback) {
      if (evtName in eventCallbacks) {
        eventCallbacks[evtName].unshift(callback);
      } else {
        eventCallbacks[evtName] = [callback];
      }
    }

    function setupEventHandler(handler) {
      const res = genEventHandler({ type: "element", elementID: elementID }, handler);
      Object.keys(res).forEach((evtName) =>
        res[evtName].forEach((callback) => addCallback(evtName, callback)));
    }

    // First, populate the "eventCallbacks" object
    evtHandlers.forEach(function (hndlr) { setupEventHandler(hndlr) });

    // Second, assign to each event a callback which invokes all event handlers
    const ret = {};
    Object.keys(eventCallbacks).forEach(function(eventName) {
      const onEvtName = "on" + eventName;
      ret[onEvtName] = function(evt) {
        // Event name is "click". The property we need to set is "onclick".
        eventCallbacks[eventName].forEach(function(f) {
          f(evt);
        })
      }
    });
    return ret;
  }

  function setupSubscriptions(subscriptions) {
    if (subscriptions === undefined) { return; }
    for (let i = 0; i < subscriptions.length; i++) {
      setupSubscription(subscriptions[i]);
    }
  }

  /**
   * evalAttr: Evaluates a Links Attr, resulting in a list of
   * plain HTML attributes and event handlers to install.
   *
   * Argument: `attr`, a Links variant of type "
   * Result: An object of the form
   *   { attributes: {key: value},
   *     eventHandlers: [EventHandler] }
   **/
  function evalAttr(attr) {
    const lbl = attr["_label"]
    const val = attr["_value"]

    function makeRes(attrs, handlers) {
      return { attributes: attrs, eventHandlers: handlers }
    }

    if (lbl == "AttrEmpty") {
      // Nothing doing.
      return makeRes({}, []);
    } else if (lbl == "AttrAppend") {
      // Recursively evaluate both.
      const a1 = val["1"];
      const a2 = val["2"];
      const a1Res = evalAttr(a1);
      const a2Res = evalAttr(a2);
      return makeRes(
        Object.assign(a1Res.attributes, a2Res.attributes),
        a1Res.eventHandlers.concat(a2Res.eventHandlers));
    } else if (lbl == "AttrAttribute") {
      const attr = {};
      attr[val["1"]] = val["2"];
      return makeRes(attr, []);
    } else if (lbl == "AttrEventHandler") {
      return makeRes({}, [val])
    } else {
      throw("Unknown attribute type: " + lbl);
    }
  }


  /**
   * evalHTML: Evaluates a Links
   * Argument: A Links
   * Result: A list of child VDom nodes.
   **/
  function evalHTML(html) {
    const lbl = html["_label"];
    const val = html["_value"];

    if (lbl == "HTMLEmpty") {
      return [];
    } else if (lbl =="HTMLAppend") {
      return evalHTML(val["1"]).concat(evalHTML(val["2"]));
    } else if (lbl == "HTMLText") {
      return [String(val)];
    } else if (lbl === "HTMLRaw") {
      return [h("span", { innerHTML: String(val) }, [])];
    } else if (lbl === "HTMLWidget") {
      // Internal widget setup for the virtual-dom library.
      function Widget(tagName, attrs) {
        this.type = "Widget";
        this.tagName = tagName;
        this.attrs = attrs;
      }
      Widget.prototype.init = function () {
        const elem = document.createElement(this.tagName);
        _$List.forEach(this.attrs, function(attr) {
          elem.setAttribute(attr[1], attr[2]);
        });
        return elem;
      }

      // The innards of a widget are *not* affected by diffing.
      Widget.prototype.update = function (prev, elem) { }

      // Check to see whether an 'id' has been set explicitly.
      let id = null;
      _$List.forEach(val[2], function(attr) {
        if (attr[1] === "id") {
          id = attr[1];
        }
      });

      // If the ID has been set, use the ID to persist over renders by
      // storing in the 'widgets' dictionary.
      let widget = null;
      if (id !== null) {
        if (widgets.hasOwnProperty(id)) {
          widget = widgets[id];
        } else {
          widget = new Widget(val[1], val[2]);
          widgets[id] = widget;
        }
      } else {
        widget = new Widget(val[1], val[2]);
      }
      return [h('div', [widget])];
    } else if (lbl === "HTMLTag") {
      // First, get ourselves a dictionary of attributes and event handlers
      const attrRes = evalAttr(val.attrs);
      const attrs = attrRes.attributes;
      const evtHandlers = attrRes.eventHandlers;

      // If there's no user-assigned ID, assign a fresh one
      if (!attrs["id"]) {
        attrs["id"] = genElementID();
      }

      const evtHandlerAttrs = setupEventHandlers(attrs["id"], evtHandlers);
      // Add the event handlers to the generated attributes.
      const combinedAttrs = Object.assign(attrs, evtHandlerAttrs);
      const children = evalHTML(val.children);
      return [h(val["tagName"], combinedAttrs, children)];
    } else {
      throw("Unsupported HTML type: " + lbl);
    }
  }

  // Top-level node must be a tree instead of a forest, so wrap it in a div.
  function evalToplevelHTML(html) {
    return h("div", [], evalHTML(html));
  }

  function _runDom(str, doc, ap, subs) {
    evtHandlerAP = ap;
    currentVDom = evalToplevelHTML(doc);
    rootNode = createElement(currentVDom);
    document.getElementById(str).appendChild(rootNode);
    updateSubscriptions(subs);
  }

  function _updateDom(doc, subs) {
    var newTree = evalToplevelHTML(doc);
    var patches = diff(currentVDom, newTree);
    currentVDom = newTree;
    rootNode = patch(rootNode, patches);
    updateSubscriptions(subs);
  }

  // Wrappers to make direct-style functions callable from the FFI.
  var runDom = _$Links.kify(_runDom);
  var updateDom = _$Links.kify(_updateDom);
  return { "runDom": runDom, "updateDom": updateDom };
})();

const runDom = VDom.runDom;
const updateDom = VDom.updateDom;
