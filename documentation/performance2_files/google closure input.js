// ==ClosureCompiler==
// @output_file_name default.js
// @compilation_level ADVANCED_OPTIMIZATIONS
// @language ECMASCRIPT5
// ==/ClosureCompiler==

var YAHOO = {};
YAHOO.util = {};

/**
 * YAHOO.env is used to keep track of what is known about the YUI library and
 * the browsing environment
 * class YAHOO.env
 * static
 */
YAHOO.env = YAHOO.env || {

    /**
     * Keeps the version info for all YUI modules that have reported themselves
     * property modules
     * type Object[]
     */
    modules: [],
    
    /**
     * List of functions that should be executed every time a YUI module
     * reports itselfprim.
     * property listeners
     * type Function[]
     */
    listeners: []
};

/**
 * Returns the version data for the specified module:
 *      <dl>
 *      <dt>name:</dt>      <dd>The name of the module</dd>
 *      <dt>version:</dt>   <dd>The version in use</dd>
 *      <dt>build:</dt>     <dd>The build number in use</dd>
 *      <dt>versions:</dt>  <dd>All versions that were registered</dd>
 *      <dt>builds:</dt>    <dd>All builds that were registered.</dd>
 *      <dt>mainClass:</dt> <dd>An object that was was stamped with the
 *                 current version and build. If 
 *                 mainClass.VERSION != version or mainClass.BUILD != build,
 *                 multiple versions of pieces of the library have been
 *                 loaded, potentially causing issues.</dd>
 *       </dl>
 *
 * method getVersion
 * static
 * param {String}  name the name of the module (event, slider, etc)
 * return {Object} The version info
 */
YAHOO.env.getVersion = function(name) {
    return YAHOO.env.modules[name] || null;
};

/**
 * Do not fork for a browser if it can be avoided.  Use feature detection when
 * you can.  Use the user agent as a last resort.  YAHOO.env.ua stores a version
 * number for the browser engine, 0 otherwise.  This value may or may not map
 * to the version number of the browser using the engine.  The value is 
 * presented as a float so that it can easily be used for boolean evaluation 
 * as well as for looking for a particular range of versions.  Because of this, 
 * some of the granularity of the version info may be lost (e.g., Gecko 1.8.0.9 
 * reports 1.8).
 * class YAHOO.env.ua
 * static
 */
YAHOO.env.ua = function() {
    var o={

        /**
         * Internet Explorer version number or 0.  Example: 6
         * property ie
         * type float
         */
        ie:0,

        /**
         * Opera version number or 0.  Example: 9.2
         * property opera
         * type float
         */
        opera:0,

        /**
         * Gecko engine revision number.  Will evaluate to 1 if Gecko 
         * is detected but the revision could not be found. Other browsers
         * will be 0.  Example: 1.8
         * <pre>
         * Firefox 1.0.0.4: 1.7.8   <-- Reports 1.7
         * Firefox 1.5.0.9: 1.8.0.9 <-- Reports 1.8
         * Firefox 2.0.0.3: 1.8.1.3 <-- Reports 1.8
         * Firefox 3 alpha: 1.9a4   <-- Reports 1.9
         * </pre>
         * property gecko
         * type float
         */
        gecko:0,

        /**
         * AppleWebKit version.  KHTML browsers that are not WebKit browsers 
         * will evaluate to 1, other browsers 0.  Example: 418.9.1
         * <pre>
         * Safari 1.3.2 (312.6): 312.8.1 <-- Reports 312.8 -- currently the 
         *                                   latest available for Mac OSX 10.3.
         * Safari 2.0.2:         416     <-- hasOwnProperty introduced
         * Safari 2.0.4:         418     <-- preventDefault fixed
         * Safari 2.0.4 (419.3): 418.9.1 <-- One version of Safari may run
         *                                   different versions of webkit
         * Safari 2.0.4 (419.3): 419     <-- Tiger installations that have been
         *                                   updated, but not updated
         *                                   to the latest patch.
         * Webkit 212 nightly:   522+    <-- Safari 3.0 precursor (with native SVG
         *                                   and many major issues fixed).  
         * 3.x yahoo.com, flickr:422     <-- Safari 3.x hacks the user agent
         *                                   string when hitting yahoo.com and 
         *                                   flickr.com.
         * Safari 3.0.4 (523.12):523.12  <-- First Tiger release - automatic update
         *                                   from 2.x via the 10.4.11 OS patch
         * Webkit nightly 1/2008:525+    <-- Supports DOMContentLoaded event.
         *                                   yahoo.com user agent hack removed.
         *                                   
         * </pre>
         * http://developer.apple.com/internet/safari/uamatrix.html
         * property webkit
         * type float
         */
        webkit: 0,

        /**
         * The mobile property will be set to a string containing any relevant
         * user agent information when a modern mobile browser is detected.
         * Currently limited to Safari on the iPhone/iPod Touch, Nokia N-series
         * devices with the WebKit-based browser, and Opera Mini.  
         * property mobile 
         * type string
         */
        mobile: null,

        /**
         * Adobe AIR version number or 0.  Only populated if webkit is detected.
         * Example: 1.0
         * property air
         * type float
         */
        air: 0

    };

    var ua=navigator.userAgent, m;

    // Modern KHTML browsers should qualify as Safari X-Grade
    if ((/KHTML/).test(ua)) {
        o.webkit=1;
    }
    // Modern WebKit browsers are at least X-Grade
    m=ua.match(/AppleWebKit\/([^\s]*)/);
    if (m&&m[1]) {
        o.webkit=parseFloat(m[1]);

        // Mobile browser check
        if (/ Mobile\//.test(ua)) {
            o.mobile = "Apple"; // iPhone or iPod Touch
        } else {
            m=ua.match(/NokiaN[^\/]*/);
            if (m) {
                o.mobile = m[0]; // Nokia N-series, ex: NokiaN95
            }
        }

        m=ua.match(/AdobeAIR\/([^\s]*)/);
        if (m) {
            o.air = m[0]; // Adobe AIR 1.0 or better
        }

    }

    if (!o.webkit) { // not webkit
        // @todo check Opera/8.01 (J2ME/MIDP; Opera Mini/2.0.4509/1316; fi; U; ssr)
        m=ua.match(/Opera[\s\/]([^\s]*)/);
        if (m&&m[1]) {
            o.opera=parseFloat(m[1]);
            m=ua.match(/Opera Mini[^;]*/);
            if (m) {
                o.mobile = m[0]; // ex: Opera Mini/2.0.4509/1316
            }
        } else { // not opera or webkit
            m=ua.match(/MSIE\s([^;]*)/);
            if (m&&m[1]) {
                o.ie=parseFloat(m[1]);
            } else { // not opera, webkit, or ie
                m=ua.match(/Gecko\/([^\s]*)/);
                if (m) {
                    o.gecko=1; // Gecko detected, look for revision
                    m=ua.match(/rv:([^\s\)]*)/);
                    if (m&&m[1]) {
                        o.gecko=parseFloat(m[1]);
                    }
                }
            }
        }
    }
    
    return o;
}();


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
    -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,62, -1,62,-1,63,
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

// _compose: add the members of the second object into the first and return it.
function _compose(x, y) {
  for (var i in y) {
    x[i] = y[i]
  }
  return x;
}

/**
 * @constructor
 */
function Some(x) { return { content: x } }

/*
    The global object JSON contains two methods.

    JSON.stringify(value) takes a JavaScript value and produces a JSON text.
    The value must not be cyclical.

    This JSON.stringify returns an object {str:s, funcs:fs}
    where s is the traditional JSON string representation and fs is a 
    table of functions referenced in s. A function appears as a string
    _closureTable[id] where id is a reference into the fs table.

    JSON.parse(text) takes a JSON text and produces a JavaScript value. 
    Returns false if there is an error, and otherwise it returns a
    new Some object {content:obj} where obj is the object denoted by text.
*/
var JSON = function () {
    var m = {
            '\b': '\\b',
            '\t': '\\t',
            '\n': '\\n',
            '\f': '\\f',
            '\r': '\\r',
            '"' : '\\"',
            '\\': '\\\\'
        },
        nextFuncID = 0,      // WARNING: this counter could grow without bound
        s = {
            'function' : function (x) {
              if (x.location == 'server') {
                var envJSON = s.object(x.environment);
                return {funcs: envJSON.funcs,
                        str: 'serverFunc[' + x.func + ']' + 
                             envJSON.str};
              }
              var id = nextFuncID++;
              // FIXME: Just use a global table at the same scope as nextFuncID
              //   and clear it out at the start of each call to stringify
              var closureTable = {};
              closureTable[id] = function (_env) { return x };
              return  {funcs: closureTable, str:'_closureTable[' + id + ']'}
            },
            'boolean': function (x) {
                return {funcs: [], str: String(x)};
            },
            number: function (x) {
                var result = isFinite(x) ? String(x) : 'null';
                return  {funcs: [], str: result};
            },
            string: function (x) {
                if (/["\\\x00-\x1f]/.test(x)) {
                    x = x.replace(/([\x00-\x1f\\"])/g, function(a, b) {
                        var c = m[b];
                        if (c) {
                            return c;
                        }
                        c = b.charCodeAt();
                        var result = '\\u00' +
                            Math.floor(c / 16).toString(16) +
                            (c % 16).toString(16);
                        return  {funcs: [], str: result};
                    });
                }
                return {funcs: [], str: '"' + x + '"'};
            },
            object: function (x) {
                if (x) {
                    var a = [], b, c = {}, f, i, l, v;
                    if (x instanceof Array) {
                        // [HACK]
                        //   SL: for sending XML to the server
                        var isXml = false;
                        var i = 0;
                        if(x.length == 2 && x[0] == 'TEXT') {
                          isXml = true;
                          i = 1;
                          a[0] = '{\"_xml\":[\"TEXT\",';
                        } else if (x.length == 4 && x[0] == 'ELEMENT') {
                          isXml = true;
                          i = 1;
                          a[0] = '{\"_xml\":[\"ELEMENT\",';
                        } else {
                          a[0] = '[';
                        }
                        l = x.length;
                        for (; i < l; i += 1) {
                            v = x[i];
                            f = s[typeof v];
                            if (f) {
                                vx = f(v);
                                v = vx.str;
                                _compose(c, vx.funcs);
                                if (typeof v == 'string') {
                                    if (b) {
                                        a[a.length] = ',';
                                    }
                                    a[a.length] = v;
                                    b = true;
                                }
                            }
                        }
                        if(isXml) {
                          a[a.length] = ']}'
                        } else {
                          a[a.length] = ']';
                        }
                    } else if (x instanceof Object) {
                        a[0] = '{';
                        for (i in x) {
                            v = x[i];
                            f = s[typeof v];
                            if (f) {
                                vx = f(v);
                                v = vx.str;
                                _compose(c, vx.funcs);

                                if (typeof v == 'string') {
                                    if (b) {
                                        a[a.length] = ',';
                                    }
                                    var temp = s.string(i);
                                    a.push(temp.str, ':', v);
                                    b = true;
                                }
                            }
                        }
                        a[a.length] = '}';
                    } else {
                        return;
                    }
                    return {funcs: c, str: a.join('')};
                }
                return 'null';
            }
        };
    return {
        copyright: '(c)2005 JSON.org',
        license: 'http://www.crockford.com/JSON/license.html',
/*
    Stringify a JavaScript value, producing a JSON text.
*/
        stringify: function (v) {
            var f = s[typeof v];
            nextFuncID = 0;
            if (f) {
                vx = f(v);
                v = vx.str;
                if (typeof v == 'string') {
                    return vx;
                }
            }
            throw("Internal error: unable to JSONize " + v);
        },

        stringifyB64: function(v) {
            return _base64encode(this.stringify(v));
        },
/*
    Parse a JSON text, producing a JavaScript value.
    It returns false if there is a syntax error.
*/
        parse: function (text) {
            // The code inside the try block doesnt like newlines, so
            //   we escape them. [SL]
            var text = text.replace(/\n/g, '\\n');
            try {
                // This series of regexes removes any double-quoted strings
                // (accounting for escaped double-quotes) and checks that
                // the result only uses a limited character set, that of 
                // legal JSON expressions.
                if (/[^,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]/.test(
                        text.replace(/"(\\.|[^"\\])*"/g, '')))
                  return false;
                return(new Some(eval("(" + text + ")")))
            } catch (e) {
                // TBD: use exceptions instead of Some/false ?
                _debug("JSON syntax error in: " + text);
                return false;
            }
        },

        parseB64: function(text) {
          return this.parse(_base64decode(text));
        },

        parseB64Safe: function(text) {
          return this.parseB64(text.replace('\n', ''));
        }

    };
}();

// alert("end of JSON");

//

var Regex = (function() {
function l(o) { return o._label; }
function v(o) { return o._value; }

function assert(n, msg) {
  if (!n) {
    throw ("Assertion failure! " + msg)
  }
}

function repeat(input) {
  switch (l(input)) {
    case 'Question' : return '?';
    case 'Star'     : return '*';
    case 'Plus'     : return '+';
    default         : assert(false, 'repeat : ' + l(input));
  }
}

function group(input) {
  return '(?:' + input + ')'
}

var specials = ['.','*','+','?','|','(',')','[',']','{','}','\\','$','^'];

// (\.|\*|...|\^)
var quoteRE = new RegExp(
  '(\\' + specials.join('|\\') + ')', 'g'
);

// (\.|\*|...|\^) |-> \$1
function quote(s) {
  return s.replace(quoteRE, '\\$1');
}

function compile(input) {
  var val = v(input);
  switch (l(input)) {
     case 'StartAnchor' : return '^';
     case 'EndAnchor' : return '$';
     case 'Range'  : return '[' + quote(val[1]._c)  + '-' + quote(val[2]._c) + ']';
     case 'Simply' : return quote($str(val));
     case 'Any'    : return '.';
     case 'Seq'    : {
        var output = '';
        for (var i = 0; i < val.length; i++) {
          output += compile(val[i]);
        }
        return output;
     }
     case 'Group'     : return group(compile(val));
     case 'Repeat'    : return compile(val[2]) + repeat(val[1]);
     case 'Quote'     : return compile(val);
     case 'Alternate' : return compile(val[1]) + '|' + compile(val[2]);
     case 'Replace'   : _debug("replace isn't yet supported on the client");
     default          : assert(false, 'compile : ' + input);
  }
}

function Range(l,r) { return {_label:'Range', _value:{1:l,2:r}}; }
function Simply(value) { return {_label:'Simply', _value:value}; }
var Any = { _label:'Any', _value:{}};
function Seq(value) { return {_label:'Seq', _value:value}; }
function Repeat(l,r) { return {_label:'Repeat', _value:{1:l,2:r}}; }
var Question = { _label:'Question', _value:{}};
var Plus = { _label:'Plus', _value:{}};
var Star = { _label:'Star', _value:{}};
function Group(value) { return {_label:'Group', _value:value}; }


// WARNING:
//   these tests are completely out of date
//   many of them probably won't work
var tests = 
  [
    (function(s) {
      return ["splicing", Simply (s), s, true];
    }("some .*string$\" ++?")),
    
    ["range 0", Range ('0', '9'), "3", true],
    ["range 1", Range ('0', '9'), "0", true],
    ["range 2", Range ('0', '9'), "9", true],
    ["range 3", Range ('0', '9'), ".", false],
    ["range 4", Range ('a', 'z'), "p", true],
    ["range 5", Range ('A', 'Z'), "p", false],

    ["star 0", Repeat (Star, Any), "23r2r3", true],
    ["star 1", Repeat (Star, Any), "", true],
    ["star 2", Repeat (Star, (Simply ("abc"))), "abc", true],
    ["star 3", Repeat (Star, (Simply ("abc"))), "abcabc", true],
    ["star 4", Repeat (Star, (Simply ("abc"))), "", true],
    ["star 5", Repeat (Star, (Simply ("abc"))), "a", false],
    ["star 6", Repeat (Star, (Simply ("abc"))), "abca", false],

    ["plus 0", Repeat (Plus, Any), "23r2r3", true],
    ["plus 1", Repeat (Plus, Any), "", false],
    ["plus 2", Repeat (Plus, (Simply ("abc"))), "abc", true],
    ["plus 3", Repeat (Plus, (Simply ("abc"))), "abcabc", true],
    ["plus 4", Repeat (Plus, (Simply ("abc"))), "", false],
    ["plus 5", Repeat (Plus, (Simply ("abc"))), "a", false],
    ["plus 6", Repeat (Plus, (Simply ("abc"))), "abca", false],

    ["nesting 0", Seq ([Simply ("A"),
                        Repeat (Plus, Simply ("B"))]), "ABBB", true],

    ["nesting 1", Seq ([Simply ("A"),
                        Repeat (Plus, Simply ("B"))]), "ABAB", false],

    ["nesting 2", Repeat (Plus, Seq ([Simply ("A"),
                                      Simply ("B")])), "ABAB", true],

    ["nesting 3", Repeat (Plus, Seq ([Simply ("A"),
                                      Simply ("B")])), "ABBB", false],
  ]

function run_tests(tests) {
  for (var i = 0; i < tests.length; i++) {
    var test = tests[i];
    var n = test[0], r = new RegExp('^' + compile(test[1]) + '$'), s = test[2], b = test[3];
    if ((r(s) != null) == b) {
      print("PASS: " + n);
    }
    else {
      print("FAIL: " + n);
    }
  }
}
return ({ 'compile' : compile });
})();

// alert("end of REGEX");


/**
 * @constructor
 */
YAHOO.util.CustomEvent = function(type, oScope, silent, signature) {

    YAHOO.type = type;

    YAHOO.scope = oScope || window;

    YAHOO.silent = silent;

    
    YAHOO.signature = signature || YAHOO.util.CustomEvent.LIST;

    
    YAHOO.subscribers = [];

    var onsubscribeType = "_YUICEOnSubscribe";

    // Only add subscribe events for events that are not generated by 
    // CustomEvent
    if (type !== onsubscribeType) {

        /**
         * Custom events provide a custom event that fires whenever there is
         * a new subscriber to the event.  This provides an opportunity to
         * handle the case where there is a non-repeating event that has
         * already fired has a new subscriber.  
         *
         * event subscribeEvent
         * type YAHOO.util.CustomEvent
         * param {Function} fn The function to execute
         * param {Object}   obj An object to be passed along when the event 
         *                       fires
         * param {boolean|Object}  override If true, the obj passed in becomes 
         *                                   the execution scope of the listener.
         *                                   if an object, that object becomes the
         *                                   the execution scope.
         */
        YAHOO.subscribeEvent = 
                new YAHOO.util.CustomEvent(onsubscribeType, this, true);

    } 


    /**
     * In order to make it possible to execute the rest of the subscriber
     * stack when one thows an exception, the subscribers exceptions are
     * caught.  The most recent exception is stored in this property
     * property lastError
     * type Error
     */
    YAHOO.lastError = null;
};

/**
 * Subscriber listener sigature constant.  The LIST type returns three
 * parameters: the event type, the array of args passed to fire, and
 * the optional custom object
 * property YAHOO.util.CustomEvent.LIST
 * static
 * type int
 */
YAHOO.util.CustomEvent.LIST = 0;

/**
 * Subscriber listener sigature constant.  The FLAT type returns two
 * parameters: the first argument passed to fire and the optional 
 * custom object
 * property YAHOO.util.CustomEvent.FLAT
 * static
 * type int
 */
YAHOO.util.CustomEvent.FLAT = 1;

YAHOO.util.CustomEvent.prototype = {

    /**
     * Subscribes the caller to this event
     * method subscribe
     * param {Function} fn        The function to execute
     * param {Object}   obj       An object to be passed along when the event 
     *                             fires
     * param {boolean|Object}  override If true, the obj passed in becomes 
     *                                   the execution scope of the listener.
     *                                   if an object, that object becomes the
     *                                   the execution scope.
     */
    subscribe: function(fn, obj, override) {

        if (!fn) {
throw new Error("Invalid callback for subscriber to '" + this.type + "'");
        }

        if (this.subscribeEvent) {
            this.subscribeEvent.fire(fn, obj, override);
        }

        this.subscribers.push( new YAHOO.util.Subscriber(fn, obj, override) );
    },

    /**
     * Unsubscribes subscribers.
     * method unsubscribe
     * param {Function} fn  The subscribed function to remove, if not supplied
     *                       all will be removed
     * param {Object}   obj  The custom object passed to subscribe.  This is
     *                        optional, but if supplied will be used to
     *                        disambiguate multiple listeners that are the same
     *                        (e.g., you subscribe many object using a function
     *                        that lives on the prototype)
     * return {boolean} True if the subscriber was found and detached.
     */
    unsubscribe: function(fn, obj) {

        if (!fn) {
            return this.unsubscribeAll();
        }

        var found = false;
        for (var i=0, len=this.subscribers.length; i<len; ++i) {
            var s = this.subscribers[i];
            if (s && s.contains(fn, obj)) {
                this._delete(i);
                found = true;
            }
        }

        return found;
    },

    /**
     * Notifies the subscribers.  The callback functions will be executed
     * from the scope specified when the event was created, and with the 
     * following parameters:
     *   <ul>
     *   <li>The type of event</li>
     *   <li>All of the arguments fire() was executed with as an array</li>
     *   <li>The custom object (if any) that was passed into the subscribe() 
     *       method</li>
     *   </ul>
     * method fire 
     * param {Object*} arguments an arbitrary set of parameters to pass to 
     *                            the handler.
     * return {boolean} false if one of the subscribers returned false, 
     *                   true otherwise
     */
    fire: function() {

        this.lastError = null;

        var errors = [],
            len=this.subscribers.length;

        if (!len && this.silent) {
            return true;
        }

        var args=[].slice.call(arguments, 0), ret=true, i, rebuild=false;

        // make a copy of the subscribers so that there are
        // no index problems if one subscriber removes another.
        var subs = this.subscribers.slice(), throwErrors = YAHOO.util.Event.throwErrors;

        for (i=0; i<len; ++i) {
            var s = subs[i];
            if (!s) {
                rebuild=true;
            } else {

                var scope = s.getScope(this.scope);

                if (this.signature == YAHOO.util.CustomEvent.FLAT) {
                    var param = null;
                    if (args.length > 0) {
                        param = args[0];
                    }

                    try {
                        ret = s.fn.call(scope, param, s.obj);
                    } catch(e) {
                        this.lastError = e;
                        // errors.push(e);
                        if (throwErrors) {
                            throw e;
                        }
                    }
                } else {
                    try {
                        ret = s.fn.call(scope, this.type, args, s.obj);
                    } catch(ex) {
                        this.lastError = ex;
                        if (throwErrors) {
                            throw ex;
                        }
                    }
                }

                if (false === ret) {

                    break;
                    // return false;
                }
            }
        }

        return (ret !== false);
    },

    /**
     * Removes all listeners
     * method unsubscribeAll
     * return {int} The number of listeners unsubscribed
     */
    unsubscribeAll: function() {
        for (var i=this.subscribers.length-1; i>-1; i--) {
            this._delete(i);
        }

        this.subscribers=[];

        return i;
    },

    /**
     * method _delete
     * private
     */
    _delete: function(index) {
        var s = this.subscribers[index];
        if (s) {
            delete s.fn;
            delete s.obj;
        }

        // this.subscribers[index]=null;
        this.subscribers.splice(index, 1);
    },

    /**
     * method toString
     */
    toString: function() {
         return "CustomEvent: " + "'" + this.type  + "', " + 
             "scope: " + this.scope;

    }
};

/////////////////////////////////////////////////////////////////////

/**
 * Stores the subscriber information to be used when the event fires.
 * param {Function} fn       The function to execute
 * param {Object}   obj      An object to be passed along when the event fires
 * param {boolean}  override If true, the obj passed in becomes the execution
 *                            scope of the listener
 * class Subscriber
 * @constructor
 */
YAHOO.util.Subscriber = function(fn, obj, override) {

    /**
     * The callback that will be execute when the event fires
     * property fn
     * type function
     */
    this.fn = fn;

    /**
     * An optional custom object that will passed to the callback when
     * the event fires
     * property obj
     * type object
     */
    this.obj = YAHOO.lang.isUndefined(obj) ? null : obj;

    /**
     * The default execution scope for the event listener is defined when the
     * event is created (usually the object which contains the event).
     * By setting override to true, the execution scope becomes the custom
     * object passed in by the subscriber.  If override is an object, that 
     * object becomes the scope.
     * property override
     * type boolean|object
     */
    this.override = override;

};

/**
 * Returns the execution scope for this listener.  If override was set to true
 * the custom obj will be the scope.  If override is an object, that is the
 * scope, otherwise the default scope will be used.
 * method getScope
 * param {Object} defaultScope the scope to use if this listener does not
 *                              override it.
 */
YAHOO.util.Subscriber.prototype.getScope = function(defaultScope) {
    if (this.override) {
        if (this.override === true) {
            return this.obj;
        } else {
            return this.override;
        }
    }
    return defaultScope;
};

/**
 * Returns true if the fn and obj match this objects properties.
 * Used by the unsubscribe method to match the right subscriber.
 *
 * method contains
 * param {Function} fn the function to execute
 * param {Object} obj an object to be passed along when the event fires
 * return {boolean} true if the supplied arguments match this 
 *                   subscriber's signature.
 */
YAHOO.util.Subscriber.prototype.contains = function(fn, obj) {
    if (obj) {
        return (this.fn == fn && this.obj == obj);
    } else {
        return (this.fn == fn);
    }
};

/**
 * method toString
 */
YAHOO.util.Subscriber.prototype.toString = function() {
    return "Subscriber { obj: " + this.obj  + 
           ", override: " +  (this.override || "no") + " }";
};

/**
 * The Event Utility provides utilities for managing DOM Events and tools
 * for building event systems
 *
 * module event
 * title Event Utility
 * namespace YAHOO.util
 * requires yahoo
 */

// The first instance of Event will win if it is loaded more than once.
// @TODO this needs to be changed so that only the state data that needs to
// be preserved is kept, while methods are overwritten/added as needed.
// This means that the module pattern can't be used.
if (!YAHOO.util.Event) {

/**
 * The event utility provides functions to add and remove event listeners,
 * event cleansing.  It also tries to automatically remove listeners it
 * registers during the unload event.
 *
 * class Event
 * static
 */
    YAHOO.util.Event = function() {

        /**
         * True after the onload event has fired
         * property loadComplete
         * type boolean
         * static
         * private
         */
        var loadComplete =  false;

        /**
         * Cache of wrapped listeners
         * property listeners
         * type array
         * static
         * private
         */
        var listeners = [];

        /**
         * User-defined unload function that will be fired before all events
         * are detached
         * property unloadListeners
         * type array
         * static
         * private
         */
        var unloadListeners = [];

        /**
         * Cache of DOM0 event handlers to work around issues with DOM2 events
         * in Safari
         * property legacyEvents
         * static
         * private
         */
        var legacyEvents = [];

        /**
         * Listener stack for DOM0 events
         * property legacyHandlers
         * static
         * private
         */
        var legacyHandlers = [];

        /**
         * The number of times to poll after window.onload.  This number is
         * increased if additional late-bound handlers are requested after
         * the page load.
         * property retryCount
         * static
         * private
         */
        var retryCount = 0;

        /**
         * onAvailable listeners
         * property onAvailStack
         * static
         * private
         */
        var onAvailStack = [];

        /**
         * Lookup table for legacy events
         * property legacyMap
         * static
         * private
         */
        var legacyMap = [];

        /**
         * Counter for auto id generation
         * property counter
         * static
         * private
         */
        var counter = 0;
        
        /**
         * Normalized keycodes for webkit/safari
         * property webkitKeymap
         * type {int: int}
         * private
         * static
         * final
         */
        var webkitKeymap = {
            63232: 38, // up
            63233: 40, // down
            63234: 37, // left
            63235: 39, // right
            63276: 33, // page up
            63277: 34, // page down
            25: 9      // SHIFT-TAB (Safari provides a different key code in
                       // this case, even though the shiftKey modifier is set)
        };
        
        // String constants used by the addFocusListener and removeFocusListener methods
        var _FOCUS = YAHOO.env.ua.ie ? "focusin" : "focus";
        var _BLUR = YAHOO.env.ua.ie ? "focusout" : "blur";      

        return {

            /**
             * The number of times we should look for elements that are not
             * in the DOM at the time the event is requested after the document
             * has been loaded.  The default is 2000@amp;20 ms, so it will poll
             * for 40 seconds or until all outstanding handlers are bound
             * (whichever comes first).
             * property POLL_RETRYS
             * type int
             * static
             * final
             */
            POLL_RETRYS: 2000,

            /**
             * The poll interval in milliseconds
             * property POLL_INTERVAL
             * type int
             * static
             * final
             */
            POLL_INTERVAL: 20,

            /**
             * Element to bind, int constant
             * property EL
             * type int
             * static
             * final
             */
            EL: 0,

            /**
             * Type of event, int constant
             * property TYPE
             * type int
             * static
             * final
             */
            TYPE: 1,

            /**
             * Function to execute, int constant
             * property FN
             * type int
             * static
             * final
             */
            FN: 2,

            /**
             * Function wrapped for scope correction and cleanup, int constant
             * property WFN
             * type int
             * static
             * final
             */
            WFN: 3,

            /**
             * Object passed in by the user that will be returned as a 
             * parameter to the callback, int constant.  Specific to
             * unload listeners
             * property OBJ
             * type int
             * static
             * final
             */
            UNLOAD_OBJ: 3,

            /**
             * Adjusted scope, either the element we are registering the event
             * on or the custom object passed in by the listener, int constant
             * property ADJ_SCOPE
             * type int
             * static
             * final
             */
            ADJ_SCOPE: 4,

            /**
             * The original obj passed into addListener
             * property OBJ
             * type int
             * static
             * final
             */
            OBJ: 5,

            /**
             * The original scope parameter passed into addListener
             * property OVERRIDE
             * type int
             * static
             * final
             */
            OVERRIDE: 6,

            /**
             * The original capture parameter passed into _addListener
             * property CAPTURE
             * type int
             * static
             * final
             */
            CAPTURE: 7,


            /**
             * addListener/removeListener can throw errors in unexpected scenarios.
             * These errors are suppressed, the method returns false, and this property
             * is set
             * property lastError
             * static
             * type Error
             */
            lastError: null,

            /**
             * Safari detection
             * property isSafari
             * private
             * static
             * deprecated use YAHOO.env.ua.webkit
             */
            isSafari: YAHOO.env.ua.webkit,
            
            /**
             * webkit version
             * property webkit
             * type string
             * private
             * static
             * deprecated use YAHOO.env.ua.webkit
             */
            webkit: YAHOO.env.ua.webkit,
            
            /**
             * IE detection 
             * property isIE
             * private
             * static
             * deprecated use YAHOO.env.ua.ie
             */
            isIE: YAHOO.env.ua.ie,

            /**
             * poll handle
             * property _interval
             * static
             * private
             */
            _interval: null,

            /**
             * document readystate poll handle
             * property _dri
             * static
             * private
             */
             _dri: null,

            /**
             * True when the document is initially usable
             * property DOMReady
             * type boolean
             * static
             */
            DOMReady: false,

            /**
             * Errors thrown by subscribers of custom events are caught
             * and the error message is written to the debug console.  If
             * this property is set to true, it will also re-throw the
             * error.
             * property throwErrors
             * type boolean
             * default false
             */
            throwErrors: false,

            /**
             * method startInterval
             * static
             * private
             */
            startInterval: function() {
                if (!this._interval) {
                    var selfprim = this;
                    var callback = function() { selfprim._tryPreloadAttach(); };
                    this._interval = setInterval(callback, this.POLL_INTERVAL);
                }
            },

            /**
             * Executes the supplied callback when the item with the supplied
             * id is found.  This is meant to be used to execute behavior as
             * soon as possible as the page loads.  If you use this after the
             * initial page load it will poll for a fixed time for the element.
             * The number of times it will poll and the frequency are
             * configurable.  By default it will poll for 10 seconds.
             *
             * <p>The callback is executed with a single parameter:
             * the custom object parameter, if provided.</p>
             *
             * method onAvailable
             *
             * param {string||string[]}   p_id the id of the element, or an array
             * of ids to look for.
             * param {function} p_fn what to execute when the element is found.
             * param {object}   p_obj an optional object to be passed back as
             *                   a parameter to p_fn.
             * param {boolean|object}  p_override If set to true, p_fn will execute
             *                   in the scope of p_obj, if set to an object it
             *                   will execute in the scope of that object
             * param checkContent {boolean} check child node readiness (onContentReady)
             * static
             */
            onAvailable: function(p_id, p_fn, p_obj, p_override, checkContent) {

                var a = (YAHOO.lang.isString(p_id)) ? [p_id] : p_id;

                for (var i=0; i<a.length; i=i+1) {
                    onAvailStack.push({id:         a[i], 
                                       fn:         p_fn, 
                                       obj:        p_obj, 
                                       override:   p_override, 
                                       checkReady: checkContent });
                }

                retryCount = this.POLL_RETRYS;

                this.startInterval();
            },

            /**
             * Works the same way as onAvailable, but additionally checks the
             * state of sibling elements to determine if the content of the
             * available element is safe to modify.
             *
             * <p>The callback is executed with a single parameter:
             * the custom object parameter, if provided.</p>
             *
             * method onContentReady
             *
             * param {string}   p_id the id of the element to look for.
             * param {function} p_fn what to execute when the element is ready.
             * param {object}   p_obj an optional object to be passed back as
             *                   a parameter to p_fn.
             * param {boolean|object}  p_override If set to true, p_fn will execute
             *                   in the scope of p_obj.  If an object, p_fn will
             *                   exectute in the scope of that object
             *
             * static
             */
            onContentReady: function(p_id, p_fn, p_obj, p_override) {
                this.onAvailable(p_id, p_fn, p_obj, p_override, true);
            },

            /**
             * Executes the supplied callback when the DOM is first usable.  This
             * will execute immediately if called after the DOMReady event has
             * fired.   @todo the DOMContentReady event does not fire when the
             * script is dynamically injected into the page.  This means the
             * DOMReady custom event will never fire in FireFox or Opera when the
             * library is injected.  It _will_ fire in Safari, and the IE 
             * implementation would allow for us to fire it if the defered script
             * is not available.  We want this to behave the same in all browsers.
             * Is there a way to identify when the script has been injected 
             * instead of included inline?  Is there a way to know whether the 
             * window onload event has fired without having had a listener attached 
             * to it when it did so?
             *
             * <p>The callback is a CustomEvent, so the signature is:</p>
             * <p>type &lt;string&gt;, args &lt;array&gt;, customobject &lt;object&gt;</p>
             * <p>For DOMReady events, there are no fire argments, so the
             * signature is:</p>
             * <p>"DOMReady", [], obj</p>
             *
             *
             * method onDOMReady
             *
             * param {function} p_fn what to execute when the element is found.
             * param {object}   p_obj an optional object to be passed back as
             *                   a parameter to p_fn.
             * param {boolean|object}  p_scope If set to true, p_fn will execute
             *                   in the scope of p_obj, if set to an object it
             *                   will execute in the scope of that object
             *
             * static
             */
            onDOMReady: function(p_fn, p_obj, p_override) {
                if (this.DOMReady) {
                    setTimeout(function() {
                        var s = window;
                        if (p_override) {
                            if (p_override === true) {
                                s = p_obj;
                            } else {
                                s = p_override;
                            }
                        }
                        p_fn.call(s, "DOMReady", [], p_obj);
                    }, 0);
                } else {
                    this.DOMReadyEvent.subscribe(p_fn, p_obj, p_override);
                }
            },


            /**
             * Appends an event handler
             *
             * method _addListener
             *
             * param {String|HTMLElement|Array|NodeList} el An id, an element 
             *  reference, or a collection of ids and/or elements to assign the 
             *  listener to.
             * param {String}   sType     The type of event to append
             * param {Function} fn        The method the event invokes
             * param {Object}   obj    An arbitrary object that will be 
             *                             passed as a parameter to the handler
             * param {Boolean|object}  override  If true, the obj passed in becomes
             *                             the execution scope of the listener. If an
             *                             object, this object becomes the execution
             *                             scope.
             * param {boolen}      capture capture or bubble phase
             * return {Boolean} True if the action was successful or defered,
             *                        false if one or more of the elements 
             *                        could not have the listener attached,
             *                        or if the operation throws an exception.
             * private
             * static
             */
            _addListener: function(el, sType, fn, obj, override, capture) {

                if (!fn || !fn.call) {
                    return false;
                }

                // The el argument can be an array of elements or element ids.
                if ( this._isValidCollection(el)) {
                    var ok = true;
                    for (var i=0,len=el.length; i<len; ++i) {
                        ok = this._addListener(el[i], 
                                       sType, 
                                       fn, 
                                       obj, 
                                       override, 
                                       capture) && ok;
                    }
                    return ok;

                } else if (YAHOO.lang.isString(el)) {
                    var oEl = this.getEl(el);
                    // If the el argument is a string, we assume it is 
                    // actually the id of the element.  If the page is loaded
                    // we convert el to the actual element, otherwise we 
                    // defer attaching the event until onload event fires

                    // check to see if we need to delay hooking up the event 
                    // until after the page loads.
                    if (oEl) {
                        el = oEl;
                    } else {
                        // defer adding the event until the element is available
                        this.onAvailable(el, function() {
                           YAHOO.util.Event._addListener(el, sType, fn, obj, override, capture);
                        });

                        return true;
                    }
                }

                // Element should be an html element or an array if we get 
                // here.
                if (!el) {
                    return false;
                }

                // we need to make sure we fire registered unload events 
                // prior to automatically unhooking them.  So we hang on to 
                // these instead of attaching them to the window and fire the
                // handles explicitly during our one unload event.
                if ("unload" == sType && obj !== this) {
                    unloadListeners[unloadListeners.length] =
                            [el, sType, fn, obj, override, capture];
                    return true;
                }


                // if the user chooses to override the scope, we use the custom
                // object passed in, otherwise the executing scope will be the
                // HTML element that the event is registered on
                var scope = el;
                if (override) {
                    if (override === true) {
                        scope = obj;
                    } else {
                        scope = override;
                    }
                }

                // wrap the function so we can return the obj object when
                // the event fires;
                var wrappedFn = function(e) {
                        return fn.call(scope, YAHOO.util.Event.getEvent(e, el), 
                                obj);
                    };

                var li = [el, sType, fn, wrappedFn, scope, obj, override, capture];
                var index = listeners.length;
                // cache the listener so we can try to automatically unload
                listeners[index] = li;

                if (this.useLegacyEvent(el, sType)) {
                    var legacyIndex = this.getLegacyIndex(el, sType);

                    // Add a new dom0 wrapper if one is not detected for this
                    // element
                    if ( legacyIndex == -1 || 
                                el != legacyEvents[legacyIndex][0] ) {

                        legacyIndex = legacyEvents.length;
                        legacyMap[el.id + sType] = legacyIndex;

                        // cache the signature for the DOM0 event, and 
                        // include the existing handler for the event, if any
                        legacyEvents[legacyIndex] = 
                            [el, sType, el["on" + sType]];
                        legacyHandlers[legacyIndex] = [];

                        el["on" + sType] = 
                            function(e) {
                                YAHOO.util.Event.fireLegacyEvent(
                                    YAHOO.util.Event.getEvent(e), legacyIndex);
                            };
                    }

                    // add a reference to the wrapped listener to our custom
                    // stack of events
                    //legacyHandlers[legacyIndex].push(index);
                    legacyHandlers[legacyIndex].push(li);

                } else {
                    try {
                        this._simpleAdd(el, sType, wrappedFn, capture);
                    } catch(ex) {
                        // handle an error trying to attach an event.  If it fails
                        // we need to clean up the cache
                        this.lastError = ex;
                        this._removeListener(el, sType, fn, capture);
                        return false;
                    }
                }

                return true;
                
            },


            /**
             * Appends an event handler
             *
             * method addListener
             *
             * param {String|HTMLElement|Array|NodeList} el An id, an element 
             *  reference, or a collection of ids and/or elements to assign the 
             *  listener to.
             * param {String}   sType     The type of event to append
             * param {Function} fn        The method the event invokes
             * param {Object}   obj    An arbitrary object that will be 
             *                             passed as a parameter to the handler
             * param {Boolean|object}  override  If true, the obj passed in becomes
             *                             the execution scope of the listener. If an
             *                             object, this object becomes the execution
             *                             scope.
             * return {Boolean} True if the action was successful or defered,
             *                        false if one or more of the elements 
             *                        could not have the listener attached,
             *                        or if the operation throws an exception.
             * static
             */
            addListener: function (el, sType, fn, obj, override) {
                return this._addListener(el, sType, fn, obj, override, false);
            },

            /**
             * Appends a focus event handler.  (The focusin event is used for Internet Explorer, 
             * the focus, capture-event for Opera, WebKit, and Gecko.)
             *
             * method addFocusListener
             *
             * param {String|HTMLElement|Array|NodeList} el An id, an element 
             *  reference, or a collection of ids and/or elements to assign the 
             *  listener to.
             * param {Function} fn        The method the event invokes
             * param {Object}   obj    An arbitrary object that will be 
             *                             passed as a parameter to the handler
             * param {Boolean|object}  override  If true, the obj passed in becomes
             *                             the execution scope of the listener. If an
             *                             object, this object becomes the execution
             *                             scope.
             * return {Boolean} True if the action was successful or defered,
             *                        false if one or more of the elements 
             *                        could not have the listener attached,
             *                        or if the operation throws an exception.
             * static
             */
            addFocusListener: function (el, fn, obj, override) {
                return this._addListener(el, _FOCUS, fn, obj, override, true);
            },          


            /**
             * Removes a focus event listener
             *
             * method removeFocusListener
             *
             * param {String|HTMLElement|Array|NodeList} el An id, an element 
             *  reference, or a collection of ids and/or elements to remove
             *  the listener from.
             * param {Function} fn the method the event invokes.  If fn is
             *  undefined, then all event handlers for the type of event are 
             *  removed.
             * return {boolean} true if the unbind was successful, false 
             *  otherwise.
             * static
             */
            removeFocusListener: function (el, fn) { 
                return this._removeListener(el, _FOCUS, fn, true);
            },

            /**
             * Appends a blur event handler.  (The focusout event is used for Internet Explorer, 
             * the focusout, capture-event for Opera, WebKit, and Gecko.)
             *
             * method addBlurListener
             *
             * param {String|HTMLElement|Array|NodeList} el An id, an element 
             *  reference, or a collection of ids and/or elements to assign the 
             *  listener to.
             * param {Function} fn        The method the event invokes
             * param {Object}   obj    An arbitrary object that will be 
             *                             passed as a parameter to the handler
             * param {Boolean|object}  override  If true, the obj passed in becomes
             *                             the execution scope of the listener. If an
             *                             object, this object becomes the execution
             *                             scope.
             * return {Boolean} True if the action was successful or defered,
             *                        false if one or more of the elements 
             *                        could not have the listener attached,
             *                        or if the operation throws an exception.
             * static
             */
            addBlurListener: function (el, fn, obj, override) {
                return this._addListener(el, _BLUR, fn, obj, override, true);
            },          

            /**
             * Removes a blur event listener
             *
             * method removeBlurListener
             *
             * param {String|HTMLElement|Array|NodeList} el An id, an element 
             *  reference, or a collection of ids and/or elements to remove
             *  the listener from.
             * param {Function} fn the method the event invokes.  If fn is
             *  undefined, then all event handlers for the type of event are 
             *  removed.
             * return {boolean} true if the unbind was successful, false 
             *  otherwise.
             * static
             */
            removeBlurListener: function (el, fn) { 
            
                return this._removeListener(el, _BLUR, fn, true);
            
            },

            /**
             * When using legacy events, the handler is routed to this object
             * so we can fire our custom listener stack.
             * method fireLegacyEvent
             * static
             * private
             */
            fireLegacyEvent: function(e, legacyIndex) {
                var ok=true, le, lh, li, scope, ret;
                
                lh = legacyHandlers[legacyIndex].slice();
                for (var i=0, len=lh.length; i<len; ++i) {
                // for (var i in lh.length) {
                    li = lh[i];
                    if ( li && li[this.WFN] ) {
                        scope = li[this.ADJ_SCOPE];
                        ret = li[this.WFN].call(scope, e);
                        ok = (ok && ret);
                    }
                }

                // Fire the original handler if we replaced one.  We fire this
                // after the other events to keep stopPropagation/preventDefault
                // that happened in the DOM0 handler from touching our DOM2
                // substitute
                le = legacyEvents[legacyIndex];
                if (le && le[2]) {
                    le[2](e);
                }
                
                return ok;
            },

            /**
             * Returns the legacy event index that matches the supplied 
             * signature
             * method getLegacyIndex
             * static
             * private
             */
            getLegacyIndex: function(el, sType) {
                var key = this.generateId(el) + sType;
                if (typeof legacyMap[key] == "undefined") { 
                    return -1;
                } else {
                    return legacyMap[key];
                }
            },

            /**
             * Logic that determines when we should automatically use legacy
             * events instead of DOM2 events.  Currently this is limited to old
             * Safari browsers with a broken preventDefault
             * method useLegacyEvent
             * static
             * private
             */
            useLegacyEvent: function(el, sType) {
return (this.webkit && this.webkit < 419 && ("click"==sType || "dblclick"==sType));
            },
                    
            /**
             * Removes an event listener
             *
             * method _removeListener
             *
             * param {String|HTMLElement|Array|NodeList} el An id, an element 
             *  reference, or a collection of ids and/or elements to remove
             *  the listener from.
             * param {String} sType the type of event to remove.
             * param {Function} fn the method the event invokes.  If fn is
             *  undefined, then all event handlers for the type of event are 
             *  removed.
             * param {boolen}      capture capture or bubble phase             
             * return {boolean} true if the unbind was successful, false 
             *  otherwise.
             * static
             * private
             */
            _removeListener: function(el, sType, fn, capture) {
                var i, len, li;

                // The el argument can be a string
                if (typeof el == "string") {
                    el = this.getEl(el);
                // The el argument can be an array of elements or element ids.
                } else if ( this._isValidCollection(el)) {
                    var ok = true;
                    for (i=el.length-1; i>-1; i--) {
                        ok = ( this._removeListener(el[i], sType, fn, capture) && ok );
                    }
                    return ok;
                }

                if (!fn || !fn.call) {
                    //return false;
                    return this.purgeElement(el, false, sType);
                }

                if ("unload" == sType) {

                    for (i=unloadListeners.length-1; i>-1; i--) {
                        li = unloadListeners[i];
                        if (li && 
                            li[0] == el && 
                            li[1] == sType && 
                            li[2] == fn) {
                                unloadListeners.splice(i, 1);
                                // unloadListeners[i]=null;
                                return true;
                        }
                    }

                    return false;
                }

                var cacheItem = null;

                // The index is a hidden parameter; needed to remove it from
                // the method signature because it was tempting users to
                // try and take advantage of it, which is not possible.
                var index = arguments[4];
  
                if ("undefined" === typeof index) {
                    index = this._getCacheIndex(el, sType, fn);
                }

                if (index >= 0) {
                    cacheItem = listeners[index];
                }

                if (!el || !cacheItem) {
                    return false;
                }


                if (this.useLegacyEvent(el, sType)) {
                    var legacyIndex = this.getLegacyIndex(el, sType);
                    var llist = legacyHandlers[legacyIndex];
                    if (llist) {
                        for (i=0, len=llist.length; i<len; ++i) {
                        // for (i in llist.length) {
                            li = llist[i];
                            if (li && 
                                li[this.EL] == el && 
                                li[this.TYPE] == sType && 
                                li[this.FN] == fn) {
                                    llist.splice(i, 1);
                                    // llist[i]=null;
                                    break;
                            }
                        }
                    }

                } else {
                    try {
                        this._simpleRemove(el, sType, cacheItem[this.WFN], capture);
                    } catch(ex) {
                        this.lastError = ex;
                        return false;
                    }
                }

                // removed the wrapped handler
                delete listeners[index][this.WFN];
                delete listeners[index][this.FN];
                listeners.splice(index, 1);
                // listeners[index]=null;

                return true;

            },


            /**
             * Removes an event listener
             *
             * method removeListener
             *
             * param {String|HTMLElement|Array|NodeList} el An id, an element 
             *  reference, or a collection of ids and/or elements to remove
             *  the listener from.
             * param {String} sType the type of event to remove.
             * param {Function} fn the method the event invokes.  If fn is
             *  undefined, then all event handlers for the type of event are 
             *  removed.
             * return {boolean} true if the unbind was successful, false 
             *  otherwise.
             * static
             */
            removeListener: function(el, sType, fn) {

				return this._removeListener(el, sType, fn, false);

            },


            /**
             * Returns the event's target element.  Safari sometimes provides
             * a text node, and this is automatically resolved to the text
             * node's parent so that it behaves like other browsers.
             * method getTarget
             * param {Event} ev the event
             * param {boolean} resolveTextNode when set to true the target's
             *                  parent will be returned if the target is a 
             *                  text node.  @deprecated, the text node is
             *                  now resolved automatically
             * return {HTMLElement} the event's target
             * static
             */
            getTarget: function(ev, resolveTextNode) {
                var t = ev.target || ev.srcElement;
                return this.resolveTextNode(t);
            },

            /**
             * In some cases, some browsers will return a text node inside
             * the actual element that was targeted.  This normalizes the
             * return value for getTarget and getRelatedTarget.
             * method resolveTextNode
             * param {HTMLElement} node node to resolve
             * return {HTMLElement} the normized node
             * static
             */
            resolveTextNode: function(n) {
                try {
                    if (n && 3 == n.nodeType) {
                        return n.parentNode;
                    }
                } catch(e) { }

                return n;
            },

            /**
             * Returns the event's pageX
             * method getPageX
             * param {Event} ev the event
             * return {int} the event's pageX
             * static
             */
            getPageX: function(ev) {
                var x = ev.pageX;
                if (!x && 0 !== x) {
                    x = ev.clientX || 0;

                    if ( this.isIE ) {
                        x += this._getScrollLeft();
                    }
                }

                return x;
            },

            /**
             * Returns the event's pageY
             * method getPageY
             * param {Event} ev the event
             * return {int} the event's pageY
             * static
             */
            getPageY: function(ev) {
                var y = ev.pageY;
                if (!y && 0 !== y) {
                    y = ev.clientY || 0;

                    if ( this.isIE ) {
                        y += this._getScrollTop();
                    }
                }


                return y;
            },

            /**
             * Returns the pageX and pageY properties as an indexed array.
             * method getXY
             * param {Event} ev the event
             * return {[x, y]} the pageX and pageY properties of the event
             * static
             */
            getXY: function(ev) {
                return [this.getPageX(ev), this.getPageY(ev)];
            },

            /**
             * Returns the event's related target 
             * method getRelatedTarget
             * param {Event} ev the event
             * return {HTMLElement} the event's relatedTarget
             * static
             */
            getRelatedTarget: function(ev) {
                var t = ev.relatedTarget;
                if (!t) {
                    if (ev.type == "mouseout") {
                        t = ev.toElement;
                    } else if (ev.type == "mouseover") {
                        t = ev.fromElement;
                    }
                }

                return this.resolveTextNode(t);
            },

            /**
             * Returns the time of the event.  If the time is not included, the
             * event is modified using the current time.
             * method getTime
             * param {Event} ev the event
             * return {Date} the time of the event
             * static
             */
            getTime: function(ev) {
                if (!ev.time) {
                    var t = new Date().getTime();
                    try {
                        ev.time = t;
                    } catch(ex) { 
                        this.lastError = ex;
                        return t;
                    }
                }

                return ev.time;
            },

            /**
             * Convenience method for stopPropagation + preventDefault
             * method stopEvent
             * param {Event} ev the event
             * static
             */
            stopEvent: function(ev) {
                this.stopPropagation(ev);
                this.preventDefault(ev);
            },

            /**
             * Stops event propagation
             * method stopPropagation
             * param {Event} ev the event
             * static
             */
            stopPropagation: function(ev) {
                if (ev.stopPropagation) {
                    ev.stopPropagation();
                } else {
                    ev.cancelBubble = true;
                }
            },

            /**
             * Prevents the default behavior of the event
             * method preventDefault
             * param {Event} ev the event
             * static
             */
            preventDefault: function(ev) {
                if (ev.preventDefault) {
                    ev.preventDefault();
                } else {
                    ev.returnValue = false;
                }
            },
             
            /**
             * Finds the event in the window object, the caller's arguments, or
             * in the arguments of another method in the callstack.  This is
             * executed automatically for events registered through the event
             * manager, so the implementer should not normally need to execute
             * this function at all.
             * method getEvent
             * param {Event} e the event parameter from the handler
             * param {HTMLElement} boundEl the element the listener is attached to
             * return {Event} the event 
             * static
             */
            getEvent: function(e, boundEl) {
                var ev = e || window.event;

                if (!ev) {
                    var c = this.getEvent.caller;
                    while (c) {
                        ev = c.arguments[0];
                        if (ev && Event == ev.constructor) {
                            break;
                        }
                        c = c.caller;
                    }
                }

                return ev;
            },

            /**
             * Returns the charcode for an event
             * method getCharCode
             * param {Event} ev the event
             * return {int} the event's charCode
             * static
             */
            getCharCode: function(ev) {
                var code = ev.keyCode || ev.charCode || 0;

                // webkit key normalization
                if (YAHOO.env.ua.webkit && (code in webkitKeymap)) {
                    code = webkitKeymap[code];
                }
                return code;
            },

            /**
             * Locating the saved event handler data by function ref
             *
             * method _getCacheIndex
             * static
             * private
             */
            _getCacheIndex: function(el, sType, fn) {
                for (var i=0, l=listeners.length; i<l; i=i+1) {
                    var li = listeners[i];
                    if ( li                 && 
                         li[this.FN] == fn  && 
                         li[this.EL] == el  && 
                         li[this.TYPE] == sType ) {
                        return i;
                    }
                }

                return -1;
            },

            /**
             * Generates an unique ID for the element if it does not already 
             * have one.
             * method generateId
             * param el the element to create the id for
             * return {string} the resulting id of the element
             * static
             */
            generateId: function(el) {
                var id = el.id;

                if (!id) {
                    id = "yuievtautoid-" + counter;
                    ++counter;
                    el.id = id;
                }

                return id;
            },


            /**
             * We want to be able to use getElementsByTagName as a collection
             * to attach a group of events to.  Unfortunately, different 
             * browsers return different types of collections.  This function
             * tests to determine if the object is array-like.  It will also 
             * fail if the object is an array, but is empty.
             * method _isValidCollection
             * param o the object to test
             * return {boolean} true if the object is array-like and populated
             * static
             * private
             */
            _isValidCollection: function(o) {
                try {
                    return ( o                     && // o is something
                             typeof o !== "string" && // o is not a string
                             o.length              && // o is indexed
                             !o.tagName            && // o is not an HTML element
                             !o.alert              && // o is not a window
                             typeof o[0] !== "undefined" );
                } catch(ex) {
                    return false;
                }

            },

            /**
             * private
             * property elCache
             * DOM element cache
             * static
             * deprecated Elements are not cached due to issues that arise when
             * elements are removed and re-added
             */
            elCache: {},

            /**
             * We cache elements bound by id because when the unload event 
             * fires, we can no longer use document.getElementById
             * method getEl
             * static
             * private
             * deprecated Elements are not cached any longer
             */
            getEl: function(id) {
                return (typeof id === "string") ? document.getElementById(id) : id;
            },

            /**
             * Clears the element cache
             * deprecated Elements are not cached any longer
             * method clearCache
             * static
             * private
             */
            clearCache: function() { },

            /**
             * Custom event the fires when the dom is initially usable
             * event DOMReadyEvent
             */
            DOMReadyEvent: new YAHOO.util.CustomEvent("DOMReady", this),

            /**
             * hook up any deferred listeners
             * method _load
             * static
             * private
             */
            _load: function(e) {

                if (!loadComplete) {
                    loadComplete = true;
                    var EU = YAHOO.util.Event;

                    // Just in case DOMReady did not go off for some reason
                    EU._ready();

                    // Available elements may not have been detected before the
                    // window load event fires. Try to find them now so that the
                    // the user is more likely to get the onAvailable notifications
                    // before the window load notification
                    EU._tryPreloadAttach();

                }
            },

            /**
             * Fires the DOMReady event listeners the first time the document is
             * usable.
             * method _ready
             * static
             * private
             */
            _ready: function(e) {
                var EU = YAHOO.util.Event;
                if (!EU.DOMReady) {
                    EU.DOMReady=true;

                    // Fire the content ready custom event
                    EU.DOMReadyEvent.fire();

                    // Remove the DOMContentLoaded (FF/Opera)
                    EU._simpleRemove(document, "DOMContentLoaded", EU._ready);
                }
            },

            /**
             * Polling function that runs before the onload event fires, 
             * attempting to attach to DOM Nodes as soon as they are 
             * available
             * method _tryPreloadAttach
             * static
             * private
             */
            _tryPreloadAttach: function() {

                if (onAvailStack.length === 0) {
                    retryCount = 0;
                    clearInterval(this._interval);
                    this._interval = null;
                    return;
                }

                if (this.locked) {
                    return;
                }

                if (this.isIE) {
                    // Hold off if DOMReady has not fired and check current
                    // readyState to protect against the IE operation aborted
                    // issue.
                    if (!this.DOMReady) {
                        this.startInterval();
                        return;
                    }
                }

                this.locked = true;


                // keep trying until after the page is loaded.  We need to 
                // check the page load state prior to trying to bind the 
                // elements so that we can be certain all elements have been 
                // tested appropriately
                var tryAgain = !loadComplete;
                if (!tryAgain) {
                    tryAgain = (retryCount > 0 && onAvailStack.length > 0);
                }

                // onAvailable
                var notAvail = [];

                var executeItem = function (el, item) {
                    var scope = el;
                    if (item.override) {
                        if (item.override === true) {
                            scope = item.obj;
                        } else {
                            scope = item.override;
                        }
                    }
                    item.fn.call(scope, item.obj);
                };

                var i, len, item, el, ready=[];

                // onAvailable onContentReady
                for (i=0, len=onAvailStack.length; i<len; i=i+1) {
                    item = onAvailStack[i];
                    if (item) {
                        el = this.getEl(item.id);
                        if (el) {
                            if (item.checkReady) {
                                if (loadComplete || el.nextSibling || !tryAgain) {
                                    ready.push(item);
                                    onAvailStack[i] = null;
                                }
                            } else {
                                executeItem(el, item);
                                onAvailStack[i] = null;
                            }
                        } else {
                            notAvail.push(item);
                        }
                    }
                }
                
                // make sure onContentReady fires after onAvailable
                for (i=0, len=ready.length; i<len; i=i+1) {
                    item = ready[i];
                    executeItem(this.getEl(item.id), item);
                }


                retryCount--;

                if (tryAgain) {
                    for (i=onAvailStack.length-1; i>-1; i--) {
                        item = onAvailStack[i];
                        if (!item || !item.id) {
                            onAvailStack.splice(i, 1);
                        }
                    }

                    this.startInterval();
                } else {
                    clearInterval(this._interval);
                    this._interval = null;
                }

                this.locked = false;

            },

            /**
             * Removes all listeners attached to the given element via addListener.
             * Optionally, the node's children can also be purged.
             * Optionally, you can specify a specific type of event to remove.
             * method purgeElement
             * param {HTMLElement} el the element to purge
             * param {boolean} recurse recursively purge this element's children
             * as well.  Use with caution.
             * param {string} sType optional type of listener to purge. If
             * left out, all listeners will be removed
             * static
             */
            purgeElement: function(el, recurse, sType) {
                var oEl = (YAHOO.lang.isString(el)) ? this.getEl(el) : el;
                var elListeners = this.getListeners(oEl, sType), i, len;
                if (elListeners) {
                    for (i=elListeners.length-1; i>-1; i--) {
                        var l = elListeners[i];
                        this._removeListener(oEl, l.type, l.fn, l.capture);
                    }
                }

                if (recurse && oEl && oEl.childNodes) {
                    for (i=0,len=oEl.childNodes.length; i<len ; ++i) {
                        this.purgeElement(oEl.childNodes[i], recurse, sType);
                    }
                }
            },

            /**
             * Returns all listeners attached to the given element via addListener.
             * Optionally, you can specify a specific type of event to return.
             * method getListeners
             * param el {HTMLElement|string} the element or element id to inspect 
             * param sType {string} optional type of listener to return. If
             * left out, all listeners will be returned
             * return {Object} the listener. Contains the following fields:
             * &nbsp;&nbsp;type:   (string)   the type of event
             * &nbsp;&nbsp;fn:     (function) the callback supplied to addListener
             * &nbsp;&nbsp;obj:    (object)   the custom object supplied to addListener
             * &nbsp;&nbsp;adjust: (boolean|object)  whether or not to adjust the default scope
             * &nbsp;&nbsp;scope: (boolean)  the derived scope based on the adjust parameter
             * &nbsp;&nbsp;scope: (capture)  the capture parameter supplied to addListener
             * &nbsp;&nbsp;index:  (int)      its position in the Event util listener cache
             * static
             */           
            getListeners: function(el, sType) {
                var results=[], searchLists;
                if (!sType) {
                    searchLists = [listeners, unloadListeners];
                } else if (sType === "unload") {
                    searchLists = [unloadListeners];
                } else {
                    searchLists = [listeners];
                }

                var oEl = (YAHOO.lang.isString(el)) ? this.getEl(el) : el;

                for (var j=0;j<searchLists.length; j=j+1) {
                    var searchList = searchLists[j];
                    if (searchList) {
                        for (var i=0,len=searchList.length; i<len ; ++i) {
                            var l = searchList[i];
                            if ( l  && l[this.EL] === oEl && 
                                    (!sType || sType === l[this.TYPE]) ) {
                                results.push({
                                    type:   l[this.TYPE],
                                    fn:     l[this.FN],
                                    obj:    l[this.OBJ],
                                    adjust: l[this.OVERRIDE],
                                    scope:  l[this.ADJ_SCOPE],
                                    capture:  l[this.CAPTURE],                                    
                                    index:  i
                                });
                            }
                        }
                    }
                }

                return (results.length) ? results : null;
            },

            /**
             * Removes all listeners registered by pe.event.  Called 
             * automatically during the unload event.
             * method _unload
             * static
             * private
             */
            _unload: function(e) {

                var EU = YAHOO.util.Event, i, j, l, len, index,
                         ul = unloadListeners.slice();

                // execute and clear stored unload listeners
                for (i=0,len=unloadListeners.length; i<len; ++i) {
                    l = ul[i];
                    if (l) {
                        var scope = window;
                        if (l[EU.ADJ_SCOPE]) {
                            if (l[EU.ADJ_SCOPE] === true) {
                                scope = l[EU.UNLOAD_OBJ];
                            } else {
                                scope = l[EU.ADJ_SCOPE];
                            }
                        }
                        l[EU.FN].call(scope, EU.getEvent(e, l[EU.EL]), l[EU.UNLOAD_OBJ] );
                        ul[i] = null;
                        l=null;
                        scope=null;
                    }
                }

                unloadListeners = null;

                // Remove listeners to handle IE memory leaks
                //if (YAHOO.env.ua.ie && listeners && listeners.length > 0) {
                
                // 2.5.0 listeners are removed for all browsers again.  FireFox preserves
                // at least some listeners between page refreshes, potentially causing
                // errors during page load (mouseover listeners firing before they
                // should if the user moves the mouse at the correct moment).
                if (listeners) {
                    for (j=listeners.length-1; j>-1; j--) {
                        l = listeners[j];
                        if (l) {
                            EU._removeListener(l[EU.EL], l[EU.TYPE], l[EU.FN], l[EU.CAPTURE], j);
                        } 
                    }
                    l=null;
                }

                legacyEvents = null;

                EU._simpleRemove(window, "unload", EU._unload);

            },

            /**
             * Returns scrollLeft
             * method _getScrollLeft
             * static
             * private
             */
            _getScrollLeft: function() {
                return this._getScroll()[1];
            },

            /**
             * Returns scrollTop
             * method _getScrollTop
             * static
             * private
             */
            _getScrollTop: function() {
                return this._getScroll()[0];
            },

            /**
             * Returns the scrollTop and scrollLeft.  Used to calculate the 
             * pageX and pageY in Internet Explorer
             * method _getScroll
             * static
             * private
             */
            _getScroll: function() {
                var dd = document.documentElement, db = document.body;
                if (dd && (dd.scrollTop || dd.scrollLeft)) {
                    return [dd.scrollTop, dd.scrollLeft];
                } else if (db) {
                    return [db.scrollTop, db.scrollLeft];
                } else {
                    return [0, 0];
                }
            },
            
            /**
             * Used by old versions of CustomEvent, restored for backwards
             * compatibility
             * method regCE
             * private
             * static
             * deprecated still here for backwards compatibility
             */
            regCE: function() {
                // does nothing
            },

            /**
             * Adds a DOM event directly without the caching, cleanup, scope adj, etc
             *
             * method _simpleAdd
             * param {HTMLElement} el      the element to bind the handler to
             * param {string}      sType   the type of event handler
             * param {function}    fn      the callback to invoke
             * param {boolen}      capture capture or bubble phase
             * static
             * private
             */
            _simpleAdd: function () {
                if (window.addEventListener) {
                    return function(el, sType, fn, capture) {
                        el.addEventListener(sType, fn, (capture));
                    };
                } else if (window.attachEvent) {
                    return function(el, sType, fn, capture) {
                        el.attachEvent("on" + sType, fn);
                    };
                } else {
                    return function(){};
                }
            }(),

            /**
             * Basic remove listener
             *
             * method _simpleRemove
             * param {HTMLElement} el      the element to bind the handler to
             * param {string}      sType   the type of event handler
             * param {function}    fn      the callback to invoke
             * param {boolen}      capture capture or bubble phase
             * static
             * private
             */
            _simpleRemove: function() {
                if (window.removeEventListener) {
                    return function (el, sType, fn, capture) {
                        el.removeEventListener(sType, fn, (capture));
                    };
                } else if (window.detachEvent) {
                    return function (el, sType, fn) {
                        el.detachEvent("on" + sType, fn);
                    };
                } else {
                    return function(){};
                }
            }()
        };

    }();

    (function() {
        var EU = YAHOO.util.Event;

        /**
         * YAHOO.util.Event.on is an alias for addListener
         * method on
         * see addListener
         * static
         */
        EU.on = EU.addListener;

        /**
         * YAHOO.util.Event.onFocus is an alias for addFocusListener
         * method on
         * see addFocusListener
         * static
         */
        EU.onFocus = EU.addFocusListener;

        /**
         * YAHOO.util.Event.onBlur is an alias for addBlurListener
         * method onBlur
         * see addBlurListener
         * static
         */     
        EU.onBlur = EU.addBlurListener;


/*! DOMReady: based on work by: Dean Edwards/John Resig/Matthias Miller */

        // Internet Explorer: use the readyState of a defered script.
        // This isolates what appears to be a safe moment to manipulate
        // the DOM prior to when the document's readyState suggests
        // it is safe to do so.
        if (EU.isIE) {

            // Process onAvailable/onContentReady items when the 
            // DOM is ready.
            YAHOO.util.Event.onDOMReady(
                    YAHOO.util.Event._tryPreloadAttach,
                    YAHOO.util.Event, true);
            
            var n = document.createElement('p');  

            EU._dri = setInterval(function() {
                try {
                    // throws an error if doc is not ready
                    n.doScroll('left');
                    clearInterval(EU._dri);
                    EU._dri = null;
                    EU._ready();
                    n = null;
                } catch (ex) { 
                }
            }, EU.POLL_INTERVAL); 

        
        // The document's readyState in Safari currently will
        // change to loaded/complete before images are loaded.
        } else if (EU.webkit && EU.webkit < 525) {

            EU._dri = setInterval(function() {
                var rs=document.readyState;
                if ("loaded" == rs || "complete" == rs) {
                    clearInterval(EU._dri);
                    EU._dri = null;
                    EU._ready();
                }
            }, EU.POLL_INTERVAL); 

        // FireFox and Opera: These browsers provide a event for this
        // moment.  The latest WebKit releases now support this event.
        } else {

            EU._simpleAdd(document, "DOMContentLoaded", EU._ready);

        }
        /////////////////////////////////////////////////////////////


        EU._simpleAdd(window, "load", EU._load);
        EU._simpleAdd(window, "unload", EU._unload);
        EU._tryPreloadAttach();
    })();

}
/**
 * EventProvider is designed to be used with YAHOO.augment to wrap 
 * CustomEvents in an interface that allows events to be subscribed to 
 * and fired by name.  This makes it possible for implementing code to
 * subscribe to an event that either has not been created yet, or will
 * not be created at all.
 *
 * Class EventProvider
 */
YAHOO.util.EventProvider = function() { };

YAHOO.util.EventProvider.prototype = {

    /**
     * Private storage of custom events
     * property __yui_events
     * type Object[]
     * private
     */
    __yui_events: null,

    /**
     * Private storage of custom event subscribers
     * property __yui_subscribers
     * type Object[]
     * private
     */
    __yui_subscribers: null,
    
    /**
     * Subscribe to a CustomEvent by event type
     *
     * method subscribe
     * param p_type     {string}   the type, or name of the event
     * param p_fn       {function} the function to exectute when the event fires
     * param p_obj      {Object}   An object to be passed along when the event 
     *                              fires
     * param p_override {boolean}  If true, the obj passed in becomes the 
     *                              execution scope of the listener
     */
    subscribe: function(p_type, p_fn, p_obj, p_override) {

        this.__yui_events = this.__yui_events || {};
        var ce = this.__yui_events[p_type];

        if (ce) {
            ce.subscribe(p_fn, p_obj, p_override);
        } else {
            this.__yui_subscribers = this.__yui_subscribers || {};
            var subs = this.__yui_subscribers;
            if (!subs[p_type]) {
                subs[p_type] = [];
            }
            subs[p_type].push(
                { fn: p_fn, obj: p_obj, override: p_override } );
        }
    },

    /**
     * Unsubscribes one or more listeners the from the specified event
     * method unsubscribe
     * param p_type {string}   The type, or name of the event.  If the type
     *                          is not specified, it will attempt to remove
     *                          the listener from all hosted events.
     * param p_fn   {Function} The subscribed function to unsubscribe, if not
     *                          supplied, all subscribers will be removed.
     * param p_obj  {Object}   The custom object passed to subscribe.  This is
     *                        optional, but if supplied will be used to
     *                        disambiguate multiple listeners that are the same
     *                        (e.g., you subscribe many object using a function
     *                        that lives on the prototype)
     * return {boolean} true if the subscriber was found and detached.
     */
    unsubscribe: function(p_type, p_fn, p_obj) {
        this.__yui_events = this.__yui_events || {};
        var evts = this.__yui_events;
        if (p_type) {
            var ce = evts[p_type];
            if (ce) {
                return ce.unsubscribe(p_fn, p_obj);
            }
        } else {
            var ret = true;
            for (var i in evts) {
                if (YAHOO.lang.hasOwnProperty(evts, i)) {
                    ret = ret && evts[i].unsubscribe(p_fn, p_obj);
                }
            }
            return ret;
        }

        return false;
    },
    
    /**
     * Removes all listeners from the specified event.  If the event type
     * is not specified, all listeners from all hosted custom events will
     * be removed.
     * method unsubscribeAll
     * param p_type {string}   The type, or name of the event
     */
    unsubscribeAll: function(p_type) {
        return this.unsubscribe(p_type);
    },

    /**
     * Creates a new custom event of the specified type.  If a custom event
     * by that name already exists, it will not be re-created.  In either
     * case the custom event is returned. 
     *
     * method createEvent
     *
     * param p_type {string} the type, or name of the event
     * param p_config {object} optional config params.  Valid properties are:
     *
     *  <ul>
     *    <li>
     *      scope: defines the default execution scope.  If not defined
     *      the default scope will be this instance.
     *    </li>
     *    <li>
     *      silent: if true, the custom event will not generate log messages.
     *      This is false by default.
     *    </li>
     *    <li>
     *      onSubscribeCallback: specifies a callback to execute when the
     *      event has a new subscriber.  This will fire immediately for
     *      each queued subscriber if any exist prior to the creation of
     *      the event.
     *    </li>
     *  </ul>
     *
     *  @return {CustomEvent} the custom event
     *
     */
    createEvent: function(p_type, p_config) {

        this.__yui_events = this.__yui_events || {};
        var opts = p_config || {};
        var events = this.__yui_events;

        if (events[p_type]) {
        } else {

            var scope  = opts.scope  || this;
            var silent = (opts.silent);

            var ce = new YAHOO.util.CustomEvent(p_type, scope, silent,
                    YAHOO.util.CustomEvent.FLAT);
            events[p_type] = ce;

            if (opts.onSubscribeCallback) {
                ce.subscribeEvent.subscribe(opts.onSubscribeCallback);
            }

            this.__yui_subscribers = this.__yui_subscribers || {};
            var qs = this.__yui_subscribers[p_type];

            if (qs) {
                for (var i=0; i<qs.length; ++i) {
                    ce.subscribe(qs[i].fn, qs[i].obj, qs[i].override);
                }
            }
        }

        return events[p_type];
    },


   /**
     * Fire a custom event by name.  The callback functions will be executed
     * from the scope specified when the event was created, and with the 
     * following parameters:
     *   <ul>
     *   <li>The first argument fire() was executed with</li>
     *   <li>The custom object (if any) that was passed into the subscribe() 
     *       method</li>
     *   </ul>
     * method fireEvent
     * param p_type    {string}  the type, or name of the event
     * param arguments {Object*} an arbitrary set of parameters to pass to 
     *                            the handler.
     * return {boolean} the return value from CustomEvent.fire
     *                   
     */
    fireEvent: function(p_type, arg1, arg2, etc) {

        this.__yui_events = this.__yui_events || {};
        var ce = this.__yui_events[p_type];

        if (!ce) {
            return null;
        }

        var args = [];
        for (var i=1; i<arguments.length; ++i) {
            args.push(arguments[i]);
        }
        return ce.fire.apply(ce, args);
    },

    /**
     * Returns true if the custom event of the provided type has been created
     * with createEvent.
     * method hasEvent
     * param type {string} the type, or name of the event
     */
    hasEvent: function(type) {
        if (this.__yui_events) {
            if (this.__yui_events[type]) {
                return true;
            }
        }
        return false;
    }

};

//@TODO optimize
//@TODO use event utility, lang abstractions
//@TODO replace

/**
* KeyListener is a utility that provides an easy interface for listening for
* keydown/keyup events fired against DOM elements.
* namespace YAHOO.util
* class KeyListener
* @constructor
* param {HTMLElement} attachTo The element or element ID to which the key 
*                               event should be attached
* param {String}      attachTo The element or element ID to which the key
*                               event should be attached
* param {Object}      keyData  The object literal representing the key(s) 
*                               to detect. Possible attributes are 
*                               shift(boolean), alt(boolean), ctrl(boolean) 
*                               and keys(either an int or an array of ints 
*                               representing keycodes).
* param {Function}    handler  The CustomEvent handler to fire when the 
*                               key event is detected
* param {Object}      handler  An object literal representing the handler. 
* param {String}      event    Optional. The event (keydown or keyup) to 
*                               listen for. Defaults automatically to keydown.
*
* knownissue the "keypress" event is completely broken in Safari 2.x and below.
*             the workaround is use "keydown" for key listening.  However, if
*             it is desired to prevent the default behavior of the keystroke,
*             that can only be done on the keypress event.  This makes key
*             handling quite ugly.
* knownissue keydown is also broken in Safari 2.x and below for the ESC key.
*             There currently is no workaround other than choosing another
*             key to listen for.
*/
YAHOO.util.KeyListener = function(attachTo, keyData, handler, event) {    
    if (!event) {
        event = YAHOO.util.KeyListener.KEYDOWN;
    }

    /**
    * The CustomEvent fired internally when a key is pressed
    * event keyEvent
    * private
    * param {Object} keyData The object literal representing the key(s) to 
    *                         detect. Possible attributes are shift(boolean), 
    *                         alt(boolean), ctrl(boolean) and keys(either an 
    *                         int or an array of ints representing keycodes).
    */
    var keyEvent = new YAHOO.util.CustomEvent("keyPressed");
    
    /**
    * The CustomEvent fired when the KeyListener is enabled via the enable() 
    * function
    * event enabledEvent
    * param {Object} keyData The object literal representing the key(s) to 
    *                         detect. Possible attributes are shift(boolean), 
    *                         alt(boolean), ctrl(boolean) and keys(either an 
    *                         int or an array of ints representing keycodes).
    */
    this.enabledEvent = new YAHOO.util.CustomEvent("enabled");

    /**
    * The CustomEvent fired when the KeyListener is disabled via the 
    * disable() function
    * event disabledEvent
    * param {Object} keyData The object literal representing the key(s) to 
    *                         detect. Possible attributes are shift(boolean), 
    *                         alt(boolean), ctrl(boolean) and keys(either an 
    *                         int or an array of ints representing keycodes).
    */
    this.disabledEvent = new YAHOO.util.CustomEvent("disabled");

    if (typeof attachTo == 'string') {
        attachTo = document.getElementById(attachTo);
    }

    if (typeof handler == 'function') {
        keyEvent.subscribe(handler);
    } else {
        keyEvent.subscribe(handler.fn, handler.scope, handler.correctScope);
    }

    /**
    * Handles the key event when a key is pressed.
    * method handleKeyPress
    * param {DOMEvent} e   The keypress DOM event
    * param {Object}   obj The DOM event scope object
    * private
    */
    function handleKeyPress(e, obj) {
        if (! keyData.shift) {  
            keyData.shift = false; 
        }
        if (! keyData.alt) {    
            keyData.alt = false;
        }
        if (! keyData.ctrl) {
            keyData.ctrl = false;
        }

        // check held down modifying keys first
        if (e.shiftKey == keyData.shift && 
            e.altKey   == keyData.alt &&
            e.ctrlKey  == keyData.ctrl) { // if we pass this, all modifiers match
            
            var dataItem;

            if (keyData.keys instanceof Array) {
                for (var i=0;i<keyData.keys.length;i++) {
                    dataItem = keyData.keys[i];

                    if (dataItem == e.charCode ) {
                        keyEvent.fire(e.charCode, e);
                        break;
                    } else if (dataItem == e.keyCode) {
                        keyEvent.fire(e.keyCode, e);
                        break;
                    }
                }
            } else {
                dataItem = keyData.keys;
                if (dataItem == e.charCode ) {
                    keyEvent.fire(e.charCode, e);
                } else if (dataItem == e.keyCode) {
                    keyEvent.fire(e.keyCode, e);
                }
            }
        }
    }

    /**
    * Enables the KeyListener by attaching the DOM event listeners to the 
    * target DOM element
    * method enable
    */
    this.enable = function() {
        if (! this.enabled) {
            YAHOO.util.Event.addListener(attachTo, event, handleKeyPress);
            this.enabledEvent.fire(keyData);
        }
        /**
        * Boolean indicating the enabled/disabled state of the Tooltip
        * property enabled
        * type Boolean
        */
        this.enabled = true;
    };

    /**
    * Disables the KeyListener by removing the DOM event listeners from the 
    * target DOM element
    * method disable
    */
    this.disable = function() {
        if (this.enabled) {
            YAHOO.util.Event.removeListener(attachTo, event, handleKeyPress);
            this.disabledEvent.fire(keyData);
        }
        this.enabled = false;
    };

    /**
    * Returns a String representation of the object.
    * method toString
    * return {String}  The string representation of the KeyListener
    */ 
    this.toString = function() {
        return "KeyListener [" + keyData.keys + "] " + attachTo.tagName + 
                (attachTo.id ? "[" + attachTo.id + "]" : "");
    };

};

/**
 * Constant representing the DOM "keydown" event.
 * property YAHOO.util.KeyListener.KEYDOWN
 * static
 * final
 * type String
 */
YAHOO.util.KeyListener.KEYDOWN = "keydown";

/**
 * Constant representing the DOM "keyup" event.
 * property YAHOO.util.KeyListener.KEYUP
 * static
 * final
 * type String
 */
YAHOO.util.KeyListener.KEYUP = "keyup";

/*
Copyright (c) 2008, Yahoo! Inc. All rights reserved.
Code licensed under the BSD License:
http://developer.yahoo.net/yui/license.txt
version: 2.6.0
*/
/**
 * The YAHOO object is the single global object used by YUI Library.  It
 * contains utility function for setting up namespaces, inheritance, and
 * logging.  YAHOO.util, YAHOO.widget, and YAHOO.example are namespaces
 * created automatically for and used by the library.
 * module yahoo
 * title  YAHOO Global
 */

/**
 * YAHOO_config is not included as part of the library.  Instead it is an 
 * object that can be defined by the implementer immediately before 
 * including the YUI library.  The properties included in this object
 * will be used to configure global properties needed as soon as the 
 * library begins to load.
 * class YAHOO_config
 * static
 */

/**
 * A reference to a function that will be executed every time a YAHOO module
 * is loaded.  As parameter, this function will receive the version
 * information for the module. See <a href="YAHOO.env.html#getVersion">
 * YAHOO.env.getVersion</a> for the description of the version data structure.
 * property listener
 * type Function
 * static
 * default undefined
 */

/**
 * Set to true if the library will be dynamically loaded after window.onload.
 * Defaults to false 
 * property injecting
 * type boolean
 * static
 * default undefined
 */

/**
 * Instructs the yuiloader component to dynamically load yui components and
 * their dependencies.  See the yuiloader documentation for more information
 * about dynamic loading
 * property load
 * static
 * default undefined
 * see yuiloader
 */

/**
 * Forces the use of the supplied locale where applicable in the library
 * property locale
 * type string
 * static
 * default undefined
 */



/**
 * Returns the namespace specified and creates it if it doesn't exist
 * <pre>
 * YAHOO.namespace("property.package");
 * YAHOO.namespace("YAHOO.property.package");
 * </pre>
 * Either of the above would create YAHOO.property, then
 * YAHOO.property.package
 *
 * Be careful when naming packages. Reserved words may work in some browsers
 * and not others. For instance, the following will fail in Safari:
 * <pre>
 * YAHOO.namespace("really.long.nested.namespace");
 * </pre>
 * This fails because "long" is a future reserved word in ECMAScript
 *
 * method namespace
 * static
 * param  {String*} arguments 1-n namespaces to create 
 * return {Object}  A reference to the last namespace object created
 */
YAHOO.namespace = function() {
    var a=arguments, o=null, i, j, d;
    for (i=0; i<a.length; i=i+1) {
        d=a[i].split(".");
        o=YAHOO;

        // YAHOO is implied, so it is ignored if it is included
        for (j=(d[0] == "YAHOO") ? 1 : 0; j<d.length; j=j+1) {
            o[d[j]]=o[d[j]] || {};
            o=o[d[j]];
        }
    }

    return o;
};

/**
 * Uses YAHOO.widget.Logger to output a log message, if the widget is
 * available.
 *
 * @method log
 * @static
 * @param  {String}  msg  The message to log.
 * @param  {String}  cat  The log category for the message.  Default
 *                        categories are "info", "warn", "error", time".
 *                        Custom categories can be used as well. (opt)
 * @param  {String}  src  The source of the the message (opt)
 * @return {Boolean}      True if the log operation was successful.
 */
YAHOO.log = function(msg, cat, src) {
    //~ var l=YAHOO.widget.Logger;
    //~ if(l && l.log) {
        //~ return l.log(msg, cat, src);
    //~ } else {
        return false;
    //~ }
};

/**
 * Registers a module with the YAHOO object
 * method register
 * static
 * param {String}   name    the name of the module (event, slider, etc)
 * param {Function} mainClass a reference to class in the module.  This
 *                             class will be tagged with the version info
 *                             so that it will be possible to identify the
 *                             version that is in use when multiple versions
 *                             have loaded
 * param {Object}   data      metadata object for the module.  Currently it
 *                             is expected to contain a "version" property
 *                             and a "build" property at minimum.
 */
YAHOO.register = function(name, mainClass, data) {
    var mods = YAHOO.env.modules;
    if (!mods[name]) {
        mods[name] = { versions:[], builds:[] };
    }
    var m=mods[name],v=data.version,b=data.build,ls=YAHOO.env.listeners;
    m.name = name;
    m.version = v;
    m.build = b;
    m.versions.push(v);
    m.builds.push(b);
    m.mainClass = mainClass;
    // fire the module load listeners
    for (var i=0;i<ls.length;i=i+1) {
        ls[i](m);
    }
    // label the main class
    if (mainClass) {
        mainClass.VERSION = v;
        mainClass.BUILD = b;
    } else {
        YAHOO.log("mainClass is undefined for module " + name, "warn");
    }
};

/*
 * Initializes the global by creating the default namespaces and applying
 * any new configuration information that is detected.  This is the setup
 * for env.
 * method init
 * static
 * private
 */
(function() {
    YAHOO.namespace("util", "widget", "example");
    if ("undefined" !== typeof YAHOO_config) {
        var l=YAHOO_config.listener,ls=YAHOO.env.listeners,unique=true,i;
        if (l) {
            // if YAHOO is loaded multiple times we need to check to see if
            // this is a new config object.  If it is, add the new component
            // load listener to the stack
            for (i=0;i<ls.length;i=i+1) {
                if (ls[i]==l) {
                    unique=false;
                    break;
                }
            }
            if (unique) {
                ls.push(l);
            }
        }
    }
})();
/**
 * Provides the language utilites and extensions used by the library
 * class YAHOO.lang
 */
YAHOO.lang = YAHOO.lang || {};

(function() {

var L = YAHOO.lang,

    // ADD = ["toString", "valueOf", "hasOwnProperty"],
    ADD = ["toString", "valueOf"],

    OB = {

    /**
     * Determines whether or not the provided object is an array.
     * Testing typeof/instanceof/constructor of arrays across frame 
     * boundaries isn't possible in Safari unless you have a reference
     * to the other frame to test against its Array prototype.  To
     * handle this case, we test well-known array properties instead.
     * properties.
     * method isArray
     * param {any} o The object being testing
     * return {boolean} the result
     */
    isArray: function(o) { 
        if (o) {
           return L.isNumber(o.length) && L.isFunction(o.splice);
        }
        return false;
    },

    /**
     * Determines whether or not the provided object is a boolean
     * method isBoolean
     * param {any} o The object being testing
     * return {boolean} the result
     */
    isBoolean: function(o) {
        return typeof o === 'boolean';
    },
    
    /**
     * Determines whether or not the provided object is a function
     * method isFunction
     * param {any} o The object being testing
     * return {boolean} the result
     */
    isFunction: function(o) {
        return typeof o === 'function';
    },
        
    /**
     * Determines whether or not the provided object is null
     * method isNull
     * param {any} o The object being testing
     * return {boolean} the result
     */
    isNull: function(o) {
        return o === null;
    },
        
    /**
     * Determines whether or not the provided object is a legal number
     * method isNumber
     * param {any} o The object being testing
     * return {boolean} the result
     */
    isNumber: function(o) {
        return typeof o === 'number' && isFinite(o);
    },
      
    /**
     * Determines whether or not the provided object is of type object
     * or function
     * method isObject
     * param {any} o The object being testing
     * return {boolean} the result
     */  
    isObject: function(o) {
return (o && (typeof o === 'object' || L.isFunction(o))) || false;
    },
        
    /**
     * Determines whether or not the provided object is a string
     * method isString
     * param {any} o The object being testing
     * return {boolean} the result
     */
    isString: function(o) {
        return typeof o === 'string';
    },
        
    /**
     * Determines whether or not the provided object is undefined
     * method isUndefined
     * param {any} o The object being testing
     * return {boolean} the result
     */
    isUndefined: function(o) {
        return typeof o === 'undefined';
    },
    
 
    /**
     * IE will not enumerate native functions in a derived object even if the
     * function was overridden.  This is a workaround for specific functions 
     * we care about on the Object prototype. 
     * property _IEEnumFix
     * param {Function} r  the object to receive the augmentation
     * param {Function} s  the object that supplies the properties to augment
     * static
     * private
     */
    _IEEnumFix: (YAHOO.env.ua.ie) ? function(r, s) {
            for (var i=0;i<ADD.length;i=i+1) {
                var fname=ADD[i],f=s[fname];
                if (L.isFunction(f) && f!=Object.prototype[fname]) {
                    r[fname]=f;
                }
            }
    } : function(){},
       
    /**
     * Utility to set up the prototype, constructor and superclass properties to
     * support an inheritance strategy that can chain constructors and methods.
     * Static members will not be inherited.
     *
     * method extend
     * static
     * param {Function} subc   the object to modify
     * param {Function} superc the object to inherit
     * param {Object} overrides  additional properties/methods to add to the
     *                              subclass prototype.  These will override the
     *                              matching items obtained from the superclass 
     *                              if present.
     */
    extend: function(subc, superc, overrides) {
        if (!superc||!subc) {
            throw new Error("extend failed, please check that " +
                            "all dependencies are included.");
        }
        /**
         * @constructor
         */
        var F = function() {};
        F.prototype=superc.prototype;
        subc.prototype=new F();
        subc.prototype.constructor=subc;
        subc.superclass=superc.prototype;
        if (superc.prototype.constructor == Object.prototype.constructor) {
            superc.prototype.constructor=superc;
        }
    
        if (overrides) {
            for (var i in overrides) {
                if (L.hasOwnProperty(overrides, i)) {
                    subc.prototype[i]=overrides[i];
                }
            }

            L._IEEnumFix(subc.prototype, overrides);
        }
    },
   
    /**
     * Applies all properties in the supplier to the receiver if the
     * receiver does not have these properties yet.  Optionally, one or 
     * more methods/properties can be specified (as additional 
     * parameters).  This option will overwrite the property if receiver 
     * has it already.  If true is passed as the third parameter, all 
     * properties will be applied and _will_ overwrite properties in 
     * the receiver.
     *
     * method augmentObject
     * static
     * since 2.3.0
     * param {Function} r  the object to receive the augmentation
     * param {Function} s  the object that supplies the properties to augment
     * param {String*|boolean}  arguments zero or more properties methods 
     *        to augment the receiver with.  If none specified, everything
     *        in the supplier will be used unless it would
     *        overwrite an existing property in the receiver. If true
     *        is specified as the third parameter, all properties will
     *        be applied and will overwrite an existing property in
     *        the receiver
     */
    augmentObject: function(r, s) {
        if (!s||!r) {
            throw new Error("Absorb failed, verify dependencies.");
        }
        var a=arguments, i, p, override=a[2];
        if (override && override!==true) { // only absorb the specified properties
            for (i=2; i<a.length; i=i+1) {
                r[a[i]] = s[a[i]];
            }
        } else { // take everything, overwriting only if the third parameter is true
            for (p in s) { 
                if (override || !(p in r)) {
                    r[p] = s[p];
                }
            }
            
            L._IEEnumFix(r, s);
        }
    },
 
    /**
     * Same as YAHOO.lang.augmentObject, except it only applies prototype properties
     * see YAHOO.lang.augmentObject
     * method augmentProto
     * static
     * param {Function} r  the object to receive the augmentation
     * param {Function} s  the object that supplies the properties to augment
     * param {String*|boolean}  arguments zero or more properties methods 
     *        to augment the receiver with.  If none specified, everything 
     *        in the supplier will be used unless it would overwrite an existing 
     *        property in the receiver.  if true is specified as the third 
     *        parameter, all properties will be applied and will overwrite an 
     *        existing property in the receiver
     */
    augmentProto: function(r, s) {
        if (!s||!r) {
            throw new Error("Augment failed, verify dependencies.");
        }
        //var a=[].concat(arguments);
        var a=[r.prototype,s.prototype];
        for (var i=2;i<arguments.length;i=i+1) {
            a.push(arguments[i]);
        }
        L.augmentObject.apply(this, a);
    },

      
    /**
     * Returns a simple string representation of the object or array.
     * Other types of objects will be returned unprocessed.  Arrays
     * are expected to be indexed.  Use object notation for
     * associative arrays.
     * method dump
     * since 2.3.0
     * param o {Object} The object to dump
     * param d {int} How deep to recurse child objects, default 3
     * return {String} the dump result
     */
    dump: function(o, d) {
        var i,len,s=[],OBJ="{...}",FUN="f(){...}",
            COMMA=', ', ARROW=' => ';

        // Cast non-objects to string
        // Skip dates because the std toString is what we want
        // Skip HTMLElement-like objects because trying to dump 
        // an element will cause an unhandled exception in FF 2.x
        if (!L.isObject(o)) {
            return o + "";
        } else if (o instanceof Date || ("nodeType" in o && "tagName" in o)) {
            return o;
        } else if  (L.isFunction(o)) {
            return FUN;
        }

        // dig into child objects the depth specifed. Default 3
        d = (L.isNumber(d)) ? d : 3;

        // arrays [1, 2, 3]
        if (L.isArray(o)) {
            s.push("[");
            for (i=0,len=o.length;i<len;i=i+1) {
                if (L.isObject(o[i])) {
                    s.push((d > 0) ? L.dump(o[i], d-1) : OBJ);
                } else {
                    s.push(o[i]);
                }
                s.push(COMMA);
            }
            if (s.length > 1) {
                s.pop();
            }
            s.push("]");
        // objects {k1 => v1, k2 => v2}
        } else {
            s.push("{");
            for (i in o) {
                if (L.hasOwnProperty(o, i)) {
                    s.push(i + ARROW);
                    if (L.isObject(o[i])) {
                        s.push((d > 0) ? L.dump(o[i], d-1) : OBJ);
                    } else {
                        s.push(o[i]);
                    }
                    s.push(COMMA);
                }
            }
            if (s.length > 1) {
                s.pop();
            }
            s.push("}");
        }

        return s.join("");
    },

    /**
     * Does variable substitution on a string. It scans through the string 
     * looking for expressions enclosed in { } braces. If an expression 
     * is found, it is used a key on the object.  If there is a space in
     * the key, the first word is used for the key and the rest is provided
     * to an optional function to be used to programatically determine the
     * value (the extra information might be used for this decision). If 
     * the value for the key in the object, or what is returned from the
     * function has a string value, number value, or object value, it is 
     * substituted for the bracket expression and it repeats.  If this
     * value is an object, it uses the Object's toString() if this has
     * been overridden, otherwise it does a shallow dump of the key/value
     * pairs.
     * method substitute
     * since 2.3.0
     * param s {String} The string that will be modified.
     * param o {Object} An object containing the replacement values
     * param f {Function} An optional function that can be used to
     *                     process each match.  It receives the key,
     *                     value, and any extra metadata included with
     *                     the key inside of the braces.
     * return {String} the substituted string
     */
    substitute: function (s, o, f) {
        var i, j, k, key, v, meta, saved=[], token, 
            DUMP='dump', SPACE=' ', LBRACE='{', RBRACE='}';


        for (;;) {
            i = s.lastIndexOf(LBRACE);
            if (i < 0) {
                break;
            }
            j = s.indexOf(RBRACE, i);
            if (i + 1 >= j) {
                break;
            }

            //Extract key and meta info 
            token = s.substring(i + 1, j);
            key = token;
            meta = null;
            k = key.indexOf(SPACE);
            if (k > -1) {
                meta = key.substring(k + 1);
                key = key.substring(0, k);
            }

            // lookup the value
            v = o[key];

            // if a substitution function was provided, execute it
            if (f) {
                v = f(key, v, meta);
            }

            if (L.isObject(v)) {
                if (L.isArray(v)) {
                    v = L.dump(v, parseInt(meta, 10));
                } else {
                    meta = meta || "";

                    // look for the keyword 'dump', if found force obj dump
                    var dump = meta.indexOf(DUMP);
                    if (dump > -1) {
                        meta = meta.substring(4);
                    }

                    // use the toString if it is not the Object toString 
                    // and the 'dump' meta info was not found
                    if (v.toString===Object.prototype.toString||dump>-1) {
                        v = L.dump(v, parseInt(meta, 10));
                    } else {
                        v = v.toString();
                    }
                }
            } else if (!L.isString(v) && !L.isNumber(v)) {
                // This {block} has no replace string. Save it for later.
                v = "~-" + saved.length + "-~";
                saved[saved.length] = token;

                // break;
            }

            s = s.substring(0, i) + v + s.substring(j + 1);


        }

        // restore saved {block}s
        for (i=saved.length-1; i>=0; i=i-1) {
            s = s.replace(new RegExp("~-" + i + "-~"), "{"  + saved[i] + "}", "g");
        }

        return s;
    },


    /**
     * Returns a string without any leading or trailing whitespace.  If 
     * the input is not a string, the input will be returned untouched.
     * method trim
     * since 2.3.0
     * param s {string} the string to trim
     * return {string} the trimmed string
     */
    trim: function(s){
        try {
            return s.replace(/^\s+|\s+$/g, "");
        } catch(e) {
            return s;
        }
    },

    /**
     * Returns a new object containing all of the properties of
     * all the supplied objects.  The properties from later objects
     * will overwrite those in earlier objects.
     * method merge
     * since 2.3.0
     * param arguments {Object*} the objects to merge
     * return the new merged object
     */
    merge: function() {
        var o={}, a=arguments;
        for (var i=0, l=a.length; i<l; i=i+1) {
            L.augmentObject(o, a[i], true);
        }
        return o;
    },

    /**
     * Executes the supplied function in the context of the supplied 
     * object 'when' milliseconds later.  Executes the function a 
     * single time unless periodic is set to true.
     * method later
     * since 2.4.0
     * param when {int} the number of milliseconds to wait until the fn 
     * is executed
     * param o the context object
     * param fn {Function|String} the function to execute or the name of 
     * the method in the 'o' object to execute
     * param data [Array] data that is provided to the function.  This accepts
     * either a single item or an array.  If an array is provided, the
     * function is executed with one parameter for each array item.  If
     * you need to pass a single array parameter, it needs to be wrapped in
     * an array [myarray]
     * param periodic {boolean} if true, executes continuously at supplied 
     * interval until canceled
     * return a timer object. Call the cancel() method on this object to 
     * stop the timer.
     */
    later: function(when, o, fn, data, periodic) {
        when = when || 0; 
        o = o || {};
        var m=fn, d=data, f, r;

        if (L.isString(fn)) {
            m = o[fn];
        }

        if (!m) {
            throw new TypeError("method undefined");
        }

        if (!L.isArray(d)) {
            d = [data];
        }

        f = function() {
            m.apply(o, d);
        };

        r = (periodic) ? setInterval(f, when) : setTimeout(f, when);

        return {
            interval: periodic,
            cancel: function() {
                if (this.interval) {
                    clearInterval(r);
                } else {
                    clearTimeout(r);
                }
            }
        };
    },
    
    /**
     * A convenience method for detecting a legitimate non-null value.
     * Returns false for null/undefined/NaN, true for other values, 
     * including 0/false/''
     * method isValue
     * since 2.3.0
     * param o {any} the item to test
     * return {boolean} true if it is not null/undefined/NaN || false
     */
    isValue: function(o) {
        // return (o || o === false || o === 0 || o === ''); // Infinity fails
return (L.isObject(o) || L.isString(o) || L.isNumber(o) || L.isBoolean(o));
    }

};

/**
 * Determines whether or not the property was added
 * to the object instance.  Returns false if the property is not present
 * in the object, or was inherited from the prototype.
 * This abstraction is provided to enable hasOwnProperty for Safari 1.3.x.
 * There is a discrepancy between YAHOO.lang.hasOwnProperty and
 * Object.prototype.hasOwnProperty when the property is a primitive added to
 * both the instance AND prototype with the same value:
 * <pre>
 * var A = function() {};
 * A.prototype.foo = 'foo';
 * var a = new A();
 * a.foo = 'foo';
 * alert(a.hasOwnProperty('foo')); // true
 * alert(YAHOO.lang.hasOwnProperty(a, 'foo')); // false when using fallback
 * </pre>
 * method hasOwnProperty
 * param {any} o The object being testing
 * param prop {string} the name of the property to test
 * return {boolean} the result
 */
L.hasOwnProperty = (Object.prototype.hasOwnProperty) ?
    function(o, prop) {
        return o && o.hasOwnProperty(prop);
    } : function(o, prop) {
        return !L.isUndefined(o[prop]) && 
                o.constructor.prototype[prop] !== o[prop];
    };

// new lang wins
OB.augmentObject(L, OB, true);

/*
 * An alias for <a href="YAHOO.lang.html">YAHOO.lang</a>
 * class YAHOO.util.Lang
 */
YAHOO.util.Lang = L;
 
/**
 * Same as YAHOO.lang.augmentObject, except it only applies prototype 
 * properties.  This is an alias for augmentProto.
 * see YAHOO.lang.augmentObject
 * method augment
 * static
 * param {Function} r  the object to receive the augmentation
 * param {Function} s  the object that supplies the properties to augment
 * param {String*|boolean}  arguments zero or more properties methods to 
 *        augment the receiver with.  If none specified, everything
 *        in the supplier will be used unless it would
 *        overwrite an existing property in the receiver.  if true
 *        is specified as the third parameter, all properties will
 *        be applied and will overwrite an existing property in
 *        the receiver
 */
L.augment = L.augmentProto;

/**
 * An alias for <a href="YAHOO.lang.html#augment">YAHOO.lang.augment</a>
 * for YAHOO
 * method augment
 * static
 * param {Function} r  the object to receive the augmentation
 * param {Function} s  the object that supplies the properties to augment
 * param {String*}  arguments zero or more properties methods to 
 *        augment the receiver with.  If none specified, everything
 *        in the supplier will be used unless it would
 *        overwrite an existing property in the receiver
 */
YAHOO.augment = L.augmentProto;
       
/**
 * An alias for <a href="YAHOO.lang.html#extend">YAHOO.lang.extend</a>
 * method extend
 * static
 * param {Function} subc   the object to modify
 * param {Function} superc the object to inherit
 * param {Object} overrides  additional properties/methods to add to the
 *        subclass prototype.  These will override the
 *        matching items obtained from the superclass if present.
 */
YAHOO.extend = L.extend;

})();
YAHOO.register("yahoo", YAHOO, {version: "2.6.0", build: "1321"});

// alert("end of YAHOO");


//

/**
 * keycode constants for a subset of the special keys
 * property KEY
 * static
 * final
 */
YAHOO.util.KeyListener.KEY = {
    ALT          : 18,
    BACK_SPACE   : 8,
    CAPS_LOCK    : 20,
    CONTROL      : 17,
    DELETE       : 46,
    DOWN         : 40,
    END          : 35,
    ENTER        : 13,
    ESCAPE       : 27,
    HOME         : 36,
    LEFT         : 37,
    META         : 224,
    NUM_LOCK     : 144,
    PAGE_DOWN    : 34,
    PAGE_UP      : 33, 
    PAUSE        : 19,
    PRINTSCREEN  : 44,
    RIGHT        : 39,
    SCROLL_LOCK  : 145,
    SHIFT        : 16,
    SPACE        : 32,
    TAB          : 9,
    UP           : 38
};
YAHOO.register("event", YAHOO.util.Event, {version: "2.6.0", build: "1321"});


// alert("end of EVENT");

//

(function() {
	var timeouts = [];
	//~ var offset = 0;

	var messageName = "0TMsg";

	// Like setTimeout, but only takes a function argument.  There's
	// no time argument (always zero) and no arguments (you have to
	// use a closure).
	function setZeroTimeout(fn) {
		//fn();
		timeouts.push(fn);//push(fn);
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

				var f = timeouts.shift();
				f();
				//delete f; 
			}
		}
	}

	window.addEventListener("message", handleMessage, true);

	// Add the one thing we want added to the window object.
	window.setZeroTimeout = setZeroTimeout; //function(fn) { setTimeout(fn, 0) };// setZeroTimeout;
})();

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
    //null instanceof Node;
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
                                          return new String(constructor);
     else return 'UNKNOWN TYPE'
  },
  // Could also add type-predicates for Array, Error, Function, Date, etc.
  show : function(value) {
     if (DEBUG.is_xmlnode(value)) return xmldump(value)
     else return JSON.stringify(value).str
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

  // Continue a thread at server after a client call has finished.
  // _kappa is our local (client) continuation, for use when the
  // server is really finished, and _continuation is the server-side
  // continuation which the server asked client to invoke it with.
  var _remoteContinue = function (_kappa, _continuation, _mailbox,
                                  _closureTable, _synch) {
    return function (_res) {
    _debug("Continuing at server with value \"" + _res + "\" and continuation " +
           _continuation);
    var _request = AJAX.newRequest();
    var _rootURL = _removeCGIArgs(location.href);
    _request.open('POST', _rootURL, !_synch);
    // TBD: pass the closureTable as a parameter of _remoteCallHandler
    //   instead of stuffing it in the request.
    if (!_synch)
      _request.onreadystatechange = _remoteCallHandler(_kappa, _request, _synch);
    _request.setRequestHeader('Content-Type',
                              'application/x-www-form-urlencoded');

    _request.pid = _current_pid;

    var _resultJSON = JSON.stringify(_res);
    var _mailboxJSON = JSON.stringify(_mailbox);
    _request.funcs = _closureTable;
    // TBD: how do we know these request tables won't conflict?
    //   json.js uses serial number, is this good enough?
    // FIXME: _closureTable is left over from previous requests; this
    //   is a possible memory leak.
    _compose(_request.funcs, _resultJSON.funcs);
    _compose(_request.funcs, _mailboxJSON.funcs);
    _request.send("__continuation=" + _continuation +
                  "&__result=" + _base64encode(_resultJSON.str));
    if (_synch) {
      _remoteCallHandler(_kappa, _request, _synch)();
    }
    }
  }

  // Resolve function references in the object _obj, specified as records
  // {function:f, environment:e}, where the environment is optional. If
  // an environment is specified, we assume that the function denoted by
  // f is actually a wrapper and that f(e) is the desired function.
  // Without an environment, f itselfprim denotes the desired function, a
  // standard CPS compiled Links function. This is recursive, so each
  // object in _obj also has its functions resolved.
  function resolveFunctions(_obj, _closureTable) {
    if (_obj instanceof Object)
      for (var i in _obj) {
         if (_obj[i].func) {
           _debug("resolving " + _obj[i].func);
           _debug(eval(_obj[i].func));
           _debug("in environment " + _obj[i].environment);
           _debug(eval(_obj[i].environment));
           // FIXME: all this "eval" stuff is dangerous

           var f;
           if (!DEBUG.is_object(_obj[i].environment)) {
_debug("Note: environmentless function resolved");
             f = eval(_obj[i].func)({});
           } else
             f = eval(_obj[i].func)(eval(_obj[i].environment));
           f.location = _obj[i].location; // This may be set to 'server' by the server serializer.
           f.func = _obj[i].func;
           f.environment = _obj[i].environment;
           _obj[i] = f;
         } else if (_obj[i] instanceof Object)
           resolveFunctions(_obj[i], _closureTable);
      }
  }

  // Perform a client call as specified in _callPackage, then re-invoke
  // the server using _remoteContinue
  // NOTE: variables defined within this function could shadow
  // the Links function we're trying to execute. Hence all local
  // vars are prefixed with underscore. Beware also of package variables
  // above shadowing.
  var _invokeClientCall = function (_kappa, _callPackage,
                                    _closureTable, _synch) {
    _debug('Invoking client call to ' + _callPackage.__name + '.');
//    _debug('arguments: ' + _callPackage.__args);
//    _debug('arguments: ' + JSON.stringify(_callPackage.__args).str);
    var _f = eval(_callPackage.__name);
    //   FIXME: the eval is redundant, because done in
    //   _remoteCallHandler; also this name may actually be a
    //   closure-table reference, expecting "request" to be defined.
    _yield(_f, _callPackage.__args,
           _remoteContinue(_kappa, _callPackage.__continuation,
                           _mailboxes[_current_pid] || [],
                           _closureTable, _synch));
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

       _debug("Server response: " + _base64decode(request.responseText));

       var serverResponse = JSON.parseB64Safe(request.responseText);
       if (!serverResponse) throw "Fatal error: nonsense returned from server.";
       serverResponse = serverResponse.content; // unpack the Some construction

       var x = {content: serverResponse};
       resolveFunctions(x, request.funcs);
       serverResponse = x.content;

       _debug("Server response decoded: "); _dump(serverResponse);

       // Check whether we are bouncing the trampoline with a client call
       //   or continuing with a final result.
       // TBD: Would be more elegant to use JS constructors instead of
       //   using a signal member like __continuation.

       if ((serverResponse instanceof Object)
           && ('__continuation' in serverResponse)) {
          // Bouncing the trampoline

          _debug("Client function name, before evaluation, is " +
                 serverResponse.__name);
          // FIXME--try this: resolveFunctions({function:serverResponse.__name,environment:{}});
          { var _closureTable = request.funcs;
            // hackish; this is trying to do what resolveFunctions should do
            serverResponse.__name = eval(serverResponse.__name)({}); }

          // FIXME: serverResponse.__name is eval'd in ctxt where many
          //        vars are defined, dangerous; use resolveFunctions.

          _current_pid = request.pid;
          _invokeClientCall(kappa, serverResponse, request.funcs, synch);

        } else {
          _debug("Client continuing after remote server call, value " +
                 serverResponse);
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
      return str;
//       var result = [];
//       // NOTE:
//       //   IE does not support the form
//       //     for (var i in s) { ... }
//       //   when s is a string, nor does it support s[i].
//       for (var i = 0; i < str.length; ++i) {
//         result.push(str.charAt(i));
//       }
//       return result;
    },

    charlistToString : function (chlist) {
      return chlist;
//       DEBUG.assert(DEBUG.is_array(chlist),
//                    "_charlistToString Expected an array, got: " + chlist);
//       var str = "";
//       for (var i in chlist) {
//         str += chlist[i]
//       }
//       return str;
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

    //// Remote calls

    remoteCall : function(kappa) {
     DEBUG.assert_noisy(DEBUG.is_function(kappa),
       "remoteCall given non-function as continuation");
     return function(name, env, argss) {
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

       // TBD: make request.funcs a parameter of remotecallhandler
       // instead of stuffing it in the request.
       if (!synchronous)
         request.onreadystatechange =
                _remoteCallHandler(setpid_kappa, request, synchronous);

       request.pid = _current_pid;
       var argsJSON = JSON.stringify(argss);

       if (!env) env = {};
       var envJSON = JSON.stringify(env);
       request.funcs = _compose(argsJSON.funcs, envJSON.funcs);

       var argString =
         "__name=" + _base64encode(name) +
         "&__args=" + _base64encode(argsJSON.str) +
         "&__env=" + _base64encode(envJSON.str)

       for (var i = 0; i < cgiEnv.length; ++i) {
         argString = argString + "&" + $str(cgiEnv[i][1]) + "=" + $str(cgiEnv[i][2]);
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
          node.style.cssText = $str(attrs['style']);
        } else {
          node.setAttribute(name, $str(attrs[name]));
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
      return LINKS.eq2(l, r);
    }
    return false;
  };

  // supposedly this prevented LINKS.eq to be optimized,
  // because the p in for(p in x) is non-local;
  // so I put it in another function; 
  LINKS.eq2 = function(l,r) {
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
   _dwindow.scroll(0, _dwindow.scrollMaxY);
   }
}

var debug = LINKS.kify(_debug);

function _alertDialog(msg) {
//   DEBUG.assert(DEBUG.is_charlist(msg), "_alertDialog expected charlist, got: "
//                  + msg);
  return (alert($str(msg)));
}
var alertDialog = LINKS.kify(_alertDialog);

function _tilde(s, regex) {
//    _debug("string: " + $str(s));
    var r = Regex.compile(regex);
//    _debug("compiled regex: " + r);
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
               "_charlistToString Expected an array, got: " + cs);
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
//   DEBUG.assert_noisy(DEBUG.is_array(id),
//                      "_getNodeById Expected an array, got: " + id);
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

function _domHasAttribute(nodeRef, attr) {
  return nodeRef.hasAttribute($str(attr));
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
  return $chl(nodeRef.style[$str(attr)]);
}

function _domGetNodeValueFromRef(node) {
  return $chl(node.value);
}
var domGetNodeValueFromRef = LINKS.kify(_domGetNodeValueFromRef)

var domGetTagNameFromRef = LINKS.kify(_domGetTagNameFromRef);
var domHasAttribute = LINKS.kify(_domHasAttribute);
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
//  DEBUG.assert(DEBUG.is_array(s), "_stringToXml Expected an array, got: " + s);
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
function _clientTime () { return Date.now(); }
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

function _spawn(f) { // f is a zero-argument function
  _maxPid++;
  var childPid = _maxPid;
  //_mailboxes[childPid] = [];
  _makeMailbox(childPid);
  window.setZeroTimeout(function () {
               _debug("launched process #" + childPid);
               _current_pid = childPid;
               f(function () { delete _mailboxes[childPid] })});
  return childPid;
}

function spawn(f, kappa) {
   kappa(_spawn(f));
}

function _spawnWrapper(env) {  // necessary wrapper for server->client calls
  return spawn;
}

function spawnWait(f, kappa) {
  // f is a zero-argument CPS function
  var parentPid = _current_pid
  _maxPid++;
  var childPid = _maxPid;
  // _mailboxes[childPid] = [];
  _makeMailbox(childPid);

  _debug("launched process #" + childPid);
  _current_pid = childPid;

  f(function(v) {_current_pid = parentPid; delete _mailboxes[childPid]; kappa(v)});
}

function _selfprim() {
     return _current_pid;
}

function selfprim(kappa) {
     return kappa(_selfprim())
}

function _haveMail() {
  return _mailboxes[_selfprim()].length != 0;
}
var haveMail = LINKS.kify(_haveMail);

var _sched_pause = 0;

function _wakeup(pid) {
  if (_blocked_procs[pid]) {
    _debug("Waking up " + pid);
    var proc = _blocked_procs[pid];
    delete _blocked_procs[pid];
    window.setZeroTimeout(proc);
  }
  else {
    // already awake?
  }
}

function _send(pid, msg) {
//  _dump(_mailboxes)
  _debug("sending message '" + msg._label + "' to pid " + pid);
  if (!_mailboxes[pid])
    _makeMailbox(pid);
  _mailboxes[pid].unshift(msg);
  _wakeup(pid);
  _debug(pid + ' now has ' + _mailboxes[pid].length + ' message(s)');
  _dumpSchedStatus();
  return {};
}

function send(pid, msg, kappa) {
  kappa(_send(pid, msg));
}

function _sendWrapper(env) {  // necessary wrapper for server->client calls
  return send;
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
    // _debug("blocked: "+current_pid)
  }
}

function _recvWrapper(env) {  // necessary wrapper for server->client calls
  return recv;
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

// SESSIONS

_aps = []
_nextAP = 0
_channels = []
_nextChannel = 0


function _new() {
  // console.log("Creating new access point " + _nextAP);
  _aps[_nextAP] = {accepters: [], requesters: []};
  _nextAP = _nextAP + 1;
  return _nextAP - 1;
}

function accept(ap, kappa) {
  // console.log("Accept called on AP " + ap);
  if (_aps[ap].requesters.length > 0) {
    r = _aps[ap].requesters.pop();
    _channels[r.channel][0] = {proc: _current_pid, messages: []}
    _wakeup(r.proc);
    kappa({channel: r.channel, direction: 0});
  } else {
    var current_pid = _current_pid;
    var channel = _nextChannel;
    _nextChannel = _nextChannel + 1;
    _channels[channel] = [{proc: _current_pid, messages: []}, {proc: 0, messages: []}]
    _aps[ap].accepters.unshift({proc: current_pid, channel: channel});
    // console.log("Blocking accepter on AP " + ap);
    _block_proc(current_pid,
                function () {
                  // console.log("Waking accepter on AP " + ap);
                  _current_pid = current_pid;
                  kappa({channel: channel, direction: 0});
                });
  }
}

function request(ap, kappa) {
  // console.log("Request called on AP " + ap);
  if (_aps[ap].accepters.length > 0) {
    r = _aps[ap].accepters.pop();
    _channels[r.channel][1] = {proc: _current_pid, messages: []}
    _wakeup(r.proc);
    kappa({channel: r.channel, direction: 1});
  } else {
    var current_pid = _current_pid;
    var channel = _nextChannel;
    _nextChannel = _nextChannel + 1;
    _channels[channel] = [{proc: 0, messages: []}, {proc: _current_pid, messages: []}]
    _aps[ap].requesters.unshift({proc: current_pid, channel: channel});
    // console.log("Blocking requester on AP " + ap);
    _block_proc(current_pid,
                function () {
                  // console.log("Waking requester on AP " + ap);
                  _current_pid = current_pid;
                  kappa({channel: channel, direction: 1});
                });
  }
}

function _give(v, c) {
  // console.log("Giving " + v + " to channel " + c.channel + "-" + c.direction);
  _channels[c.channel][c.direction].messages.unshift(v);
  _wakeup(_channels[c.channel][1 - c.direction].proc);
  return c;
}

function grab(c, kappa) {
  // console.log("Grabbing from channel " + c.channel + "-" + c.direction);
  if (_channels[c.channel][1 - c.direction].messages.length > 0) {
    var msg = _channels[c.channel][1 - c.direction].messages.pop();
    // console.log("Grabbed " + msg);
    return (kappa({1:msg, 2:c}));
  } else {
    var current_pid = _current_pid;
    _block_proc(current_pid,
                function () {
                  _current_pid = current_pid;
                  grab(c, kappa);
                });
  }
}

// SCHEDULER

var _yieldCount = 0;
var _yieldGranularity = 60;
var _callCount = 0;
var _yieldCalls = 0;
var _yieldContCalls = 0;
var _callingTimeout = false;

/**
 * @constructor
 */
function _Continuation(v) { this.v = v }

// yield: give up control for another "thread" to work.
// if we're running in an event handler then don't yield (but
// do throw away the stack periodically instead).
function _yield(f, args, k) {
	//_callingTimeout = false;
	
	++_yieldCount;
	if (_yieldCount == _yieldGranularity) {
		_yieldCount = 0;
		if (_handlingEvent) {
			throw new _Continuation(
				function () {
					return f.apply(f, ___append(args, k));
				});
		} else {
			//_callingTimeout = true;

			var current_pid = _current_pid;
			window.setZeroTimeout((
				function() {
					_current_pid = current_pid;
					return f.apply(f, ___append(args, k))}));
		}
	} else {
		return f.apply(f, ___append(args, k));
	}
}

function _yieldCont(k, arg) {
	//return k(arg);
	++_yieldCount;
	if (_yieldCount == _yieldGranularity) {
		_yieldCount = 0;
		if (_handlingEvent) {
			throw new _Continuation(function () { k(arg) });
		} else {
			var current_pid = _current_pid;
			window.setZeroTimeout((
				function() {
					_current_pid = current_pid;
					k(arg) }));
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
    env[i] = {'1':$chl(name), '2':$chl(cgiEnv[name])};
    ++i;
  }

  cgiEnv = env;
}

function _environment() {
  return cgiEnv;
}
var environment = LINKS.kify(_environment);

function _redirect(url) {
  window.location = $str(url);
}
var redirect = LINKS.kify(_redirect);

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

function _setInterval(fn, interval) {
	window.setInterval(fn, interval);
	return;
}
var setInterval = LINKS.kify(_setInterval);


//
//
// LINKS GAME LIBRARY
//
//

// NOTES
// I made the interface a bit inconsistent by sometimes passing a DomNode
// 	and other times passing a string (with DomNode's id) to functions
// 	FIXME: will fix that later
//
//

function _jsSetInterval(fn, interval) {
	window.setInterval(function () { fn(_idy) }, interval);
	return;
}
//var jsSetInterval = LINKS.kify(_jsSetInterval);
function jsSetInterval(fn, interval, kappa) {
    //window.setInterval(function () { fn(_idy) }, interval);
    //kappa({});
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
	ctx.save();
	return;
}
function jsRestore(ctx, kappa) {
    _jsRestore(ctx);
    kappa({});
}

function _jsSetOnKeyDown(node, fn) {
	// note: node has to exist in the document, otherwise we get a JavaScript error
	document.getElementById($str(node)).addEventListener('keydown', function(e) { fn(e, _idy) }, true);
	return;
}
function jsSetOnKeyDown(node, fn, kappa) {
    _jsSetOnKeyDown(node, fn);
    kappa({});
}

function _jsSetOnEvent(node, event, fn, capture) {
	document.getElementById($str(node)).addEventListener($str(event), function(e) { fn(e, _idy) }, capture);
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
	globalObjects[$str(name)] = obj;
	return;
}
function jsSaveGlobalObject(name, obj, kappa) {
    _jsSaveGlobalObject(name, obj);
    kappa({});
}

function _jsLoadGlobalObject(name) {
	return globalObjects[$str(name)];
	return;
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
	ctx.fillText($str(text), x, y);
}
function jsFillText(ctx, text, x, y) {
	_jsFillText(ctx, text, x, y);
	kappa({});
}

function _jsDrawImage(ctx, node, x, y) {
	ctx.drawImage(node, x, y);
}
function jsDrawImage(ctx, node, x, y) {
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

function _jsBeginPath(ctx) {
    ctx.beginPath();
}
function jsBeginPath(ctx, kappa) {
	_jsBeginPath(ctx);
	kappa({});
}

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


function _jsSetFillColor(ctx, color) {	
	ctx.fillStyle = $str(color);
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
	var imageData = canvas.toDataURL($str(mime));//.replace("image/png", "image/octet-stream");;
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


// alert("end of JSLIB");

function _getDatabaseConfig() {
  return {}
}
var getDatabaseConfig = LINKS.kify(_getDatabaseConfig);

var cgiEnv = {};
_makeCgiEnvironment();

// start of APP


  _startTimer();function xmlToVariant(x1, __kappa) { LINKS.remoteCall(__kappa)('xmlToVariant', {}, {'1':x1}) }
function serverTime(__kappa) { LINKS.remoteCall(__kappa)('serverTime', {}, {}) }
function InsertRows(x1, x2, __kappa) { LINKS.remoteCall(__kappa)('InsertRows', {}, {'1':x1, '2':x2}) }
function InsertReturning(x1, x2, x3, __kappa) { LINKS.remoteCall(__kappa)('InsertReturning', {}, {'1':x1, '2':x2, '3':x3}) }
function ltilde(x1, x2, __kappa) { LINKS.remoteCall(__kappa)('ltilde', {}, {'1':x1, '2':x2}) }
function stilde(x1, x2, __kappa) { LINKS.remoteCall(__kappa)('stilde', {}, {'1':x1, '2':x2}) }
function unsafePickleCont(x1, __kappa) { LINKS.remoteCall(__kappa)('unsafePickleCont', {}, {'1':x1}) }
function pickle_value(x1, __kappa) { LINKS.remoteCall(__kappa)('pickle_value', {}, {'1':x1}) }
function unpickle_value(x1, __kappa) { LINKS.remoteCall(__kappa)('unpickle_value', {}, {'1':x1}) }
function unsafe_cast(x1, __kappa) { LINKS.remoteCall(__kappa)('unsafe_cast', {}, {'1':x1}) }
function parseXml(x1, __kappa) { LINKS.remoteCall(__kappa)('parseXml', {}, {'1':x1}) }
function dumpTypes(x1, __kappa) { LINKS.remoteCall(__kappa)('dumpTypes', {}, {'1':x1}) }
function _188(_env) { return  (function (_186, _187, __kappa) { LINKS.remoteCall(__kappa)('188', _env, {'1':_186, '2':_187}) }) }
function _197(_env) { return  (function (_195, _196, __kappa) { LINKS.remoteCall(__kappa)('197', _env, {'1':_195, '2':_196}) }) }
function _194(_env) { return  (function (_193, __kappa) { LINKS.remoteCall(__kappa)('194', _env, {'1':_193}) }) }
function _202(_env) { return  (function (_200, _201, __kappa) { LINKS.remoteCall(__kappa)('202', _env, {'1':_200, '2':_201}) }) }
function _208(_env) { return  (function (_206, _207, __kappa) { LINKS.remoteCall(__kappa)('208', _env, {'1':_206, '2':_207}) }) }
function _224(_env) { return  (function (_222, _223, __kappa) { LINKS.remoteCall(__kappa)('224', _env, {'1':_222, '2':_223}) }) }
function _221(_env) { return  (function (_220, __kappa) { LINKS.remoteCall(__kappa)('221', _env, {'1':_220}) }) }
function _229(_env) { return  (function (_227, _228, __kappa) { LINKS.remoteCall(__kappa)('229', _env, {'1':_227, '2':_228}) }) }
function _239(_env) { return  (function (_238, __kappa) { LINKS.remoteCall(__kappa)('239', _env, {'1':_238}) }) }
function _245(_env) { return  (function (_244, __kappa) { LINKS.remoteCall(__kappa)('245', _env, {'1':_244}) }) }
function _253(_env) { return  (function (_251, _252, __kappa) { LINKS.remoteCall(__kappa)('253', _env, {'1':_251, '2':_252}) }) }
function _258(_env) { return  (function (_256, _257, __kappa) { LINKS.remoteCall(__kappa)('258', _env, {'1':_256, '2':_257}) }) }
function _268(_env) { return  (function (_267, __kappa) { LINKS.remoteCall(__kappa)('268', _env, {'1':_267}) }) }
function _278(_env) { return  (function (_277, __kappa) { LINKS.remoteCall(__kappa)('278', _env, {'1':_277}) }) }
function _287(_env) { return  (function (_286, __kappa) { LINKS.remoteCall(__kappa)('287', _env, {'1':_286}) }) }
function _285(_env) { return  (function (_283, _284, __kappa) { LINKS.remoteCall(__kappa)('285', _env, {'1':_283, '2':_284}) }) }
function _296(_env) { return  (function (_295, __kappa) { LINKS.remoteCall(__kappa)('296', _env, {'1':_295}) }) }
function _294(_env) { return  (function (_293, __kappa) { LINKS.remoteCall(__kappa)('294', _env, {'1':_293}) }) }
function _292(_env) { return  (function (_291, __kappa) { LINKS.remoteCall(__kappa)('292', _env, {'1':_291}) }) }
function _299(_env) { return  (function (_298, __kappa) { LINKS.remoteCall(__kappa)('299', _env, {'1':_298}) }) }
function _310(_env) { return  (function (_308, _309, __kappa) { LINKS.remoteCall(__kappa)('310', _env, {'1':_308, '2':_309}) }) }
function _321(_env) { return  (function (_320, __kappa) { LINKS.remoteCall(__kappa)('321', _env, {'1':_320}) }) }
function _324(_env) { return  (function (_323, __kappa) { LINKS.remoteCall(__kappa)('324', _env, {'1':_323}) }) }
function _337(_env) { return  (function (_336, __kappa) { LINKS.remoteCall(__kappa)('337', _env, {'1':_336}) }) }
function _330(_env) { return  (function (_328, _329, __kappa) { LINKS.remoteCall(__kappa)('330', _env, {'1':_328, '2':_329}) }) }
function _340(_env) { return  (function (_339, __kappa) { LINKS.remoteCall(__kappa)('340', _env, {'1':_339}) }) }
function _348(_env) { return  (function (_347, __kappa) { LINKS.remoteCall(__kappa)('348', _env, {'1':_347}) }) }
function _346(_env) { return  (function (_344, _345, __kappa) { LINKS.remoteCall(__kappa)('346', _env, {'1':_344, '2':_345}) }) }
function _351(_env) { return  (function (_350, __kappa) { LINKS.remoteCall(__kappa)('351', _env, {'1':_350}) }) }
function _354(_env) { return  (function (_353, __kappa) { LINKS.remoteCall(__kappa)('354', _env, {'1':_353}) }) }
function _364(_env) { return  (function (_362, _363, __kappa) { LINKS.remoteCall(__kappa)('364', _env, {'1':_362, '2':_363}) }) }
function _373(_env) { return  (function (_372, __kappa) { LINKS.remoteCall(__kappa)('373', _env, {'1':_372}) }) }
function _380(_env) {
return  (function (_377, _378, _379, __kappa) { LINKS.remoteCall(__kappa)('380', _env, {'1':_377, '2':_378, '3':_379}) })
}
function _393(_env) { return  (function (_391, _392, __kappa) { LINKS.remoteCall(__kappa)('393', _env, {'1':_391, '2':_392}) }) }
function _400(_env) {
return  (function (_397, _398, _399, __kappa) { LINKS.remoteCall(__kappa)('400', _env, {'1':_397, '2':_398, '3':_399}) })
}
function _417(_env) {
return  (function (_414, _415, _416, __kappa) { LINKS.remoteCall(__kappa)('417', _env, {'1':_414, '2':_415, '3':_416}) })
}
function _413(_env) { return  (function (_412, __kappa) { LINKS.remoteCall(__kappa)('413', _env, {'1':_412}) }) }
function _424(_env) {
return  (function (_421, _422, _423, __kappa) { LINKS.remoteCall(__kappa)('424', _env, {'1':_421, '2':_422, '3':_423}) })
}
function _435(_env) { return  (function (_433, _434, __kappa) { LINKS.remoteCall(__kappa)('435', _env, {'1':_433, '2':_434}) }) }
function _438(_env) { return  (function (_437, __kappa) { LINKS.remoteCall(__kappa)('438', _env, {'1':_437}) }) }
function _441(_env) { return  (function (_440, __kappa) { LINKS.remoteCall(__kappa)('441', _env, {'1':_440}) }) }
function _444(_env) { return  (function (_443, __kappa) { LINKS.remoteCall(__kappa)('444', _env, {'1':_443}) }) }
function _461(_env) { return  (function (_459, _460, __kappa) { LINKS.remoteCall(__kappa)('461', _env, {'1':_459, '2':_460}) }) }
function _451(_env) { return  (function (_449, _450, __kappa) { LINKS.remoteCall(__kappa)('451', _env, {'1':_449, '2':_450}) }) }
function _464(_env) { return  (function (_463, __kappa) { LINKS.remoteCall(__kappa)('464', _env, {'1':_463}) }) }
function _474(_env) { return  (function (_472, _473, __kappa) { LINKS.remoteCall(__kappa)('474', _env, {'1':_472, '2':_473}) }) }
function _479(_env) { return  (function (_478, __kappa) { LINKS.remoteCall(__kappa)('479', _env, {'1':_478}) }) }
function _491(_env) { return  (function (_489, _490, __kappa) { LINKS.remoteCall(__kappa)('491', _env, {'1':_489, '2':_490}) }) }
function _494(_env) { return  (function (_493, __kappa) { LINKS.remoteCall(__kappa)('494', _env, {'1':_493}) }) }
function _496(_env) { return  (function (_495, __kappa) { LINKS.remoteCall(__kappa)('496', _env, {'1':_495}) }) }
function _499(_env) { return  (function (_498, __kappa) { LINKS.remoteCall(__kappa)('499', _env, {'1':_498}) }) }
function _506(_env) { return  (function (_504, _505, __kappa) { LINKS.remoteCall(__kappa)('506', _env, {'1':_504, '2':_505}) }) }
function _511(_env) { return  (function (_509, _510, __kappa) { LINKS.remoteCall(__kappa)('511', _env, {'1':_509, '2':_510}) }) }
function _529(_env) { return  (function (_527, _528, __kappa) { LINKS.remoteCall(__kappa)('529', _env, {'1':_527, '2':_528}) }) }
function _543(_env) { return  (function (_542, __kappa) { LINKS.remoteCall(__kappa)('543', _env, {'1':_542}) }) }
function _540(_env) { return  (function (_539, __kappa) { LINKS.remoteCall(__kappa)('540', _env, {'1':_539}) }) }
function _546(_env) { return  (function (_545, __kappa) { LINKS.remoteCall(__kappa)('546', _env, {'1':_545}) }) }
function _550(_env) { return  (function (_549, __kappa) { LINKS.remoteCall(__kappa)('550', _env, {'1':_549}) }) }
function _553(_env) { return  (function (_552, __kappa) { LINKS.remoteCall(__kappa)('553', _env, {'1':_552}) }) }
function _556(_env) { return  (function (_555, __kappa) { LINKS.remoteCall(__kappa)('556', _env, {'1':_555}) }) }
function _559(_env) { return  (function (_558, __kappa) { LINKS.remoteCall(__kappa)('559', _env, {'1':_558}) }) }
function _576(_env) { return  (function (_574, _575, __kappa) { LINKS.remoteCall(__kappa)('576', _env, {'1':_574, '2':_575}) }) }
function _586(_env) { return  (function (_585, __kappa) { LINKS.remoteCall(__kappa)('586', _env, {'1':_585}) }) }
function _589(_env) { return  (function (_588, __kappa) { LINKS.remoteCall(__kappa)('589', _env, {'1':_588}) }) }
function _601(_env) { return  (function (_599, _600, __kappa) { LINKS.remoteCall(__kappa)('601', _env, {'1':_599, '2':_600}) }) }
function _598(_env) {
return  (function (_595, _596, _597, __kappa) { LINKS.remoteCall(__kappa)('598', _env, {'1':_595, '2':_596, '3':_597}) })
}
function _606(_env) { return  (function (_604, _605, __kappa) { LINKS.remoteCall(__kappa)('606', _env, {'1':_604, '2':_605}) }) }
function _618(_env) { return  (function (_617, __kappa) { LINKS.remoteCall(__kappa)('618', _env, {'1':_617}) }) }
function _627(_env) { return  (function (_625, _626, __kappa) { LINKS.remoteCall(__kappa)('627', _env, {'1':_625, '2':_626}) }) }
function _630(_env) { return  (function (_629, __kappa) { LINKS.remoteCall(__kappa)('630', _env, {'1':_629}) }) }
function _637(_env) { return  (function (_636, __kappa) { LINKS.remoteCall(__kappa)('637', _env, {'1':_636}) }) }
function _640(_env) { return  (function (_639, __kappa) { LINKS.remoteCall(__kappa)('640', _env, {'1':_639}) }) }
function _645(_env) { return  (function (_643, _644, __kappa) { LINKS.remoteCall(__kappa)('645', _env, {'1':_643, '2':_644}) }) }
function _656(_env) { return  (function (_654, _655, __kappa) { LINKS.remoteCall(__kappa)('656', _env, {'1':_654, '2':_655}) }) }
function _671(_env) { return  (function (_669, _670, __kappa) { LINKS.remoteCall(__kappa)('671', _env, {'1':_669, '2':_670}) }) }
function _685(_env) { return  (function (_683, _684, __kappa) { LINKS.remoteCall(__kappa)('685', _env, {'1':_683, '2':_684}) }) }
function _700(_env) { return  (function (_698, _699, __kappa) { LINKS.remoteCall(__kappa)('700', _env, {'1':_698, '2':_699}) }) }
function _714(_env) { return  (function (_712, _713, __kappa) { LINKS.remoteCall(__kappa)('714', _env, {'1':_712, '2':_713}) }) }
function _726(_env) { return  (function (_724, _725, __kappa) { LINKS.remoteCall(__kappa)('726', _env, {'1':_724, '2':_725}) }) }
function _743(_env) { return  (function (_742, __kappa) { LINKS.remoteCall(__kappa)('743', _env, {'1':_742}) }) }
function _751(_env) { return  (function (_750, __kappa) { LINKS.remoteCall(__kappa)('751', _env, {'1':_750}) }) }
function _757(_env) { return  (function (_755, _756, __kappa) { LINKS.remoteCall(__kappa)('757', _env, {'1':_755, '2':_756}) }) }
function _769(_env) { return  (function (_767, _768, __kappa) { LINKS.remoteCall(__kappa)('769', _env, {'1':_767, '2':_768}) }) }
function _766(_env) { return  (function (_765, __kappa) { LINKS.remoteCall(__kappa)('766', _env, {'1':_765}) }) }
function _772(_env) { return  (function (_771, __kappa) { LINKS.remoteCall(__kappa)('772', _env, {'1':_771}) }) }
function _778(_env) { return  (function (_777, __kappa) { LINKS.remoteCall(__kappa)('778', _env, {'1':_777}) }) }
function _776(_env) { return  (function (_775, __kappa) { LINKS.remoteCall(__kappa)('776', _env, {'1':_775}) }) }
function _785(_env) { return  (function (_784, __kappa) { LINKS.remoteCall(__kappa)('785', _env, {'1':_784}) }) }
function _783(_env) { return  (function (_782, __kappa) { LINKS.remoteCall(__kappa)('783', _env, {'1':_782}) }) }
function _788(_env) { return  (function (_787, __kappa) { LINKS.remoteCall(__kappa)('788', _env, {'1':_787}) }) }
function _805(_env) { return  (function (_803, _804, __kappa) { LINKS.remoteCall(__kappa)('805', _env, {'1':_803, '2':_804}) }) }
function _802(_env) { return  (function (_801, __kappa) { LINKS.remoteCall(__kappa)('802', _env, {'1':_801}) }) }
function _799(_env) { return  (function (_798, __kappa) { LINKS.remoteCall(__kappa)('799', _env, {'1':_798}) }) }
function _821(_env) { return  (function (_819, _820, __kappa) { LINKS.remoteCall(__kappa)('821', _env, {'1':_819, '2':_820}) }) }
function _827(_env) { return  (function (_825, _826, __kappa) { LINKS.remoteCall(__kappa)('827', _env, {'1':_825, '2':_826}) }) }
function _850(_env) { return  (function (_848, _849, __kappa) { LINKS.remoteCall(__kappa)('850', _env, {'1':_848, '2':_849}) }) }
function _847(_env) { return  (function (_846, __kappa) { LINKS.remoteCall(__kappa)('847', _env, {'1':_846}) }) }
function _845(_env) { return  (function (_844, __kappa) { LINKS.remoteCall(__kappa)('845', _env, {'1':_844}) }) }
function _843(_env) { return  (function (_842, __kappa) { LINKS.remoteCall(__kappa)('843', _env, {'1':_842}) }) }
function _855(_env) { return  (function (_854, __kappa) { LINKS.remoteCall(__kappa)('855', _env, {'1':_854}) }) }
function _853(_env) { return  (function (_852, __kappa) { LINKS.remoteCall(__kappa)('853', _env, {'1':_852}) }) }
function _861(_env) { return  (function (_859, _860, __kappa) { LINKS.remoteCall(__kappa)('861', _env, {'1':_859, '2':_860}) }) }
function _866(_env) { return  (function (_864, _865, __kappa) { LINKS.remoteCall(__kappa)('866', _env, {'1':_864, '2':_865}) }) }
function _869(_env) { return  (function (_868, __kappa) { LINKS.remoteCall(__kappa)('869', _env, {'1':_868}) }) }
function _873(_env) { return  (function (_872, __kappa) { LINKS.remoteCall(__kappa)('873', _env, {'1':_872}) }) }
function _877(_env) { return  (function (_876, __kappa) { LINKS.remoteCall(__kappa)('877', _env, {'1':_876}) }) }
function _889(_env) { return  (function (_888, __kappa) { LINKS.remoteCall(__kappa)('889', _env, {'1':_888}) }) }
function _886(_env) { return  (function (_885, __kappa) { LINKS.remoteCall(__kappa)('886', _env, {'1':_885}) }) }
function _893(_env) { return  (function (_892, __kappa) { LINKS.remoteCall(__kappa)('893', _env, {'1':_892}) }) }
function _932(_env) { return  (function (_931, __kappa) { LINKS.remoteCall(__kappa)('932', _env, {'1':_931}) }) }
function _922(_env) { return  (function (_921, __kappa) { LINKS.remoteCall(__kappa)('922', _env, {'1':_921}) }) }
function _928(_env) { return  (function (_927, __kappa) { LINKS.remoteCall(__kappa)('928', _env, {'1':_927}) }) }
function _908(_env) { return  (function (_907, __kappa) { LINKS.remoteCall(__kappa)('908', _env, {'1':_907}) }) }
function _906(_env) { return  (function (_905, __kappa) { LINKS.remoteCall(__kappa)('906', _env, {'1':_905}) }) }
function _917(_env) { return  (function (_916, __kappa) { LINKS.remoteCall(__kappa)('917', _env, {'1':_916}) }) }
function _938(_env) { return  (function (_937, __kappa) { LINKS.remoteCall(__kappa)('938', _env, {'1':_937}) }) }
function _936(_env) { return  (function (_935, __kappa) { LINKS.remoteCall(__kappa)('936', _env, {'1':_935}) }) }
function _947(_env) { return  (function (_946, __kappa) { LINKS.remoteCall(__kappa)('947', _env, {'1':_946}) }) }
function _945(_env) { return  (function (_944, __kappa) { LINKS.remoteCall(__kappa)('945', _env, {'1':_944}) }) }
function _943(_env) { return  (function (_942, __kappa) { LINKS.remoteCall(__kappa)('943', _env, {'1':_942}) }) }
function _951(_env) { return  (function (_950, __kappa) { LINKS.remoteCall(__kappa)('951', _env, {'1':_950}) }) }
function _949(_env) { return  (function (__kappa) { LINKS.remoteCall(__kappa)('949', _env, {}) }) }
function _961(_env) {
return  (function (_958, _959, _960, __kappa) { LINKS.remoteCall(__kappa)('961', _env, {'1':_958, '2':_959, '3':_960}) })
}
function _956(_env) { return  (function (__kappa) { LINKS.remoteCall(__kappa)('956', _env, {}) }) }
function _974(_env) {
return 
  (function (_968, _969, _970, _971, _972, _973, __kappa) {
  LINKS.remoteCall(__kappa)('974', _env, {'1':_968, '2':_969, '3':_970, '4':_971, '5':_972, '6':_973})
  })
}
function _994(_env) { return  (function (_993, __kappa) { LINKS.remoteCall(__kappa)('994', _env, {'1':_993}) }) }
function _986(_env) { return  (function (_985, __kappa) { LINKS.remoteCall(__kappa)('986', _env, {'1':_985}) }) }
function _990(_env) { return  (function (_989, __kappa) { LINKS.remoteCall(__kappa)('990', _env, {'1':_989}) }) }
function _1017(_env) { return  (function (_1015, _1016, __kappa) { LINKS.remoteCall(__kappa)('1017', _env, {'1':_1015, '2':_1016}) }) }
function _1014(_env) { return  (function (_1013, __kappa) { LINKS.remoteCall(__kappa)('1014', _env, {'1':_1013}) }) }
function _1009(_env) { return  (function (_1008, __kappa) { LINKS.remoteCall(__kappa)('1009', _env, {'1':_1008}) }) }
function _1020(_env) { return  (function (_1019, __kappa) { LINKS.remoteCall(__kappa)('1020', _env, {'1':_1019}) }) }
function _1040(_env) { return  (function (_1039, __kappa) { LINKS.remoteCall(__kappa)('1040', _env, {'1':_1039}) }) }
function _1033(_env) { return  (function (_1031, _1032, __kappa) { LINKS.remoteCall(__kappa)('1033', _env, {'1':_1031, '2':_1032}) }) }
function _1030(_env) { return  (function (_1029, __kappa) { LINKS.remoteCall(__kappa)('1030', _env, {'1':_1029}) }) }
function _1037(_env) { return  (function (_1036, __kappa) { LINKS.remoteCall(__kappa)('1037', _env, {'1':_1036}) }) }
function _1051(_env) { return  (function (_1049, _1050, __kappa) { LINKS.remoteCall(__kappa)('1051', _env, {'1':_1049, '2':_1050}) }) }
function _1048(_env) { return  (function (_1047, __kappa) { LINKS.remoteCall(__kappa)('1048', _env, {'1':_1047}) }) }
function _1054(_env) { return  (function (_1053, __kappa) { LINKS.remoteCall(__kappa)('1054', _env, {'1':_1053}) }) }
function _1071(_env) {
return 
  (function (_1067, _1068, _1069, _1070, __kappa) { LINKS.remoteCall(__kappa)('1071', _env, {'1':_1067, '2':_1068, '3':_1069, '4':_1070}) })
}
function _1066(_env) {
return  (function (_1063, _1064, _1065, __kappa) { LINKS.remoteCall(__kappa)('1066', _env, {'1':_1063, '2':_1064, '3':_1065}) })
}
function _1091(_env) {
return  (function (_1088, _1089, _1090, __kappa) { LINKS.remoteCall(__kappa)('1091', _env, {'1':_1088, '2':_1089, '3':_1090}) })
}
function _1087(_env) { return  (function (_1084, __kappa) { LINKS.remoteCall(__kappa)('1087', _env, {'1':_1084}) }) }
function _1082(_env) { return  (function (_1081, __kappa) { LINKS.remoteCall(__kappa)('1082', _env, {'1':_1081}) }) }
function _1106(_env) { return  (function (_1104, _1105, __kappa) { LINKS.remoteCall(__kappa)('1106', _env, {'1':_1104, '2':_1105}) }) }
function _1103(_env) { return  (function (_1102, __kappa) { LINKS.remoteCall(__kappa)('1103', _env, {'1':_1102}) }) }
function _1100(_env) { return  (function (_1099, __kappa) { LINKS.remoteCall(__kappa)('1100', _env, {'1':_1099}) }) }
function _1118(_env) { return  (function (_1117, __kappa) { LINKS.remoteCall(__kappa)('1118', _env, {'1':_1117}) }) }
function _1110(_env) { return  (function (_1109, __kappa) { LINKS.remoteCall(__kappa)('1110', _env, {'1':_1109}) }) }
function _1115(_env) { return  (function (_1113, _1114, __kappa) { LINKS.remoteCall(__kappa)('1115', _env, {'1':_1113, '2':_1114}) }) }
function _1133(_env) { return  (function (_1132, __kappa) { LINKS.remoteCall(__kappa)('1133', _env, {'1':_1132}) }) }
function _1124(_env) { return  (function (_1123, __kappa) { LINKS.remoteCall(__kappa)('1124', _env, {'1':_1123}) }) }
function _1131(_env) { return  (function (_1129, _1130, __kappa) { LINKS.remoteCall(__kappa)('1131', _env, {'1':_1129, '2':_1130}) }) }
function _1137(_env) { return  (function (_1136, __kappa) { LINKS.remoteCall(__kappa)('1137', _env, {'1':_1136}) }) }
function _1143(_env) { return  (function (_1141, _1142, __kappa) { LINKS.remoteCall(__kappa)('1143', _env, {'1':_1141, '2':_1142}) }) }
function _1147(_env) { return  (function (_1146, __kappa) { LINKS.remoteCall(__kappa)('1147', _env, {'1':_1146}) }) }
function _1151(_env) { return  (function (_1150, __kappa) { LINKS.remoteCall(__kappa)('1151', _env, {'1':_1150}) }) }
function _1155(_env) { return  (function (_1154, __kappa) { LINKS.remoteCall(__kappa)('1155', _env, {'1':_1154}) }) }
function _1159(_env) { return  (function (_1158, __kappa) { LINKS.remoteCall(__kappa)('1159', _env, {'1':_1158}) }) }
function _1163(_env) { return  (function (_1162, __kappa) { LINKS.remoteCall(__kappa)('1163', _env, {'1':_1162}) }) }
function _1181(_env) { return  (function (_1180, __kappa) { LINKS.remoteCall(__kappa)('1181', _env, {'1':_1180}) }) }
function _1179(_env) { return  (function (_1178, __kappa) { LINKS.remoteCall(__kappa)('1179', _env, {'1':_1178}) }) }
function _1176(_env) { return  (function (_1175, __kappa) { LINKS.remoteCall(__kappa)('1176', _env, {'1':_1175}) }) }
function _1173(_env) { return  (function (_1172, __kappa) { LINKS.remoteCall(__kappa)('1173', _env, {'1':_1172}) }) }
function _1200(_env) { return  (function (_1198, _1199, __kappa) { LINKS.remoteCall(__kappa)('1200', _env, {'1':_1198, '2':_1199}) }) }
function _1197(_env) { return  (function (_1196, __kappa) { LINKS.remoteCall(__kappa)('1197', _env, {'1':_1196}) }) }
function _1195(_env) { return  (function (_1194, __kappa) { LINKS.remoteCall(__kappa)('1195', _env, {'1':_1194}) }) }
function _1203(_env) { return  (function (_1202, __kappa) { LINKS.remoteCall(__kappa)('1203', _env, {'1':_1202}) }) }
function _1214(_env) { return  (function (_1213, __kappa) { LINKS.remoteCall(__kappa)('1214', _env, {'1':_1213}) }) }
function _1207(_env) { return  (function (_1206, __kappa) { LINKS.remoteCall(__kappa)('1207', _env, {'1':_1206}) }) }
function _1212(_env) { return  (function (_1210, _1211, __kappa) { LINKS.remoteCall(__kappa)('1212', _env, {'1':_1210, '2':_1211}) }) }
function _1217(_env) { return  (function (_1216, __kappa) { LINKS.remoteCall(__kappa)('1217', _env, {'1':_1216}) }) }
function _1220(_env) { return  (function (_1219, __kappa) { LINKS.remoteCall(__kappa)('1220', _env, {'1':_1219}) }) }
function _1224(_env) { return  (function (_1223, __kappa) { LINKS.remoteCall(__kappa)('1224', _env, {'1':_1223}) }) }
function _1247(_env) {
return  (function (_1244, _1245, _1246, __kappa) { LINKS.remoteCall(__kappa)('1247', _env, {'1':_1244, '2':_1245, '3':_1246}) })
}
function _1243(_env) { return  (function (_1242, __kappa) { LINKS.remoteCall(__kappa)('1243', _env, {'1':_1242}) }) }
function _1241(_env) { return  (function (_1240, __kappa) { LINKS.remoteCall(__kappa)('1241', _env, {'1':_1240}) }) }
function _1236(_env) { return  (function (_1235, __kappa) { LINKS.remoteCall(__kappa)('1236', _env, {'1':_1235}) }) }
function _1234(_env) { return  (function (_1233, __kappa) { LINKS.remoteCall(__kappa)('1234', _env, {'1':_1233}) }) }
function _1252(_env) { return  (function (_1250, _1251, __kappa) { LINKS.remoteCall(__kappa)('1252', _env, {'1':_1250, '2':_1251}) }) }
function _1271(_env) { return  (function (_1270, __kappa) { LINKS.remoteCall(__kappa)('1271', _env, {'1':_1270}) }) }
function _1303(_env) { return  (function (_1301, _1302, __kappa) { LINKS.remoteCall(__kappa)('1303', _env, {'1':_1301, '2':_1302}) }) }
function _1300(_env) { return  (function (_1299, __kappa) { LINKS.remoteCall(__kappa)('1300', _env, {'1':_1299}) }) }
function _1296(_env) { return  (function (_1295, __kappa) { LINKS.remoteCall(__kappa)('1296', _env, {'1':_1295}) }) }
function _1292(_env) { return  (function (_1291, __kappa) { LINKS.remoteCall(__kappa)('1292', _env, {'1':_1291}) }) }
function _1288(_env) { return  (function (_1287, __kappa) { LINKS.remoteCall(__kappa)('1288', _env, {'1':_1287}) }) }
function _1342(_env) {
return  (function (_1339, _1340, _1341, __kappa) { LINKS.remoteCall(__kappa)('1342', _env, {'1':_1339, '2':_1340, '3':_1341}) })
}
function _1338(_env) { return  (function (_1337, __kappa) { LINKS.remoteCall(__kappa)('1338', _env, {'1':_1337}) }) }
function _1334(_env) { return  (function (_1333, __kappa) { LINKS.remoteCall(__kappa)('1334', _env, {'1':_1333}) }) }
function _1330(_env) { return  (function (_1329, __kappa) { LINKS.remoteCall(__kappa)('1330', _env, {'1':_1329}) }) }
function _1318(_env) { return  (function (_1317, __kappa) { LINKS.remoteCall(__kappa)('1318', _env, {'1':_1317}) }) }
function _1316(_env) { return  (function (_1315, __kappa) { LINKS.remoteCall(__kappa)('1316', _env, {'1':_1315}) }) }
function _1328(_env) { return  (function (_1327, __kappa) { LINKS.remoteCall(__kappa)('1328', _env, {'1':_1327}) }) }
function _1345(_env) { return  (function (_1344, __kappa) { LINKS.remoteCall(__kappa)('1345', _env, {'1':_1344}) }) }
function _1366(_env) { return  (function (_1365, __kappa) { LINKS.remoteCall(__kappa)('1366', _env, {'1':_1365}) }) }
function _1403(_env) { return  (function (_1402, __kappa) { LINKS.remoteCall(__kappa)('1403', _env, {'1':_1402}) }) }
function _1401(_env) { return  (function (_1400, __kappa) { LINKS.remoteCall(__kappa)('1401', _env, {'1':_1400}) }) }
function _1397(_env) { return  (function (_1396, __kappa) { LINKS.remoteCall(__kappa)('1397', _env, {'1':_1396}) }) }
function _1393(_env) { return  (function (_1392, __kappa) { LINKS.remoteCall(__kappa)('1393', _env, {'1':_1392}) }) }
function _1388(_env) { return  (function (_1387, __kappa) { LINKS.remoteCall(__kappa)('1388', _env, {'1':_1387}) }) }
function _1380(_env) { return  (function (_1379, __kappa) { LINKS.remoteCall(__kappa)('1380', _env, {'1':_1379}) }) }
function _1411(_env) { return  (function (_1410, __kappa) { LINKS.remoteCall(__kappa)('1411', _env, {'1':_1410}) }) }
function _1408(_env) { return  (function (_1407, __kappa) { LINKS.remoteCall(__kappa)('1408', _env, {'1':_1407}) }) }
function _1416(_env) { return  (function (_1415, __kappa) { LINKS.remoteCall(__kappa)('1416', _env, {'1':_1415}) }) }
function _1428(_env) { return  (function (_1426, _1427, __kappa) { LINKS.remoteCall(__kappa)('1428', _env, {'1':_1426, '2':_1427}) }) }
function _1425(_env) { return  (function (_1423, _1424, __kappa) { LINKS.remoteCall(__kappa)('1425', _env, {'1':_1423, '2':_1424}) }) }
function _1453(_env) { return  (function (_1451, _1452, __kappa) { LINKS.remoteCall(__kappa)('1453', _env, {'1':_1451, '2':_1452}) }) }
function _1450(_env) { return  (function (_1449, __kappa) { LINKS.remoteCall(__kappa)('1450', _env, {'1':_1449}) }) }
function _1448(_env) { return  (function (_1447, __kappa) { LINKS.remoteCall(__kappa)('1448', _env, {'1':_1447}) }) }
function _1475(_env) {
return 
  (function (_1471, _1472, _1473, _1474, __kappa) { LINKS.remoteCall(__kappa)('1475', _env, {'1':_1471, '2':_1472, '3':_1473, '4':_1474}) })
}
function _1463(_env) { return  (function (_1462, __kappa) { LINKS.remoteCall(__kappa)('1463', _env, {'1':_1462}) }) }
function _1469(_env) { return  (function (_1468, __kappa) { LINKS.remoteCall(__kappa)('1469', _env, {'1':_1468}) }) }
function _1482(_env) { return  (function (_1481, __kappa) { LINKS.remoteCall(__kappa)('1482', _env, {'1':_1481}) }) }
function _1479(_env) { return  (function (_1478, __kappa) { LINKS.remoteCall(__kappa)('1479', _env, {'1':_1478}) }) }
function _1490(_env) { return  (function (_1489, __kappa) { LINKS.remoteCall(__kappa)('1490', _env, {'1':_1489}) }) }
function _1488(_env) { return  (function (_1487, __kappa) { LINKS.remoteCall(__kappa)('1488', _env, {'1':_1487}) }) }
function _1497(_env) { return  (function (_1496, __kappa) { LINKS.remoteCall(__kappa)('1497', _env, {'1':_1496}) }) }
function _1495(_env) { return  (function (_1494, __kappa) { LINKS.remoteCall(__kappa)('1495', _env, {'1':_1494}) }) }
function _1504(_env) { return  (function (_1503, __kappa) { LINKS.remoteCall(__kappa)('1504', _env, {'1':_1503}) }) }
function _1501(_env) { return  (function (_1500, __kappa) { LINKS.remoteCall(__kappa)('1501', _env, {'1':_1500}) }) }
function _1512(_env) { return  (function (_1511, __kappa) { LINKS.remoteCall(__kappa)('1512', _env, {'1':_1511}) }) }
function _1510(_env) { return  (function (returnf_1507, __kappa) { LINKS.remoteCall(__kappa)('1510', _env, {'1':returnf_1507}) }) }
function _1520(_env) { return  (function (_1519, __kappa) { LINKS.remoteCall(__kappa)('1520', _env, {'1':_1519}) }) }
function _1517(_env) { return  (function (__kappa) { LINKS.remoteCall(__kappa)('1517', _env, {}) }) }
function _1528(_env) { return  (function (_1527, __kappa) { LINKS.remoteCall(__kappa)('1528', _env, {'1':_1527}) }) }
function _1525(_env) { return  (function (__kappa) { LINKS.remoteCall(__kappa)('1525', _env, {}) }) }
function _1533(_env) { return  (function (_1531, _1532, __kappa) { LINKS.remoteCall(__kappa)('1533', _env, {'1':_1531, '2':_1532}) }) }
function _1535(_env) { return  (function (__kappa) { LINKS.remoteCall(__kappa)('1535', _env, {}) }) }
function _1541(_env) { return  (function (__kappa) { LINKS.remoteCall(__kappa)('1541', _env, {}) }) }
function _1540(_env) { return  (function (esc_1537, __kappa) { LINKS.remoteCall(__kappa)('1540', _env, {'1':esc_1537}) }) }
function _1545(_env) { return  (function (_1544, __kappa) { LINKS.remoteCall(__kappa)('1545', _env, {'1':_1544}) }) }
function _1549(_env) { return  (function (_1548, __kappa) { LINKS.remoteCall(__kappa)('1549', _env, {'1':_1548}) }) }
function _1554(_env) { return  (function (_1552, _1553, __kappa) { LINKS.remoteCall(__kappa)('1554', _env, {'1':_1552, '2':_1553}) }) }
function _1559(_env) { return  (function (_1557, _1558, __kappa) { LINKS.remoteCall(__kappa)('1559', _env, {'1':_1557, '2':_1558}) }) }
function _1562(_env) { return  (function (_1561, __kappa) { LINKS.remoteCall(__kappa)('1562', _env, {'1':_1561}) }) }
function _1570(_env) {
return  (function (_1567, _1568, _1569, __kappa) { LINKS.remoteCall(__kappa)('1570', _env, {'1':_1567, '2':_1568, '3':_1569}) })
}
function _1575(_env) { return  (function (_1574, __kappa) { LINKS.remoteCall(__kappa)('1575', _env, {'1':_1574}) }) }
function _1912(_env) { return  (main) }
function _1593(_env) { return  (function (_1592, __kappa) { LINKS.remoteCall(__kappa)('1593', _env, {'1':_1592}) }) }
function _1599(_env) { return  (function (_1598, __kappa) { LINKS.remoteCall(__kappa)('1599', _env, {'1':_1598}) }) }
function _1597(_env) { return  (function (_1596, __kappa) { LINKS.remoteCall(__kappa)('1597', _env, {'1':_1596}) }) }
function _1609(_env) { return  (function (_1608, __kappa) { LINKS.remoteCall(__kappa)('1609', _env, {'1':_1608}) }) }
function _1615(_env) { return  (function (_1614, __kappa) { LINKS.remoteCall(__kappa)('1615', _env, {'1':_1614}) }) }
function _1613(_env) { return  (function (_1612, __kappa) { LINKS.remoteCall(__kappa)('1613', _env, {'1':_1612}) }) }
function _1636(_env) { return  (function (_1634, _1635, __kappa) { LINKS.remoteCall(__kappa)('1636', _env, {'1':_1634, '2':_1635}) }) }
function _1657(_env) {
return 
  (function (_1653, _1654, _1655, _1656, __kappa) { LINKS.remoteCall(__kappa)('1657', _env, {'1':_1653, '2':_1654, '3':_1655, '4':_1656}) })
}
function _1798(_env) {
return 
  (function (_1793, _1794, _1795, _1796, _1797, __kappa) {
  LINKS.remoteCall(__kappa)('1798', _env, {'1':_1793, '2':_1794, '3':_1795, '4':_1796, '5':_1797})
  })
}
function _1674(_env) { return  (function (_1673, __kappa) { LINKS.remoteCall(__kappa)('1674', _env, {'1':_1673}) }) }
function _1692(_env) {
return 
  (function (_1688, _1689, _1690, _1691, __kappa) { LINKS.remoteCall(__kappa)('1692', _env, {'1':_1688, '2':_1689, '3':_1690, '4':_1691}) })
}
function _1705(_env) { return  (function (_1704, __kappa) { LINKS.remoteCall(__kappa)('1705', _env, {'1':_1704}) }) }
function _1700(_env) { return  (function (_1699, __kappa) { LINKS.remoteCall(__kappa)('1700', _env, {'1':_1699}) }) }
function _1778(_env) { return  (function (_1777, __kappa) { LINKS.remoteCall(__kappa)('1778', _env, {'1':_1777}) }) }
function _1773(_env) { return  (function (_1772, __kappa) { LINKS.remoteCall(__kappa)('1773', _env, {'1':_1772}) }) }
function _1729(_env) { return  (function (_1728, __kappa) { LINKS.remoteCall(__kappa)('1729', _env, {'1':_1728}) }) }
function _1756(_env) { return  (function (_1755, __kappa) { LINKS.remoteCall(__kappa)('1756', _env, {'1':_1755}) }) }
function _1805(_env) { return  (function (__kappa) { LINKS.remoteCall(__kappa)('1805', _env, {}) }) }
function _1801(_env) { return  (function (_1800, __kappa) { LINKS.remoteCall(__kappa)('1801', _env, {'1':_1800}) }) }
function _1807(_env) { return  (function (__kappa) { LINKS.remoteCall(__kappa)('1807', _env, {}) }) }
function _1813(_env) { return  (function (_1812, __kappa) { LINKS.remoteCall(__kappa)('1813', _env, {'1':_1812}) }) }
function _1814(_env) { return  (function (__kappa) { LINKS.remoteCall(__kappa)('1814', _env, {}) }) }
function _1822(_env) {
return  (function (_1819, _1820, _1821, __kappa) { LINKS.remoteCall(__kappa)('1822', _env, {'1':_1819, '2':_1820, '3':_1821}) })
}
function _1831(_env) { return  (function (__kappa) { LINKS.remoteCall(__kappa)('1831', _env, {}) }) }
function _1842(_env) {
return 
  (function (_1837, _1838, _1839, _1840, _1841, __kappa) {
  LINKS.remoteCall(__kappa)('1842', _env, {'1':_1837, '2':_1838, '3':_1839, '4':_1840, '5':_1841})
  })
}
function _1866(_env) { return  (function (__kappa) { LINKS.remoteCall(__kappa)('1866', _env, {}) }) }
function _1870(_env) { return  (function (_1869, __kappa) { LINKS.remoteCall(__kappa)('1870', _env, {'1':_1869}) }) }
function _1875(_env) { return  (function (__kappa) { LINKS.remoteCall(__kappa)('1875', _env, {}) }) }
function _1880(_env) { return  (function (_1879, __kappa) { LINKS.remoteCall(__kappa)('1880', _env, {'1':_1879}) }) }
function _1884(_env) { return  (function (_1883, __kappa) { LINKS.remoteCall(__kappa)('1884', _env, {'1':_1883}) }) }
function _1892(_env) { return  (function (__kappa) { LINKS.remoteCall(__kappa)('1892', _env, {}) }) }
function _1899(_env) { return  (function (__kappa) { LINKS.remoteCall(__kappa)('1899', _env, {}) }) }
function _1900(_env) { return  (function (__kappa) { LINKS.remoteCall(__kappa)('1900', _env, {}) }) }
function _1905(_env) { return  (function (_1904, __kappa) { LINKS.remoteCall(__kappa)('1905', _env, {'1':_1904}) }) }
function _1909(_env) { return  (function (_1908, __kappa) { LINKS.remoteCall(__kappa)('1909', _env, {'1':_1908}) }) }
function selectElem(_186, _187, __kappa) { var i_184 = _187; var xs_185 = _186; __kappa(_hd(_drop(i_184, xs_185))) }
function compose(_195, _196, __kappa) {
var g_189 = _196;
  var f_190 = _195;
  function _fun__g16_194(_193, __kappa) { var x_191 = _193; _yield(g_189, [x_191], function (_192) { _yield(f_190, [_192], __kappa) }) }
  _yieldCont(__kappa, _fun__g16_194)
}
function replicate(_200, _201, __kappa) {
var item_198 = _201;
  var n_199 = _200;
  if ( LINKS.eq(n_199, 0) ) {
  _yieldCont(__kappa, Nil)
  } else { _yield(replicate, [n_199 - 1, item_198], function (_203) { _yieldCont(__kappa, _Cons(item_198, _203)) }) }
}
function concatMap(_206, _207, __kappa) {
var l_204 = _207;
  var f_205 = _206;
  var _211 = l_204;
  if ( LINKS.eq(_211, Nil) ) {
  _yieldCont(__kappa, Nil)
  } else {
  (function (_212) {
    (function (_213) {
      var tl_210 = _213;
        var hd_209 = _212;
        _yield(f_205, [hd_209], function (_215) {
        _yield(concatMap, [f_205, tl_210], function (_214) { _yieldCont(__kappa, _Concat(_215, _214)) })
        })
      })(_tl(_211))
    })(_hd(_211))
  }
}
function filter(_222, _223, __kappa) {
var l_216 = _223;
  var p_217 = _222;
  function _fun__g17_221(_220, __kappa) {
  var x_218 = _220;
    _yield(p_217, [x_218], function (_219) { if ( _219 ) { _yieldCont(__kappa, _Cons(x_218, Nil)) } else { _yieldCont(__kappa, Nil) } })
  }
  _yield(concatMap, [_fun__g17_221, l_216], __kappa)
}
function sortBy(_227, _228, __kappa) {
var l_225 = _228;
  var f_226 = _227;
  var _232 = l_225;
  if ( LINKS.eq(_232, Nil) ) {
  _yieldCont(__kappa, Nil)
  } else {
  (function (_233) {
    (function (_234) {
      var xs_231 = _234;
        var x_230 = _233;
        function _fun__g18_239(_238, __kappa) {
        var y_235 = _238;
          _yield(f_226, [y_235], function (_237) { _yield(f_226, [x_230], function (_236) { _yieldCont(__kappa, _237 < _236) }) })
        }
        _yield(filter, [_fun__g18_239, xs_231], function (lt_240) {
        function _fun__g19_245(_244, __kappa) {
          var y_241 = _244;
            _yield(f_226, [y_241], function (_243) { _yield(f_226, [x_230], function (_242) { _yieldCont(__kappa, _243 >= _242) }) })
          }
          _yield(filter, [_fun__g19_245, xs_231], function (ge_246) {
          _yield(sortBy, [f_226, lt_240], function (_248) {
            _yield(sortBy, [f_226, ge_246], function (_247) { _yieldCont(__kappa, _Concat(_248, _Concat(_Cons(x_230, Nil), _247))) })
            })
          })
        })
      })(_tl(_232))
    })(_hd(_232))
  }
}
function sortByBase(_251, _252, __kappa) { var l_249 = _252; var f_250 = _251; _yield(sortBy, [f_250, l_249], __kappa) }
function map(_256, _257, __kappa) {
var l_254 = _257;
  var f_255 = _256;
  var _261 = l_254;
  if ( LINKS.eq(_261, Nil) ) {
  _yieldCont(__kappa, Nil)
  } else {
  (function (_262) {
    (function (_263) {
      var tl_260 = _263;
        var hd_259 = _262;
        _yield(f_255, [hd_259], function (_265) { _yield(map, [f_255, tl_260], function (_264) { _yieldCont(__kappa, _Cons(_265, _264)) }) })
      })(_tl(_261))
    })(_hd(_261))
  }
}
function init(_267, __kappa) {
var list_266 = _267;
  var _272 = list_266;
  if ( LINKS.eq(_272, Nil) ) {
  error('Internal Error: Pattern matching failed', __kappa)
  } else {
  (function (_273) {
    (function (_274) {
      if ( LINKS.eq(_274, Nil) ) {
        var y_269 = _273; _yieldCont(__kappa, Nil)
        } else { var t_271 = _274; var h_270 = _273; _yield(init, [t_271], function (_275) { _yieldCont(__kappa, _Cons(h_270, _275)) }) }
      })(_tl(_272))
    })(_hd(_272))
  }
}
function id(_277, __kappa) { var x_276 = _277; _yieldCont(__kappa, x_276) }
function uncurry(_286, __kappa) {
var f_279 = _286;
  function _fun__g20_285(_283, _284, __kappa) {
  var y_280 = _284; var x_281 = _283; _yield(f_279, [x_281], function (_282) { _yield(_282, [y_280], __kappa) })
  }
  _yieldCont(__kappa, _fun__g20_285)
}
function curry(_295, __kappa) {
var f_288 = _295;
  function _fun__g21_294(_293, __kappa) {
  var x_289 = _293;
    function _fun__g22_292(_291, __kappa) { var y_290 = _291; _yield(f_288, [x_289, y_290], __kappa) }
    _yieldCont(__kappa, _fun__g22_292)
  }
  _yieldCont(__kappa, _fun__g21_294)
}
function concat(_298, __kappa) {
var list_297 = _298;
  var _302 = list_297;
  if ( LINKS.eq(_302, Nil) ) {
  _yieldCont(__kappa, Nil)
  } else {
  (function (_303) {
    (function (_304) {
      var xs_301 = _304; var x_300 = _303; _yield(concat, [xs_301], function (_305) { _yieldCont(__kappa, _Concat(x_300, _305)) })
      })(_tl(_302))
    })(_hd(_302))
  }
}
function dropWhile(_308, _309, __kappa) {
var list_306 = _309;
  var pred_307 = _308;
  var _313 = list_306;
  if ( LINKS.eq(_313, Nil) ) {
  _yieldCont(__kappa, Nil)
  } else {
  (function (_314) {
    (function (_315) {
      var t_312 = _315;
        var h_311 = _314;
        _yield(pred_307, [h_311], function (_316) {
        var _317 = _316;
          if ( LINKS.eq(_317, true) ) {
          _yield(dropWhile, [pred_307, t_312], __kappa)
          } else {
          if ( LINKS.eq(_317, false) ) { _yieldCont(__kappa, list_306) } else { error('Internal Error: Pattern matching failed', __kappa) }
          }
        })
      })(_tl(_313))
    })(_hd(_313))
  }
}
function even(_320, __kappa) { var i_318 = _320; _yieldCont(function (_319) { _yieldCont(__kappa, LINKS.eq(_319, 0)) }, i_318 % 2) }
function first(_323, __kappa) { var p_322 = _323; _yieldCont(__kappa, p_322['1']) }
function reverse(_336, __kappa) {
var l_325 = _336;
  function aux_330(_328, _329, __kappa) {
  var o_326 = _329;
    var l_327 = _328;
    var _333 = l_327;
    if ( LINKS.eq(_333, Nil) ) {
    _yieldCont(__kappa, o_326)
    } else {
    (function (_334) {
      (function (_335) { var xs_332 = _335; var x_331 = _334; _yield(aux_330, [xs_332, _Cons(x_331, o_326)], __kappa) })(_tl(_333))
      })(_hd(_333))
    }
  }
  _yield(aux_330, [l_325, Nil], __kappa)
}
function sixth(_339, __kappa) { var p_338 = _339; _yieldCont(__kappa, p_338['6']) }
function flip(_347, __kappa) {
var f_341 = _347;
  function _fun__g23_346(_344, _345, __kappa) { var y_342 = _345; var x_343 = _344; _yield(f_341, [y_342, x_343], __kappa) }
  _yieldCont(__kappa, _fun__g23_346)
}
function third(_350, __kappa) { var p_349 = _350; _yieldCont(__kappa, p_349['3']) }
function and(_353, __kappa) {
var l_352 = _353;
  var _357 = l_352;
  if ( LINKS.eq(_357, Nil) ) {
  _yieldCont(__kappa, true)
  } else {
  (function (_358) {
    (function (_359) {
      var xs_356 = _359; var x_355 = _358; if ( x_355 ) { _yield(and, [xs_356], __kappa) } else { _yieldCont(__kappa, false) }
      })(_tl(_357))
    })(_hd(_357))
  }
}
function any(_362, _363, __kappa) {
var l_360 = _363;
  var p_361 = _362;
  var _367 = l_360;
  if ( LINKS.eq(_367, Nil) ) {
  _yieldCont(__kappa, false)
  } else {
  (function (_368) {
    (function (_369) {
      var xs_366 = _369;
        var x_365 = _368;
        _yield(p_361, [x_365], function (_370) { if ( _370 ) { _yieldCont(__kappa, true) } else { _yield(any, [p_361, xs_366], __kappa) } })
      })(_tl(_367))
    })(_hd(_367))
  }
}
function second(_372, __kappa) { var p_371 = _372; _yieldCont(__kappa, p_371['2']) }
function fold_right(_377, _378, _379, __kappa) {
var l_374 = _379;
  var u_375 = _378;
  var f_376 = _377;
  var _383 = l_374;
  if ( LINKS.eq(_383, Nil) ) {
  _yieldCont(__kappa, u_375)
  } else {
  (function (_384) {
    (function (_385) {
      var xs_382 = _385; var x_381 = _384; _yield(fold_right, [f_376, u_375, xs_382], function (_386) { _yield(f_376, [x_381, _386], __kappa) })
      })(_tl(_383))
    })(_hd(_383))
  }
}
function fold_right1(_391, _392, __kappa) {
var l_387 = _392;
  var f_388 = _391;
  (function (_389) { (function (_390) { _yield(fold_right, [f_388, _389, _390], __kappa) })(_tl(l_387)) })(_hd(l_387))
}
function substAt(_397, _398, _399, __kappa) {
var y_394 = _399;
  var i_395 = _398;
  var xs_396 = _397;
  var _403 = xs_396;
  if ( LINKS.eq(_403, Nil) ) {
  _yieldCont(__kappa, Nil)
  } else {
  (function (_404) {
    (function (_405) {
      var xs_402 = _405;
        var x_401 = _404;
        if ( i_395 <= 0 ) {
        _yieldCont(__kappa, _Cons(y_394, xs_402))
        } else { _yield(substAt, [xs_402, i_395 - 1, y_394], function (_406) { _yieldCont(__kappa, _Cons(x_401, _406)) }) }
      })(_tl(_403))
    })(_hd(_403))
  }
}
function swap(_414, _415, _416, __kappa) {
var x2_407 = _416;
  var x1_408 = _415;
  var xs_409 = _414;
  function _fun__g24_413(_412, __kappa) {
  var _for__g1_410 = _412;
    var x_411 = _412;
    if ( LINKS.eq(x_411, x1_408) ) {
    _yieldCont(__kappa, _Cons(x2_407, Nil))
    } else { if ( LINKS.eq(x_411, x2_407) ) { _yieldCont(__kappa, _Cons(x1_408, Nil)) } else { _yieldCont(__kappa, _Cons(x_411, Nil)) } }
  }
  _yield(concatMap, [_fun__g24_413, xs_409], __kappa)
}
function fold_left(_421, _422, _423, __kappa) {
var l_418 = _423;
  var u_419 = _422;
  var p_420 = _421;
  var _427 = l_418;
  if ( LINKS.eq(_427, Nil) ) {
  _yieldCont(__kappa, u_419)
  } else {
  (function (_428) {
    (function (_429) {
      var xs_426 = _429; var x_425 = _428; _yield(p_420, [u_419, x_425], function (_430) { _yield(fold_left, [p_420, _430, xs_426], __kappa) })
      })(_tl(_427))
    })(_hd(_427))
  }
}
function _bang_bang(_433, _434, __kappa) { var n_431 = _434; var xs_432 = _433; _yield(selectElem, [xs_432, n_431], __kappa) }
function fourth(_437, __kappa) { var p_436 = _437; _yieldCont(__kappa, p_436['4']) }
function product(_440, __kappa) {
var ns_439 = _440; _yield(fold_left, [function (x, y, __kappa) { _yieldCont(__kappa, x * y) }, 1, ns_439], __kappa)
}
function ninth(_443, __kappa) { var p_442 = _443; _yieldCont(__kappa, p_442['9']) }
function mapi(_459, _460, __kappa) {
var xs_445 = _460;
  var f_446 = _459;
  function mi_451(_449, _450, __kappa) {
  var xs_447 = _450;
    var i_448 = _449;
    var _454 = xs_447;
    if ( LINKS.eq(_454, Nil) ) {
    _yieldCont(__kappa, Nil)
    } else {
    (function (_455) {
      (function (_456) {
        var xs_453 = _456;
          var x_452 = _455;
          _yield(f_446, [x_452, i_448], function (_458) {
          _yield(mi_451, [i_448 + 1, xs_453], function (_457) { _yieldCont(__kappa, _Cons(_458, _457)) })
          })
        })(_tl(_454))
      })(_hd(_454))
    }
  }
  _yield(mi_451, [0, xs_445], __kappa)
}
function or(_463, __kappa) {
var l_462 = _463;
  var _467 = l_462;
  if ( LINKS.eq(_467, Nil) ) {
  _yieldCont(__kappa, false)
  } else {
  (function (_468) {
    (function (_469) {
      var xs_466 = _469; var x_465 = _468; if ( x_465 ) { _yieldCont(__kappa, true) } else { _yield(or, [xs_466], __kappa) }
      })(_tl(_467))
    })(_hd(_467))
  }
}
function elem(_472, _473, __kappa) {
var l_470 = _473;
  var x_471 = _472;
  if ( _not(LINKS.eq(l_470, Nil)) ) {
  (function (_476) {
    if ( LINKS.eq(x_471, _476) ) { _yieldCont(__kappa, true) } else { (function (_475) { _yield(elem, [x_471, _475], __kappa) })(_tl(l_470)) }
    })(_hd(l_470))
  } else { _yieldCont(__kappa, false) }
}
function last(_478, __kappa) {
var list_477 = _478;
  var _483 = list_477;
  if ( LINKS.eq(_483, Nil) ) {
  error('Internal Error: Pattern matching failed', __kappa)
  } else {
  (function (_484) {
    (function (_485) {
      if ( LINKS.eq(_485, Nil) ) {
        var x_480 = _484; _yieldCont(__kappa, x_480)
        } else { var t_482 = _485; var h_481 = _484; _yield(last, [t_482], __kappa) }
      })(_tl(_483))
    })(_hd(_483))
  }
}
function assert(_489, _490, __kappa) {
var msg_486 = _490;
  var ok_487 = _489;
  var _kappa1916 = function (_488) { _yieldCont(__kappa, {}) };
  if ( _not(ok_487) ) { _kappa1916(_debug('assertion failed: ' + msg_486)) } else { _yieldCont(_kappa1916, {}) }
}
function abs(_493, __kappa) { var i_492 = _493; if ( i_492 < 0 ) { _yieldCont(__kappa, - i_492) } else { _yieldCont(__kappa, i_492) } }
function ignore(_495, __kappa) { _yieldCont(__kappa, {}) }
function curtail(_498, __kappa) { var list_497 = _498; _yield(init, [list_497], __kappa) }
function fold_left1(_504, _505, __kappa) {
var l_500 = _505;
  var p_501 = _504;
  (function (_502) { (function (_503) { _yield(fold_left, [p_501, _502, _503], __kappa) })(_tl(l_500)) })(_hd(l_500))
}
function zip(_509, _510, __kappa) {
var r_507 = _510;
  var l_508 = _509;
  var _516 = {'2':r_507, '1':l_508};
  var _518 = _516['1'];
  var _519 = _516['2'];
  if ( LINKS.eq(_518, Nil) ) {
  if ( LINKS.eq(_519, Nil) ) { _yieldCont(__kappa, Nil) } else { _yieldCont(__kappa, Nil) }
  } else {
  (function (_520) {
    (function (_521) {
      if ( LINKS.eq(_519, Nil) ) {
        _yieldCont(__kappa, Nil)
        } else {
        (function (_522) {
          (function (_523) {
            var rt_513 = _523;
              var rh_512 = _522;
              var lt_515 = _521;
              var lh_514 = _520;
              _yield(zip, [lt_515, rt_513], function (_524) { _yieldCont(__kappa, _Cons({'2':rh_512, '1':lh_514}, _524)) })
            })(_tl(_519))
          })(_hd(_519))
        }
      })(_tl(_518))
    })(_hd(_518))
  }
}
function all(_527, _528, __kappa) {
var l_525 = _528;
  var p_526 = _527;
  var _532 = l_525;
  if ( LINKS.eq(_532, Nil) ) {
  _yieldCont(__kappa, true)
  } else {
  (function (_533) {
    (function (_534) {
      var xs_531 = _534;
        var x_530 = _533;
        _yield(p_526, [x_530], function (_535) { if ( _535 ) { _yield(all, [p_526, xs_531], __kappa) } else { _yieldCont(__kappa, false) } })
      })(_tl(_532))
    })(_hd(_532))
  }
}
function the(_542, __kappa) {
var l_536 = _542;
  (function (x_537) {
  function _fun__g25_540(_539, __kappa) { var y_538 = _539; _yieldCont(__kappa, LINKS.eq(x_537, y_538)) }
    _yield(all, [_fun__g25_540, l_536], function (_541) {
    if ( _541 ) { _yieldCont(__kappa, x_537) } else { __kappa(_error('list argument to \'the\' had varying values.')) }
    })
  })(_hd(l_536))
}
function sum(_545, __kappa) {
var ns_544 = _545; _yield(fold_left, [function (x, y, __kappa) { _yieldCont(__kappa, x + y) }, 0, ns_544], __kappa)
}
function odd(_549, __kappa) { var i_547 = _549; _yieldCont(function (_548) { _yieldCont(__kappa, LINKS.eq(_548, 1)) }, i_547 % 2) }
function seventh(_552, __kappa) { var p_551 = _552; _yieldCont(__kappa, p_551['7']) }
function tenth(_555, __kappa) { var p_554 = _555; _yieldCont(__kappa, p_554['10']) }
function unzip(_558, __kappa) {
var l_557 = _558;
  var _563 = l_557;
  if ( LINKS.eq(_563, Nil) ) {
  _yieldCont(__kappa, {'2':Nil, '1':Nil})
  } else {
  (function (_564) {
    (function (_565) {
      var _567 = _564['1'];
        var _568 = _564['2'];
        var xs_562 = _565;
        var b_560 = _568;
        var a_561 = _567;
        _yield(unzip, [xs_562], function (_571) {
        var d_569 = _571['2']; var c_570 = _571['1']; _yieldCont(__kappa, {'2':_Cons(b_560, d_569), '1':_Cons(a_561, c_570)})
        })
      })(_tl(_563))
    })(_hd(_563))
  }
}
function join(_574, _575, __kappa) {
var list_572 = _575;
  var glue_573 = _574;
  var _580 = list_572;
  if ( LINKS.eq(_580, Nil) ) {
  _yieldCont(__kappa, Nil)
  } else {
  (function (_581) {
    (function (_582) {
      if ( LINKS.eq(_582, Nil) ) {
        var x_577 = _581; _yieldCont(__kappa, x_577)
        } else {
        var xs_579 = _582;
          var x_578 = _581;
          _yield(join, [glue_573, xs_579], function (_583) { _yieldCont(__kappa, _Concat(x_578, _Concat(glue_573, _583))) })
        }
      })(_tl(_580))
    })(_hd(_580))
  }
}
function fifth(_585, __kappa) { var p_584 = _585; _yieldCont(__kappa, p_584['5']) }
function eighth(_588, __kappa) { var p_587 = _588; _yieldCont(__kappa, p_587['8']) }
function intRange(_599, _600, __kappa) {
var n_590 = _600;
  var m_591 = _599;
  function intRangeAux_598(_595, _596, _597, __kappa) {
  var result_592 = _597;
    var n_593 = _596;
    var m_594 = _595;
    if ( m_594 > n_593 ) {
    _yieldCont(__kappa, result_592)
    } else { _yield(intRangeAux_598, [m_594, n_593 - 1, _Cons(n_593, result_592)], __kappa) }
  }
  _yield(intRangeAux_598, [m_591, n_590, Nil], __kappa)
}
function takeWhile(_604, _605, __kappa) {
var list_602 = _605;
  var pred_603 = _604;
  var _609 = list_602;
  if ( LINKS.eq(_609, Nil) ) {
  _yieldCont(__kappa, Nil)
  } else {
  (function (_610) {
    (function (_611) {
      var t_608 = _611;
        var h_607 = _610;
        _yield(pred_603, [h_607], function (_612) {
        var _613 = _612;
          if ( LINKS.eq(_613, true) ) {
          _yield(takeWhile, [pred_603, t_608], function (_614) { _yieldCont(__kappa, _Cons(h_607, _614)) })
          } else {
          if ( LINKS.eq(_613, false) ) { _yieldCont(__kappa, Nil) } else { error('Internal Error: Pattern matching failed', __kappa) }
          }
        })
      })(_tl(_609))
    })(_hd(_609))
  }
}
function empty(_617, __kappa) {
var l_615 = _617; var _616 = l_615; if ( LINKS.eq(_616, Nil) ) { _yieldCont(__kappa, true) } else { _yieldCont(__kappa, false) }
}
function fromMaybe(_625, _626, __kappa) {
var y_619 = _626;
  var x_620 = _625;
  var _622 = x_620;
  switch (_622._label) {case 'Just': {var _623 = _622._value; var x_621 = _623; _yieldCont(__kappa, x_621); break;}
   case 'Nothing': {var _624 = _622._value; _yieldCont(__kappa, y_619); break;}
  }
}
function isInt(_629, __kappa) {
var x_628 = _629;
  _yieldCont(__kappa, _tilde(x_628, {'_label':'Seq', '_value':_Cons({'_label':'Repeat', '_value':{'2':{'_label':'Simply', '_value':'-'}, '1':{'_label':'Question', '_value':{}}}}, _Cons({'_label':'Repeat', '_value':{'2':{'_label':'Range', '_value':{'2':{'_c':'9'}, '1':{'_c':'0'}}}, '1':{'_label':'Plus', '_value':{}}}}, _Cons({'_label':'EndAnchor', '_value':{}}, Nil)))}))
}
function isJust(_636, __kappa) {
var x_631 = _636;
  var _633 = x_631;
  switch (_633._label) {case 'Just': {var _634 = _633._value; var x_632 = _634; _yieldCont(__kappa, true); break;}
   case 'Nothing': {var _635 = _633._value; _yieldCont(__kappa, false); break;}
  }
}
function isFloat(_639, __kappa) {
var x_638 = _639;
  _yieldCont(__kappa, _tilde(x_638, {'_label':'Seq', '_value':_Cons({'_label':'Repeat', '_value':{'2':{'_label':'Simply', '_value':'-'}, '1':{'_label':'Question', '_value':{}}}}, _Cons({'_label':'Repeat', '_value':{'2':{'_label':'Range', '_value':{'2':{'_c':'9'}, '1':{'_c':'0'}}}, '1':{'_label':'Plus', '_value':{}}}}, _Cons({'_label':'Repeat', '_value':{'2':{'_label':'Group', '_value':{'_label':'Seq', '_value':_Cons({'_label':'Quote', '_value':{'_label':'Simply', '_value':'.'}}, _Cons({'_label':'Repeat', '_value':{'2':{'_label':'Range', '_value':{'2':{'_c':'9'}, '1':{'_c':'0'}}}, '1':{'_label':'Plus', '_value':{}}}}, Nil))}}, '1':{'_label':'Question', '_value':{}}}}, _Cons({'_label':'EndAnchor', '_value':{}}, Nil))))}))
}
function search(_643, _644, __kappa) {
var l_641 = _644;
  var p_642 = _643;
  var _648 = l_641;
  if ( LINKS.eq(_648, Nil) ) {
  _yieldCont(__kappa, {'_label':'Nothing', '_value':{}})
  } else {
  (function (_649) {
    (function (_650) {
      var xs_647 = _650;
        var x_646 = _649;
        _yield(p_642, [x_646], function (_651) {
        if ( _651 ) { _yieldCont(__kappa, {'_label':'Just', '_value':x_646}) } else { _yield(search, [p_642, xs_647], __kappa) }
        })
      })(_tl(_648))
    })(_hd(_648))
  }
}
function assoc(_654, _655, __kappa) {
var l_652 = _655;
  var x_653 = _654;
  var _660 = l_652;
  if ( LINKS.eq(_660, Nil) ) {
  (function (_666) { _yieldCont(__kappa, _666) })(_error('Not found ' + x_653))
  } else {
  (function (_661) {
    (function (_662) {
      var _664 = _661['1'];
        var _665 = _661['2'];
        var xs_659 = _662;
        var v_657 = _665;
        var k_658 = _664;
        if ( LINKS.eq(k_658, x_653) ) { _yieldCont(__kappa, v_657) } else { _yield(assoc, [x_653, xs_659], __kappa) }
      })(_tl(_660))
    })(_hd(_660))
  }
}
function memassoc(_669, _670, __kappa) {
var l_667 = _670;
  var x_668 = _669;
  var _675 = l_667;
  if ( LINKS.eq(_675, Nil) ) {
  _yieldCont(__kappa, false)
  } else {
  (function (_676) {
    (function (_677) {
      var _679 = _676['1'];
        var _680 = _676['2'];
        var xs_674 = _677;
        var v_672 = _680;
        var k_673 = _679;
        if ( LINKS.eq(k_673, x_668) ) { _yieldCont(__kappa, true) } else { _yield(memassoc, [x_668, xs_674], __kappa) }
      })(_tl(_675))
    })(_hd(_675))
  }
}
function assocAll(_683, _684, __kappa) {
var l_681 = _684;
  var x_682 = _683;
  var _689 = l_681;
  if ( LINKS.eq(_689, Nil) ) {
  _yieldCont(__kappa, Nil)
  } else {
  (function (_690) {
    (function (_691) {
      var _693 = _690['1'];
        var _694 = _690['2'];
        var xs_688 = _691;
        var v_686 = _694;
        var k_687 = _693;
        if ( LINKS.eq(k_687, x_682) ) {
        _yield(assocAll, [x_682, xs_688], function (_695) { _yieldCont(__kappa, _Cons(v_686, _695)) })
        } else { _yield(assocAll, [x_682, xs_688], __kappa) }
      })(_tl(_689))
    })(_hd(_689))
  }
}
function lookup(_698, _699, __kappa) {
var l_696 = _699;
  var x_697 = _698;
  var _704 = l_696;
  if ( LINKS.eq(_704, Nil) ) {
  _yieldCont(__kappa, {'_label':'Nothing', '_value':{}})
  } else {
  (function (_705) {
    (function (_706) {
      var _708 = _705['1'];
        var _709 = _705['2'];
        var xs_703 = _706;
        var b_701 = _709;
        var a_702 = _708;
        if ( LINKS.eq(a_702, x_697) ) {
        _yieldCont(__kappa, {'_label':'Just', '_value':b_701})
        } else { _yield(lookup, [x_697, xs_703], __kappa) }
      })(_tl(_704))
    })(_hd(_704))
  }
}
function find(_712, _713, __kappa) {
var l_710 = _713;
  var p_711 = _712;
  var _717 = l_710;
  if ( LINKS.eq(_717, Nil) ) {
  (function (_721) { _yieldCont(__kappa, _721) })(_error('Not_found'))
  } else {
  (function (_718) {
    (function (_719) {
      var xs_716 = _719;
        var x_715 = _718;
        _yield(p_711, [x_715], function (_720) { if ( _720 ) { _yieldCont(__kappa, x_715) } else { _yield(find, [p_711, xs_716], __kappa) } })
      })(_tl(_717))
    })(_hd(_717))
  }
}
function removeAssoc(_724, _725, __kappa) {
var l_722 = _725;
  var x_723 = _724;
  var _730 = l_722;
  if ( LINKS.eq(_730, Nil) ) {
  _yieldCont(__kappa, Nil)
  } else {
  (function (_731) {
    (function (_732) {
      var _734 = _731['1'];
        var _735 = _731['2'];
        var xs_729 = _732;
        var v_727 = _735;
        var k_728 = _734;
        if ( LINKS.eq(k_728, x_723) ) {
        _yieldCont(__kappa, xs_729)
        } else { _yield(removeAssoc, [x_723, xs_729], function (_736) { _yieldCont(__kappa, _Cons({'2':v_727, '1':k_728}, _736)) }) }
      })(_tl(_730))
    })(_hd(_730))
  }
}
function fromJust(_742, __kappa) {
var x_737 = _742;
  var _739 = x_737;
  switch (_739._label) {case 'Just': {var _740 = _739._value; var x_738 = _740; _yieldCont(__kappa, x_738); break;}
   case 'Nothing': {var _741 = _739._value; __kappa(_error('fromJust failed')); break;}
  }
}
function asList(_750, __kappa) { LINKS.remoteCall(__kappa)('751', {}, {'1':_750}) }
var multipart = _Cons({'2':'multipart/form-data', '1':'enctype'}, Nil);
function runState(_755, _756, __kappa) { var i_753 = _756; var s_754 = _755; _yield(s_754, [i_753], __kappa) }
function _greaterthan_greaterthan_equals(_767, _768, __kappa) {
var f_758 = _768;
  var x_759 = _767;
  function _fun__g27_766(_765, __kappa) {
  var s1_760 = _765;
    _yield(x_759, [s1_760], function (_763) {
    var s2_761 = _763['2']; var v_762 = _763['1']; _yield(f_758, [v_762], function (_764) { _yield(runState, [_764, s2_761], __kappa) })
    })
  }
  _yieldCont(__kappa, _fun__g27_766)
}
function pureE(_771, __kappa) { var x_770 = _771; _yieldCont(__kappa, {'_label':'Just', '_value':x_770}) }
function _return(_777, __kappa) {
var a_773 = _777;
  function _fun__g28_776(_775, __kappa) { var s_774 = _775; _yieldCont(__kappa, {'2':s_774, '1':a_773}) }
  _yieldCont(__kappa, _fun__g28_776)
}
function xml(_784, __kappa) {
var x_779 = _784;
  function _fun__g29_783(_782, __kappa) {
  var e_780 = _782; _yield(pureE, [{}], function (_781) { _yieldCont(__kappa, {'2':_781, '1':x_779}) })
  }
  _yield(_return, [{'2':_fun__g29_783, '1':x_779}], __kappa)
}
function text(_787, __kappa) { var s_786 = _787; _yield(xml, [_stringToXml(s_786)], __kappa) }
function plug(_803, _804, __kappa) {
var m_789 = _804;
  var f_790 = _803;
  function _fun__g31_802(_801, __kappa) {
  var collector_791 = _801['2'];
    var x_792 = _801['1'];
    _yield(f_790, [x_792], function (_800) {
    function _fun__g30_799(_798, __kappa) {
      var env_793 = _798;
        _yield(collector_791, [env_793], function (_796) {
        var c_794 = _796['2']; var xml_795 = _796['1']; _yield(f_790, [xml_795], function (_797) { _yieldCont(__kappa, {'2':c_794, '1':_797}) })
        })
      }
      _yield(_return, [{'2':_fun__g30_799, '1':_800}], __kappa)
    })
  }
  _yield(_greaterthan_greaterthan_equals, [m_789, _fun__g31_802], __kappa)
}
function applyE(_819, _820, __kappa) {
var x_806 = _820;
  var f_807 = _819;
  var _810 = {'2':x_806, '1':f_807};
  var _812 = _810['1'];
  var _813 = _810['2'];
  switch (_812._label) {case 'Just': {var _814 = _812._value; switch (_813._label) {case 'Just': {var _815 = _813._value; var x_808 = _815;
  var f_809 = _814;
  _yield(f_809, [x_808], function (_816) { _yieldCont(__kappa, {'_label':'Just', '_value':_816}) }); break;}
   default: {var _817 = _813; _yieldCont(__kappa, {'_label':'Nothing', '_value':{}}); break;}
  }; break;}
   default: {var _818 = _812; _yieldCont(__kappa, {'_label':'Nothing', '_value':{}}); break;}
  }
}
function fmap(_825, _826, __kappa) {
var x_822 = _826;
  var f_823 = _825;
  _yield(compose, [_return, f_823], function (_824) { _yield(_greaterthan_greaterthan_equals, [x_822, _824], __kappa) })
}
function _monkey_monkey_monkey(_848, _849, __kappa) {
var k_828 = _849;
  var m_829 = _848;
  function _fun__g34_847(_846, __kappa) {
  var c_830 = _846['2'];
    var x_831 = _846['1'];
    function _fun__g33_845(_844, __kappa) {
    var f_832 = _844['2'];
      var y_833 = _844['1'];
      function _fun__g32_843(_842, __kappa) {
      var e_834 = _842;
        _yield(f_832, [e_834], function (_837) {
        var f_835 = _837['2'];
          var fx_836 = _837['1'];
          _yield(c_830, [e_834], function (_840) {
          var c_838 = _840['2'];
            var cx_839 = _840['1'];
            _yield(applyE, [f_835, c_838], function (_841) { _yieldCont(__kappa, {'2':_841, '1':_Concat(cx_839, fx_836)}) })
          })
        })
      }
      _yield(_return, [{'2':_fun__g32_843, '1':_Concat(x_831, y_833)}], __kappa)
    }
    _yield(_greaterthan_greaterthan_equals, [k_828, _fun__g33_845], __kappa)
  }
  _yield(_greaterthan_greaterthan_equals, [m_829, _fun__g34_847], __kappa)
}
function pure(_854, __kappa) {
var v_851 = _854;
  function _fun__g35_853(_852, __kappa) { _yieldCont(__kappa, {'2':{'_label':'Just', '_value':v_851}, '1':Nil}) }
  _yield(_return, [{'2':_fun__g35_853, '1':Nil}], __kappa)
}
function _monkey_monkey_equals_greaterthan(_859, _860, __kappa) {
var f_856 = _860; var m_857 = _859; _yield(pure, [f_856], function (_858) { _yield(_monkey_monkey_monkey, [m_857, _858], __kappa) })
}
function _dollar(_864, _865, __kappa) { var x_862 = _865; var f_863 = _864; _yield(f_863, [x_862], __kappa) }
function makeName(_868, __kappa) { var i_867 = _868; _yieldCont(__kappa, 'input_' + _intToString(i_867)) }
function _fun__g36_873(_872, __kappa) {
var i_870 = _872; _yield(makeName, [i_870], function (_871) { _yieldCont(__kappa, {'2':i_870 + 1, '1':_871}) })
}
var nextName = _fun__g36_873;
function sequence(_876, __kappa) {
var xs_875 = _876;
  var _880 = xs_875;
  if ( LINKS.eq(_880, Nil) ) {
  _yield(_return, [Nil], function (_890) { _yieldCont(__kappa, _890) })
  } else {
  (function (_881) {
    (function (_882) {
      var xs_879 = _882;
        var x_878 = _881;
        function _fun__g38_889(_888, __kappa) {
        var x_883 = _888;
          _yield(sequence, [xs_879], function (_887) {
          function _fun__g37_886(_885, __kappa) { var xs_884 = _885; _yield(_return, [_Cons(x_883, xs_884)], __kappa) }
            _yield(_greaterthan_greaterthan_equals, [_887, _fun__g37_886], __kappa)
          })
        }
        _yield(_greaterthan_greaterthan_equals, [x_878, _fun__g38_889], __kappa)
      })(_tl(_880))
    })(_hd(_880))
  }
}
function formlets(_892, __kappa) {
var fs_891 = _892;
  var _897 = fs_891;
  if ( LINKS.eq(_897, Nil) ) {
  _yield(xml, [Nil], function (_930) {
    function _fun__g39_932(_931, __kappa) { _yieldCont(__kappa, Nil) }
      _yield(pure, [_fun__g39_932], function (_933) { _yield(_monkey_monkey_monkey, [_930, _933], __kappa) })
    })
  } else {
  (function (_898) {
    (function (_899) {
      if ( LINKS.eq(_899, Nil) ) {
        var x_894 = _898;
          function _fun__g40_922(_921, __kappa) { var _formlet__g9_919 = _921; var f_920 = _921; _yieldCont(__kappa, _formlet__g9_919) }
          _yield(pure, [_fun__g40_922], function (_923) {
          _yield(_monkey_monkey_monkey, [x_894, _923], function (_924) {
            function _fun__g41_928(_927, __kappa) { var _formlet__g8_925 = _927; var f_926 = _927; _yieldCont(__kappa, _Cons(f_926, Nil)) }
              _yield(pure, [_fun__g41_928], function (_929) { _yield(_monkey_monkey_monkey, [_924, _929], __kappa) })
            })
          })
        } else {
        var xs_896 = _899;
          var x_895 = _898;
          _yield(formlets, [xs_896], function (_900) {
          function _fun__g42_908(_907, __kappa) {
            var _formlet__g13_901 = _907;
              var fs_902 = _907;
              function _fun__g43_906(_905, __kappa) {
              var _formlet__g12_903 = _905; var f_904 = _905; _yieldCont(__kappa, {'2':_formlet__g13_901, '1':_formlet__g12_903})
              }
              _yieldCont(__kappa, _fun__g43_906)
            }
            _yield(pure, [_fun__g42_908], function (_909) {
            _yield(_monkey_monkey_monkey, [_900, _909], function (_910) {
              _yield(_monkey_monkey_monkey, [x_895, _910], function (_911) {
                function _fun__g44_917(_916, __kappa) {
                  var _formlet__g11_912 = _916['2'];
                    var fs_913 = _916['2'];
                    var _formlet__g10_914 = _916['1'];
                    var f_915 = _916['1'];
                    _yieldCont(__kappa, _Cons(f_915, fs_913))
                  }
                  _yield(pure, [_fun__g44_917], function (_918) { _yield(_monkey_monkey_monkey, [_911, _918], __kappa) })
                })
              })
            })
          })
        }
      })(_tl(_897))
    })(_hd(_897))
  }
}
function _fun__g45_938(_937, __kappa) {
if ( LINKS.eq(_937, Nil) ) { _yieldCont(__kappa, Nil) } else { error('Internal Error: Pattern matching failed', __kappa) }
}
function _fun__g46_936(_935, __kappa) { var gen_934 = _935; _yieldCont(__kappa, {'2':gen_934, '1':Nil}) }
var unitP = {'3':_fun__g46_936, '2':_fun__g45_938, '1':0};
function bodyP(_946, __kappa) {
var xml_940 = _946;
  function _fun__g47_945(_944, __kappa) {
  if ( LINKS.eq(_944, Nil) ) { _yieldCont(__kappa, xml_940) } else { error('Internal Error: Pattern matching failed', __kappa) }
  }
  function _fun__g48_943(_942, __kappa) { var gen_941 = _942; _yieldCont(__kappa, {'2':gen_941, '1':Nil}) }
  _yieldCont(__kappa, {'3':_fun__g48_943, '2':_fun__g47_945, '1':0})
}
function pickleCont(_950, __kappa) {
var cont_948 = _950; function _fun__g49_949(__kappa) { _yield(cont_948, [], __kappa) } _yield(unsafePickleCont, [_fun__g49_949], __kappa)
}
function mkForm(_958, _959, _960, __kappa) {
var attributes_952 = _960;
  var contents_953 = _959;
  var cont_954 = _958;
  function _fun__g50_956(__kappa) { (function (_955) { _yield(cont_954, [_955], __kappa) })(_environment()) }
  (function (_957) {
  _yieldCont(__kappa, _addAttributes(LINKS.XML('form', {'method':'POST', 'enctype':'application/x-www-form-urlencoded', 'action':'#'}, [_stringToXml('\n    '), LINKS.XML('input', {'value':_957, 'type':'hidden', 'name':'_k'}, []), _stringToXml('\n    '), contents_953, _stringToXml('\n  ')]), attributes_952))
  })(_Concat('BAG8BwC8Bw==&_jsonArgs=', stringifyB64({'954':cont_954})))
}
function validate(_968, _969, _970, _971, _972, _973, __kappa) {
var attributes_962 = _973;
  var i_963 = _972;
  var zs_964 = _971;
  var k_965 = _970;
  var h_966 = _969;
  var c_967 = _968;
  function _fun__g52_994(_993, __kappa) {
  var env_975 = _993;
    _yield(c_967, [env_975], function (_978) {
    var v_976 = _978['2'];
      var xml_977 = _978['1'];
      var _980 = v_976;
      switch (_980._label) {case 'Just': {var _981 = _980._value; var v_979 = _981; _yield(h_966, [v_979], __kappa); break;}
       case 'Nothing': {var _982 = _980._value; function z_986(_985, __kappa) {
      var zs_983 = _985;
        _yield(validate, [c_967, h_966, k_965, zs_983, i_963, attributes_962], function (_984) {
        _yield(mkForm, [_984, xml_977, attributes_962], __kappa)
        })
      }
      _yield(substAt, [zs_964, i_963, z_986], function (zs_987) {
      function _fun__g51_990(_989, __kappa) { var z_988 = _989; _yield(z_988, [zs_987], __kappa) }
        _yield(map, [_fun__g51_990, zs_987], function (_991) { _yield(k_965, [_991], function (_992) { _yield(bodyP, [_992], __kappa) }) })
      }); break;}
      }
    })
  }
  _yieldCont(__kappa, _fun__g52_994)
}
function joinP(_1015, _1016, __kappa) {
var fs2_995 = _1016['3'];
  var k2_996 = _1016['2'];
  var i2_997 = _1016['1'];
  var fs1_998 = _1015['3'];
  var k1_999 = _1015['2'];
  var i1_1000 = _1015['1'];
  function _fun__g53_1014(_1013, __kappa) {
  var xs_1010 = _1013;
    _yield(k1_999, [_take(i1_1000, xs_1010)], function (_1011) {
    _yield(k2_996, [_drop(i1_1000, xs_1010)], function (_1012) { _yieldCont(__kappa, _Concat(_1011, _1012)) })
    })
  }
  function _fun__g54_1009(_1008, __kappa) {
  var gen_1001 = _1008;
    _yield(fs1_998, [gen_1001], function (_1004) {
    var gen_1002 = _1004['2'];
      var ms1_1003 = _1004['1'];
      _yield(fs2_995, [gen_1002], function (_1007) {
      var gen_1005 = _1007['2']; var ms2_1006 = _1007['1']; _yieldCont(__kappa, {'2':gen_1005, '1':_Concat(ms1_1003, ms2_1006)})
      })
    })
  }
  _yieldCont(__kappa, {'3':_fun__g54_1009, '2':_fun__g53_1014, '1':i1_1000 + i2_997})
}
function joinManyP(_1019, __kappa) { var gs_1018 = _1019; _yield(fold_left, [joinP, unitP, gs_1018], __kappa) }
function renderPagePure(_1039, __kappa) {
var fs_1021 = _1039['3'];
  var k_1022 = _1039['2'];
  var n_1023 = _1039['1'];
  _yield(fs_1021, [0], function (_1025) {
  var ms_1024 = _1025['1'];
    function _fun__g55_1033(_1031, _1032, __kappa) {
    var i_1026 = _1032;
      var m_1027 = _1031;
      function _fun__g56_1030(_1029, __kappa) { var zs_1028 = _1029; _yield(m_1027, [k_1022, zs_1028, i_1026], __kappa) }
      _yieldCont(__kappa, _fun__g56_1030)
    }
    _yield(mapi, [_fun__g55_1033, ms_1024], function (zs_1034) {
    function _fun__g57_1037(_1036, __kappa) { var z_1035 = _1036; _yield(z_1035, [zs_1034], __kappa) }
      _yield(map, [_fun__g57_1037, zs_1034], function (_1038) { _yield(k_1022, [_1038], __kappa) })
    })
  })
}
function plugP(_1049, _1050, __kappa) {
var fs_1041 = _1050['3'];
  var k_1042 = _1050['2'];
  var i_1043 = _1050['1'];
  var context_1044 = _1049;
  function _fun__g58_1048(_1047, __kappa) {
  var xs_1045 = _1047; _yield(k_1042, [xs_1045], function (_1046) { _yield(context_1044, [_1046], __kappa) })
  }
  _yieldCont(__kappa, {'3':fs_1041, '2':_fun__g58_1048, '1':i_1043})
}
function renderPage(_1053, __kappa) { var p_1052 = _1053; _yield(renderPagePure, [p_1052], __kappa) }
function mkCheckedFormBuilder(_1067, _1068, _1069, _1070, __kappa) {
var attributes_1055 = _1070;
  var h_1056 = _1069;
  var c_1057 = _1068;
  var x_1058 = _1067;
  function _fun__g59_1066(_1063, _1064, _1065, __kappa) {
  var i_1059 = _1065;
    var zs_1060 = _1064;
    var k_1061 = _1063;
    _yield(validate, [c_1057, h_1056, k_1061, zs_1060, i_1059, attributes_1055], function (_1062) {
    _yield(mkForm, [_1062, x_1058, attributes_1055], __kappa)
    })
  }
  _yieldCont(__kappa, _fun__g59_1066)
}
function formP(_1088, _1089, _1090, __kappa) {
var attributes_1072 = _1090;
  var h_1073 = _1089;
  var f_1074 = _1088;
  function _fun__g60_1087(_1084, __kappa) {
  (function (_1085) {
    (function (_1086) {
      var x_1083 = _1085;
        if ( LINKS.eq(_1086, Nil) ) { _yieldCont(__kappa, x_1083) } else { error('Internal Error: Pattern matching failed', __kappa) }
      })(_tl(_1084))
    })(_hd(_1084))
  }
  function _fun__g61_1082(_1081, __kappa) {
  var gen_1075 = _1081;
    _yield(runState, [f_1074, gen_1075], function (_1079) {
    var gen_1076 = _1079['2'];
      var c_1077 = _1079['1']['2'];
      var x_1078 = _1079['1']['1'];
      _yield(mkCheckedFormBuilder, [x_1078, c_1077, h_1073, attributes_1072], function (_1080) {
      _yieldCont(__kappa, {'2':gen_1076, '1':_Cons(_1080, Nil)})
      })
    })
  }
  _yieldCont(__kappa, {'3':_fun__g61_1082, '2':_fun__g60_1087, '1':1})
}
function mkInput(_1104, _1105, __kappa) {
var mkFilledXml_1092 = _1105;
  var mkXml_1093 = _1104;
  function _fun__g63_1103(_1102, __kappa) {
  var name_1094 = _1102;
    _yield(mkXml_1093, [name_1094], function (_1101) {
    function _fun__g62_1100(_1099, __kappa) {
      var env_1095 = _1099;
        _yield(assoc, [name_1094, env_1095], function (v_1096) {
        _yield(mkFilledXml_1092, [name_1094, v_1096], function (_1098) {
          _yield(pureE, [v_1096], function (_1097) { _yieldCont(__kappa, {'2':_1097, '1':_1098}) })
          })
        })
      }
      _yield(_return, [{'2':_fun__g62_1100, '1':_1101}], __kappa)
    })
  }
  _yield(_greaterthan_greaterthan_equals, [nextName, _fun__g63_1103], __kappa)
}
function _fun__g66_1118(_1117, __kappa) {
var gen_1107 = _1117;
  function _fun__g64_1110(_1109, __kappa) { var name_1108 = _1109; _yieldCont(__kappa, LINKS.XML('input', {'name':name_1108}, [])) }
  function _fun__g65_1115(_1113, _1114, __kappa) {
  var v_1111 = _1114; var name_1112 = _1113; _yieldCont(__kappa, LINKS.XML('input', {'value':v_1111, 'name':name_1112}, []))
  }
  _yield(mkInput, [_fun__g64_1110, _fun__g65_1115], function (_1116) { _yield(_1116, [gen_1107], __kappa) })
}
var input = _fun__g66_1118;
function inputA(_1132, __kappa) {
var attributes_1120 = _1132;
  function _fun__g67_1124(_1123, __kappa) {
  var name_1121 = _1123;
    _yield(removeAssoc, ['name', attributes_1120], function (_1122) {
    _yieldCont(__kappa, _addAttributes(LINKS.XML('input', {'name':name_1121}, []), _1122))
    })
  }
  function _fun__g68_1131(_1129, _1130, __kappa) {
  var v_1125 = _1130;
    var name_1126 = _1129;
    _yield(removeAssoc, ['value', attributes_1120], function (_1127) {
    _yield(removeAssoc, ['name', _1127], function (_1128) {
      _yieldCont(__kappa, _addAttributes(LINKS.XML('input', {'value':v_1125, 'name':name_1126}, []), _1128))
      })
    })
  }
  _yield(mkInput, [_fun__g67_1124, _fun__g68_1131], __kappa)
}
function inputFileA(_1136, __kappa) {
var attributes_1134 = _1136;
  _yield(removeAssoc, ['type', attributes_1134], function (_1135) { _yield(inputA, [_Cons({'2':'file', '1':'type'}, _1135)], __kappa) })
}
function inputValueA(_1141, _1142, __kappa) {
var attributes_1138 = _1142;
  var v_1139 = _1141;
  _yield(removeAssoc, ['value', attributes_1138], function (_1140) { _yield(inputA, [_Cons({'2':v_1139, '1':'value'}, _1140)], __kappa) })
}
function inputPasswordA(_1146, __kappa) {
var attributes_1144 = _1146;
  _yield(removeAssoc, ['type', attributes_1144], function (_1145) { _yield(inputA, [_Cons({'2':'password', '1':'type'}, _1145)], __kappa) })
}
function _fun__g69_1151(_1150, __kappa) {
var gen_1148 = _1150; _yield(inputPasswordA, [Nil], function (_1149) { _yield(_1149, [gen_1148], __kappa) })
}
var inputPassword = _fun__g69_1151;
function inputValue(_1154, __kappa) { var v_1153 = _1154; _yield(inputValueA, [v_1153, Nil], __kappa) }
function _fun__g70_1159(_1158, __kappa) {
var gen_1156 = _1158; _yield(inputFileA, [Nil], function (_1157) { _yield(_1157, [gen_1156], __kappa) })
}
var inputFile = _fun__g70_1159;
function submit(_1162, __kappa) {
var text_1161 = _1162; _yieldCont(__kappa, LINKS.XML('button', {'type':'submit'}, [_stringToXml(text_1161)]))
}
function checkboxDefault(_1180, __kappa) {
var checked_1164 = _1180;
  function _fun__g73_1179(_1178, __kappa) {
  var gen_1165 = _1178;
    function _fun__g72_1176(_1175, __kappa) {
    var name_1166 = _1175;
      var xml_1167 = LINKS.XML('input', {'value':'yes', 'type':'checkbox', 'name':name_1166}, []);
      var _kappa1918 = function (_1174) {
      function _fun__g71_1173(_1172, __kappa) {
        var env_1168 = _1172;
          _yield(memassoc, [name_1166, env_1168], function (checked_1169) {
          var _kappa1917 = function (_1171) {
            _yield(pureE, [checked_1169], function (_1170) { _yieldCont(__kappa, {'2':_1170, '1':_addAttributes(xml_1167, _1171)}) })
            };
            if ( checked_1169 ) { _yieldCont(_kappa1917, _Cons({'2':'Y', '1':'checked'}, Nil)) } else { _yieldCont(_kappa1917, Nil) }
          })
        }
        _yield(_return, [{'2':_fun__g71_1173, '1':_addAttributes(xml_1167, _1174)}], __kappa)
      };
      if ( checked_1164 ) { _yieldCont(_kappa1918, _Cons({'2':'Y', '1':'checked'}, Nil)) } else { _yieldCont(_kappa1918, Nil) }
    }
    _yield(_greaterthan_greaterthan_equals, [nextName, _fun__g72_1176], function (_1177) { _yield(_1177, [gen_1165], __kappa) })
  }
  _yieldCont(__kappa, _fun__g73_1179)
}
function button(_1198, _1199, __kappa) {
var behaviour_1182 = _1199;
  var text_1183 = _1198;
  function _fun__g75_1197(_1196, __kappa) {
  var name_1184 = _1196;
    var _1185 = behaviour_1182;
    var _kappa1919 = function (behaviour_1189) {
    var xml_1190 = LINKS.XML('button', {'value':'here', 'type':behaviour_1189, 'name':name_1184}, [_stringToXml(text_1183)]);
      function _fun__g74_1195(_1194, __kappa) {
      var env_1191 = _1194;
        _yield(memassoc, [name_1184, env_1191], function (_1192) {
        _yield(pureE, [_1192], function (_1193) { _yieldCont(__kappa, {'2':_1193, '1':xml_1190}) })
        })
      }
      _yield(_return, [{'2':_fun__g74_1195, '1':xml_1190}], __kappa)
    };
    switch (_1185._label) {case 'Button': {var _1186 = _1185._value; _yieldCont(_kappa1919, 'button'); break;}
     case 'Reset': {var _1187 = _1185._value; _yieldCont(_kappa1919, 'reset'); break;}
     case 'Submit': {var _1188 = _1185._value; _yieldCont(_kappa1919, 'submit'); break;}
    }
  }
  _yield(_greaterthan_greaterthan_equals, [nextName, _fun__g75_1197], __kappa)
}
function pushButton(_1202, __kappa) { var text_1201 = _1202; _yield(button, [text_1201, {'_label':'Button', '_value':{}}], __kappa) }
function textarea(_1213, __kappa) {
var value_1204 = _1213;
  function _fun__g76_1207(_1206, __kappa) {
  var name_1205 = _1206; _yieldCont(__kappa, LINKS.XML('textarea', {'name':name_1205}, [_stringToXml(value_1204)]))
  }
  function _fun__g77_1212(_1210, _1211, __kappa) {
  var value_1208 = _1211; var name_1209 = _1210; _yieldCont(__kappa, LINKS.XML('textarea', {'name':name_1209}, [_stringToXml(value_1208)]))
  }
  _yield(mkInput, [_fun__g76_1207, _fun__g77_1212], __kappa)
}
function resetButton(_1216, __kappa) { var text_1215 = _1216; _yield(button, [text_1215, {'_label':'Reset', '_value':{}}], __kappa) }
function submitButton(_1219, __kappa) { var text_1218 = _1219; _yield(button, [text_1218, {'_label':'Submit', '_value':{}}], __kappa) }
function _fun__g78_1224(_1223, __kappa) {
var gen_1221 = _1223; _yield(checkboxDefault, [false], function (_1222) { _yield(_1222, [gen_1221], __kappa) })
}
var checkbox = _fun__g78_1224;
function preludeOptionKeyed(_1244, _1245, _1246, __kappa) {
var selected_1226 = _1246;
  var text_1227 = _1245['2'];
  var data_1228 = _1245['1'];
  var key_1229 = _1244;
  if ( selected_1226 ) {
  function _fun__g79_1243(_1242, __kappa) {
    var k_1237 = _1242;
      function _fun__g80_1241(_1240, __kappa) {
      var aKey_1238 = _1240;
        if ( LINKS.eq(aKey_1238, key_1229) ) {
        _yield(k_1237, [aKey_1238], function (_1239) { _yieldCont(__kappa, _Cons(data_1228, _1239)) })
        } else { _yield(k_1237, [aKey_1238], __kappa) }
      }
      _yieldCont(__kappa, _fun__g80_1241)
    }
    _yieldCont(__kappa, {'2':_fun__g79_1243, '1':LINKS.XML('option', {'value':key_1229, 'selected':'SELECTED'}, [_stringToXml(text_1227)])})
  } else {
  function _fun__g81_1236(_1235, __kappa) {
    var k_1230 = _1235;
      function _fun__g82_1234(_1233, __kappa) {
      var aKey_1231 = _1233;
        if ( LINKS.eq(aKey_1231, key_1229) ) {
        _yield(k_1230, [aKey_1231], function (_1232) { _yieldCont(__kappa, _Cons(data_1228, _1232)) })
        } else { _yield(k_1230, [aKey_1231], __kappa) }
      }
      _yieldCont(__kappa, _fun__g82_1234)
    }
    _yieldCont(__kappa, {'2':_fun__g81_1236, '1':LINKS.XML('option', {'value':key_1229}, [_stringToXml(text_1227)])})
  }
}
function preludeOptionsKeyed(_1250, _1251, __kappa) {
var def_1248 = _1251;
  var items_1249 = _1250;
  var _1256 = items_1249;
  if ( LINKS.eq(_1256, Nil) ) {
  function _fun__g83_1271(_1270, __kappa) { _yieldCont(__kappa, Nil) } _yieldCont(__kappa, {'2':_fun__g83_1271, '1':Nil})
  } else {
  (function (_1257) {
    (function (_1258) {
      var _1260 = _1257['1'];
        var _1261 = _1257['2'];
        var items_1255 = _1258;
        var item_1253 = _1261;
        var itemKey_1254 = _1260;
        _yield(first, [item_1253], function (_1264) {
        _yield(preludeOptionKeyed, [itemKey_1254, item_1253, LINKS.eq(_1264, def_1248)], function (_1265) {
          var r1_1262 = _1265['2'];
            var xml_1263 = _1265['1'];
            _yield(preludeOptionsKeyed, [items_1255, def_1248], function (_1268) {
            var r2_1266 = _1268['2'];
              var morexml_1267 = _1268['1'];
              _yield(r1_1262, [r2_1266], function (_1269) { _yieldCont(__kappa, {'2':_1269, '1':_Concat(xml_1263, morexml_1267)}) })
            })
          })
        })
      })(_tl(_1256))
    })(_hd(_1256))
  }
}
function choiceDefault(_1301, _1302, __kappa) {
var selected_1272 = _1302;
  var items_1273 = _1301;
  function _fun__g87_1300(_1299, __kappa) {
  var fieldName_1274 = _1299;
    function _fun__g84_1296(_1295, __kappa) { var _for__g3_1293 = _1295; var x_1294 = _1295; _yieldCont(__kappa, _Cons(nextName, Nil)) }
    _yield(concatMap, [_fun__g84_1296, items_1273], function (_1297) {
    _yield(sequence, [_1297], function (_1298) {
      function _fun__g86_1292(_1291, __kappa) {
        var optKeys_1275 = _1291;
          _yield(zip, [optKeys_1275, items_1273], function (keyedItems_1276) {
          _yield(preludeOptionsKeyed, [keyedItems_1276, selected_1272], function (_1290) {
            var optsXml_1289 = _1290['1'];
              function _fun__g85_1288(_1287, __kappa) {
              var env_1277 = _1287;
                _yield(assoc, [fieldName_1274, env_1277], function (selectedKey_1278) {
                _yield(assoc, [selectedKey_1278, keyedItems_1276], function (_1280) {
                  var selected_1279 = _1280['1'];
                    _yield(preludeOptionsKeyed, [keyedItems_1276, selected_1279], function (_1283) {
                    var retrieve_1281 = _1283['2'];
                      var optsXml_1282 = _1283['1'];
                      _yield(retrieve_1281, [selectedKey_1278], function (_1284) {
                      (function (_1285) {
                        _yield(pureE, [_1285], function (_1286) {
                          _yieldCont(__kappa, {'2':_1286, '1':LINKS.XML('select', {'name':fieldName_1274}, [optsXml_1282])})
                          })
                        })(_hd(_1284))
                      })
                    })
                  })
                })
              }
              _yield(_return, [{'2':_fun__g85_1288, '1':LINKS.XML('select', {'name':fieldName_1274}, [optsXml_1289])}], __kappa)
            })
          })
        }
        _yield(_greaterthan_greaterthan_equals, [_1298, _fun__g86_1292], __kappa)
      })
    })
  }
  _yield(_greaterthan_greaterthan_equals, [nextName, _fun__g87_1300], __kappa)
}
function inputRadiogroup(_1339, _1340, _1341, __kappa) {
var layout_1304 = _1341;
  var selected_1305 = _1340;
  var items_1306 = _1339;
  function _fun__g92_1338(_1337, __kappa) {
  var name_1307 = _1337;
    function _fun__g88_1334(_1333, __kappa) { var _for__g4_1331 = _1333; var x_1332 = _1333; _yieldCont(__kappa, _Cons(nextName, Nil)) }
    _yield(concatMap, [_fun__g88_1334, items_1306], function (_1335) {
    _yield(sequence, [_1335], function (_1336) {
      function _fun__g91_1330(_1329, __kappa) {
        var keys_1308 = _1329;
          _yield(zip, [keys_1308, items_1306], function (keyedItems_1309) {
          function itemXml_1318(_1317, __kappa) {
            var selected_1310 = _1317;
              function _fun__g89_1316(_1315, __kappa) {
              var _for__g5_1311 = _1315;
                var data_1312 = _1315['2'];
                var key_1313 = _1315['1'];
                var _kappa1920 = function (_1314) {
                _yieldCont(__kappa, _Cons({'2':data_1312, '1':_addAttributes(LINKS.XML('input', {'value':key_1313, 'type':'radio', 'name':name_1307}, []), _1314)}, Nil))
                };
                if ( LINKS.eq(data_1312, selected_1310) ) {
                _yieldCont(_kappa1920, _Cons({'2':'on', '1':'checked'}, Nil))
                } else { _yieldCont(_kappa1920, Nil) }
              }
              _yield(concatMap, [_fun__g89_1316, keyedItems_1309], __kappa)
            }
            _yield(itemXml_1318, [selected_1305], function (_1319) {
            _yield(layout_1304, [_1319], function (xml_1320) {
              function _fun__g90_1328(_1327, __kappa) {
                var env_1321 = _1327;
                  _yield(assoc, [name_1307, env_1321], function (selectedValue_1322) {
                  _yield(assoc, [selectedValue_1322, keyedItems_1309], function (selectedData_1323) {
                    _yield(itemXml_1318, [selectedData_1323], function (_1324) {
                      _yield(layout_1304, [_1324], function (xml_1325) {
                        _yield(pureE, [selectedData_1323], function (_1326) { _yieldCont(__kappa, {'2':_1326, '1':xml_1325}) })
                        })
                      })
                    })
                  })
                }
                _yield(_return, [{'2':_fun__g90_1328, '1':xml_1320}], __kappa)
              })
            })
          })
        }
        _yield(_greaterthan_greaterthan_equals, [_1336, _fun__g91_1330], __kappa)
      })
    })
  }
  _yield(_greaterthan_greaterthan_equals, [nextName, _fun__g92_1338], __kappa)
}
function preludeMultiOptionsKeyed(_1344, __kappa) {
var items_1343 = _1344;
  var _1349 = items_1343;
  if ( LINKS.eq(_1349, Nil) ) {
  function _fun__g93_1366(_1365, __kappa) { _yieldCont(__kappa, Nil) } _yieldCont(__kappa, {'2':_fun__g93_1366, '1':Nil})
  } else {
  (function (_1350) {
    (function (_1351) {
      var _1353 = _1350['1'];
        var _1354 = _1350['2'];
        var items_1348 = _1351;
        var item_1346 = _1354;
        var itemKey_1347 = _1353;
        _yield(first, [item_1346], function (_1358) {
        _yield(second, [item_1346], function (_1357) {
          _yield(third, [item_1346], function (_1359) {
            _yield(preludeOptionKeyed, [itemKey_1347, {'2':_1357, '1':_1358}, _1359], function (_1360) {
              var r1_1355 = _1360['2'];
                var xml_1356 = _1360['1'];
                _yield(preludeMultiOptionsKeyed, [items_1348], function (_1363) {
                var r2_1361 = _1363['2'];
                  var morexml_1362 = _1363['1'];
                  _yield(r1_1355, [r2_1361], function (_1364) { _yieldCont(__kappa, {'2':_1364, '1':_Concat(xml_1356, morexml_1362)}) })
                })
              })
            })
          })
        })
      })(_tl(_1349))
    })(_hd(_1349))
  }
}
function choices(_1402, __kappa) {
var items_1367 = _1402;
  function _fun__g98_1401(_1400, __kappa) {
  var fieldName_1368 = _1400;
    function _fun__g94_1397(_1396, __kappa) { var _for__g6_1394 = _1396; var x_1395 = _1396; _yieldCont(__kappa, _Cons(nextName, Nil)) }
    _yield(concatMap, [_fun__g94_1397, items_1367], function (_1398) {
    _yield(sequence, [_1398], function (_1399) {
      function _fun__g97_1393(_1392, __kappa) {
        var optKeys_1369 = _1392;
          _yield(zip, [optKeys_1369, items_1367], function (keyedItems_1370) {
          _yield(preludeMultiOptionsKeyed, [keyedItems_1370], function (_1391) {
            var retrieve_1389 = _1391['2'];
              var xml_1390 = _1391['1'];
              function _fun__g96_1388(_1387, __kappa) {
              var env_1371 = _1387;
                _yield(assocAll, [fieldName_1368, env_1371], function (selectedKeys_1372) {
                function _fun__g95_1380(_1379, __kappa) {
                  var _for__g7_1373 = _1379;
                    var sel_1374 = _1379['2']['3'];
                    var text_1375 = _1379['2']['2'];
                    var data_1376 = _1379['2']['1'];
                    var key_1377 = _1379['1'];
                    _yield(elem, [key_1377, selectedKeys_1372], function (_1378) {
                    _yieldCont(__kappa, _Cons({'2':{'3':_1378, '2':text_1375, '1':data_1376}, '1':key_1377}, Nil))
                    })
                  }
                  _yield(concatMap, [_fun__g95_1380, keyedItems_1370], function (keyedItems_1381) {
                  _yield(preludeMultiOptionsKeyed, [keyedItems_1381], function (_1384) {
                    var retrieve_1382 = _1384['2'];
                      var xml_1383 = _1384['1'];
                      _yield(concatMap, [retrieve_1382, selectedKeys_1372], function (_1385) {
                      _yield(pureE, [_1385], function (_1386) {
                        _yieldCont(__kappa, {'2':_1386, '1':LINKS.XML('select', {'name':fieldName_1368, 'multiple':'multiple'}, [xml_1383])})
                        })
                      })
                    })
                  })
                })
              }
              _yield(_return, [{'2':_fun__g96_1388, '1':LINKS.XML('select', {'name':fieldName_1368, 'multiple':'multiple'}, [xml_1390])}], __kappa)
            })
          })
        }
        _yield(_greaterthan_greaterthan_equals, [_1399, _fun__g97_1393], __kappa)
      })
    })
  }
  _yield(_greaterthan_greaterthan_equals, [nextName, _fun__g98_1401], __kappa)
}
function choicesNone(_1410, __kappa) {
var items_1404 = _1410;
  function _fun__g99_1408(_1407, __kappa) {
  var text_1405 = _1407['2']; var v_1406 = _1407['1']; _yieldCont(__kappa, {'3':false, '2':text_1405, '1':v_1406})
  }
  _yield(map, [_fun__g99_1408, items_1404], function (_1409) { _yield(choices, [_1409], __kappa) })
}
function choice(_1415, __kappa) {
var items_1412 = _1415;
  (function (_1414) { var selected_1413 = _1414['1']; _yield(choiceDefault, [items_1412, selected_1413], __kappa) })(_hd(items_1412))
}
function errorMsg(_1426, _1427, __kappa) {
var msg_1417 = _1427;
  var f_1418 = _1426;
  function _fun__g100_1425(_1423, _1424, __kappa) {
  var xml_1419 = _1424;
    var x_1420 = _1423;
    _yield(f_1418, [x_1420], function (_1422) {
    if ( _1422 ) {
      _yieldCont(__kappa, {'_label':'Left', '_value':xml_1419})
      } else {
      _yield(msg_1417, [x_1420], function (_1421) {
        _yieldCont(__kappa, {'_label':'Right', '_value':_Concat(LINKS.XML('span', {'class':'errorinput'}, [xml_1419, _stringToXml(' ')]), LINKS.XML('span', {'class':'error'}, [_stringToXml(' '), _stringToXml(_1421)]))})
        })
      }
    })
  }
  _yieldCont(__kappa, _fun__g100_1425)
}
function satisfies(_1451, _1452, __kappa) {
var validate_1429 = _1452;
  var f_1430 = _1451;
  function _fun__g102_1450(_1449, __kappa) {
  var collector_1431 = _1449['2'];
    var xml_1432 = _1449['1'];
    function _fun__g101_1448(_1447, __kappa) {
    var env_1433 = _1447;
      _yield(collector_1431, [env_1433], function (_1436) {
      var val_1434 = _1436['2'];
        var cxml_1435 = _1436['1'];
        var _1438 = val_1434;
        switch (_1438._label) {case 'Just': {var _1439 = _1438._value; var v_1437 = _1439;
        _yield(validate_1429, [v_1437, cxml_1435], function (_1442) {
        var _1443 = _1442;
          switch (_1443._label) {case 'Left': {var _1444 = _1443._value; var xml_1441 = _1444;
          _yieldCont(__kappa, {'2':{'_label':'Just', '_value':v_1437}, '1':xml_1441}); break;}
           case 'Right': {var _1445 = _1443._value; var xml_1440 = _1445;
          _yieldCont(__kappa, {'2':{'_label':'Nothing', '_value':{}}, '1':xml_1440}); break;}
          }
        }); break;}
         case 'Nothing': {var _1446 = _1438._value; _yieldCont(__kappa, {'2':{'_label':'Nothing', '_value':{}}, '1':cxml_1435}); break;}
        }
      })
    }
    _yield(_return, [{'2':_fun__g101_1448, '1':xml_1432}], __kappa)
  }
  _yield(_greaterthan_greaterthan_equals, [f_1430, _fun__g102_1450], __kappa)
}
function transform(_1471, _1472, _1473, _1474, __kappa) {
var g_1454 = _1474;
  var error_1455 = _1473;
  var predicate_1456 = _1472;
  var f_1457 = _1471;
  _yield(errorMsg, [predicate_1456, error_1455], function (_1458) {
  _yield(satisfies, [f_1457, _1458], function (_1459) {
    function _fun__g103_1463(_1462, __kappa) { var _formlet__g15_1460 = _1462; var v_1461 = _1462; _yieldCont(__kappa, _formlet__g15_1460) }
      _yield(pure, [_fun__g103_1463], function (_1464) {
      _yield(_monkey_monkey_monkey, [_1459, _1464], function (_1465) {
        function _fun__g104_1469(_1468, __kappa) { var _formlet__g14_1466 = _1468; var v_1467 = _1468; _yield(g_1454, [v_1467], __kappa) }
          _yield(pure, [_fun__g104_1469], function (_1470) { _yield(_monkey_monkey_monkey, [_1465, _1470], __kappa) })
        })
      })
    })
  })
}
function _fun__g106_1482(_1481, __kappa) {
var gen_1476 = _1481;
  function _fun__g105_1479(_1478, __kappa) { var s_1477 = _1478; _yieldCont(__kappa, s_1477 + ' is not an integer') }
  _yield(transform, [input, isInt, _fun__g105_1479, stringToInt], function (_1480) { _yield(_1480, [gen_1476], __kappa) })
}
var inputInt = _fun__g106_1482;
function inputIntValue(_1489, __kappa) {
var v_1484 = _1489;
  _yield(inputA, [_Cons({'2':_intToString(v_1484), '1':'value'}, Nil)], function (_1485) {
  function _fun__g107_1488(_1487, __kappa) { var s_1486 = _1487; _yieldCont(__kappa, s_1486 + 'is not an integer') }
    _yield(transform, [_1485, isInt, _fun__g107_1488, stringToInt], __kappa)
  })
}
function inputIntA(_1496, __kappa) {
var attributes_1491 = _1496;
  _yield(inputA, [attributes_1491], function (_1492) {
  function _fun__g108_1495(_1494, __kappa) { var s_1493 = _1494; _yieldCont(__kappa, s_1493 + ' is not an integer') }
    _yield(transform, [_1492, isInt, _fun__g108_1495, stringToInt], __kappa)
  })
}
function _fun__g110_1504(_1503, __kappa) {
var gen_1498 = _1503;
  function _fun__g109_1501(_1500, __kappa) { var s_1499 = _1500; _yieldCont(__kappa, s_1499 + ' is not a float') }
  _yield(transform, [input, isFloat, _fun__g109_1501, stringToFloat], function (_1502) { _yield(_1502, [gen_1498], __kappa) })
}
var inputFloat = _fun__g110_1504;
function sendSuspend(_1511, __kappa) {
var pagef_1506 = _1511;
  function _1510(returnf_1507, __kappa) {
  _yield(pagef_1506, [returnf_1507], function (_1508) { _yield(renderPage, [_1508], function (_1509) { __kappa(_exit(_1509)) }) })
  }
  _yield(_1510, [__kappa], __kappa)
}
function linfork(_1519, __kappa) {
var f_1513 = _1519;
  (function (_1514) {
  var ap_1515 = _1514;
    function _fun__g111_1517(__kappa) { _yield(accept, [ap_1515], function (_1516) { _yield(f_1513, [_1516], __kappa) }) }
    (function (_1518) { _yield(request, [ap_1515], __kappa) })(_spawn(_fun__g111_1517))
  })(_new())
}
function fork(_1527, __kappa) {
var f_1521 = _1527;
  (function (_1522) {
  var ap_1523 = _1522;
    function _fun__g112_1525(__kappa) { _yield(accept, [ap_1523], function (_1524) { _yield(f_1521, [_1524], __kappa) }) }
    (function (_1526) { _yield(request, [ap_1523], __kappa) })(_spawn(_fun__g112_1525))
  })(_new())
}
function replicate(_1531, _1532, __kappa) {
var f_1529 = _1532;
  var ap_1530 = _1531;
  _yield(accept, [ap_1530], function (x_1534) {
  function _fun__g113_1535(__kappa) { _yield(f_1529, [x_1534], __kappa) }
    (function (_1536) { _yield(replicate, [ap_1530, f_1529], __kappa) })(_spawn(_fun__g113_1535))
  })
}
function freshResource(__kappa) {
function _1540(esc_1537, __kappa) {
  (function (_1538) { (function (_1539) { __kappa(_exit(0)) })(_redirect('?_cont=' + _1538)) })(_reifyK(esc_1537))
  }
  _yield(_1540, [__kappa], __kappa)
}
function wait(_1544, __kappa) { var s_1542 = _1544; _yield(grab, [s_1542], function (_1543) { _yield(ignore, [_1543], __kappa) }) }
function close(_1548, __kappa) { var s_1546 = _1548; (function (_1547) { _yield(ignore, [_1547], __kappa) })(_give({}, s_1546)) }
function fmin(_1552, _1553, __kappa) {
var b_1550 = _1553; var a_1551 = _1552; if ( a_1551 < b_1550 ) { _yieldCont(__kappa, a_1551) } else { _yieldCont(__kappa, b_1550) }
}
function fmax(_1557, _1558, __kappa) {
var b_1555 = _1558; var a_1556 = _1557; if ( a_1556 > b_1555 ) { _yieldCont(__kappa, a_1556) } else { _yieldCont(__kappa, b_1555) }
}
function fabs(_1561, __kappa) { var x_1560 = _1561; if ( x_1560 < 0 ) { _yieldCont(__kappa, - x_1560) } else { _yieldCont(__kappa, x_1560) } }
function putPixel(_1567, _1568, _1569, __kappa) {
var y_1563 = _1569;
  var x_1564 = _1568;
  var ctx_1565 = _1567;
  (function (_1566) { _yieldCont(__kappa, {}) })(_jsFillRect(ctx_1565, x_1564, y_1563, 2, 2))
}
function clear(_1574, __kappa) {
var ctx_1571 = _1574;
  (function (_1572) {
  (function (_1573) { __kappa(_jsClearRect(ctx_1571, 0, 0, _1572, _1573)) })(_jsCanvasHeight(ctx_1571))
  })(_jsCanvasWidth(ctx_1571))
}
function main(__kappa) {
var canvasId_1576 = 'gameCanvas';
  var canvas2Id_1577 = 'gameCanvas2';
  var containerId_1578 = 'gameContainer';
  var canvasWidth_1579 = 600;
  var canvasHeight_1580 = 500;
  var leftKeyCode_1581 = 37;
  var rightKeyCode_1582 = 39;
  var upKeyCode_1583 = 38;
  var downKeyCode_1584 = 40;
  var restartKeyCode_1585 = 113;
  var initialFpsInfo_1586 = {'upFrames':0, 'loFpsFrame':0, 'loFps':1000000, 'hiFps':0, 'frameCount':0, 'fpsAcc':0, 'downFrames':0, 'dFps':0, 'avgFps':0};
  var initialChartParams_1587 = {'yScale':canvasHeight_1580, 'xScale':canvasWidth_1579, 'snap':false, 'round':0, 'measure':0.5};
  function _fun__g118_1593(_1592, __kappa) {
  var _for__g115_1588 = _1592['2'];
    var y_1589 = _1592['2'];
    var _for__g114_1590 = _1592['1'];
    var x_1591 = _1592['1'];
    _yieldCont(__kappa, _Cons({'2':y_1589, '1':x_1591}, Nil))
  }
  function _fun__g120_1599(_1598, __kappa) {
  var _for__g114_1594 = _1598;
    function _fun__g119_1597(_1596, __kappa) { var _for__g115_1595 = _1596; _yieldCont(__kappa, {'2':_for__g115_1595, '1':_for__g114_1594}) }
    _yield(map, [_fun__g119_1597, _Cons(0, Nil)], __kappa)
  }
  _yield(intRange, [0, _floatToInt(initialChartParams_1587['xScale'])], function (_1600) {
  _yield(concatMap, [_fun__g120_1599, _1600], function (_1601) {
    _yield(concatMap, [_fun__g118_1593, _1601], function (initialDatapoints_1602) {
      function _fun__g121_1609(_1608, __kappa) {
        var _for__g117_1603 = _1608['2'];
          var y_1604 = _1608['2'];
          var _for__g116_1605 = _1608['1'];
          var x_1606 = _1608['1'];
          _yieldCont(function (_1607) {
          if ( LINKS.eq(_1607, 0) ) { _yieldCont(__kappa, _Cons({'2':y_1604, '1':_intToFloat(x_1606)}, Nil)) } else { _yieldCont(__kappa, Nil) }
          }, x_1606 % 60)
        }
        function _fun__g123_1615(_1614, __kappa) {
        var _for__g116_1610 = _1614;
          function _fun__g122_1613(_1612, __kappa) {
          var _for__g117_1611 = _1612; _yieldCont(__kappa, {'2':_for__g117_1611, '1':_for__g116_1610})
          }
          _yield(map, [_fun__g122_1613, _Cons(initialChartParams_1587['yScale'] - 1, Nil)], __kappa)
        }
        _yield(intRange, [0, _floatToInt(initialChartParams_1587['xScale'])], function (_1616) {
        _yield(concatMap, [_fun__g123_1615, _1616], function (_1617) {
          _yield(concatMap, [_fun__g121_1609, _1617], function (xAxis_1618) {
            var doubleBuffer_1619 = true;
              var step_1620 = 1 / 60;
              function swapBuffers_1636(_1634, _1635, __kappa) {
              var dispCanvas_1621 = _1635;
                var mainCanvas_1622 = _1634;
                (function (_1623) {
                (function (_1624) {
                  var ctx_1625 = _1624;
                    (function (_1632) {
                    (function (_1633) {
                      (function (_1629) {
                        (function (_1630) {
                          _yield(ignore, [_1630], function (_1631) {
                            (function (_1626) {
                              (function (_1627) {
                                _yield(ignore, [_1627], function (_1628) { _yield(clear, [ctx_1625], __kappa) })
                                })(_domSetStyleAttrFromRef(_1626, 'display', 'none'))
                              })(_getNodeById(dispCanvas_1621))
                            })
                          })(_domSetStyleAttrFromRef(_1629, 'display', 'block'))
                        })(_getNodeById(mainCanvas_1622))
                      })(_jsDrawImage(ctx_1625, _1632, 0, 0))
                    })(_getNodeById(mainCanvas_1622))
                  })(_jsGetContext2D(_1623))
                })(_getNodeById(dispCanvas_1621))
              }
              function drawFps_1657(_1653, _1654, _1655, _1656, __kappa) {
              var chartParams_1637 = _1656;
                var dFps_1638 = _1655;
                var fpsInfo_1639 = _1654;
                var ctx_1640 = _1653;
                var fpsInfo_1641 = LINKS.union(LINKS.erase(fpsInfo_1639, ['dFps', 'frameCount']), {'frameCount':fpsInfo_1639['frameCount'] + 1, 'dFps':dFps_1638});
                (function (_1652) {
                (function (_1650) {
                  (function (_1651) {
                    var _kappa1925 = function (fpsInfo_1642) {
                      var _kappa1924 = function (fpsInfo_1643) {
                        var fpsInfo_1644 = LINKS.union(LINKS.erase(fpsInfo_1643, ['fpsAcc']), {'fpsAcc':fpsInfo_1643['fpsAcc'] + dFps_1638});
                          var aFpsFrames_1645 = _floatToInt(chartParams_1637['xScale']);
                          var _kappa1923 = function (fpsInfo_1646) {
                          var _kappa1922 = function (fpsInfo_1647) {
                            var _kappa1921 = function (_1649) { _yieldCont(__kappa, fpsInfo_1647) };
                              if ( LINKS.eq(fpsInfo_1647['hiFps'], 0) ) {
                              _kappa1921(_jsFillText(ctx_1640, 'loading data: ' + (_intToString(fpsInfo_1647['frameCount']) + ('/' + _intToString(aFpsFrames_1645))), 100, 10))
                              } else {
                              (function (_1648) {
                                _kappa1921(_jsFillText(ctx_1640, 'highest FPS: ' + _1648, 100, 10))
                                })(_strsub(_floatToString(fpsInfo_1647['hiFps']), 0, 7))
                              }
                            };
                            if ( fpsInfo_1646['avgFps'] > 0 ) {
                            if ( dFps_1638 < (fpsInfo_1646['avgFps'] * chartParams_1637['measure']) ) {
                              _yieldCont(_kappa1922, LINKS.union(LINKS.erase(fpsInfo_1646, ['downFrames']), {'downFrames':fpsInfo_1646['downFrames'] + 1}))
                              } else {
                              _yieldCont(_kappa1922, LINKS.union(LINKS.erase(fpsInfo_1646, ['upFrames']), {'upFrames':fpsInfo_1646['upFrames'] + 1}))
                              }
                            } else { _yieldCont(_kappa1922, LINKS.union(LINKS.erase(fpsInfo_1646, ['hiFps']), {'hiFps':0})) }
                          };
                          if ( fpsInfo_1644['frameCount'] > aFpsFrames_1645 ) {
                          _yieldCont(_kappa1923, LINKS.union(LINKS.erase(fpsInfo_1644, ['avgFps', 'fpsAcc', 'frameCount']), {'frameCount':0, 'fpsAcc':0, 'avgFps':fpsInfo_1644['fpsAcc'] / _intToFloat(aFpsFrames_1645)}))
                          } else { _yieldCont(_kappa1923, fpsInfo_1644) }
                        };
                        if ( fpsInfo_1642['hiFps'] < dFps_1638 ) {
                        _yieldCont(_kappa1924, LINKS.union(LINKS.erase(fpsInfo_1642, ['hiFps']), {'hiFps':dFps_1638}))
                        } else { _yieldCont(_kappa1924, fpsInfo_1642) }
                      };
                      if ( fpsInfo_1641['loFps'] > dFps_1638 ) {
                      _yieldCont(_kappa1925, LINKS.union(LINKS.erase(fpsInfo_1641, ['loFps', 'loFpsFrame']), {'loFpsFrame':fpsInfo_1641['frameCount'] - 1, 'loFps':dFps_1638}))
                      } else { _yieldCont(_kappa1925, fpsInfo_1641) }
                    })(_jsFillText(ctx_1640, 'FPS: ' + _1650, 10, 10))
                  })(_strsub(_floatToString(dFps_1638), 0, 7))
                })(_jsSetFillColor(ctx_1640, '#000'))
              }
              function draw_1798(_1793, _1794, _1795, _1796, _1797, __kappa) {
              var chartParams_1658 = _1797;
                var fpsInfo_1659 = _1796;
                var now_1660 = _1795;
                var lastTime_1661 = _1794;
                var datapoints_1662 = _1793;
                (function (_1665) {
                (function (_1666) {
                  var _kappa1931 = function (_1667) {
                    var _kappa1930 = function (_1668) {
                      var dispCanvas_1663 = _1668['2'];
                        var mainCanvas_1664 = _1668['1'];
                        (function (_1669) {
                        (function (ctx_1670) {
                          _yield(clear, [ctx_1670], function (_1792) {
                            function scalePoint_1674(_1673, __kappa) {
                              var y_1671 = _1673['2'];
                                var x_1672 = _1673['1'];
                                _yieldCont(__kappa, {'2':chartParams_1658['yScale'] - ((y_1671 / fpsInfo_1659['hiFps']) * chartParams_1658['yScale']), '1':x_1672})
                              }
                              function drawChartLine_1692(_1688, _1689, _1690, _1691, __kappa) {
                              var msg_1675 = _1691;
                                var y_1676 = _1690;
                                var color_1677 = _1689;
                                var ctx_1678 = _1688;
                                (function (_1687) {
                                (function (_1686) {
                                  _yield(scalePoint_1674, [{'2':y_1676, '1':0}], function (_1680) {
                                    var scaledY_1679 = _1680['2'];
                                      (function (_1685) {
                                      (function (_1684) {
                                        (function (_1683) {
                                          (function (_1681) {
                                            (function (_1682) {
                                              _yieldCont(__kappa, scaledY_1679)
                                              })(_jsFillText(ctx_1678, msg_1675 + _1681, 10, scaledY_1679))
                                            })(_strsub(_floatToString(y_1676), 0, 5))
                                          })(_jsStroke(ctx_1678))
                                        })(_jsLineTo(ctx_1678, chartParams_1658['xScale'], scaledY_1679))
                                      })(_jsMoveTo(ctx_1678, 0, scaledY_1679))
                                    })
                                  })(_jsBeginPath(ctx_1678))
                                })(_jsStrokeStyle(ctx_1678, color_1677))
                              }
                              function markYAxis_1705(_1704, __kappa) {
                              var fraction_1693 = _1704;
                                _yield(scalePoint_1674, [{'2':fraction_1693 * fpsInfo_1659['hiFps'], '1':0}], function (_1695) {
                                var yP_1694 = _1695['2'];
                                  function _fun__g124_1700(_1699, __kappa) {
                                  var x_1698 = _1699; _yield(putPixel, [ctx_1670, _intToFloat(x_1698), yP_1694], __kappa)
                                  }
                                  _yield(intRange, [0, 6], function (_1701) {
                                  _yield(map, [_fun__g124_1700, _1701], function (_1702) {
                                    _yield(ignore, [_1702], function (_1703) {
                                      (function (_1696) {
                                        (function (_1697) { _yieldCont(__kappa, {}) })(_jsFillText(ctx_1670, _1696, 10, yP_1694))
                                        })(_strsub(_floatToString(fraction_1693 * fpsInfo_1659['hiFps']), 0, 7))
                                      })
                                    })
                                  })
                                })
                              }
                              var offset_1706 = fpsInfo_1659['frameCount'];
                              var dFps_1707 = 1000 / (_intToFloat(now_1660 - lastTime_1661) + 1);
                              var leftPoints_1708 = _take(offset_1706, datapoints_1662);
                              var middlePoint_1709 = _Cons({'2':dFps_1707, '1':offset_1706 + 1}, Nil);
                              var datapointsLength_1710 = _length(datapoints_1662);
                              var diff_1711 = _floatToInt(chartParams_1658['xScale']) - datapointsLength_1710;
                              var rightPoints_1712 = _drop(offset_1706 + 1, _take(_floatToInt(chartParams_1658['xScale']), datapoints_1662));
                              (function (_1790) {
                              (function (_1791) {
                                (function (_1788) {
                                  (function (_1789) {
                                    var _kappa1929 = function (_1786) {
                                      (function (_1787) {
                                        (function (_1783) {
                                          (function (_1784) {
                                            (function (_1785) {
                                              _yield(drawChartLine_1692, [ctx_1670, 'red', fpsInfo_1659['loFps'], 'lowest FPS: '], function (_1713) {
                                                _yield(drawChartLine_1692, [ctx_1670, 'blue', fpsInfo_1659['avgFps'], 'average FPS: '], function (_1714) {
                                                  var measure_1715 = fpsInfo_1659['avgFps'] * chartParams_1658['measure'];
                                                    _yield(drawChartLine_1692, [ctx_1670, 'green', measure_1715, 'reference FPS: '], function (mesPoint_1716) {
                                                    (function (_1782) {
                                                      (function (_1781) {
                                                        function _fun__g126_1778(_1777, __kappa) {
                                                          var y_1768 = _1777['2'];
                                                            var x_1769 = _1777['1'];
                                                            function _fun__g125_1773(_1772, __kappa) {
                                                            var i_1771 = _1772;
                                                              _yield(putPixel, [ctx_1670, x_1769, y_1768 - _intToFloat(i_1771)], __kappa)
                                                            }
                                                            _yield(intRange, [0, 6], function (_1774) {
                                                            _yield(map, [_fun__g125_1773, _1774], function (_1775) {
                                                              _yield(ignore, [_1775], function (_1776) {
                                                                (function (_1770) {
                                                                  _yieldCont(__kappa, {})
                                                                  })(_jsFillText(ctx_1670, _floatToString(x_1769), x_1769, y_1768 - 10))
                                                                })
                                                              })
                                                            })
                                                          }
                                                          _yield(map, [_fun__g126_1778, xAxis_1618], function (_1779) {
                                                          _yield(ignore, [_1779], function (_1780) {
                                                            (function (_1767) {
                                                              (function (_1717) {
                                                                _yield(scalePoint_1674, [_1717], function (firstPoint_1718) {
                                                                  (function (_1766) {
                                                                    _yield(_bang_bang, [middlePoint_1709, 0], function (_1721) {
                                                                      _yield(scalePoint_1674, [_1721], function (_1722) {
                                                                        var midPointy_1719 = _1722['2'];
                                                                          var midPointx_1720 = _1722['1'];
                                                                          function plotPoint_1729(_1728, __kappa) {
                                                                          var p_1723 = _1728;
                                                                            _yield(scalePoint_1674, [p_1723], function (_1726) {
                                                                            var y_1724 = _1726['2'];
                                                                              var x_1725 = _1726['1'];
                                                                              _yield(putPixel, [ctx_1670, _intToFloat(x_1725), y_1724], function (_1727) {
                                                                              if ( y_1724 < mesPoint_1716 ) {
                                                                                _yieldCont(__kappa, 1)
                                                                                } else { _yieldCont(__kappa, - 1) }
                                                                              })
                                                                            })
                                                                          }
                                                                          (function (_1765) {
                                                                          _yield(map, [plotPoint_1729, leftPoints_1708], function (plottedLeftPoints_1730) {
                                                                            var _kappa1928 = function (_1764) {
                                                                              (function (_1753) {
                                                                                _yield(map, [plotPoint_1729, middlePoint_1709], function (_1751) {
                                                                                  _yield(ignore, [_1751], function (_1752) {
                                                                                    (function (_1750) {
                                                                                      _yield(map, [plotPoint_1729, rightPoints_1712], function (_1748) {
                                                                                        _yield(ignore, [_1748], function (_1749) {
                                                                                          (function (_1747) {
                                                                                            (function (_1746) {
                                                                                              _yield(markYAxis_1705, [0.25], function (_1745) {
                                                                                                _yield(markYAxis_1705, [0.5], function (_1744) {
                                                                                                  _yield(markYAxis_1705, [0.75], function (_1743) {
                                                                                                    _yield(drawFps_1657, [ctx_1670, fpsInfo_1659, dFps_1707, chartParams_1658], function (fpsInfo_1731) {
                                                                                                      var datapoints_1732 = _Concat(leftPoints_1708, _Concat(middlePoint_1709, rightPoints_1712));
                                                                                                        var _kappa1927 = function (_1742) {
                                                                                                        var _kappa1926 = function (_1741) {
                                                                                                          _yieldCont(__kappa, {'2':datapoints_1732, '1':fpsInfo_1731})
                                                                                                          };
                                                                                                          if ( chartParams_1658['snap'] ) {
                                                                                                          (function (_1733) {
                                                                                                            (function (_1735) {
                                                                                                              (function (_1736) {
                                                                                                                (function (_1734) {
                                                                                                                  (function (_1737) {
                                                                                                                    (function (_1738) {
                                                                                                                      (function (_1739) {
                                                                                                                        (function (_1740) {
                                                                                                                          _yieldCont(_kappa1926, {})
                                                                                                                          })(_jsSaveCanvas(_1738, _1739, 'image/png'))
                                                                                                                        })(_getNodeById('download'))
                                                                                                                      })(_getNodeById(mainCanvas_1664))
                                                                                                                    })(_domSetAttributeFromRef(_1733, 'download', _1736 + ('-round-' + (_intToString(chartParams_1658['round']) + ('-timestamp-' + _intToString(_1734))))))
                                                                                                                  })(_clientTime())
                                                                                                                })(_domGetNodeValueFromRef(_1735))
                                                                                                              })(_getNodeById('otherInfo'))
                                                                                                            })(_getNodeById('download'))
                                                                                                          } else { _yieldCont(_kappa1926, {}) }
                                                                                                        };
                                                                                                        if ( doubleBuffer_1619 ) {
                                                                                                        _yield(swapBuffers_1636, [mainCanvas_1664, dispCanvas_1663], _kappa1927)
                                                                                                        } else { _yieldCont(_kappa1927, {}) }
                                                                                                      })
                                                                                                    })
                                                                                                  })
                                                                                                })
                                                                                              })(_jsSetFillColor(ctx_1670, '#770'))
                                                                                            })(_jsStroke(ctx_1670))
                                                                                          })
                                                                                        })
                                                                                      })(_jsSetFillColor(ctx_1670, '#888'))
                                                                                    })
                                                                                  })
                                                                                })(_jsSetFillColor(ctx_1670, '#2a2'))
                                                                              };
                                                                              if ( chartParams_1658['snap'] ) {
                                                                              function _fun__g127_1756(_1755, __kappa) {
                                                                                var x_1754 = _1755; _yieldCont(__kappa, LINKS.eq(x_1754, 1))
                                                                                }
                                                                                _yield(filter, [_fun__g127_1756, plottedLeftPoints_1730], function (_1757) {
                                                                                var leftPointsAboveMeasureCount_1758 = _length(_1757);
                                                                                  var leftPointsBelowMeasureCount_1759 = _length(plottedLeftPoints_1730) - leftPointsAboveMeasureCount_1758;
                                                                                  (function (_1763) {
                                                                                  (function (_1762) {
                                                                                    (function (_1760) {
                                                                                      (function (_1761) {
                                                                                        _yieldCont(_kappa1928, {})
                                                                                        })(_jsFillText(ctx_1670, 'ratio: ' + _1760, 300, mesPoint_1716 + 15))
                                                                                      })(_strsub(_floatToString(_intToFloat(leftPointsAboveMeasureCount_1758) / _intToFloat(leftPointsBelowMeasureCount_1759)), 0, 7))
                                                                                    })(_jsFillText(ctx_1670, 'frames below: ' + _intToString(leftPointsBelowMeasureCount_1759), 150, mesPoint_1716 + 15))
                                                                                  })(_jsFillText(ctx_1670, 'frames above: ' + _intToString(leftPointsAboveMeasureCount_1758), 10, mesPoint_1716 + 15))
                                                                                })
                                                                              } else { _yieldCont(_kappa1928, {}) }
                                                                            })
                                                                          })(_jsSetFillColor(ctx_1670, '#222'))
                                                                        })
                                                                      })
                                                                    })(_jsMoveTo(ctx_1670, _intToFloat(firstPoint_1718['1']), firstPoint_1718['2']))
                                                                  })
                                                                })(_hd(datapoints_1662))
                                                              })(_jsBeginPath(ctx_1670))
                                                            })
                                                          })
                                                        })(_jsSetFillColor(ctx_1670, 'black'))
                                                      })(_jsFillText(ctx_1670, 'frame', chartParams_1658['xScale'] - 40, chartParams_1658['yScale'] - 20))
                                                    })
                                                  })
                                                })
                                              })(_jsFillText(ctx_1670, _1784, 10, 30))
                                            })(_domGetNodeValueFromRef(_1783))
                                          })(_getNodeById('otherInfo'))
                                        })(_jsFillText(ctx_1670, 'double buffering: ' + _1786, 470, 10))
                                      };
                                      if ( doubleBuffer_1619 ) { _yieldCont(_kappa1929, 'on') } else { _yieldCont(_kappa1929, 'off') }
                                    })(_jsFillText(ctx_1670, 'yieldCount: ' + _intToString(_1788), 320, 10))
                                  })(_debugGetStats('yieldCount'))
                                })(_jsFillText(ctx_1670, 'granularity: ' + _intToString(_1790), 230, 10))
                              })(_debugGetStats('yieldGranularity'))
                            })
                          })(_jsGetContext2D(_1669))
                        })(_getNodeById(mainCanvas_1664))
                      };
                      if ( _1667 ) {
                      _yieldCont(_kappa1930, {'2':canvas2Id_1577, '1':canvasId_1576})
                      } else { _yieldCont(_kappa1930, {'2':canvasId_1576, '1':canvas2Id_1577}) }
                    };
                    if ( LINKS.eq(_1666, 'none') ) { _yieldCont(_kappa1931, true) } else { _yieldCont(_kappa1931, _not(doubleBuffer_1619)) }
                  })(_domGetStyleAttrFromRef(_1665, 'display'))
                })(_getNodeById(canvasId_1576))
              }
              function masterProc_1805(__kappa) {
              function masterLoop_1801(_1800, __kappa) {
                var procId_1799 = _1800;
                  _yield(recv, [], function (_1802) {
                  (function (_1803) { _yield(masterLoop_1801, [procId_1799], __kappa) })(_send(procId_1799, _1802['2']))
                  })
                }
                _yield(recv, [], function (_1804) { _yield(masterLoop_1801, [_1804['1']], __kappa) })
              }
              function _fun__g128_1807(__kappa) { _yield(masterProc_1805, [], function (_1806) { _yieldCont(__kappa, _1806) }) }
              (function (masterProcId_1808) {
              function dumProc_1813(_1812, __kappa) {
                var i_1809 = _1812;
                  _yield(recv, [], function (_1810) { var i_1811 = _Concat(i_1809, _Cons(_1810, Nil)); _yieldCont(__kappa, {}) })
                }
                function _fun__g129_1814(__kappa) { _yield(dumProc_1813, [Nil], __kappa) }
                (function (dumProcId_1815) {
                function updateLogic_1822(_1819, _1820, _1821, __kappa) {
                  var i_1816 = _1821;
                    var chartParams_1817 = _1820;
                    var dt_1818 = _1819;
                    if ( dt_1818 > step_1620 ) {
                    var _1825 = i_1816;
                      var _kappa1939 = function (lastInput_1828) {
                      var _kappa1938 = function (chartParams_1829) {
                        (function (_1830) {
                          _yield(updateLogic_1822, [dt_1818 - step_1620, chartParams_1829, Nil], __kappa)
                          })(_send(masterProcId_1808, {'2':{'_label':'KeyDown', '_value':- 1}, '1':dumProcId_1815}))
                        };
                        if ( LINKS.eq(lastInput_1828, {'_label':'KeyDown', '_value':upKeyCode_1583}) ) {
                        _yieldCont(_kappa1938, LINKS.union(LINKS.erase(chartParams_1817, ['measure']), {'measure':chartParams_1817['measure'] + 0.01}))
                        } else {
                        if ( LINKS.eq(lastInput_1828, {'_label':'KeyDown', '_value':downKeyCode_1584}) ) {
                          _yieldCont(_kappa1938, LINKS.union(LINKS.erase(chartParams_1817, ['measure']), {'measure':chartParams_1817['measure'] - 0.01}))
                          } else {
                          if ( LINKS.eq(lastInput_1828, {'_label':'KeyDown', '_value':leftKeyCode_1581}) ) {
                            _yieldCont(_kappa1938, LINKS.union(LINKS.erase(chartParams_1817, ['xScale']), {'xScale':chartParams_1817['xScale'] - 1}))
                            } else {
                            if ( LINKS.eq(lastInput_1828, {'_label':'KeyDown', '_value':rightKeyCode_1582}) ) {
                              _yieldCont(_kappa1938, LINKS.union(LINKS.erase(chartParams_1817, ['measure']), {'measure':chartParams_1817['xScale'] + 1}))
                              } else { _yieldCont(_kappa1938, chartParams_1817) }
                            }
                          }
                        }
                      };
                      if ( LINKS.eq(_1825, Nil) ) {
                      _yieldCont(_kappa1939, {'_label':'KeyDown', '_value':- 1})
                      } else {
                      (function (_1826) {
                        (function (_1827) { var xs_1824 = _1827; var x_1823 = _1826; _yieldCont(_kappa1939, x_1823) })(_tl(_1825))
                        })(_hd(_1825))
                      }
                    } else { _yieldCont(__kappa, {'2':dt_1818, '1':chartParams_1817}) }
                  }
                  function updateState_1831(__kappa) {
                  function mainLoop_1842(_1837, _1838, _1839, _1840, _1841, __kappa) {
                    var datapoints_1832 = _1841;
                      var fpsInfo_1833 = _1840;
                      var lastTime_1834 = _1839;
                      var dt_1835 = _1838;
                      var chartParams_1836 = _1837;
                      (function (now_1843) {
                      _yield(fmin, [1, _intToFloat(now_1843 - lastTime_1834) / 1000], function (_1844) {
                        var dt_1845 = dt_1835 + _1844;
                          (function (_1847) {
                          var _kappa1937 = function (i_1848) {
                            _yield(last, [i_1848], function (lastInput_1849) {
                              if ( LINKS.eq(lastInput_1849, {'_label':'KeyUp', '_value':restartKeyCode_1585}) ) {
                                _yieldCont(__kappa, {})
                                } else {
                                var _kappa1936 = function (chartParams_1850) {
                                  _yield(updateLogic_1822, [dt_1845, chartParams_1850, i_1848], function (_1853) {
                                    var dtprim_1851 = _1853['2'];
                                      var chartParamsPrim_1852 = _1853['1'];
                                      _yield(draw_1798, [datapoints_1832, lastTime_1834, now_1843, fpsInfo_1833, chartParamsPrim_1852], function (_1856) {
                                      var datapoints_1854 = _1856['2'];
                                        var fpsInfo_1855 = _1856['1'];
                                        var chartParamsPrim_1857 = LINKS.union(LINKS.erase(chartParamsPrim_1852, ['round']), {'round':chartParamsPrim_1852['round'] + 1});
                                        _yield(mainLoop_1842, [chartParamsPrim_1857, dtprim_1851, now_1843, fpsInfo_1855, datapoints_1854], __kappa)
                                      })
                                    })
                                  };
                                  if ( LINKS.eq(lastInput_1849, {'_label':'KeyDown', '_value':- 3}) ) {
                                  _yieldCont(_kappa1936, LINKS.union(LINKS.erase(chartParams_1836, ['snap']), {'snap':true}))
                                  } else { _yieldCont(_kappa1936, LINKS.union(LINKS.erase(chartParams_1836, ['snap']), {'snap':false})) }
                                }
                              })
                            };
                            if ( _1847 ) {
                            _yield(recv, [], _kappa1937)
                            } else {
                            (function (_1846) {
                              _yield(recv, [], _kappa1937)
                              })(_send(masterProcId_1808, {'2':{'_label':'KeyDown', '_value':- 2}, '1':dumProcId_1815}))
                            }
                          })(_haveMail())
                        })
                      })(_clientTime())
                    }
                    _yield(recv, [], function (_1863) {
                    _yield(ignore, [_1863], function (_1864) {
                      (function (_1861) {
                        _yield(mainLoop_1842, [initialChartParams_1587, 0, _1861, initialFpsInfo_1586, initialDatapoints_1602], function (_1862) {
                          (function (_1859) {
                            var _kappa1935 = function (_1860) { _yield(updateState_1831, [], __kappa) };
                              if ( _not(_1859) ) {
                              (function (_1858) { _kappa1935(_send(_1858, Nil)) })(_self())
                              } else { _yieldCont(_kappa1935, {}) }
                            })(_haveMail())
                          })
                        })(_clientTime())
                      })
                    })
                  }
                  function _fun__g130_1866(__kappa) { _yield(updateState_1831, [], function (_1865) { _yieldCont(__kappa, _1865) }) }
                  (function (updateProcId_1867) {
                  function inputStateLoop_1870(_1869, __kappa) {
                    var i_1868 = _1869;
                      _yield(recv, [], function (r_1871) {
                      var _kappa1934 = function (i_1872) {
                        var _kappa1933 = function (_1873) { _yield(inputStateLoop_1870, [i_1872], __kappa) };
                          if ( _not(LINKS.eq(r_1871, {'_label':'KeyDown', '_value':- 1})) ) {
                          _kappa1933(_send(updateProcId_1867, i_1872))
                          } else { _yieldCont(_kappa1933, {}) }
                        };
                        if ( _not(LINKS.eq(r_1871, {'_label':'KeyDown', '_value':- 1})) ) {
                        _yieldCont(_kappa1934, _Concat(i_1868, _Cons(r_1871, Nil)))
                        } else { _yieldCont(_kappa1934, Nil) }
                      })
                    }
                    function _fun__g131_1875(__kappa) { _yield(inputStateLoop_1870, [Nil], function (_1874) { _yieldCont(__kappa, _1874) }) }
                    (function (inputProcId_1876) {
                    function onKeyUp_1880(_1879, __kappa) {
                      var e_1877 = _1879;
                        (function (_1878) {
                        _yieldCont(__kappa, {})
                        })(_send(inputProcId_1876, {'_label':'KeyUp', '_value':_getCharCode(e_1877)}))
                      }
                      function onKeyDown_1884(_1883, __kappa) {
                      var e_1881 = _1883;
                        (function (_1882) {
                        _yieldCont(__kappa, {})
                        })(_send(inputProcId_1876, {'_label':'KeyDown', '_value':_getCharCode(e_1881)}))
                      }
                      (function (_1911) {
                      function initialize_1892(__kappa) {
                        _yield(recv, [], function (_1890) {
                          _yield(ignore, [_1890], function (_1891) {
                            (function (_1889) {
                              (function (_1888) {
                                (function (_1885) {
                                  (function (_1886) {
                                    _yield(ignore, [_1886], function (_1887) { __kappa(_send(updateProcId_1867, Nil)) })
                                    })(_domSetStyleAttrFromRef(_1885, 'display', 'none'))
                                  })(_getNodeById('info'))
                                })(_jsSetOnEvent(containerId_1578, 'keyup', onKeyUp_1880, true))
                              })(_jsSetOnKeyDown(containerId_1578, onKeyDown_1884))
                            })
                          })
                        }
                        function downloadCanvas_1899(__kappa) {
                        (function (_1893) {
                          (function (_1894) {
                            var _kappa1932 = function (canvas_1895) {
                              (function (_1896) {
                                (function (_1897) {
                                  (function (_1898) { _yieldCont(__kappa, {}) })(_jsSaveCanvas(_1896, _1897, 'image/png'))
                                  })(_getNodeById('download'))
                                })(_getNodeById(canvas_1895))
                              };
                              if ( LINKS.eq(_1894, 'none') ) {
                              _yieldCont(_kappa1932, canvas2Id_1577)
                              } else { _yieldCont(_kappa1932, canvasId_1576) }
                            })(_domGetStyleAttrFromRef(_1893, 'display'))
                          })(_getNodeById(canvasId_1576))
                        }
                        function _fun__g132_1900(__kappa) { _yield(initialize_1892, [], __kappa) }
                        (function (initializeProcId_1901) {
                        function _fun__g133_1905(_1904, __kappa) {
                          var event_1902 = _1904; (function (_1903) { _yieldCont(__kappa, {}) })(_send(initializeProcId_1901, 0))
                          }
                          (function (_1906) {
                          function _fun__g134_1909(_1908, __kappa) {
                            var event_1907 = _1908; __kappa(_send(inputProcId_1876, {'_label':'KeyDown', '_value':- 3}))
                            }
                            (function (_1910) {
                            _yield(bodyP, [LINKS.XML('html', {}, [_stringToXml('\n			'), LINKS.XML('head', {}, [_stringToXml('\n            '), LINKS.XML('style', {}, [_stringToXml('\n\n				#info '), _stringToXml('{'), _stringToXml('\n					font-size: 96px;\n				'), _stringToXml('}'), _stringToXml('\n\n				#'), _stringToXml(containerId_1578), _stringToXml(' '), _stringToXml('{'), _stringToXml('\n					position: relative;\n					width: '), _stringToXml(_floatToString(canvasWidth_1579)), _stringToXml('px;\n					height: 500px;\n					background-color: #ddd\n				'), _stringToXml('}'), _stringToXml('\n\n				#'), _stringToXml(canvas2Id_1577), _stringToXml(' '), _stringToXml('{'), _stringToXml('\n					display: none;\n					position: absolute;\n					top: 0px;\n					left: 0px;\n				'), _stringToXml('}'), _stringToXml('\n\n				#'), _stringToXml(canvasId_1576), _stringToXml(' '), _stringToXml('{'), _stringToXml('\n					display: block;\n					position: absolute;\n					top: 0px;\n					left: 0px;\n				'), _stringToXml('}'), _stringToXml('\n            ')]), _stringToXml('\n			')]), _stringToXml('\n\n			'), LINKS.XML('body', {}, [_stringToXml('\n				'), LINKS.XML('div', {'id':'container'}, [_stringToXml('\n					'), LINKS.XML('h1', {}, [_stringToXml('Links performance benchmark')]), _stringToXml('\n\n					'), LINKS.XML('div', {'tabindex':'1', 'key':_1906, 'id':containerId_1578}, [_stringToXml('\n						'), LINKS.XML('canvas', {'width':_floatToString(canvasWidth_1579), 'id':canvas2Id_1577, 'height':'500'}, []), _stringToXml('\n						'), LINKS.XML('canvas', {'width':_floatToString(canvasWidth_1579), 'id':canvasId_1576, 'height':'500'}, []), _stringToXml('\n						'), LINKS.XML('div', {'id':'info'}, [_stringToXml('Click this rectangle to start.')]), _stringToXml('\n					')]), _stringToXml('\n					'), LINKS.XML('div', {'id':'msg'}, []), _stringToXml('\n					'), LINKS.XML('a', {'tabindex':'2', 'key':_1910, 'id':'download', 'download':'1.png'}, [_stringToXml('Download as image')]), _stringToXml('\n					'), LINKS.XML('form', {}, [_stringToXml('\n						Other info: '), LINKS.XML('input', {'type':'text', 'style':'width:500px', 'id':'otherInfo'}, []), _stringToXml('\n					')]), _stringToXml(' \n				')]), _stringToXml('\n			')]), _stringToXml('\n		')])], __kappa)
                            })(_registerEventHandlers(_Cons({'2':_fun__g134_1909, '1':'focus'}, Nil)))
                          })(_registerEventHandlers(_Cons({'2':_fun__g133_1905, '1':'focus'}, Nil)))
                        })(_spawn(_fun__g132_1900))
                      })(_send(masterProcId_1808, {'2':{'_label':'KeyDown', '_value':- 2}, '1':inputProcId_1876}))
                    })(_spawn(_fun__g131_1875))
                  })(_spawn(_fun__g130_1866))
                })(_spawn(_fun__g129_1814))
              })(_spawn(_fun__g128_1807))
            })
          })
        })
      })
    })
  })
}
_yield(main, [], function (_1913) { _yield(renderPage, [_1913], _start) });
