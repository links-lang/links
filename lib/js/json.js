/*
Copyright (c) 2005 JSON.org

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The Software shall be used for Good, not Evil.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

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

/*
    The global object JSON contains two methods.

    JSON.stringify(value) takes a JavaScript value and produces a JSON text.
    The value must not be cyclical.

    JSON.parse(text) takes a JSON text and produces a JavaScript value. It will
    return false if there is an error.
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
        s = {
            'function' : function (x) {
              throw("Internal error: Can't JSONize functions.");
            },
            'boolean': function (x) {
                return String(x);
            },
            number: function (x) {
                return isFinite(x) ? String(x) : 'null';
            },
            string: function (x) {
                if (/["\\\x00-\x1f]/.test(x)) {
                    x = x.replace(/([\x00-\x1f\\"])/g, function(a, b) {
                        var c = m[b];
                        if (c) {
                            return c;
                        }
                        c = b.charCodeAt();
                        return '\\u00' +
                            Math.floor(c / 16).toString(16) +
                            (c % 16).toString(16);
                    });
                }
                return '"' + x + '"';
            },
            object: function (x) {
                if (x) {
                    var a = [], b, f, i, l, v;
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
                                v = f(v);
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
                                v = f(v);
                                if (typeof v == 'string') {
                                    if (b) {
                                        a[a.length] = ',';
                                    }
                                    a.push(s.string(i), ':', v);
                                    b = true;
                                }
                            }
                        }
                        a[a.length] = '}';
                    } else {
                        return;
                    }
                    return a.join('');
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
            if (f) {
                v = f(v);
                if (typeof v == 'string') {
                    return v;
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
                return !(/[^,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]/.test(
                        text.replace(/"(\\.|[^"\\])*"/g, ''))) &&
                    eval("(" + text + ")");
            } catch (e) {
                // [SL]
                //   Returning false here is insane!
                //   How are we supposed to distinguish between a
                //   boolean expression and a syntax error?
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

