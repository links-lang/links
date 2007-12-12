var $str = LINKS.charlistToString;
var $chr = LINKS.stringToCharlist;

function _highlightCode()
{
  if (window.document.editor)
  {
    window.document.editor.highlight();
  }
  else
  {
    throw ("No editor in current doc!");
  }
}

var highlightCode = LINKS.kify(_highlightCode, 0);

function _splitLines(text)
{
  text = ($str(text)).split("%0A");
  for (var i = 0; i < text.length; i++)
  {
    text[i] = $chr(unescape(text[i].replace(/>/, "&gt;")));
  }

  return text;
}

var splitEscapedLinesJS = LINKS.kify(_splitLines, 1);

function _getValueFromRef(ref)
{
  return LINKS.stringToCharlist(ref.value);
}

var getValueFromRef = LINKS.kify(_getValueFromRef, 1);

function _escape(text)
{
  return $chr(escape($str(text)));
}

function _unescape(text)
{
  return $chr(unescape($str(text)));
}

var escapeHtml = LINKS.kify(_escape, 1);
var unescapeHtml = LINKS.kify(_unescape, 1);

function _convertSpaces(text)
{
  return $chr($str(text).replace(/\u00a0/g, ' '));
}

var convertSpaces = LINKS.kify(_convertSpaces, 1);

function _loadEditor()
{
  var node = document.createElement("div");
  node.setAttribute("name", "editor");
  _debug("Created node.");
  //var editor = new CodeMirror(node, {height: "100%"});
  _debug("Editor fetched!");
  //document.editor = editor;
  _debug("Returning..");
  return node;
}

var loadEditor = LINKS.kify(_loadEditor, 0);

function _loadIncludes()
{
  var includes = "<script src=\"CodeMirror/Mochi.js\" type=\"text/javascript\"></script><script src=\"CodeMirror/util.js\" type=\"text/javascript\"></script><script src=\"CodeMirror/tokenizelinks.js\" type=\"text/javascript\"></script><script src=\"CodeMirror/parselinks.js\" type=\"text/javascript\"></script><script src=\"CodeMirror/stringstream.js\" type=\"text/javascript\"></script><script src=\"CodeMirror/select.js\" type=\"text/javascript\"></script><script src=\"CodeMirror/codemirror.js\" type=\"text/javascript\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\"CodeMirror/highlight.css\"/>";
  document.write(includes);
  _debug("Loaded includes.");
}

var loadIncludes = LINKS.kify(_loadEditor, 0);

function _getIframeBody(node)
{
  return node; //.childNodes[0].getContentDocument.getElementByName("body");
}

var getIframeBody = LINKS.kify(_getIframeBody(), 1);

function _stringToString(string)
{
  _debug("In: " + $str(string));
  var out = $chl($str(string));
  _debug("out: " + out);
  return out;
}

var stringToString = LINKS.kify(_stringToString, 1);