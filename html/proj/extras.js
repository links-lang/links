/*
 * Extra functionality needed for the editor.
 * TODO: Cleanup, lots of unused stuff.
 */

var $str = LINKS.charlistToString;
var $chr = LINKS.stringToCharlist;

// Call the editor's highlight function.
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

// Split lines using javascript.
function _splitLines(text)
{
  // Split on new line.
  text = ($str(text)).split("%0A");

  // Loop through and cleanup.
  for (var i = 0; i < text.length; i++)
  {
    text[i] = $chr(unescape(text[i].replace(/>/, "&gt;")));
  }

  return text;
}

var splitEscapedLinesJS = LINKS.kify(_splitLines, 1);

// Get a node's value.
function _getValueFromRef(ref)
{
  return LINKS.stringToCharlist(ref.value);
}

var getValueFromRef = LINKS.kify(_getValueFromRef, 1);

// HTML escaping.
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

// Convert weird spaces to normal ones.
function _convertSpaces(text)
{
  return $chr($str(text).replace(/\u00a0/g, ' '));
}

var convertSpaces = LINKS.kify(_convertSpaces, 1);

// Create the editor.
function _loadEditor()
{
  var node = document.createElement("div");
  node.setAttribute("name", "editor");
  //var editor = new CodeMirror(node, {height: "100%"});
  //document.editor = editor;
  return node;
}

var loadEditor = LINKS.kify(_loadEditor, 0);

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

//var stringToString = LINKS.kify(_stringToString, 1);

// Get a list of sibling nodes.
// TODO: broken (probably)
function _getSiblings(node)
{
  var sibl = node.nextSibling;
  var sibList = (_isElementNode(sibl)) ? [sibl] : [];
  _debug(sibl);
  _debug(sibList);
  while (sibl.nextSibling)
  {
    sibl = sibl.nextSibling;
    _debug("Sibling: " + sibl);
    if (_isElementNode(sibl))
    {
      sibList.push(sibl);
    }
  }
   
  return sibList;
}

var getSiblings = LINKS.kify(_getSiblings, 1);

// Get the URL of the page.
function _getLocation()
{
  return $chr(window.location.href);
}

var getLocation = LINKS.kify(_getLocation, 0);

// Open a new window.
function _newWindow(loc)
{
  window.open($str(loc));
}

var newWindow = LINKS.kify(_newWindow, 1);

function testFn(xiEvent, xiNode)
{
  _debug("Event!");
  xiEvent.cancelBubble = true;
  if (xiEvent.stopPropagation) xiEvent.stopPropagation();
  xiEvent.preventDefault();
}