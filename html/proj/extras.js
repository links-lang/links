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

var highlightCode = LINKS.kify(_highlightCode);

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

var splitEscapedLinesJS = LINKS.kify(_splitLines);

// Get a node's value.
function _getValueFromRef(ref)
{
  return LINKS.stringToCharlist(ref.value);
}

var getValueFromRef = LINKS.kify(_getValueFromRef);

// Convert weird spaces to normal ones.
function _convertSpaces(text)
{
  return $chr($str(text).replace(/\u00a0/g, ' '));
}

var convertSpaces = LINKS.kify(_convertSpaces);

// Create the editor.
function _loadEditor()
{
  var node = document.createElement("div");
  node.setAttribute("name", "editor");
  //var editor = new CodeMirror(node, {height: "100%"});
  //document.editor = editor;
  return node;
}

var loadEditor = LINKS.kify(_loadEditor);

function _getIframeBody(node)
{
  return node; //.childNodes[0].getContentDocument.getElementByName("body");
}

var getIframeBody = LINKS.kify(_getIframeBody());

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

var getSiblings = LINKS.kify(_getSiblings);

// Get the URL of the page.
function _getLocation()
{
  return $chr(window.location.href);
}

var getLocation = LINKS.kify(_getLocation);

// Open a new window.
function _newWindow(loc)
{
  window.open($str(loc));
}

var newWindow = LINKS.kify(_newWindow);

function _unescape(xiString)
{
  return $chr(unescape($str(xiString)));
}

var jsUnescape = LINKS.kify(_unescape);

function _escape(xiString)
{
  return $chr(escape($str(xiString)));
}

var DEBUGGING = true;
var jsEscape = LINKS.kify(_escape);
var _dwindow = DEBUGGING ? open('', 'debugwindow','width=550,height=800,toolbar=0,scrollbars=yes') : null;
function _debug(msg) {
   if (DEBUGGING) {
     _dwindow.document.write('<b>' + _current_pid + '</b> : ' + msg + '<br/>');
   }
}

// Escape slashes.
function _escapeSlashes(xiString)
{
  return $chl(($str(xiString)).replace("\\", "\\\\"));
}

var jsEscapeSlashes = LINKS.kify(_escapeSlashes);

// Convert from collection to array
function _collectionToArray(coll) {
  var arr = new Array();

  for (i = 0; i < coll.length; i++)
  {
    arr[i] = coll[i];
  }
  return arr;
}

function _getElsFromDoc(doc, name) 
{
  return _collectionToArray(doc.getElementsByName($str(name)));
}

var getElementsByNameFromDocument = LINKS.kify(_getElsFromDoc);

function _getTextValue(node)
{
  return $chl(node.nodeValue);
}

var getTextValue = LINKS.kify(_getTextValue);

function _jsElem(list, item)
{
  var lItem = $str(item);
  list.map($str);
  return (list.indexOf(item) > -1);
}

var jsElem = LINKS.kify(_jsElem);
  

function _redirectClient(loc)
{
  window.location.href = $str(loc);
}

var redirectClient = LINKS.kify(_redirectClient);

function _enableEditable(node)
{
  node.contentDocument.designMode = "on";
}

var enableEditable = LINKS.kify(_enableEditable);

function _tmpIsNull(node)
{
  try
  {
    var lTmp = node.nodeType;
  } 
  catch (e)
  {
    return true;
  }
  
  return false;
}

var tmpIsNull = LINKS.kify(_tmpIsNull);

// JavaScript version of Links' client-side regex
function _jsMatches(string, pattern)
{
  var lPattern = new RegExp("^" + $str(pattern) + "$");
  var lString = $str(string); 

  return lPattern.test(lString)
}

var jsMatches = LINKS.kify(_jsMatches);
  