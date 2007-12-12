function _highlightCode(node)
{
  if (node.editor)
  {
    node.editor.highlight();
  }
  else
  {
    throw ("No editor: " + node);
  }
}

var highlightCode = LINKS.kify(_highlightCode, 1);

function _splitLines(text)
{
  text = LINKS.charlistToString(text);
  _debug("Before: " + text);
  //text = text.replace(/</g, "&lt;");
  //text = text.replace(/>/g, "&gt;");      
  text = text.split("\\n");

  for (var i = 0; i < text.length; i++)
  {
    text[i] = LINKS.stringToCharlist(text[i]); //.replace(/&gt;/g, ">"));
  }
  _debug("text length: " + String(text.length));
  return text;
}

var splitLinesJS = LINKS.kify(_splitLines, 1);

function _getValueFromRef(ref)
{
  return LINKS.stringToCharlist(ref.value);
}

var getValueFromRef = LINKS.kify(_getValueFromRef, 1);