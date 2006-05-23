#!/home/s0567141/links/links -d

fun append() client {
 deck = domOp([GetRefByID(id = "deck")]);
 newJoke = <ul><li> Q: Why don't Buddhists vacuum in the corners?</li>
               <li> A: Because they have no attachments. </li> </ul>;
 domOp([AppendChildXml(parentNode = deck, 
                       newChild = newJoke)]);
}

fun appendToDoc() client {
 doc = domOp([GetDocRef()]);
 domOp([AppendChildXml(parentNode = doc,
                       newChild = <ul id="chores"><li>laundry</li>
                                                  <li>fix bike</li></ul>
 )]);
}

fun remove() client {
  chores = domOp([GetRefByID(id = "chores")]);
  domOp([RemoveNodeRef(nodeRef = chores)]);
}

fun removeDoc() client {
  doc = domOp([GetDocRef()]);
  domOp([RemoveNodeRef(nodeRef = doc)]);
}

fun move() client {
  setup = domOp([GetRefByID(id = "setup")]);
  punchline = domOp([GetRefByID(id = "punchline")]);
  domOp([InsertBeforeRef(moveRef = punchline, beforeRef = setup)]);
}

fun moveAppend() client {
  deck = domOp([GetRefByID(id = "deck")]);
  punchline = domOp([GetRefByID(id = "punchline")]);
  domOp([AppendChildRef(moveRef = punchline, parentRef = deck)]);
}

fun representation() client {
  deckRef = domOp([GetRefByID(id = "deck")]);
  deckXml = domOp([GetRepresentation((nodeRef=deckRef))]);
  doc = domOp([GetDocRef()]);
  domOp([AppendChildXml(parentNode = doc,
                        newChild = deckXml)])
}

fun deckTagName() {
  deck = domGetRefByID("deck");
  deckXml = domGetRepresentation(deck);
  debug(getTagName(deckXml));
}

fun deckStyle() {
  deck = domGetRefByID("deck");
  deckXml = domGetRepresentation(deck);
  debug(getAttribute(deckXml, "style"));
}

<html>
 <body>
  <a l:onclick="{appendToDoc()}">Append Chores</a>
  <a l:onclick="{remove()}">Remove Chores</a>
  <a l:onclick="{append()}">Append Extra Punchline</a>
  <a l:onclick="{move()}">Move punchline before setup</a>
  <a l:onclick="{moveAppend()}">Move punchline to end</a>
  <a l:onclick="{representation()}">Copy deck using XML rep'n</a>
  <a l:onclick="{deckTagName()}">What is the tag name on `deck'?</a>
  <a l:onclick="{deckStyle()}">What is the style of `deck'?</a>
  <div id="deck" style="border: 1px solid black">
    <h1 id="setup">A Buddhist walks up to a hot dog stand.</h1>
    <p id="punchline">He says, "Can you make me one with everything?"</p>
  </div>
 </body>
</html>

