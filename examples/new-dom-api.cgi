#!/home/s0567141/links/links -d

fun append() client {
 deck = domGetRefByID("deck");
 newJoke = <ul><li> Q: Why don't Buddhists vacuum in the corners?</li>
               <li> A: Because they have no attachments. </li> </ul>;
 domAppendChildXml(newJoke, deck);
}

fun appendToDoc() client {
 doc = domGetDocRef();
 choreList = <ul id="chores"><li>laundry</li>
                             <li>fix bike</li></ul>;
 domAppendChildXml(choreList, doc);
}

fun remove() client {
  chores = domGetRefByID("chores");
  domRemoveNodeRef(chores);
}

fun removeDoc() client {
  doc = domGetDocRef();
  domRemoveNodeRef(doc);
}

fun move() client {
  setup = domGetRefByID("setup");
  punchline = domGetRefByID("punchline");
  domInsertBeforeRef(punchline, setup);
}

fun moveAppend() client {
  deck = domGetRefByID("deck");
  punchline = domGetRefByID("punchline");
  domAppendChildRef(punchline, deck);
}

fun representation() client {
  deckRef = domGetRefByID("deck");
  deckXml = domGetRepresentation(deckRef);
  doc = domGetDocRef();
  domAppendChildXml(deckXml,doc)
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
