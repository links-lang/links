#!/home/s0567141/links/links -w

fun nextMsg(lastTime) server {
  db = database "postgresql:chat:localhost:5432:s0567141:";

  result = for line <- Table "chatlog" with {line : String, time : String} 
              order [ time : asc ] 
               from db
  where (line.time >> lastTime)
  in
     [(line.line, line.time)];

  if (result == []) {
    sleep(1);
    nextMsg(lastTime)
  }
  else
    result
}

fun serverPoll(lastTime) {
  result = nextMsg(lastTime);
  hd(result)
}

fun poll(count, dummy, lastTime) client {
    (msg, lastTime) = serverPoll(lastTime);
    dummy = domutate([AppendChild((id = "log",
                                   replacement = <li>{enxml(msg)}</li>))]);
    if (count << 50)
      poll(count+1, dummy, lastTime)
    else ()
}

fun startPoll() {
  poll(0, 0, "2006-03-14 00:00:00")
}

fun thelink(a, b) {
    say("salut " ++ string_of_int(a));
    domutate([ReplaceElement((id = "bar",
                  replacement = <a id="bar" l:href="{thelink(a+1, b)}">say "salut"</a>))]);
}

fun say(text) server {
  db = database "postgresql:chat:localhost:5432:s0567141:";
  insert into ("chatlog", db) values (line = text)
}

fun page(a, b) client {
  <html>
    <head>
      <title>Chat Server (Demo for Links)</title>
    </head>
    <a id="bar" l:href="{thelink(a, b)}">say "salut"</a>
    <form l:onsubmit="{say(text)}">
      <input type="text" l:name="text" />
      <input type="submit" value="Say it" />
    </form>
    <ul id="log">
    </ul>
  </html>
}

{
 spawn(startPoll)();
 page(1, 4)
}

