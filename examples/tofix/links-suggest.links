#!/home/s0567141/links/links -w 

fun fst(x) { (a, b) = x; a }
fun snd(x) { (a, b) = x; b }

words = [("crapulent", "Sickness due to excessive eating or drinking. (Predates the invention of Mr. Crapper.)"),
         ("crystallography", "The practice of determining the crystal structure of a solid."),
         ("corpulent", "Fleshy."),
         ("oddments", "Bits 'n' bobs."),
         ("odious", "HATEFUL"),
         ("ebullient", "Bubbly; full of energy or excitement."),
         ("elated", "Over the moon with happiness.")
        ];


fun assoc(d, a, l) {
  if (l == []) d
  else if (a == hd(l).1)
    hd(l).2
  else assoc(d, a, tl(l));
}

fun lookup(word) {
<html>
    {word ++ ": " ++ assoc("", word, words)}
</html>
}

fun main(suggestions) client {
<html>
<body>
Look up a word's definition: <br />

<form method="POST" l:onsubmit="{lookup(word)}"
      l:onkeyup="{main(completions(word))}">
<input type="hidden" name="method" value="lookup" /> <br />
<input type="text" l:name="word" value="" /> <br />
<input type="submit" name="btn" value="Submit" />
</form>

<div id="results">
  {x = for suggestion <- suggestions in
   <form l:onsubmit="{lookup(suggestion)}"><input type="submit" value="{suggestion}"/></form>;
   if (x == []) "" else <div>Suggestions: <br /> {x}</div>}
</div>

</body>
</html>
}

fun join(glue, lyst) {
  if (lyst == [])
    ""
  else
    hd(lyst) ++ glue ++ join(glue, tl(lyst));
#  ""
}

fun prefixof(p, str) {
  if (p == "") true
  else if (str == "") false
  else if (hd(p) == hd(str)) prefixof(tl(p), tl(str))
  else false
}

fun completions(p) server {
  if (p == "") [] else {
    results = for w <- words
              where prefixof(p, fst(w))
              in [fst(w)];
    results
  }
}

main([])

