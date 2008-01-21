/* Tokenizer for Links code */

/* Extrapolated from the original tokenizejavascript.js */

var tokenizeLinks = function(){
  // A map of JavaScript's keywords. The a/b/c keyword distinction is
  // very rough, but it gives the parser enough information to parse
  // correct code correctly (we don't care much how we parse incorrect
  // code). The style information included in these objects is used by
  // the highlighter to pick the correct CSS style for a token.
  var keywords = function(){
    function result(type, style){
      return {type: type, style: style};
    }
    // keywords that take a parenthised expression, and then a
    // statement (if)
    var keywordA = result("keyword a", "keyword");
    // keywords that take just a statement (else)
    var keywordB = result("keyword b", "keyword");
    // keywords that optionally take an expression, and form a
    // statement (return)
    var keywordC = result("keyword c", "keyword");
    var operator = result("operator", "keyword");
    var atom = result("atom", "atom");
    var type = result("type", "type");
    return {
      "if": keywordA, "where": keywordA, "orderby": keywordA,
      "else": keywordB, "yields": keywordB, "receive": keywordB,
      "delete": keywordC, 
      "table": keywordC, "with": keywordC, "database": keywordC, "from": keywordC,
      "insert": keywordC, "values": keywordC, "set": keywordC, "page" : keywordC,
      "alien": keywordC, "as": keywordC, "in": keywordC,
      "var": result("var", "keyword"), "fun": result("function", "keyword"),
      "typename": result("typename", "keyword"),
      "for": result("for", "keyword"), "case": result("case", "keyword"), 
      "switch": result("switch", "keyword"), "update": result("for", "keyword"),
      "delete": result("for", "keyword"), "sig": result("sig", "keyword"),
      "formlet": result("formlet", "keyword"), "spawn": result("spawn", "keyword"), 
      "client": result("location", "location"), "server": result("location", "location"),
      "app": result("keyword", "keyword"), "spawnWait": result("spawn", "keyword"),
      "native": result("keyword", "keyword"), "escape": result("keyword", "keyword"),
      "op": result("fn", "keyword"), "mu": result("fn", "keyword"),
      "abs": result("fn", "keyword"), "escape": result("keyword", "keyword"),
      "infix": result("fn", "keyword"), "infixl": result("fn", "keyword"),
      "infixr": result("fn", "keyword"),
      "true": atom, "false": atom, "[]": atom, "readonly": atom,
      "Int": type, "Bool": type, "Char": type, "Float": type, "Xml": type, "Database": type, "String": type, "TableHandle": type
    };
  }();

  // Some helper regexp matchers. 
  var isOperatorChar = matcher(/[\+\-\^\*\&\%\/=<>!\.:\|\~]|mod|not/);
  var isDigit        = matcher(/[0-9]/);
  var isHexDigit     = matcher(/[0-9A-Fa-f]/);
  var isWordChar     = matcher(/[\w#_]/);
  var isNameOrAttr   = matcher(/[\w:]/);
  var isLinksTag     = matcher(/#/);

  function isWhiteSpace(ch){
    // Unfortunately, IE's regexp matcher thinks non-breaking spaces
    // aren't whitespace. Also, in our scheme newlines are no
    // whitespace (they are another special case).
    return ch != "\n" && (ch == nbsp || /\s/.test(ch));
  }

  // This function produces a MochiKit-style iterator that tokenizes
  // the output of the given stringstream (see stringstream.js).
  // Tokens are objects with a type, style, and value property. The
  // value contains the textual content of the token. Because this may
  // include trailing whitespace (for efficiency reasons), some
  // tokens, such a variable names, also have a name property
  // containing their actual textual value.
  return function(source){
    // Produce a value to return. Automatically skips and includes any
    // whitespace. The base argument is prepended to the value
    // property and assigned to the name property -- this is used when
    // the caller has already extracted the text from the stream
    // himself.
    function result(type, style, base){
      nextWhile(isWhiteSpace);
      var value = {type: type, style: style, value: (base ? base + source.get() : source.get())};
      if (base) value.name = base;
      return value;
    }
    
    // Are we waiting for a tag name?
    var isNextTagName = false;
    
    // Have we just closed a tag?
    var isNextTagClose = false;

    // Advance the text stream over characters for which test returns
    // true. (The characters that are 'consumed' like this can later
    // be retrieved by calling source.get()).
    function nextWhile(test){
      var next;
      while((next = source.peek()) && test(next))
        source.next();
    }
    // Advance the stream until the given character (not preceded by a
    // backslash) is encountered (or a newline is found).
    function nextUntilUnescaped(end){
      var escaped = false;
      var next;
      while((next = source.peek()) && next != "\n"){
        source.next();
        if (next == end && !escaped)
          break;
        escaped = next == "\\";
      }
    }
  
    function readHexNumber(){
      source.next(); // skip the 'x'
      nextWhile(isHexDigit);
      return result("number", "atom");
    }
    function readNumber(){
      nextWhile(isDigit);
      if (source.peek() == "."){
        source.next();
        nextWhile(isDigit);
      }
      if (source.peek() == "e" || source.peek() == "E"){
        source.next();
        if (source.peek() == "-")
          source.next();
        nextWhile(isDigit);
      }
      return result("number", "atom");
    }
    // Read a word, look it up in keywords. If not found, it is a
    // variable, otherwise it is a keyword of the type found.
    function readWord(){
      nextWhile(isWordChar);
      var word = source.get();
      var known = keywords.hasOwnProperty(word) && keywords.propertyIsEnumerable(word) && keywords[word];
      return known ? result(known.type, known.style, word) : result("variable", "variable", word);
    }
    function readRegexp(){
      nextUntilUnescaped("/");
      nextWhile(matcher(/[gi]/));
      return result("regexp", "string");
    }
    // Fetch the next token. Dispatches on first character in the
    // stream, or first two characters when the first is a slash. The
    // || things are a silly trick to keep simple cases on a single
    // line.
    function next(str){
      var ch = source.next();
      var peek = source.peek();
      var token = null;
      
      // Check which language we are working with
      if (str == "links")
      {
        // Tokenize for Links
        if (ch == "\n")
        {
          val = source.get();
          token = {type: "newline", style: "whitespace", value: val};
        }
        else if (isWhiteSpace(ch))
          token = nextWhile(isWhiteSpace) || result("whitespace", "whitespace");
        else if (ch == "\"")
          token = nextUntilUnescaped(ch) || result("string", "string");
        else if (ch == "'")
          token = nextUntilUnescaped(ch) || result("char", "char");
        else if (ch == "[" && source.peek() == "|") {
          // read the next character too, then set this to be a variant
          source.next();
          token = result("variant", "punctuation");
        }
        else if (ch == "|" && peek == "]") {
          // read the next character too, then set this to be punctuation
          source.next();
          token = result("|]", "punctuation");
        } 
        // with punctuation, the type of the token is the symbol itself
        else if (/[\[\]{}\(\),;\:\.]/.test(ch))
          token = result(ch, "punctuation");
        else if (ch == "0" && (peek == "x" || peek == "X"))
          token = readHexNumber();
        else if (isDigit(ch))
          token = readNumber();
        else if (ch == "#")
          token = nextUntilUnescaped(null) || result("comment", "comment");
        else if ((ch == "<") && (isWordChar(peek)) && (peek != null)) {
          // We've found HTML, and we're expecting a tag name next
          isNextTagName = true;
          token = result("html", "html");
        }
        else if (ch == "/"){
          if (this.regexp)
            token = readRegexp();
          else
            token = nextWhile(isOperatorChar) || result("operator", "operator");
        }
        else if (isOperatorChar(ch))
          token = nextWhile(isOperatorChar) || result("operator", "operator");
        else
          token = readWord();

        // JavaScript's syntax rules for when a slash might be the start
        // of a regexp and when it is just a division operator are kind
        // of non-obvious. This decides, based on the current token,
        // whether the next token could be a regular expression.
        if (token.style != "whitespace" && token != "comment")
          this.regexp = token.type == "operator" || token.type == "keyword c" || token.type.match(/[\[{}\(,;:]/);
      }
      else if (str == "html")
      {
        if (ch == "\n")
          token = {type: "newline", style: "whitespace", value: source.get()};
        else if (isWhiteSpace(ch))
          token = nextWhile(isWhiteSpace) || result("whitespace", "whitespace");
        else if (isWordChar(ch) && isNextTagName)
        {
          nextWhile(isWordChar);
          nextWhile(isLinksTag);
          token = result("tag", "html");
          isNextTagName = false;
          inTag = true;
        }
        else if ((ch == "/") && (isNextTagName || peek == ">"))
        {
          // Grab the tag name
          nextWhile(isWordChar);
          nextWhile(isLinksTag);
         
          // And eat the operators too
          nextWhile(isOperatorChar);
          token = result("endtag", "html")
          isNextTagName = false;
          inTag = false;
        }
        else if (ch == "<")
        {
          nextCh = peek;
          // See if this is a possible tag
          if (nextCh == "/")
          {
            // This is probably an end tag
            // Include the slash
            nextWhile(isOperatorChar);
           
            // Grab the rest of the tag
            nextWhile(isWordChar);
            nextWhile(isLinksTag);

            // And the final bracket
            nextUntilUnescaped(">");

            token = result("endtag", "html");
          }
          else if (isWordChar(nextCh))
          {
            // New tag
            // Grab the tag name
            nextWhile(isWordChar);
            nextWhile(isLinksTag);
            token = result("tag", "html");
          }
          else
          {
            nextWhile(isWordChar);      
            token = result("text", "text");
          }
        }
        else if (inTag && (ch == "l" && peek == ":"))
        { 
          // This is an event, so grab the : and the word
          source.next();          
          nextWhile(isWordChar);
          token = result("text", "event-variable");
        }
        else if (ch == ">")
          token = result("html", "html");
        else if (ch == "{")
        {
          if (peek == "{")
          {
            nextWhile(matcher(/{/));
            token = result("text", "text");
          }
          else
          {
            token = result("{", "punctuation");
          }
        }
        else
        {
          nextWhile(isWordChar);
          token = result("text", "text");
          isNextTagName = false;
        }
      }
      else
      {
        throw "Unknown parser type!";
      }
      return token;
    }

    // Wrap it in an iterator. The state (regexp, inHTML, topTag and currTag) is
    // exposed because a parser will need to save it when making a
    // copy of its state.
    return {next: next, regexp: true, tokenizers: {links: "links", html: "html"}};
  }
}();