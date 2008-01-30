/* Links parser
 *
 * Extrapolated from the original parsejavascript.js.
 *
 * A parser that can be plugged into the CodeMirror system has to
 * implement the following interface: It is a function that, when
 * called with a string stream (stringstream.js) as an argument,
 * returns a MochiKit-style iterator (object with a 'next' method).
 * This iterator, when called, consumes some input from the string
 * stream, and returns a token object. Token objects must have a
 * 'value' property (the text they represent), a 'style' property (the
 * CSS style that should be used to colour them). Tokens for newline
 * characters must also have a 'lexicalContext' property, which has an
 * 'indentation' method that can be used to determine the proper
 * indentation level for the next line. This method optionally takes
 * the first character of the next line as an argument, which it can
 * use to adjust the indentation level.
 *
 * So far this should be easy. The hard part is that the iterator
 * produced by the parse function must also have a 'copy' method. This
 * method, called without arguments, returns a function representing
 * the current state of the parser. When this function is later called
 * with a string stream as its argument, it returns a parser iterator
 * object that resumes parsing using the old state and the new input
 * stream. It may assume that only one parser is active at a time, and
 * clobber the state of the old parser (the implementation below
 * certianly does).
 */

// Parse function for JavaScript. Makes use of the tokenizer from
// tokenizejavascript.js. Note that your parsers do not have to be
// this complicated -- if you don't want to recognize local variables,
// in many languages it is enough to just look for braces, semicolons,
// parentheses, etc, and know when you are inside a string or comment.
Editor.Parser = (function() {
  // Token types that can be considered to be atoms.
  var atomicTypes = {"atom": true, "number": true, "variable": true, "string": true, "regexp": true, "char": true};
  
  // Constructor for the lexical context objects.
  function LLexical(indented, column, type, align, prev) {
    // indentation at start of this line
    this.indented = indented;
    // column at which this scope was opened
    this.column = column;
    // type of scope ('vardef', 'stat' (statement), '[', '{', or '(')
    this.type = type;
    // '[', '{', or '(' blocks that have any text after their opening
    // character are said to be 'aligned' -- any lines below are
    // indented all the way to the opening character.
    if (align != null)
      this.align = align;
    // Parent scope, if any.
    this.prev = prev;
  }
  // Indentation rules.
  function indentLinks(lexical) {
    return function(firstChar) {
      var closing = firstChar == lexical.type;
      if (lexical.type == "vardef")
        return lexical.indented + 4;
      if ((lexical.type == "stat") || (lexical.type == "block"))
        return lexical.indented;
      else if (lexical.type == "tag")
        return lexical.indented + 2;
      else if (lexical.align)
        return lexical.column - (closing ? 1 : 0);
      else
        return lexical.indented + (closing ? 0 : 2);
    }
  }

  // The parser-iterator-producing function itself.
  function parseLinks(input){

    // Wrap the input in a token stream using the tokenizer
    var tokens = tokenizeLinks(input);

    // Tokenizer for links
    var links = tokens.tokenizers.links;
    var html = tokens.tokenizers.html;

    // The parser state. cc is a stack of actions that have to be
    // performed to finish the current statement. For example we might
    // know that we still need to find a closing parenthesis and a
    // semicolon. Actions at the end of the stack go first. It is
    // initialized with an infinitely looping action that consumes
    // whole statements.
    var cc = [statements];
    // Context contains information about the current local scope, the
    // variables defined in that, and the scopes above it.
    var context = {variants: [], prev: null};
    // The lexical scope, used mostly for indentation.
    var lexical = new LLexical(0, 0, "block", false);

    // Current column, and the indentation at the start of the current
    // line. Used to create lexical scope objects.
    var column = 0;
    var indented = 0;
    // Variables which are used by the mark, cont, and pass functions
    // below to communicate with the driver loop in the 'next'
    // function.
    var consume, marked;
    // List of variants
    var variants = [];
  
    // The iterator object.
    var parser = {next: next, copy: copy};

    function next(){
      // Start by performing any 'lexical' actions (adjusting the
      // lexical variable), or the operations below will be working
      // with the wrong lexical state.
      while(cc[cc.length - 1].lex)
        cc.pop()();

      // What language are we parsing?
      var language = (context && context.language) ? context.language : "links";
      // Fetch a token.
      var token = tokens.next(language);
      // Adjust column and indented.
      if (token.type == "whitespace" && column == 0)
        indented = token.value.length;
      column += token.value.length;
      if (token.type == "newline"){
	indented = column = 0;
        // If the lexical scope's align property is still undefined at
        // the end of the line, it is an un-aligned scope.
        if (!("align" in lexical))
          lexical.align = false;
        // Newline tokens get a lexical context associated with them,
        // which is used for indentation.
        token.indentation = indentLinks(lexical);
      }
      // No more processing for meaningless tokens.
      if (token.type == "whitespace" || token.type == "newline" || token.type == "comment")
        return token;
      // When a meaningful token is found and the lexical scope's
      // align is undefined, it is an aligned scope.
      if (!("align" in lexical))
        lexical.align = true;

      // Execute actions until one 'consumes' the token and we can
      // return it. Marked is used to 
      while(true){
        consume = marked = registered = false;
	// Take and execute the topmost action.
        cc.pop()(token.type, token.name);
        if (consume){
	  // Marked is used to change the style of the current token.
          if (marked)
            token.style = marked;
          
	  // Here we differentiate between local and global variables.
          if (token.type == "variable" && inScope(token.name))
            token.style = registered ? marked : "localvariable";
          // Type aliases and polymorphic variants should be marked
          // @TODO: Assumes allowed at top-level only.. :(
          else if (token.type == "variable" && variantInScope(token.name))
            token.style = "type";
          return token;
        }
      }
    }

    // This makes a copy of the parser state. It stores all the
    // stateful variables in a closure, and returns a function that
    // will restore them when called with a new input stream. Note
    // that the cc array has to be copied, because it is contantly
    // being modified. Lexical objects are not mutated, and context
    // objects are not mutated in a harmful way, so they can be shared
    // between runs of the parser.
    function copy(){
      var _context = context, _lexical = lexical, _cc = cc.concat([]), _regexp = tokens.regexp, _comment = tokens.inComment;
  
      return function(input){
        context = _context;
        lexical = _lexical;
        cc = _cc.concat([]); // copies the array
        column = indented = 0;
        tokens = tokenizeLinks(input);
        tokens.regexp = _regexp;
        tokens.inComment = _comment;
        return parser;
      };
    }

    // Helper function for pushing a number of actions onto the cc
    // stack in reverse order.
    function push(fs){
      for (var i = fs.length - 1; i >= 0; i--)
        cc.push(fs[i]);
    }
    // cont and pass are used by the action functions to add other
    // actions to the stack. cont will cause the current token to be
    // consumed, pass will leave it for the next action.
    function cont(){
      push(arguments);
      consume = true;
    }
    function pass(){
      push(arguments);
      consume = false;
    }
    // Used to change the style of the current token.
    function mark(style){
      marked = style;
    }

    // Push a new scope. Will automatically link the the current
    // scope.
    function pushcontext(lang, toptag){
      context = {prev: context, vars: [], language: lang, variants: []};
      return function() { nothing = null; };
    }
    // Pop off the current scope.
    function popcontext(){
      context = context.prev;
    }
    // Set the 'lex' of this context to true, meaning that
    // it will be popped of the stack before reading a new token.
    popcontext.lex = true;

    // Register a variable in the current scope.
    function register(varname){
      if (context.vars){
        mark("variabledef");
        registered = true;
        context.vars[varname] = true;
      }
    }
    // Check whether a variable/variant is defined in the current scope.
    function inScope(varname){
      var cursor = context;
      while (cursor.vars) {
        if (cursor.vars[varname])
          return true;
        cursor = cursor.prev;
      }
      return false;
    }

    function variantInScope(variant) {
      var cursor = context;
      while (cursor) {
        if ((cursor.variants.length > 0) && (cursor.variants[variant]))
          return true;
        cursor = cursor.prev;
      }
      return false;
    }

    // Register a new variant in this scope
    function registerVariant(value) {
      context.variants[value] = true;
      mark("variant");
    }
  
    // Push a new lexical context of the given type.
    function pushlex(type){
      var result = function(){
        lexical = new LLexical(indented, column, type, null, lexical)
      };
      result.lex = true;
      return result;
    }
    // Pop off the current lexical context.
    function poplex(){
      lexical = lexical.prev;
    }
    poplex.lex = true;
    // The 'lex' flag on these actions is used by the 'next' function
    // to know they can (and have to) be ran before moving on to the
    // next token.
  
    // Creates an action that discards tokens until it finds one of
    // the given type.
    function expect(wanted){
      return function(type){
        if (type == wanted) cont();
        else pass(statement, expect(wanted));
      };
    }

    // Looks for a statement, and then calls itself.
    function statements(type){
      return pass(statement, statements);
    }
    // Dispatches various types of statements based on the type of the
    // current token.
    function statement(type){
      if (context && context.language == "html")
        pass(parsehtml);
      else if (type == "var") cont(pushlex("vardef"), maybevarlist, expect(";"), poplex);
      else if (type == "typename") cont(pushlex("vardef"), typedef, expect(";"), poplex);
      else if (type == "sig") cont(pushlex("vardef"), sigdef, poplex);
      else if (type == "keyword a") cont(pushlex("stat"), expression, statement, poplex);
      else if (type == "keyword b") cont(pushlex("stat"), statement, poplex);
      else if (type == "{") cont(pushlex("}"), block, poplex);
      else if (type == "function") cont(functiondef);
      else if (type == "for") cont(pushlex("stat"), forspec1, poplex);
      else if (type == "case") cont(expression);
      else if (type == "variable") cont(pushlex("stat"), maybeoperator, poplex);
      else if (type == "switch") cont(switchdef);
      else if (type == "formlet") cont(pushlex("stat"), expect("html"), poplex);
      else if (type == "html") pass(pushcontext(html, type), statement, popcontext);
      else pass(pushlex("stat"), expression, poplex);
    }
    // Dispatch expression types.
    function expression(type){
      if (context && context.language == "html")
        pass(parsehtml);
      else if (atomicTypes.hasOwnProperty(type)) cont(maybeoperator);
      else if (type == "function") cont(functiondef);
      else if (type == "keyword c") cont(expression);
      else if (type == "(") cont(pushlex(")"), expect(")"), poplex);
      else if (type == "operator") cont(expression);
      else if (type == "[") cont(pushlex("]"), expect("]"), poplex);
      else if (type == "{") cont(pushlex("}"), commasep(objprop), expect("}"), poplex);
      else if (type == "variant") cont(pushlex("|]"), opsep(variantdef), expect("|]"), poplex);
      else if (type == "html") cont(pushcontext(html, type), statement, popcontext);
      else if (type == "spawn") cont(pushlex("stat"), statement, poplex);
      else if (type == ":") cont(maybetype);
      else if (type == "type") cont();
      else if (type == ";") cont();
      else if (type == ",") cont(statement);
      // This token is unknown to us - eat it.
      else cont();
    }
    // Called for places where operators, function calls, or
    // subscripts are valid. Will skip on to the next action if none
    // is found.
    function maybeoperator(type){
      if (type == "operator") cont(expression);
      // TODO: ensure removal of commasep is ok
      else if (type == "(") cont(pushlex(")"), expect(")"), poplex);
      else if (type == ".") cont(property, maybeoperator);
      else if (type == ":") cont(maybetype);
      else if (type == "[") cont(pushlex("]"), expression, expect("]"));
    }
    
    // broken
    function maybetype(type) {
      if (type == "type") cont();
      else pass(statement);
    }

    // When a statement starts with a variable name, it might be a
    // label. If no colon follows, it's a regular statement.
    function maybelabel(type){
      if (type == ":") cont(poplex, statement);
      else pass(maybeoperator, poplex);
    }
    // Property names need to have their style adjusted -- the
    // tokenizer think they are variables.
    function property(type){
      if (type == "variable") {mark("property"); cont();}
    }
    // This parses a property and its value in an object literal.
    function objprop(type){
      if (type == "variable") mark("property");
      if (atomicTypes.hasOwnProperty(type)) cont(expect(":"), expression);
    }
    // Parses a comma-separated list of the things that are recognized
    // by the 'what' argument.
    function commasep(what){
      function proceed(type) {
        if (type == ",") cont(what, proceed);
      };
      return function() {
        pass(what, proceed);
      };
    }
    // Look for statements until a closing brace is found.
    function block(type){
      if (type == "}") cont();
      else pass(statement, block);
    }
    // Variable definitions are split into two actions -- 1 looks for
    // a name or the end of the definition, 2 looks for an '=' sign or
    // a comma.
    function maybevarlist(type, value) {
      if (type == "(") cont(vardef1, expect(")"), vardef2);
      else pass(vardef1);
    }

    function vardef1(type, value) {
      if (type == "variable") {register(value); cont(vardef2); } 
      else cont();
    }

    function vardef2(type, value) {
      if (type == "operator") cont(expression);
      else if (type == ",") cont(vardef1);
    }
 
    // @TODO: is the comma allowed in Links?
    function varop(type){
      if (type == "operator") cont(expression);
    }

    function typedef(type, value) {
      if (type == "variable") { registerVariant(value); cont(typedef2);}
      else cont();
    }

    function typedef2(type) {
      if (type == "operator") cont(expression);
      else cont();
    }

    function sigdef(type, value) {
      if (type == "variable") cont(sigdef);
      else if (type == ":") cont(expression);
      else cont();
    }

    // For loops.
    function forspec1(type, value){
      if (type == "(") cont(pushcontext("links"), commasep(forarg), expect(")"), statement, popcontext);
    }
   
    function forarg(type, value) {
      if (type == "var") cont(pushlex("vardef"), maybevarlist, poplex);
    }
    // A function definition creates a new context, and the variables
    // in its argument list have to be added to this context.
    function functiondef(type, value){ 
      if (type == "variable"){register(value); cont(functiondef);}
      else if (type == "(") cont(pushcontext("links"), commasep(funarg), expect(")"), maybeloc, popcontext);
    }

    function funarg(type, value){
      if (type == "variable"){register(value); cont();}
    }
 
    function maybeloc(type) {
      if (type == "keyword") cont();
      else pass(statement);
    }

    // Polymorphic variant definitions
    function variantdef(type, value) {
      if (type == "variable") { registerVariant(value); cont(variantdef); }
      else if (type == ":") cont(varianttype);
    }

    // List of items separated by an operator (like for variants)
    function opsep(what){
      function proceed(type) {
        if (type == "operator") cont(what, proceed);
      };
      return function() {
        pass(what, proceed);
      };
    }

    function varianttype(type) {
      //if (type == "variable") {mark("variable"); cont(); }
      //else 
      if (type == "variant") cont(opsep(variantdef));
      else if (type == "(") cont(expect(")"));
    }

    function switchdef(type) {
      //if (type == "variable") {mark("variable"); cont();}
      if (type == "(") cont(switchdef, expect(")"));
      else pass(statement);
    }

    function parsehtml(type) {
      if (type == "{") pass(pushcontext(links), statement, popcontext);
      else if (type == "html") cont(parsehtml);
      else if (type == "tag") cont(pushlex("tag"), tagblock, poplex);
      else if (type == "text") cont(parsehtml);
    }

    function tagblock(type) {
      if (type == "endtag") cont();
      else pass(parsehtml, tagblock);
    }

    return parser;
  }

  return {make: parseLinks, electriChars: "{}"};
})();
