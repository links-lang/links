(*
  We can't tokenize Links using an FSM because of the following
  combination of properties:

  1. XML literals must be tokenized differently from regular code.
     For example, most operator characters in Links have no special
     meaning when they occur in an XML literal.

  2. An XML literal can contain nested Links expressions (using the
     "unquote" characters '{' and '}').

  3. Links code can contain nested XML literals.

  4. XML literal elements can contain other XML literal elements.

  5. Braces can be arbitrarily nested within Links code.

  We therefore need the power of a PDA.  This is achieved by
  maintaining a stack of lexers, which is manipulated by the semantic
  actions.  The particularly notable manipulations are:

  1. On encountering '{' we push an expression lexer onto the stack.
     On encountering '}' we pop the lexer, and carry on lexing in
     the "mode" we were in before.

  2. On encountering the start of an XML literal we push an xml lexer.
     On encountering an XML closing tag we pop the lexer and carry on
     lexing in the "mode" we were in before.

  There are some other details (involving start tags and attributes)
  that are handled similarly to the above.
*)

{

open Lexing
open Utility
open Parser

(* Constructors are not first class in OCaml *)
let infix0  x = INFIX0  x
let infixl0 x = INFIXL0 x
let infixr0 x = INFIXR0 x
let infix1  x = INFIX1  x
let infixl1 x = INFIXL1 x
let infixr1 x = INFIXR1 x
let infix2  x = INFIX2  x
let infixl2 x = INFIXL2 x
let infixr2 x = INFIXR2 x
let infix3  x = INFIX3  x
let infixl3 x = INFIXL3 x
let infixr3 x = INFIXR3 x
let infix4  x = INFIX4  x
let infixl4 x = INFIXL4 x
let infixr4 x = INFIXR4 x
let infix5  x = INFIX5  x
let infixl5 x = INFIXL5 x
let infixr5 x = INFIXR5 x
let infix6  x = INFIX6  x
let infixl6 x = INFIXL6 x
let infixr6 x = INFIXR6 x
let infix7  x = INFIX7  x
let infixl7 x = INFIXL7 x
let infixr7 x = INFIXR7 x
let infix8  x = INFIX8  x
let infixl8 x = INFIXL8 x
let infixr8 x = INFIXR8 x
let infix9  x = INFIX9  x
let infixl9 x = INFIXL9 x
let infixr9 x = INFIXR9 x
let prefix  x = PREFIXOP x
let postfix x = POSTFIXOP x

let precs =
  [
    infix0, infixl0, infixr0;
    infix1, infixl1, infixr1;
    infix2, infixl2, infixr2;
    infix3, infixl3, infixr3;
    infix4, infixl4, infixr4;
    infix5, infixl5, infixr5;
    infix6, infixl6, infixr6;
    infix7, infixl7, infixr7;
    infix8, infixl8, infixr8;
    infix9, infixl9, infixr9;
  ]

let initial_optable =
(* lifted from the definition of Haskell *)
  [
    "!"  , infix9;

    "^"  , infixr8;
    "^^" , infixr8;
    "**" , infixr8;

    "*"  , infixl7;
    "/"  , infixl7;
    "+"  , infixl6;
    "*." , infixl7;
    "/." , infixl7;
    "+." , infixl6;

    "-"  , infixl6;
    ".-" , infixl6;

    "::" , infixr5;
    "++" , infixr5;

    "==" , infix4;
    "<>" , infix4;
    "<"  , infix4;
    "<=" , infix4;
    ">=" , infix4;
    ">"  , infix4;

    "&&" , infixr3;

    "||" , infixr2;
    ">>" , infixl1;
  ]

let default_precedence = infixl9

class lexer_context =
object
  val mutable optable = initial_optable
  val          lexers = Stack.create ()

  method precedence name =
    try List.assoc name optable name
    with NotFound _ -> default_precedence name

  method setprec (assoc : [`None|`Left|`Right|`Pre|`Post]) level name =
    let value = match List.nth precs level, assoc with
      | (a,_,_), `None -> a
      | (_,a,_), `Left -> a
      | (_,_,a), `Right -> a
      | _,       `Pre -> prefix
      | _,       `Post -> postfix
    in
      optable <- (name, value) :: optable

  method push_lexer (lexer : Lexing.lexbuf -> Parser.token) =
    Stack.push lexer lexers

  method pop_lexer =
    Stack.pop lexers

  method next_lexer =
    Stack.top lexers
end

let fresh_context () = new lexer_context

let bump_lines lexbuf n =
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_lnum = lexbuf.lex_curr_p.pos_lnum + n}

let count_newlines = StringUtils.count '\n'

let keywords = [
 "alien"    , ALIEN;
 "as"       , AS;
 "case"     , CASE;
 "client"   , CLIENT;
 "database" , DATABASE;
 "default"  , DEFAULT;
 "delete"   , DELETE;
 "else"     , ELSE;
 "escape"   , ESCAPE;
 "false"    , FALSE;
 "for"      , FOR;
 "forall"   , FORALL;
 "from"     , FROM;
 "fun"      , FUN;
 "formlet"  , FORMLET;
 "if"       , IF;
 "in"       , IN;
 "open"     , OPEN;
 "yields"   , YIELDS;
(*  "infix"    , INFIX; *)
(*  "infixl"   , INFIXL; *)
(*  "infixr"   , INFIXR; *)
 "insert"   , INSERT;
 "linfun"   , LINFUN;
 "module"   , MODULE;
 "mu"       , MU;
 "native"   , NATIVE;
 "nu"       , NU;
 "offer"    , OFFER;
 "orderby"  , ORDERBY;
 "op"       , OP;
 "page"     , PAGE;
 "query"    , QUERY;
 "readonly" , READONLY;

 "receive"  , RECEIVE;
 "returning", RETURNING;
 "select"   , SELECT;
 "server"   , SERVER;
 "set"      , SET;
 "sig"      , SIG;
 "spawn"    , SPAWN;
 "spawnAngel" , SPAWNANGEL;
 "spawnClient" , SPAWNCLIENT;
 "spawnDemon" , SPAWNDEMON;
 "spawnWait", SPAWNWAIT;
 "switch"   , SWITCH;
 "table"    , TABLE;
 "TableHandle", TABLEHANDLE;
 "true"     , TRUE;
 "typename" , TYPENAME;
 "update"   , UPDATE;
 "values"   , VALUES;
 "var"      , VAR; 
 "where"    , WHERE; 
 "with"     , WITH; 
(* SAND *)
 "tablekeys"     , TABLEKEYS;
]

exception LexicalError of (string * Lexing.position)
}

let def_id = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*)
let module_name = (['A'-'Z'] (['A'-'Z' 'a'-'z'])*)
let qualified_module = (module_name ((":::" module_name)*))
let qualified_var = (qualified_module (":::" def_id))
let octal_code = (['0'-'3']['0'-'7']['0'-'7'])
let hex_code   = (['0'-'9''a'-'f''A'-'F']['0'-'9''a'-'f''A'-'F'])
let def_qname = ('#' | def_id (':' def_id)*)
let def_integer = (['1'-'9'] ['0'-'9']* | '0')
let def_float = (def_integer '.' ['0'-'9']+ ('e' ('-')? def_integer)?)
let def_blank = [' ' '\t' '\n']
let char_contents = ([^ '\"' '\\']|"\\\"" |"\\\\" | "\\n" | "\\r" | "\\t" | ('\\' octal_code) | ('\\' ['x' 'X'] hex_code))
let string_contents = char_contents*
let regexrepl_fsa =  [^ '{' '/']* (* this regex is too restrictive. But can't seem to get a more precise one to work  :( *)
let regex_flags = ['l' 'n' 'g']*

let directive_prefix = ['' '@' '$']

let xml_opening = ('<' def_id)
let xml_closing_tag = ('<' '/' def_id '>')

let initopchar = [ '!' '$' '&' '*' '+' '/' '<' '=' '>' '@' '\\' '^' '-' ]
let opchar = [ '.' '!' '$' '&' '*' '+' '/' '<' '=' '>' '@' '\\' '^' '-' ]

(* Each lexer when called must return exactly one token and possibly
   modify the stack of remaining lexers.  The lexer on top of the stack
   will be called next;  when each action starts it's the current lexer.

   Each rule takes two arguments: the currently operative precedence
   table and the stack.
*)

rule lex ctxt nl = parse
  | eof                                 { END }
  | '#' ([^ '\n'] *)                    { lex ctxt nl lexbuf }
  | "End"                               { END }
  | ';'                                 { SEMICOLON }
  | directive_prefix (def_id as id)     { KEYWORD id}
  | '\n'                                { nl (); bump_lines lexbuf 1; lex ctxt nl lexbuf }
  | '_'                                 { UNDERSCORE }
  | '='                                 { EQ }
  | "->"                                { RARROW }
  | "~>"                                { SQUIGRARROW }
  | "-@"                                { LOLLI }
  | "~@"                                { SQUIGLOLLI }
  | "=>"                                { FATRARROW }
  | "-."                                { MINUSDOT }
  | '-'                                 { MINUS }
  | '('                                 { LPAREN }
  | ')'                                 { RPAREN }
  | "{|"                                { ctxt#push_lexer (lex ctxt nl); LBRACEBAR }
  | '{'                                 { ctxt#push_lexer (lex ctxt nl); LBRACE }
  | "|}"                                { ctxt#pop_lexer (* fall back *); BARRBRACE }
  | '}'                                 { ctxt#pop_lexer (* fall back *); RBRACE }
  | "<->"                               { LRARROW }
  | "<--"                               { LLARROW }
  | "<-"                                { LARROW }
  | "<|"                                { LEFTTRIANGLE }
  | "|>"                                { RIGHTTRIANGLE }
  | '<' (def_qname as id)               { (* come back here after scanning the start tag *)
                                          ctxt#push_lexer (starttag ctxt nl); LXML id }
  | "[|"                                { LBRACKETBAR }
  | "|]"                                { BARRBRACKET }
  | '['                                 { LBRACKET }
  | ']'                                 { RBRACKET }
  | "[+|"                               { LBRACKETPLUSBAR }
  | "|+]"                               { BARPLUSRBRACKET }
  | "[&|"                               { LBRACKETAMPBAR }
  | "|&]"                               { BARAMPRBRACKET }
  | "||"                                { BARBAR }
  | "&&"                                { AMPAMP }
  | '|'                                 { VBAR }
  | '~'                                 { TILDE }
  | "=~"                                { ctxt#push_lexer (regex' ctxt nl); EQUALSTILDE }
  | ','                                 { COMMA }
  | '.'                                 { DOT }
  | ".."                                { DOTDOT }
  | "::"                                { COLONCOLON }
  | ":::"                               { COLONCOLONCOLON }
  | ':'                                 { COLON }
  | '!'                                 { BANG }
  | '?'                                 { QUESTION }
  | "%" def_id as var                   { PERCENTVAR var }
  | '%'                                 { PERCENT }
  | initopchar opchar * as op           { ctxt#precedence op }
  | '`' (def_id as var) '`'             { if List.mem_assoc var keywords || Char.isUpper var.[0] then
                                              raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf))
                                          else ctxt#precedence var }
  | "'" (char_contents as c) "'"        { let c' = decode_escapes c in
                                            if String.length c' = 1 then CHAR (c'.[0])
                                            else raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
  | def_integer as var                  { UINTEGER (int_of_string var) }
  | def_float as var                    { UFLOAT (float_of_string var) }
  | ('\"' (string_contents as var) '\"'){ STRING (decode_escapes var) }
  | "infix"                             { INFIX ctxt#setprec }
  | "infixl"                            { INFIXL ctxt#setprec }
  | "infixr"                            { INFIXR ctxt#setprec }
  | "prefix"                            { PREFIX ctxt#setprec }
  | "postfix"                           { POSTFIX ctxt#setprec }
  (* | qualified_module as modd            { QUALIFIEDMODULE modd } *)
  | def_id as var                       { try List.assoc var keywords
                                          with Not_found | NotFound _ ->
                                            if Char.isUpper var.[0] then CONSTRUCTOR var
                                            else VARIABLE var }
  | qualified_var as var                { QUALIFIEDVARIABLE var }
  | def_blank                           { lex ctxt nl lexbuf }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
and starttag ctxt nl = parse
  | eof                                 { END }
  | def_qname as var                    { VARIABLE var }
  | '='                                 { EQ }
  | '#' ([^ '\n'] *)                    { starttag ctxt nl lexbuf }
  | '>'                                 { (* Switch to `xmllex' *)
                                          ctxt#pop_lexer;  ctxt#push_lexer (xmllex ctxt nl); RXML }
  | '"'                                 { (* Come back here after scanning the attr value *)
                                          ctxt#push_lexer (attrlex ctxt nl); LQUOTE }
  | '{'                                 { (* Come back here after scanning the attribute block *)
                                          ctxt#push_lexer (lex ctxt nl); LBRACE }
  | "/>"                                { ctxt#pop_lexer (* fall back *); SLASHRXML }
  | '\n'                                { nl () ; bump_lines lexbuf 1; starttag ctxt nl lexbuf }
  | def_blank                           { starttag ctxt nl lexbuf }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
and xmllex ctxt nl = parse
  | eof                                 { END }
  | "{{"                                { CDATA "{" }
  | "}}"                                { CDATA "}" }
  | "}"                                 { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
  | [^ '{' '}' '<' '&' ]* as cdata      { bump_lines lexbuf (count_newlines cdata); CDATA cdata }
  | "&amp;"                             { CDATA "&" }
  | "&lt;"                              { CDATA "<" }
  | "&gt;"                              { CDATA ">" }
  | "{|"                                { (* scan the expression, then back here *)
                                          ctxt#push_lexer (lex ctxt nl); LBRACEBAR }
  | '{'                                 { (* scan the expression, then back here *)
                                          ctxt#push_lexer (lex ctxt nl); LBRACE }
  | "</" (def_qname as var) '>'         { (* fall back *)
                                          ctxt#pop_lexer; ENDTAG var }
  | '<' (def_qname as var)              { (* switch to `starttag' to handle the nested xml, then back here *)
                                          ctxt#push_lexer (starttag ctxt nl); LXML var }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
and attrlex ctxt nl = parse
  | eof                                 { END }
  | '"'                                 { (* fall back *)
                                          ctxt#pop_lexer; RQUOTE }
  | '{'                                 { (* scan the expression, then back here *)
                                          ctxt#push_lexer (lex ctxt nl); LBRACE }
  | [^ '{' '"']* as string              { bump_lines lexbuf (count_newlines string); STRING string }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
and regex' ctxt nl = parse
  | eof                                 { END }
  | '/'                                 { ctxt#push_lexer (regex ctxt nl); SLASH }
  | "s/"                                { ctxt#push_lexer (regexrepl ctxt nl); (* push twice is intentional *)
					  ctxt#push_lexer (regexrepl ctxt nl);
					  ctxt#push_lexer (regex ctxt nl);
					  SSLASH }
  | '#' ([^ '\n'] *)                    { regex' ctxt nl lexbuf }
  | '\n'                                { nl (); bump_lines lexbuf 1; regex' ctxt nl lexbuf }
  | def_blank                           { regex' ctxt nl lexbuf }
and regex ctxt nl = parse
  | eof                                 { END }
  | '/'                                 { ctxt#pop_lexer; ctxt#pop_lexer; SLASH }
  | '/' (regex_flags as f)              { ctxt#pop_lexer; ctxt#pop_lexer; SLASHFLAGS (f) }
  | '.'                                 { DOT }
  | '[' (_ as f) '-' (_ as t) ']'       { RANGE (f,t) }
  | '?'                                 { QUESTION }
  | '*'                                 { STAR }
  | '+'                                 { PLUS }
  | '|'                                 { ALTERNATE }
  | '^'                                 { CARET }
  | '$'                                 { DOLLAR }
  | '('                                 { LPAREN }
  | ')'                                 { RPAREN }
  | '{'                                 { (* scan the expression, then back here *)
                                          ctxt#push_lexer (lex ctxt nl); LBRACE }
  | '\\' (_ as c)                       { QUOTEDMETA (String.make 1 c)}
  | '\n'                                { nl (); bump_lines lexbuf 1; regex ctxt nl lexbuf }
  | def_blank                           { regex ctxt nl lexbuf }
  | (_ as c)                            { STRING (String.make 1 c) }
and regexrepl ctxt nl = parse
  | eof                                 { END }
  | regexrepl_fsa as var                { REGEXREPL(var) }
  | '{'                                 { (* scan the expression, then back here *)
                                          ctxt#push_lexer (lex ctxt nl); LBRACE }
  | '/'                                 { ctxt#pop_lexer; ctxt#pop_lexer; SLASH}
  | '/' (regex_flags as f)              { ctxt#pop_lexer; ctxt#pop_lexer; SLASHFLAGS (f) }

{
 let lexer ctxt ~newline_hook =
   ctxt#push_lexer (lex ctxt newline_hook);
   fun lexbuf -> ctxt#next_lexer lexbuf
}
