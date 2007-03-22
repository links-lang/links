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

let optable = ref
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

let setprec table assoc level name =
  let value = match List.nth precs level, assoc with
    | (a,_,_), `None -> a
    | (_,a,_), `Left -> a
    | (_,_,a), `Right -> a 
    | _,       `Pre -> prefix
    | _,       `Post -> postfix
  in
    table := (name, value) :: !table

let precedence table x =
  try
    List.assoc x table x
  with Not_found ->
    infixl9 x

let bump_lines lexbuf n = 
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_lnum = lexbuf.lex_curr_p.pos_lnum + n}

let count_newlines str = 
  let newlines = ref 0 in
    String.iter (function
                   | '\n' -> incr newlines
                   | _    -> ()) str;
    !newlines
      
let isupper = function 'A'..'Z' -> true | _ -> false

let keywords = [
 "alien"    , ALIEN;
 "as"       , AS;
 "case"     , CASE;
 "client"   , CLIENT; 
 "database" , DATABASE;
 "delete"   , DELETE;
 "else"     , ELSE; 
 "escape"   , ESCAPE; 
 "false"    , FALSE; 
 "for"      , FOR;
 "from"     , FROM; 
 "fun"      , FUN; 
 "handle"   , HANDLE; 
 "formlet"  , FORMLET;
 "if"       , IF; 
 "in"       , IN; 
 "yields"   , YIELDS; 
(*  "infix"    , INFIX; *)
(*  "infixl"   , INFIXL; *)
(*  "infixr"   , INFIXR; *)
 "insert"   , INSERT; 
 "mu"       , MU; 
 "native"   , NATIVE;
 "orderby"  , ORDERBY;
 "readonly" , READONLY;
 "receive"  , RECEIVE;
 "server"   , SERVER; 
 "set"      , SET;
 "sig"      , SIG;
 "spawn"    , SPAWN;
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
] 
exception LexicalError of (string * Lexing.position)

}

let def_id = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*)
let octal_code = (['0'-'3']['0'-'7']['0'-'7'])
let hex_code   = (['0'-'9''a'-'f''A'-'F']['0'-'9''a'-'f''A'-'F'])
let def_qname = ('#' | def_id (':' def_id)*)
let def_integer = (['1'-'9'] ['0'-'9']* | '0')
let def_float = (def_integer '.' ['0'-'9']* ('e' ('-')? def_integer)?)
let def_blank = [' ' '\t' '\n']
let string_contents = ([^ '\"' '\\']* |"\\\"" |"\\\\" | ('\\' octal_code) | ('\\' ['x' 'X'] hex_code))*

let directive_prefix = ['' '@' '$' '%']

let xml_opening = ('<' def_id)
let xml_closing_tag = ('<' '/' def_id '>')

let opchar = [ '!' '$' '%' '&' '*' '+' '/' '<' '=' '>' '?' '@' '\\' '^' '-' '.' '|' '_' ]

(* Each lexer when called must return exactly one token and possibly
   modify the stack of remaining lexers.  The lexer on top of the stack 
   will be called next;  when each action starts it's the current lexer.

   Each rule takes two arguments: the currently operative precedence
   table and the stack.
*)

rule lex optable lexers nl = parse
  | '#' ([^ '\n'] *)                    { lex optable lexers nl lexbuf }
  | eof                                 { END }
  | ';'                                 { SEMICOLON }
  | directive_prefix (def_id as id)     { KEYWORD id}
  | '\n'                                { nl (); bump_lines lexbuf 1; lex optable lexers nl lexbuf }
  | '_'                                 { UNDERSCORE }
  | '='                                 { EQ }
  | "->"                                { RARROW }
  | "-{"                                { RARROWMBL }
  | "}->"                               { RARROWMBR }
  | "-."                                { MINUSDOT }
  | '-'                                 { MINUS }
  | '('                                 { LPAREN }
  | ')'                                 { RPAREN }
  | '{'                                 { Stack.push (lex optable lexers nl) lexers; LBRACE }
  | '}'                                 { Stack.pop lexers (* fall back *); RBRACE }
  | "<-"                                { LARROW }
  | "<--"                               { LLARROW }
  | '<' (def_qname as id)               { (* come back here after scanning the start tag *)
                                          Stack.push (starttag optable lexers nl) lexers; LXML id }
  | "[|"                                { LBRACKETBAR }
  | "|]"                                { BARRBRACKET }
  | '['                                 { LBRACKET }
  | ']'                                 { RBRACKET }
  | "||"                                { BARBAR }
  | "&&"                                { AMPAMP }
  | '|'                                 { VBAR }
  | '~'                                 { Stack.push (regex' optable lexers nl) lexers; TILDE }
  | ','                                 { COMMA }
  | '.'                                 { DOT }
  | "::"                                { COLONCOLON }
  | ':'                                 { COLON }
  | opchar + as op                      { precedence !optable op }
  | '`' (def_id as var) '`'             { if List.mem_assoc var keywords || isupper var.[0] then
                                              raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf))
                                          else precedence !optable var }
  | "'\\\\'"                            { CHAR '\\' }
  | "'\\''"                             { CHAR '\'' }
  | "'" (_ as c) "'"                    { CHAR c }
  | "'\\" (octal_code as c) "'"         { CHAR (read_octal c) }
  | "'\\" ['x''X'](hex_code as c) "'"   { CHAR (read_hex c) }
  | def_integer as var                  { UINTEGER (Num.num_of_string var) }
  | def_float as var                    { UFLOAT (float_of_string var) }
  | ('\"' (string_contents as var) '\"'){ STRING (decode_escapes var) }
  | "infix"                             { INFIX (setprec optable) }
  | "infixl"                            { INFIXL (setprec optable) }
  | "infixr"                            { INFIXR (setprec optable) }
  | "prefix"                            { PREFIX (setprec optable) }
  | "postfix"                           { POSTFIX (setprec optable) }
  | def_id as var                       { try List.assoc var keywords 
                                          with Not_found -> 
                                            if isupper var.[0] then CONSTRUCTOR var
                                            else VARIABLE var }
  | "'" def_id as var                   { QUOTEDVAR var }
  | def_blank                           { lex optable lexers nl lexbuf }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
and starttag optable lexers nl = parse
  | def_qname as var                    { VARIABLE var }
  | '='                                 { EQ }
  | '#' ([^ '\n'] *)                    { starttag optable lexers nl lexbuf }
  | '>'                                 { (* Switch to `xmllex' *)
                                          Stack.pop lexers;  Stack.push (xmllex optable lexers nl) lexers; RXML }
  | '"'                                 { (* Come back here after scanning the attr value *)
                                          Stack.push (attrlex optable lexers nl) lexers; LQUOTE }
  | "/>"                                { Stack.pop lexers (* fall back *); SLASHRXML }
  | '\n'                                { nl () ; bump_lines lexbuf 1; starttag optable lexers nl lexbuf }
  | def_blank                           { starttag optable lexers nl lexbuf }
  | eof                                 { END }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
and xmllex optable lexers nl = parse
  | "{{"                                { CDATA "{" }
  | "}}"                                { CDATA "}" }
  | "}"                                 { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
  | [^ '{' '}' '<' '&' ]* as cdata      { bump_lines lexbuf (count_newlines cdata); CDATA cdata }
  | "&amp;"                             { CDATA "&" } 
  | "&lt;"                              { CDATA "<" } 
  | "&gt;"                              { CDATA ">" } 
  | '{'                                 { (* scan the expression, then back here *)
                                          Stack.push (lex optable lexers nl) lexers; LBRACE }
  | "</" (def_qname as var) '>'         { (* fall back *)
                                          Stack.pop lexers; ENDTAG var }
  | '<' (def_qname as var)              { (* switch to `starttag' to handle the nested xml, then back here *)
                                          Stack.push (starttag optable lexers nl) lexers; LXML var }
  | eof                                 { END }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
and attrlex optable lexers nl = parse
  | '"'                                 { (* fall back *)
                                          Stack.pop lexers; RQUOTE }
  | '{'                                 { (* scan the expression, then back here *)
                                          Stack.push (lex optable lexers nl) lexers; LBRACE }
  | [^ '{' '"']* as string              { bump_lines lexbuf (count_newlines string); STRING string }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
and regex' optable lexers nl = parse
  | '/'                                 { Stack.push (regex optable lexers nl) lexers; SLASH }
  | '#' ([^ '\n'] *)                    { regex' optable lexers nl lexbuf }
  | '\n'                                { nl () ; bump_lines lexbuf 1; regex' optable lexers nl lexbuf }
  | def_blank                           { regex' optable lexers nl lexbuf }
and regex optable lexers nl = parse
  | '/'                                 { Stack.pop lexers; Stack.pop lexers; SLASH }
  | '.'                                 { DOT }
  | '[' (_ as f) '-' (_ as t) ']'       { RANGE (f,t) }
  | '?'                                 { QUESTION }
  | '*'                                 { STAR }
  | '+'                                 { PLUS }
  | '('                                 { LPAREN }
  | ')'                                 { RPAREN }
  | '{'                                 { (* scan the expression, then back here *)
                                          Stack.push (lex optable lexers nl) lexers; LBRACE }
  | '\\' (_ as c)                       
  | (_ as c)                            { STRING (String.make 1 c) }

{
 let lexer nlhook = 
  let lexers = Stack.create () in
    Stack.push (lex optable lexers nlhook) lexers;
    fun lexbuf -> Stack.top lexers lexbuf
}
