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
"switch" , SWITCH; "update"   ,  UPDATE;   "delete"    , DELETE;
"insert" , INSERT; "into"     ,  INTO;     "values"    , VALUES;
"client" , CLIENT; "server"   ,  SERVER;   "where"     , WHERE;
"if"     , IF;     "else"     ,  ELSE;     "beginswith", BEGINSWITH;     
"in"     , IN;     "fun"      ,  FUN;      "for"       , FOR;
"escape" , ESCAPE; "handle"   ,  HANDLE;   "true"      , TRUE;
"false"  , FALSE;  "table"    ,  TABLE;    "case"      , CASE;
"from"   , FROM;   "with"     ,  WITH;     "by"        , BY;
"unique" , UNIQUE; "orderby"  ,  ORDERBY;  "asc"       , ASC;
"desc"   , DESC;   "database" ,  DATABASE; "receive"   , RECEIVE;
"var"    , VAR;    "spawn"    ,  SPAWN;    "mu"        , MU;
"alien"  , ALIEN;
] 
exception LexicalError of (string * Lexing.position)

}

let def_id = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*)
let octal_code = (['0'-'3']['0'-'7']['0'-'7'])
let hex_code   = (['0'-'9''a'-'f''A'-'F']['0'-'9''a'-'f''A'-'F'])
let def_qname = (def_id (':' def_id)*)
let def_integer = (['1'-'9'] ['0'-'9']* | '0')
let def_float = (def_integer '.' ['0'-'9']* ('e' ('-')? def_integer)?)
let def_blank = [' ' '\t' '\n']
let string_contents = ([^ '\"' '\\']* |"\\\"" |"\\\\" | ('\\' octal_code) | ('\\' ['x' 'X'] hex_code))*

let directive_prefix = ['' '@' '$' '%']

let xml_opening = ('<' def_id)
let xml_closing_tag = ('<' '/' def_id '>')

(* Each lexer when called must return exactly one token and possibly
   modify the stack of remaining lexers.  The lexer on top of the stack 
   will be called next;  when each action starts it's the current lexer.
*)

rule lex lexers = parse
  | '#' ([^ '\n'] *)                    { lex lexers lexbuf }
  | eof | ";;"                          { END }
  | ';'                                 { SEMICOLON }
  | directive_prefix (def_id as id)     { KEYWORD id}
  | '\n'                                { bump_lines lexbuf 1; lex lexers lexbuf }
  | '='                                 { EQ }
  | "->"                                { RARROW }
  | "=="                                { EQEQ }
  | "<="                                { LESSEQUAL }
  | "<"                                 { LESS }
  | ">="                                { MOREEQUAL }
  | ">"                                 { MORE }
  | "<>"                                { DIFFERENT }
  | '+'                                 { PLUS }
  | '-'                                 { MINUS }
  | '*'                                 { STAR }
  | '/'                                 { SLASH }
  | "+."                                { PLUSDOT }
  | "-."                                { MINUSDOT }
  | "*."                                { STARDOT }
  | "/."                                { SLASHDOT }
  | "++"                                { PLUSPLUS }
  | "^"                                 { HAT }
  | "^^"                                { HATHAT }
  | '('                                 { LPAREN }
  | ')'                                 { RPAREN }
  | '{'                                 { Stack.push (lex lexers) lexers; LBRACE }
  | '}'                                 { Stack.pop lexers (* fall back *); RBRACE }
  | "<-"                                { LARROW }
  | '<' (def_qname as id)               { (* come back here after scanning the start tag *)
                                          Stack.push (starttag lexers) lexers; LXML id }
  | "[|"                                { LBRACKETBAR }
  | "|]"                                { BARRBRACKET }
  | '['                                 { LBRACKET }
  | ']'                                 { RBRACKET }
  | "||"                                { BARBAR }
  | "&&"                                { AMPAMP }
  | "!"                                 { BANG }
  | '|'                                 { VBAR }
  | '&'                                 { AMPER }
  | '~'                                 { Stack.push (regex' lexers) lexers; TILDE }
  | ','                                 { COMMA }
  | '.'                                 { DOT }
  | "::"                                { COLONCOLON }
  | ':'                                 { COLON }
  | "'\\\\'"                            { CHAR '\\' }
  | "'\\''"                             { CHAR '\'' }
  | "'" (_ as c) "'"                    { CHAR c }
  | "'\\" (octal_code as c) "'"         { CHAR (read_octal c) }
  | "'\\" ['x''X'](hex_code as c) "'"   { CHAR (read_hex c) }
  | def_integer as var                  { UINTEGER (Num.num_of_string var) }
  | def_float as var                    { UFLOAT (float_of_string var) }
  | ('\"' (string_contents as var) '\"'){ STRING (decode_escapes var) }
  | def_id as var                       { try List.assoc var keywords 
                                          with Not_found -> 
                                            if isupper var.[0] then CONSTRUCTOR var
                                            else VARIABLE var }
  | def_blank                           { lex lexers lexbuf }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_start_p lexbuf)) }
and starttag lexers = parse
  | def_qname as var                    { VARIABLE var }
  | '='                                 { EQ }
  | '#' ([^ '\n'] *)                    { starttag lexers lexbuf }
  | '>'                                 { (* Switch to `xmllex' *)
                                          Stack.pop lexers;  Stack.push (xmllex lexers) lexers; RXML }
  | '"'                                 { (* Come back here after scanning the attr value *)
                                          Stack.push (attrlex lexers) lexers; LQUOTE }
  | "/>"                                { Stack.pop lexers (* fall back *); SLASHRXML }
  | '\n'                                { bump_lines lexbuf 1; starttag lexers lexbuf }
  | def_blank                           { starttag lexers lexbuf }
  | eof                                 { END }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_start_p lexbuf)) }
and xmllex lexers = parse
  | "\\{"                               { CDATA "{" }
  | '\\'                                { CDATA "\\" }
  | [^ '{' '<' '\\' ]* as cdata              { bump_lines lexbuf (count_newlines cdata); CDATA cdata }
  | '{'                                 { (* scan the expression, then back here *)
                                          Stack.push (lex lexers) lexers; LBRACE }
  | "</" (def_qname as var) '>'         { (* fall back *)
                                          Stack.pop lexers; ENDTAG var }
  | '<' (def_qname as var)              { (* switch to `starttag' to handle the nested xml, then back here *)
                                          Stack.push (starttag lexers) lexers; LXML var }
  | eof                                 { END }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_start_p lexbuf)) }
and attrlex lexers = parse
  | '"'                                 { (* fall back *)
                                          Stack.pop lexers; RQUOTE }
  | '{'                                 { (* scan the expression, then back here *)
                                          Stack.push (lex lexers) lexers; LBRACE }
  | [^ '{' '"']* as string              { bump_lines lexbuf (count_newlines string); STRING string }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_start_p lexbuf)) }
and regex' lexers = parse
  | '/'                                 { Stack.push (regex lexers) lexers; SLASH }
  | '#' ([^ '\n'] *)                    { regex' lexers lexbuf }
  | '\n'                                { bump_lines lexbuf 1; regex' lexers lexbuf }
  | def_blank                           { regex' lexers lexbuf }
and regex lexers = parse
  | '/'                                 { Stack.pop lexers; Stack.pop lexers; SLASH }
  | '.'                                 { DOT }
  | '[' (_ as f) '-' (_ as t) ']'       { RANGE (f,t) }
  | '?'                                 { QUESTION }
  | '*'                                 { STAR }
  | '+'                                 { PLUS }
  | '('                                 { LPAREN }
  | ')'                                 { RPAREN }
  | '{'                                 { (* scan the expression, then back here *)
                                          Stack.push (lex lexers) lexers; LBRACE }
  | '\\' (_ as c)                       
  | (_ as c)                            { STRING (String.make 1 c) }

{

let lexer () = 
  let lexers = (Stack.create () : (Lexing.lexbuf -> Parser.token) Stack.t) in
    Stack.push (lex lexers) lexers;
    (* This flag, which records when we should insert an extra
    SEMICOLON token before the END, is a workaround for an
    end-of-stream conflict in the parser.  I wish it'd go away. *)
    let atend = ref false in
      fun lexbuf -> 
        if !atend then END
        else match Stack.top lexers lexbuf with
          | END -> atend := true; SEMICOLON
          | t   -> t
}
