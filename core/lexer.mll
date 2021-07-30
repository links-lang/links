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
open Operators

class lexer_context =
object
  val          lexers = Stack.create ()

  method push_lexer (lexer : Lexing.lexbuf -> Parser.token) =
    Stack.push lexer lexers

  method pop_lexer =
    (* We've a stack of functions, so we don't want to apply the result. *)
    ignore (Stack.pop lexers) [@warning "-5"]

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
 "by"       , BY;
 "case"     , CASE;
 "client"   , CLIENT;
 "database" , DATABASE;
 "default"  , DEFAULT;
 "delete"   , DELETE;
 "delete_left", DELETE_LEFT;
 "determined", DETERMINED;
 "do"       , DOOP;
 "else"     , ELSE;
 "escape"   , ESCAPE;
 "false"    , FALSE;
 "for"      , FOR;
 "forall"   , FORALL;
 "from"     , FROM;
 "fun"      , FUN;
 "formlet"  , FORMLET;
 "handle"   , HANDLE;
 "if"       , IF;
 "in"       , IN;
 "lens"     , LENS;
 "lensdrop" , LENSDROP;
 "lensget"  , LENSGET;
 "lensput"  , LENSPUT;
 "lensselect", LENSSELECT;
 "lensjoin" , LENSJOIN;
 "lenscheck", LENSCHECK;
 "lensserial", LENSSERIAL;
 "yields"   , YIELDS;
 "import"   , IMPORT;
 "insert"   , INSERT;
 "linfun"   , LINFUN;
 "module"   , MODULE;
 "mu"       , MU;
 "mutual"   , MUTUAL;
 "nu"       , NU;
 "offer"    , OFFER;
 "on"       , ON;
 "orderby"  , ORDERBY;
 "op"       , OP;
 "open"     , OPEN;
 "otherwise", OTHERWISE;
 "page"     , PAGE;
 "query"    , QUERY;
 "raise"    , RAISE;
 "readonly" , READONLY;
 "receive"  , RECEIVE;
 "returning", RETURNING;
 "select"   , SELECT;
 "server"   , SERVER;
 "set"      , SET;
 "shallowhandle", SHALLOWHANDLE;
 "sig"      , SIG;
 "spawn"    , SPAWN;
 "spawnClient" , SPAWNCLIENT;
 "spawnAt"  , SPAWNAT;
 "spawnAngel" , SPAWNANGEL;
 "spawnAngelAt" , SPAWNANGELAT;
 "spawnWait", SPAWNWAIT;
 "switch"   , SWITCH;
 "table"    , TABLE;
 "TableHandle", TABLEHANDLE;
 "true"     , TRUE;
 "try"     , TRY;
 "typename" , TYPENAME;
 "update"   , UPDATE;
 "unsafe"   , UNSAFE;
 "values"   , VALUES;
 "var"      , VAR;
 "where"    , WHERE;
 "with"     , WITH;
(* SAND *)
 "tablekeys"     , TABLEKEYS;
]

exception LexicalError of (string * Lexing.position)
}

let def_id = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '\'']*)
let def_attr_id = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '-' '0'-'9']*)
let module_name = (['A'-'Z'] (['A'-'Z' 'a'-'z'])*)
let octal_code = (['0'-'3']['0'-'7']['0'-'7'])
let hex_code   = (['0'-'9''a'-'f''A'-'F']['0'-'9''a'-'f''A'-'F'])
let def_qname = ('#' | def_attr_id (':' def_attr_id)*)
let def_tagname = ('#' | def_id (':' def_attr_id)*)
let def_integer = (['1'-'9'] ['0'-'9']* | '0')
let def_float = (def_integer '.' ['0'-'9']+ ('e' ('-')? def_integer)?)
let def_blank = [' ' '\t' '\r' '\n']
let char_contents = ([^ '\"' '\\']|"\\\"" |"\\\\" | "\\n" | "\\r" | "\\t" | "\\b" | ('\\' octal_code) | ('\\' ['x' 'X'] hex_code))
let string_contents = char_contents*
let regexrepl_fsa =  [^ '{' '/']* (* this regex is too restrictive. But can't seem to get a more precise one to work  :( *)
let regex_flags = ['l' 'n' 'g']*

let directive_prefix = ['' '@' '$']

let xml_opening = ('<' def_id)
let xml_closing_tag = ('<' '/' def_id '>')

let initopchar = [ '!' '$' '&' '*' '+' '/' '<' '=' '>' '@' '\\' '^' '-' '|' ]
let opchar = [ '.' '!' '$' '&' '*' '+' '/' '<' '=' '>' '@' '\\' '^' '-' '|' ]

(* Each lexer when called must return exactly one token and possibly
   modify the stack of remaining lexers.  The lexer on top of the stack
   will be called next;  when each action starts it's the current lexer.

   Each rule takes two arguments: the currently operative precedence
   table and the stack.
*)

rule lex ctxt nl = parse
  | eof                                 { EOF }
  | '#' ([^ '\n'] *)                    { lex ctxt nl lexbuf }
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
  | '<' (def_tagname as id)             { (* come back here after scanning the start tag *)
                                          ctxt#push_lexer (starttag ctxt nl); LXML id }
  | "<!--"                              { xmlcomment_lex ctxt nl lexbuf }
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
  | "|."                                { VBARDOT }
  | '~'                                 { TILDE }
  | "=~"                                { ctxt#push_lexer (regex' ctxt nl); EQUALSTILDE }
  | ','                                 { COMMA }
  | '.'                                 { DOT }
  | ".."                                { DOTDOT }
  | "::"                                { COLONCOLON }
  | ':'                                 { COLON }
  | '!'                                 { BANG }
  | '?'                                 { QUESTION }
  | '$'                                 { DOLLAR }
  | '@'                                 { AT }
  | "%" def_id as var                   { PERCENTVAR var }
  | '%'                                 { PERCENT }
  | initopchar opchar * as op           { OPERATOR op }
  | '`' (def_id as var) '`'             { if List.mem_assoc var keywords || Char.isUpper var.[0] then
                                              raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf))
                                          else OPERATOR var }
  | "'" (char_contents as c) "'"        { let c' = decode_escapes c in
                                            if String.length c' = 1 then CHAR (c'.[0])
                                            else raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
  | def_integer as var                  { UINTEGER (int_of_string var) }
  | def_float as var                    { UFLOAT (float_of_string var) }
  | ('\"' (string_contents as var) '\"'){ STRING (decode_escapes var) }
  | "infix"                             { FIXITY Associativity.None }
  | "infixl"                            { FIXITY Associativity.Left }
  | "infixr"                            { FIXITY Associativity.Right }
  | "prefix"                            { FIXITY Associativity.Right }
  | "postfix"                           { FIXITY Associativity.Left }
  | "~fun"                              { FROZEN_FUN }
  | "~linfun"                           { FROZEN_LINFUN }
  | def_id as var                       { try List.assoc var keywords
                                          with Not_found | NotFound _ ->
                                            if Char.isUpper var.[0] then CONSTRUCTOR var
                                            else VARIABLE var }
  | def_blank                           { lex ctxt nl lexbuf }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
and starttag ctxt nl = parse
  | eof                                 { EOF }
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
and xmlcomment_lex ctxt nl = parse
  | "-->"                               { ctxt#next_lexer lexbuf }
  | '\n'                                { nl() ; bump_lines lexbuf 1; xmlcomment_lex ctxt nl lexbuf }
  | eof                                 { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
  | "--"                                { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
  | _                                   { xmlcomment_lex ctxt nl lexbuf }
and xmllex ctxt nl = parse
  | eof                                 { EOF }
  | "<!--"                              { xmlcomment_lex ctxt nl lexbuf }
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
  | "</" (def_tagname as var) '>'       { (* fall back *)
                                          ctxt#pop_lexer; ENDTAG var }
  | '<' (def_tagname as var)            { (* switch to `starttag' to handle the nested xml, then back here *)
                                          ctxt#push_lexer (starttag ctxt nl); LXML var }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
and attrlex ctxt nl = parse
  | eof                                 { EOF }
  | '"'                                 { (* fall back *)
                                          ctxt#pop_lexer; RQUOTE }
  | '{'                                 { (* scan the expression, then back here *)
                                          ctxt#push_lexer (lex ctxt nl); LBRACE }
  | [^ '{' '"']* as string              { bump_lines lexbuf (count_newlines string); STRING string }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
and regex' ctxt nl = parse
  | eof                                 { EOF }
  | '/'                                 { ctxt#push_lexer (regex ctxt nl); SLASH }
  | "s/"                                { ctxt#push_lexer (regexrepl ctxt nl); (* push twice is intentional *)
                      ctxt#push_lexer (regexrepl ctxt nl);
                      ctxt#push_lexer (regex ctxt nl);
                      SSLASH }
  | '#' ([^ '\n'] *)                    { regex' ctxt nl lexbuf }
  | '\n'                                { nl (); bump_lines lexbuf 1; regex' ctxt nl lexbuf }
  | def_blank                           { regex' ctxt nl lexbuf }
and regex ctxt nl = parse
  | eof                                 { EOF }
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
  | eof                                 { EOF }
  | regexrepl_fsa as var                { REGEXREPL(var) }
  | '{'                                 { (* scan the expression, then back here *)
                                          ctxt#push_lexer (lex ctxt nl); LBRACE }
  | '/'                                 { ctxt#pop_lexer; ctxt#pop_lexer; SLASH}
  | '/' (regex_flags as f)              { ctxt#pop_lexer; ctxt#pop_lexer; SLASHFLAGS (f) }

{
 let lexer : lexer_context
         -> newline_hook:(unit -> unit)
         -> (Lexing.lexbuf -> Parser.token) =
fun ctxt ~newline_hook ->
   ctxt#push_lexer (lex ctxt newline_hook);
   fun lexbuf -> ctxt#next_lexer lexbuf
}
