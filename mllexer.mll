(**   ML-ish lexical syntax.**)
{
open Lexing
open Utility
open MLParser

let bump_lines lexbuf n = 
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_lnum = lexbuf.lex_curr_p.pos_lnum + n}

let keywords = [
  "alien"  , ALIEN;  "as"     , AS;     "and"    , AND;    "else"   , ELSE;
  "end"    , END;    "escape" , ESCAPE; "fun"    , FUN;    "if"     , IF;
  "in"     , IN;     "infix"  , INFIX;  "infixl" , INFIXL; "infixr" , INFIXR;
  "let"    , LET;    "match"  , MATCH;  "mu"     , MU;     "match"  , MATCH; 
  "then"   , THEN;   "type"   , TYPE;   "var"    , VAR;    "with"   , WITH; 
] 
exception LexicalError of (string * Lexing.position)
}

let def_id = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*)
let octal_code = (['0'-'3']['0'-'7']['0'-'7'])
let hex_code   = (['0'-'9''a'-'f''A'-'F']['0'-'9''a'-'f''A'-'F'])
let def_integer = (['1'-'9'] ['0'-'9']* | '0')
let def_float = (def_integer '.' ['0'-'9']+ ('e' ('-')? def_integer)?)
let def_blank = [' ' '\t' '\n']
let char_contents = ([^ '\"' '\\']|"\\\"" |"\\\\" | "\\n" | "\\r" | "\\t"
                     | ('\\' octal_code) | ('\\' ['x' 'X'] hex_code))
let string_contents = char_contents*

let opchar = [ '!' '$' '%' '&' '*' '+' '/' '<' '=' '>' '?' '@' '\\' '^' '-' '.' '|' '_' ':']

rule lex nl = parse
  | '#' ([^ '\n'] *)                    { lex nl lexbuf } (* TODO: MLish comment syntax (* *) *)
  | eof                                 { EOF }
  | ';'                                 { SEMICOLON }
  | '\n'                                { nl (); bump_lines lexbuf 1; lex nl lexbuf }
  | '_'                                 { UNDERSCORE }
  | '='                                 { EQ }
  | '('                                 { LPAREN }
  | ')'                                 { RPAREN }
  | "<-"                                { LARROW }
  | "->"                                { RARROW }
  | '['                                 { LBRACKET }
  | ']'                                 { RBRACKET }
  | '{'                                 { LBRACE }
  | '}'                                 { RBRACE }
  | '|'                                 { VBAR }
  | '-'                                 { MINUS } (* problem: MINUS is not available as an operator *)
  | ','                                 { COMMA }
  | '.'                                 { DOT }
  | ':'                                 { COLON }
  | opchar + as op                      { SYMBOL op }
  | '`' (def_id as var) '`'             { if List.mem_assoc var keywords || Char.isUpper var.[0] then
                                              raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf))
                                          else SYMBOL var (* is it still right to do this in the lexer? *)  }
  | "'" (char_contents as c) "'"        { let c' = decode_escapes c in
                                            if String.length c' = 1 then CHAR (c'.[0])
                                            else raise (LexicalError (lexeme lexbuf, 
                                                                      lexeme_end_p lexbuf)) }
  | def_integer as var                  { UINTEGER (Num.num_of_string var) }
  | def_float as var                    { UFLOAT (float_of_string var) }
  | ('\"' (string_contents as var) '\"'){ STRING (decode_escapes var) }
  | def_id as var                       { try List.assoc var keywords 
                                          with Not_found | NotFound _ -> 
                                            if Char.isUpper var.[0] then CONSTRUCTOR var
                                            else VARIABLE var }
  | "'" def_id as var                   { QUOTEDVAR var }
  | def_blank                           { lex nl lexbuf }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }

{
 let lexer ~newline_hook = lex newline_hook
}
