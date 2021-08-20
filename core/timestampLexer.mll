{
  open TimestampParser
  exception SyntaxError of string
}

(* In timestamps, we allow padded zeros. *)
let def_integer = ['0'-'9']+
let def_float = (def_integer '.' ['0'-'9']+ ('e' ('-')? def_integer)?)
let def_white = [' ' '\t']+

rule lex = parse
  | eof                { EOF }
  | def_integer as i   { UINTEGER (int_of_string i) }
  | def_float as f     { UFLOAT (float_of_string f) }
  | def_white          { lex lexbuf }
  | '+'                { PLUS }
  | '-'                { MINUS }
  | ':'                { COLON }
  | "infinity"         { INFINITY }
  | _                  { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
