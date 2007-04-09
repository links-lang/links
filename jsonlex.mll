(* Simple lexer for reading JSON tokens *)

{

}

let octal_code = (['0'-'3']['0'-'7']['0'-'7'])
let hex_code   = (['0'-'9''a'-'f''A'-'F']['0'-'9''a'-'f''A'-'F'])
let string_contents = ([^ '\"' '\\']* |"\\\"" |"\\\\" | "\\n" | "\\r" | "\\t" | ('\\' octal_code) | ('\\' ['x' 'X'] hex_code))*
let def_integer = (['1'-'9'] ['0'-'9']* | '0')
let def_float = (def_integer '.' ['0'-'9']* ('e' ('-')? def_integer)?)
let def_blank = [' ' '\t' '\n']

rule jsonlex = parse
  | '{'                                  { Jsonparse.LBRACE }
  | '}'                                  { Jsonparse.RBRACE }
  | ':'                                  { Jsonparse.COLON }
  | ','                                  { Jsonparse.COMMA }
  | '['                                  { Jsonparse.LBRACKET }
  | ']'                                  { Jsonparse.RBRACKET }
  | "true"                               { Jsonparse.TRUE }
  | "false"                              { Jsonparse.FALSE }
  | "null"                               { Jsonparse.NULL }
  | ('\"' (string_contents as var) '\"') { Jsonparse.STRING (Utility.decode_escapes var) }
  | def_integer as var                   { Jsonparse.INT (Num.num_of_string var) }
  | def_float as var                     { Jsonparse.FLOAT (float_of_string var) }
  | def_blank                            { jsonlex lexbuf }

