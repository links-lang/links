(* Simple lexer for reading JSON tokens *)

{

}

let octal_code = ['0'-'3']['0'-'7']['0'-'7']
let hex_code   = ['0'-'9''a'-'f''A'-'F']['0'-'9''a'-'f''A'-'F']
let string_contents = ([^ '\"' '\\']* |"\\\"" |"\\\\" | "\\n" | "\\r" | "\\t" | '\\' octal_code | '\\' ['x' 'X'] hex_code)*
let def_nat = ['0'-'9']+
let def_integer = '-'? def_nat
let def_float = def_integer '.' def_nat ('e' def_integer)?
let def_blank = [' ' '\t' '\n']

rule jsonlex = parse
  | '{'                                  { Jsonparse.LBRACE }
  | '}'                                  { Jsonparse.RBRACE }
  | '['                                  { Jsonparse.LBRACKET }
  | ']'                                  { Jsonparse.RBRACKET }
  | ':'                                  { Jsonparse.COLON }
  | ','                                  { Jsonparse.COMMA }
  | "true"                               { Jsonparse.TRUE }
  | "false"                              { Jsonparse.FALSE }
  | "null"                               { Jsonparse.NULL }
  | ('\"' (string_contents as var) '\"') { Jsonparse.STRING (Utility.decode_escapes var) }
  | def_integer as var                   { Jsonparse.INT (int_of_string var) }
  | def_float as var                     { Jsonparse.FLOAT (float_of_string var) }
  | def_blank                            { jsonlex lexbuf }

