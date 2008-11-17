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
  | '{'                                  { Oldjsonparse.LBRACE }
  | '}'                                  { Oldjsonparse.RBRACE }
  | ':'                                  { Oldjsonparse.COLON }
  | ','                                  { Oldjsonparse.COMMA }
  | '['                                  { Oldjsonparse.LBRACKET }
  | ']'                                  { Oldjsonparse.RBRACKET }
  | "true"                               { Oldjsonparse.TRUE }
  | "false"                              { Oldjsonparse.FALSE }
  | "null"                               { Oldjsonparse.NULL }
  | ('\"' (string_contents as var) '\"') { Oldjsonparse.STRING (Utility.decode_escapes var) }
  | def_integer as var                   { Oldjsonparse.INT (Num.num_of_string var) }
  | def_float as var                     { Oldjsonparse.FLOAT (float_of_string var) }
  | def_blank                            { jsonlex lexbuf }

