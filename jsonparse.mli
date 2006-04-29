type token =
  | LBRACE
  | RBRACE
  | COLON
  | COMMA
  | LBRACKET
  | RBRACKET
  | TRUE
  | FALSE
  | NULL
  | STRING of (string)
  | INT of (Num.num)
  | FLOAT of (float)

val parse_json :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Result.result
