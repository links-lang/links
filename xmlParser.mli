type token =
  | IGNORE
  | END
  | EQ
  | LQUOTE
  | RQUOTE
  | STRING of (string)
  | CDATA of (string)
  | VARIABLE of (string)
  | LXML of (string)
  | ENDTAG of (string)
  | RXML
  | SLASHRXML
  | LCDATA
  | RCDATA
  | CHAR of (char)

val xml :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Value.xmlitem
