type token =
  | END
  | EQ
  | IN
  | FUN
  | RARROW
  | IF
  | ELSE
  | EQEQ
  | LESS
  | LESSEQUAL
  | MORE
  | MOREEQUAL
  | DIFFERENT
  | BEGINSWITH
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | PLUSDOT
  | MINUSDOT
  | STARDOT
  | SLASHDOT
  | PLUSPLUS
  | HATHAT
  | HAT
  | SWITCH
  | RECEIVE
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LQUOTE
  | RQUOTE
  | LSET
  | LBAG
  | RBRACKET
  | LBRACKET
  | SORT_UP
  | SORT_DOWN
  | FOR
  | LARROW
  | HANDLE
  | WHERE
  | AMPER
  | COMMA
  | VBAR
  | DOT
  | COLON
  | TABLE
  | FROM
  | DATABASE
  | WITH
  | UNIQUE
  | ORDER
  | ASC
  | DESC
  | UPDATE
  | DELETE
  | INSERT
  | BY
  | VALUES
  | INTO
  | ESCAPE
  | CLIENT
  | SERVER
  | NAMESPACE
  | SEMICOLON
  | TRUE
  | FALSE
  | BARBAR
  | AMPAMP
  | NOT
  | UINTEGER of (Num.num)
  | UFLOAT of (float)
  | STRING of (string)
  | CDATA of (string)
  | CHAR of (char)
  | VARIABLE of (string)
  | CONSTRUCTOR of (string)
  | LXML of (string)
  | ENDTAG of (string)
  | RXML
  | SLASHRXML
  | TVARIABLE of (int)
  | TINT
  | TFLOAT
  | TBOOL
  | TSTRING

val parse_links :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sl_sugar.phrase list
