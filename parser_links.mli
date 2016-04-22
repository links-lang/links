type token =
  | END
  | EQ
  | IN
  | FUN
  | LINFUN
  | RARROW
  | LOLLI
  | FATRARROW
  | MINUSLBRACE
  | VAR
  | OP
  | SQUIGRARROW
  | SQUIGLOLLI
  | TILDE
  | IF
  | ELSE
  | MINUS
  | MINUSDOT
  | SWITCH
  | RECEIVE
  | CASE
  | SPAWN
  | SPAWNANGEL
  | SPAWNDEMON
  | SPAWNWAIT
  | HANDLE
  | SHALLOWHANDLE
  | OPEN
  | HANDLER
  | SHALLOWHANDLER
  | OFFER
  | SELECT
  | DOOP
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACEBAR
  | BARRBRACE
  | LQUOTE
  | RQUOTE
  | RBRACKET
  | LBRACKET
  | LBRACKETBAR
  | BARRBRACKET
  | LBRACKETPLUSBAR
  | BARPLUSRBRACKET
  | LBRACKETAMPBAR
  | BARAMPRBRACKET
  | LEFTTRIANGLE
  | RIGHTTRIANGLE
  | NU
  | FOR
  | LARROW
  | LLARROW
  | WHERE
  | FORMLET
  | PAGE
  | LRARROW
  | COMMA
  | VBAR
  | DOT
  | DOTDOT
  | COLON
  | COLONCOLON
  | TABLE
  | TABLEHANDLE
  | FROM
  | DATABASE
  | QUERY
  | WITH
  | YIELDS
  | ORDERBY
  | UPDATE
  | DELETE
  | INSERT
  | VALUES
  | SET
  | RETURNING
  | READONLY
  | DEFAULT
  | ESCAPE
  | CLIENT
  | SERVER
  | NATIVE
  | SEMICOLON
  | TRUE
  | FALSE
  | BARBAR
  | AMPAMP
  | UINTEGER of (int)
  | UFLOAT of (float)
  | STRING of (string)
  | CDATA of (string)
  | REGEXREPL of (string)
  | CHAR of (char)
  | VARIABLE of (string)
  | CONSTRUCTOR of (string)
  | KEYWORD of (string)
  | PERCENTVAR of (string)
  | LXML of (string)
  | ENDTAG of (string)
  | RXML
  | SLASHRXML
  | MU
  | FORALL
  | ALIEN
  | SIG
  | INCLUDE
  | BANG
  | QUESTION
  | PERCENT
  | EQUALSTILDE
  | PLUS
  | STAR
  | ALTERNATE
  | SLASH
  | SSLASH
  | CARET
  | DOLLAR
  | RANGE of (char*char)
  | QUOTEDMETA of (string)
  | SLASHFLAGS of (string)
  | UNDERSCORE
  | AS
  | INFIX of ([`Left|`Right|`None|`Pre|`Post] -> int -> string -> unit)
  | INFIXL of ([`Left|`Right|`None|`Pre|`Post] -> int -> string -> unit)
  | INFIXR of ([`Left|`Right|`None|`Pre|`Post] -> int -> string -> unit)
  | PREFIX of ([`Left|`Right|`None|`Pre|`Post] -> int -> string -> unit)
  | POSTFIX of ([`Left|`Right|`None|`Pre|`Post] -> int -> string -> unit)
  | TYPENAME
  | TYPE
  | ROW
  | PRESENCE
  | PREFIXOP of (string)
  | POSTFIXOP of (string)
  | INFIX0 of (string)
  | INFIXL0 of (string)
  | INFIXR0 of (string)
  | INFIX1 of (string)
  | INFIXL1 of (string)
  | INFIXR1 of (string)
  | INFIX2 of (string)
  | INFIXL2 of (string)
  | INFIXR2 of (string)
  | INFIX3 of (string)
  | INFIXL3 of (string)
  | INFIXR3 of (string)
  | INFIX4 of (string)
  | INFIXL4 of (string)
  | INFIXR4 of (string)
  | INFIX5 of (string)
  | INFIXL5 of (string)
  | INFIXR5 of (string)
  | INFIX6 of (string)
  | INFIXL6 of (string)
  | INFIXR6 of (string)
  | INFIX7 of (string)
  | INFIXL7 of (string)
  | INFIXR7 of (string)
  | INFIX8 of (string)
  | INFIXL8 of (string)
  | INFIXR8 of (string)
  | INFIX9 of (string)
  | INFIXL9 of (string)
  | INFIXR9 of (string)

val just_datatype :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sugartypes.datatype
val interactive :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sugartypes.sentence
val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sugartypes.binding list * Sugartypes.phrase option
