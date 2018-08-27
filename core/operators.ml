(* The operators named here are the ones that it is difficult or
   impossible to define as "user" infix operators:

      - -.  are both infix and prefix
     && ||  have special evaluation
     ::     is also used in patterns
     ~      triggers a lexer state switch
*)

type name = string [@@deriving show]

type unary_op = [
| `Minus
| `FloatMinus
| `Name of name
]
and regexflag = [`RegexList | `RegexNative | `RegexGlobal | `RegexReplace ]
    [@@deriving show]
type logical_binop = [`And | `Or ]
    [@@deriving show]
type binop = [ `Minus | `FloatMinus | `RegexMatch of regexflag list | logical_binop | `Cons | `Name of name ]
    [@@deriving show]
type operator = [ unary_op | binop | `Project of name ]
    [@@deriving show]

let string_of_unary_op =
  function
    | `Minus -> "-"
    | `FloatMinus -> ".-"
    | `Name name -> name

let string_of_binop =
  function
    | `Minus -> "-"
    | `FloatMinus -> ".-"
    | `RegexMatch _ -> "<some regex nonsense>"
    | `And -> "&&"
    | `Or -> "||"
    | `Cons -> "::"
    | `Name name -> name

let binop_of_string : string -> binop =
   function
      | "-" -> `Minus
      | ".-" -> `FloatMinus
      | "&&" -> `And
      | "||" -> `Or
      | "::" -> `Cons
      | name -> `Name name
