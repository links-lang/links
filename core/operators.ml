(* The operators named here are the ones that it is difficult or
   impossible to define as "user" infix operators:

      - -.  are both infix and prefix
     && ||  have special evaluation
     ::     is also used in patterns
     ~      triggers a lexer state switch
*)

(*JSTOLAREK: define fixity *)

type name = string [@@deriving show]

type regexflag = RegexList | RegexNative | RegexGlobal | RegexReplace
    [@@deriving show]

module UnaryOp = struct
  type t =
    | Minus
    | FloatMinus
    | Name of name
    [@@deriving show]
end

let string_of_unary_op =
  function
  | UnaryOp.Minus -> "-"
  | UnaryOp.FloatMinus -> ".-"
  | UnaryOp.Name name -> name

module BinaryOp = struct
  type t =
    | Minus
    | FloatMinus
    | RegexMatch of regexflag list
    | And
    | Or
    | Cons
    | Name of name
    [@@deriving show]
end

let string_of_binop =
  let open BinaryOp in function
  | Minus -> "-"
  | FloatMinus -> ".-"
  | RegexMatch _ -> "<some regex nonsense>"
  | And -> "&&"
  | Or -> "||"
  | Cons -> "::"
  | Name name -> name

let binop_of_string : string -> BinaryOp.t =
   let open BinaryOp in function
   | "-"  -> Minus
   | ".-" -> FloatMinus
   | "&&" -> And
   | "||" -> Or
   | "::" -> Cons
   | name -> Name name
