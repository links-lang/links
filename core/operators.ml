(* The operators named here are the ones that it is difficult or
   impossible to define as "user" infix operators:

      - -.  are both infix and prefix
     && ||  have special evaluation
     ::     is also used in patterns
     ~      triggers a lexer state switch

   Operators were extracted from Sugartypes to their own module to avoid import
   cycle with lens code.  If, at any point in the future, the import cycle no
   longer exists this module can be merged back into Sugartypes.
*)

open CommonTypes

type regexflag = RegexList | RegexNative | RegexGlobal | RegexReplace
    [@@deriving show]

module Associativity = struct
  type t = Left | Right | None
    [@@deriving show]
end

module UnaryOp = struct
  type t =
    | Minus
    | FloatMinus
    | Name of Name.t
    [@@deriving show]

  let to_string = function
    | Minus      -> "-"
    | FloatMinus -> ".-"
    | Name name  -> name
end

module BinaryOp = struct
  type t =
    | Minus
    | FloatMinus
    | RegexMatch of regexflag list
    | And
    | Or
    | Cons
    | Name of Name.t
    [@@deriving show]

  let to_string = function
    | Minus        -> "-"
    | FloatMinus   -> ".-"
    | RegexMatch _ -> "<some regex nonsense>"
    | And          -> "&&"
    | Or           -> "||"
    | Cons         -> "::"
    | Name name    -> name
end

(* Operator section *)
module Section = struct
  type t = Minus | FloatMinus | Project of Name.t | Name of Name.t
    [@@deriving show]
end
