(* The operators named here are the ones that it is difficult or
   impossible to define as "user" infix operators:

      - -.  are both infix and prefix
     && ||  have special evaluation
     ::     is also used in patterns
     ~      triggers a lexer state switch
*)

type name = string [@@deriving show]

module UnaryOp = struct
  type t =
    | Minus
    | FloatMinus
    | Name of name
    [@@deriving show]
end
