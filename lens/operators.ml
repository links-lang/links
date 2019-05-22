(* The operators named here are the ones that it is difficult or
   impossible to define as "user" infix operators:

      - -.  are both infix and prefix
     && ||  have special evaluation
     ::     is also used in patterns
     ~      triggers a lexer state switch
*)

module Unary = struct
  type t = Minus | Not [@@deriving show]

  let to_string = function
    | Minus -> "-"
    | Not -> "!"

  let of_string s =
    match s with
    | "-" -> Minus
    | "!" -> Not
    | _ as op ->
        failwith @@ "Operator " ^ op ^ " not supported by Unary.of_string."

  let fmt f v = Format.fprintf f "%s" (to_string v)
end

module Binary = struct
  type t =
    | Plus
    | Minus
    | Multiply
    | Divide
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    | Equal
    | LogicalAnd
    | LogicalOr
  [@@deriving show]

  let to_string = function
    | Plus -> "+"
    | Minus -> "-"
    | Multiply -> "*"
    | Divide -> "/"
    | Equal -> "="
    | Greater -> ">"
    | GreaterEqual -> ">="
    | Less -> "<"
    | LessEqual -> "<="
    | LogicalAnd -> "AND"
    | LogicalOr -> "OR"

  let of_string : string -> t = function
    | "-" -> Minus
    | "*" -> Multiply
    | "/" -> Divide
    | "&&" -> LogicalAnd
    | "||" -> LogicalOr
    | "=" -> Equal
    | "+" -> Plus
    | ">" -> Greater
    | ">=" -> GreaterEqual
    | "<" -> Less
    | "<=" -> LessEqual
    | _ as op ->
        failwith @@ "Operator " ^ op ^ " not supported by Binary.of_string."

  let fmt f v = Format.fprintf f "%s" (to_string v)
end
