open Operators

(* The operators named here are the ones that it is difficult or
   impossible to define as "user" infix operators:

      - -.  are both infix and prefix
     && ||  have special evaluation
     ::     is also used in patterns
     ~      triggers a lexer state switch
*)

module Unary = struct
  type t = Minus | Not [@@deriving show]

  let of_sugartype_op v =
    match v with
    | UnaryOp.Minus -> Some Minus
    | UnaryOp.FloatMinus -> Some Minus
    | _ -> None

  let to_string = function Minus -> "-" | Not -> "!"

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

  let of_sugartype_op v =
    match v with
    | BinaryOp.Minus -> Some Minus
    | BinaryOp.FloatMinus -> Some Minus
    | BinaryOp.And -> Some LogicalAnd
    | BinaryOp.Or -> Some LogicalOr
    | BinaryOp.Name "+" -> Some Plus
    | BinaryOp.Name "*" -> Some Multiply
    | BinaryOp.Name "/" -> Some Divide
    | BinaryOp.Name ">" -> Some Greater
    | BinaryOp.Name "<" -> Some Less
    | BinaryOp.Name ">=" -> Some GreaterEqual
    | BinaryOp.Name "<=" -> Some LessEqual
    | BinaryOp.Name "==" -> Some Equal
    | _ -> None

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
        failwith @@ "Operator " ^ op ^ " not supported by BinaryOp.of_string."

  let fmt f v = Format.fprintf f "%s" (to_string v)
end
