(* The operators named here are the ones that it is difficult or
   impossible to define as "user" infix operators:

      - -.  are both infix and prefix
     && ||  have special evaluation
     ::     is also used in patterns
     ~      triggers a lexer state switch
*)
open Lens_utility

module Operator_not_found_exception = struct
  type t = string

  let to_string v =
    Format.asprintf "Relational lenses do not support operator %s." v

  let get_op v = v
end

module Unary = struct
  type t = Minus | Not [@@deriving show, sexp]

  let to_string = function
    | Minus -> "-"
    | Not -> "!"

  let of_string s =
    let open Result in
    match s with
    | "-" -> return Minus
    | "!" -> return Not
    | _ -> error s

  let of_string_exn s = of_string s |> Result.ok_exn

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
  [@@deriving show, sexp]

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

  let of_string op =
    let open Result in
    match op with
    | "-" -> return Minus
    | "*" -> return Multiply
    | "/" -> return Divide
    | "&&" -> return LogicalAnd
    | "||" -> return LogicalOr
    | "=" -> return Equal
    | "+" -> return Plus
    | ">" -> return Greater
    | ">=" -> return GreaterEqual
    | "<" -> return Less
    | "<=" -> return LessEqual
    | _ -> error op

  let of_string_exn op = of_string op |> Result.ok_exn

  let fmt f v = Format.fprintf f "%s" (to_string v)
end
