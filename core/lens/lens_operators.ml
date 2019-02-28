open Operators

(* The operators named here are the ones that it is difficult or
   impossible to define as "user" infix operators:

      - -.  are both infix and prefix
     && ||  have special evaluation
     ::     is also used in patterns
     ~      triggers a lexer state switch
*)

type name = string [@@deriving show]

module Unary = struct
  type t =
    | Minus
    | Not
    | Name of name
    [@@deriving show]

  let from_links v =
    match v with
    | UnaryOp.Minus -> Minus
    | UnaryOp.FloatMinus -> Minus
    | UnaryOp.Name name -> Name name

  let to_string =
  function
    | Minus -> "-"
    | Name name -> name
    | Not -> "!"

  let fmt f v =
    Format.fprintf f "%s" (to_string v)
end

module Logical_binop = struct
  type t =
    | And
    | Or
    [@@deriving show]

  let to_string =
    function
    | And -> "AND"
    | Or -> "OR"

  let fmt f v =
    Format.fprintf f "%s" (to_string v)
end

module Binary = struct
  type t =
    | Minus
    | Equal
    | Cons
    | Logical of Logical_binop.t
    | Name of name
    [@@deriving show]

  let of_supertype_operator v =
    match v with
    | BinaryOp.Minus -> Minus
    | BinaryOp.FloatMinus -> Minus
    | BinaryOp.Cons -> Cons
    | BinaryOp.And -> Logical Logical_binop.And
    | BinaryOp.Or -> Logical Logical_binop.Or
    | BinaryOp.Name "==" -> Equal
    | BinaryOp.Name name -> Name name
    | BinaryOp.RegexMatch _ -> failwith "Regex not supported in relational lenses."

  let to_string =
    function
    | Minus -> "-"
    | Cons -> "::"
    | Equal -> "="
    | Name name -> name
    | Logical l -> Logical_binop.to_string l

  let of_string : string -> t =
    function
    | "-" -> Minus
    | "&&" -> Logical Logical_binop.And
    | "||" -> Logical Logical_binop.Or
    | "::" -> Cons
    | "=" -> Equal
    | name -> Name name

  let fmt f v =
    Format.fprintf f "%s" (to_string v)
end
