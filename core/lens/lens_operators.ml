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
    | FloatMinus
    | Not
    | Name of name
    [@@deriving show]

  let from_links v =
    match v with
    | `Minus -> Minus
    | `FloatMinus -> FloatMinus
    | `Name name -> Name name

  let to_string =
  function
    | Minus -> "-"
    | FloatMinus -> ".-"
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
    | And -> "&&"
    | Or -> "||"

  let fmt f v =
    Format.fprintf f "%s" (to_string v)
end

module Binary = struct
  type t =
    | Minus
    | FloatMinus
    | Cons
    | Logical of Logical_binop.t
    | Name of name
    [@@deriving show]

  let from_links v =
    match v with
    | `Minus -> Minus
    | `FloatMinus -> FloatMinus
    | `Cons -> Cons
    | `And -> Logical Logical_binop.And
    | `Or -> Logical Logical_binop.Or
    | `Name name -> Name name
    | `RegexMatch _ -> failwith "Regex not supported in relational lenses."

  let to_string =
    function
    | Minus -> "-"
    | FloatMinus -> ".-"
    | Cons -> "::"
    | Name name -> name
    | Logical l -> Logical_binop.to_string l

  let of_string : string -> t =
    function
    | "-" -> Minus
    | ".-" -> FloatMinus
    | "&&" -> Logical Logical_binop.And
    | "||" -> Logical Logical_binop.Or
    | "::" -> Cons
    | name -> Name name

  let fmt f v =
    Format.fprintf f "%s" (to_string v)
end
