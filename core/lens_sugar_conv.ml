open Lens_operators
open Operators
open SourceCode
open Lens_utility
module LPS = Lens_phrase_sugar
module S = Sugartypes

let unary_of_sugartype_op v =
  let open Unary in
  match v with
  | UnaryOp.Minus -> Some Minus
  | UnaryOp.FloatMinus -> Some Minus
  | _ -> None

let binary_of_sugartype_op v =
  let open Binary in
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

let cols_of_phrase key : string list =
  let open Sugartypes in
  let var_name (var : phrase) =
    match WithPos.node var with
    | Var name -> name
    | _ -> failwith "Expected a `Var type"
  in
  match WithPos.node key with
  | TupleLit keys -> List.map ~f:var_name keys
  | Var name -> [name]
  | _ -> failwith "Expected a tuple or a variable."

let rec lens_sugar_phrase_of_sugar p =
  let conv p = lens_sugar_phrase_of_sugar p in
  let pos = WithPos.pos p in
  ( match WithPos.node p with
  | S.InfixAppl ((_, op), p, q) ->
      let op = binary_of_sugartype_op op |> fun v -> Option.value_exn v in
      let p = conv p in
      let q = conv q in
      LPS.InfixAppl (op, p, q)
  | S.UnaryAppl ((_, op), p) ->
      let op = unary_of_sugartype_op op |> fun v -> Option.value_exn v in
      let p = conv p in
      LPS.UnaryAppl (op, p)
  | S.Constant c ->
      LPS.Constant (Lens_value_conv.lens_phrase_value_of_constant c)
  | S.Var v -> LPS.Var v
  | _ -> failwith "Unsupported sugar phrase." )
  |> fun v -> (pos, v)
