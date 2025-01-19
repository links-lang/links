open Operators
open Lens_utility
open Result.O
module Sugar = Phrase_sugar
module Types = Phrase_type
module Value = Phrase_value

type 'a error = { msg : string; data : 'a }

let rec typ_constant constant =
  match constant with
  | Value.Bool _ -> Types.Bool
  | Value.Int _ -> Types.Int
  | Value.Float _ -> Types.Float
  | Value.String _ -> Types.String
  | Value.Char _ -> Types.Char
  | Value.Tuple v -> Types.Tuple (List.map ~f:typ_constant v)
  | _ -> failwith "Unsupported constant."

let rec tc_infix ~env ~data ~op p q =
  tc ~env p >>= fun p ->
  tc ~env q >>= fun q ->
  match op with
  | Binary.LogicalAnd
   |Binary.LogicalOr -> (
      match (p, q) with
      | Types.Bool, Types.Bool -> Result.return Types.Bool
      | _ ->
          Result.error
            { data; msg = "Logical operator requires boolean operands." })
  | Binary.Greater
   |Binary.GreaterEqual
   |Binary.Less
   |Binary.LessEqual
   |Binary.Equal ->
      if Types.equal p q then Result.return Types.Bool
      else Result.error { data; msg = "Types do not match." }
  | Binary.Minus
   |Binary.Plus
   |Binary.Multiply
   |Binary.Divide -> (
      match (p, q) with
      | Types.Int, Types.Int -> Types.Int |> Result.return
      | Types.Float, Types.Float -> Types.Float |> Result.return
      | _ ->
          Result.error { data; msg = "Incorrect or unmatching numeric types." })

and tc_unary ~env ~data ~op p =
  tc ~env p >>= fun p ->
  match op with
  | Unary.Not -> (
      match p with
      | Types.Bool -> Result.return Types.Bool
      | _ -> Result.error { msg = "Unsupported unary negation operand."; data })
  | Unary.Minus -> (
      match p with
      | Types.Int -> Result.Ok Types.Int
      | Types.Float -> Result.Ok Types.Float
      | _ -> Result.Error { msg = "Unsuported unary minus operand."; data })

and tc ~env (data, phrase) =
  match phrase with
  | Sugar.Constant c -> typ_constant c |> Result.return
  | Sugar.Var v ->
      let res = Alias.Map.find ~key:v env in
      Result.of_option res ~error:(fun _ ->
          let msg = Format.asprintf "Column '%s' is not bound." v in
          Result.error { data; msg })
  | Sugar.InfixAppl (op, p, q) -> tc_infix ~env ~data ~op p q
  | Sugar.UnaryAppl (op, p) -> tc_unary ~env ~data ~op p

let tc_columns ~columns phrase =
  let env =
    columns
    |> List.map ~f:(fun c -> (Column.alias c, Column.typ c))
    |> Alias.Map.from_alist
  in
  tc ~env phrase
