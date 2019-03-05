open CommonTypes
open Lens_operators
open Lens_utility
open Result.O
module Sugar = Lens_phrase_sugar
module Phrase = Lens_phrase
module Types = Lens_phrase_type

type 'a error = {msg: string; data: 'a}

let typ_constant constant =
  match constant with
  | Constant.Bool _ -> Types.Bool
  | Constant.Int _ -> Types.Int
  | Constant.Float _ -> Types.Float
  | Constant.String _ -> Types.String
  | Constant.Char _ -> Types.Char

let rec tc_infix ~env ~data ~op p q =
  tc ~env p
  >>= fun p ->
  tc ~env q
  >>= fun q ->
  match op with
  | Binary.LogicalAnd | Binary.LogicalOr -> (
    match (p, q) with
    | Types.Bool, Types.Bool -> Result.return Types.Bool
    | _ ->
        Result.error {data; msg= "Logical operator requires boolean operands."}
    )
  | Binary.Greater | Binary.GreaterEqual | Binary.Less | Binary.LessEqual
   |Binary.Equal ->
      if Types.equal p q then Result.return Types.Bool
      else Result.error {data; msg= "Types do not match."}
  | Binary.Minus | Binary.Plus | Binary.Multiply | Binary.Divide -> (
    match (p, q) with
    | Types.Int, Types.Int -> Types.Int |> Result.return
    | Types.Float, Types.Float -> Types.Float |> Result.return
    | _ -> Result.error {data; msg= "Incorrect or unmatching numeric types."} )

and tc_unary ~env ~data ~op p =
  tc ~env p
  >>= fun p ->
  match op with
  | Unary.Not -> (
    match p with
    | Types.Bool -> Result.return Types.Bool
    | _ -> Result.error {msg= "Unsupported unary negation operand."; data} )
  | Unary.Minus -> (
    match p with
    | Types.Int -> Result.Ok Types.Int
    | Types.Float -> Result.Ok Types.Float
    | _ -> Result.Error {msg= "Unsuported unary minus operand."; data} )

and tc ~env (data, phrase) =
  match phrase with
  | Sugar.Constant c -> typ_constant c |> Result.return
  | Sugar.Var v ->
      let res = Lens_alias.Map.find ~key:v env in
      Result.of_option res ~error:(fun _ ->
          let msg = Format.asprintf "Column '%s' is not bound." v in
          Result.error {data; msg} )
  | Sugar.InfixAppl (op, p, q) -> tc_infix ~env ~data ~op p q
  | Sugar.UnaryAppl (op, p) -> tc_unary ~env ~data ~op p

let tc_sort ~sort phrase =
  let env =
    Lens_sort.cols sort
    |> List.map ~f:(fun c -> (Lens_column.alias c, Lens_column.typ c))
    |> Lens_alias.Map.from_alist
  in
  tc ~env phrase
