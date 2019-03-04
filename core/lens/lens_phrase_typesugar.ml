open CommonTypes
open Lens_operators
open SourceCode
open Lens_utility
module Sugartypes = Lens_phrase_sugartypes
module Phrase = Lens_phrase
module Types = Lens_phrase_type

let typ_constant constant =
  match constant with
  | Constant.Bool _ -> Types.Bool
  | Constant.Int _ -> Types.Int
  | Constant.Float _ -> Types.Float
  | Constant.String _ -> Types.String
  | Constant.Char _ -> Types.Char

let die ~pos msg = Errors.Type_error (pos, msg) |> raise

let error ~pos msg = Errors.Type_error (pos, msg)

let rec tc_infix ~env ~pos ~op p q =
  let s = tc ~env p in
  let t = tc ~env q in
  let typ =
    match op with
    | Binary.LogicalAnd | Binary.LogicalOr -> (
      match (s, t) with
      | Types.Bool, Types.Bool -> Types.Bool
      | _ ->
          die ~pos
            (Format.asprintf "Logical operator requires boolean operands.") )
    | Binary.Greater | Binary.GreaterEqual | Binary.Less | Binary.LessEqual
     |Binary.Equal ->
        if Types.equal s t then Types.Bool else die ~pos "Types do not match."
    | Binary.Minus | Binary.Plus | Binary.Multiply | Binary.Divide -> (
      match (s, t) with
      | Types.Int, Types.Int -> Types.Int
      | Types.Float, Types.Float -> Types.Float
      | _ -> die ~pos "" )
  in
  typ

and tc_unary ~env ~pos ~op p =
  let t = tc ~env p in
  let typ =
    match op with
    | Unary.Not -> (
      match t with
      | Types.Bool -> Types.Bool
      | _ -> die ~pos "Unsupported unary negation operand." )
    | Unary.Minus -> (
      match t with
      | Types.Int -> Types.Int
      | Types.Float -> Types.Float
      | _ -> die ~pos "Unsuported unary minus operand." )
  in
  typ

and tc ~env phrase =
  let pos = WithPos.pos phrase in
  match WithPos.node phrase with
  | Sugartypes.Constant c -> typ_constant c
  | Sugartypes.Var v ->
      let res = Lens_alias.Map.find ~key:v env in
      let typ =
        Option.value_exn
          ~exn:(error ~pos @@ Format.asprintf "Column '%s' not bound." v)
          res
      in
      typ
  | Sugartypes.InfixAppl (op, p, q) -> tc_infix ~env ~pos ~op p q
  | Sugartypes.UnaryAppl (op, p) -> tc_unary ~env ~pos ~op p

let tc_sort ~sort phrase =
  let env = Lens_sort.cols sort
  |> List.map ~f:(fun c -> (Lens_column.alias c, Lens_column.typ c))
  |> Lens_alias.Map.from_alist in
  tc ~env phrase
