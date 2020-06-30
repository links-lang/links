open Lens_utility

type t =
  | Bool
  | Int
  | Serial
  | Float
  | String
  | Char
  | Tuple of t list
  | Record of t String.Map.t
[@@deriving show, sexp]

let rec pp_pretty f v =
  let const s = Format.pp_constant s f () in
  match v with
  | Bool -> const "Bool"
  | Int -> const "Int"
  | Serial -> const "Serial"
  | Float -> const "Float"
  | String -> const "String"
  | Char -> const "Char"
  | Tuple l -> Format.fprintf f "Tuple(%a)" (Format.pp_comma_list pp_pretty) l
  | Record r ->
      let pp_entry f (key, t) = Format.fprintf f "%s : %a" key pp_pretty t in
      String.Map.to_list (fun a b -> (a, b)) r
      |> Format.fprintf f "Record(%a)" (Format.pp_comma_list pp_entry)

let equal s t = s = t

let simplify t =
  match t with
  | Serial -> Int
  | _ -> t
