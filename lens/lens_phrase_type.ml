open Lens_utility

type t =
  | Bool
  | Int
  | Float
  | String
  | Char
  | Tuple of t list
  | Record of t String.Map.t
[@@deriving show]

let equal s t = s = t
