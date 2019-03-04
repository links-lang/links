open CommonTypes
module Value = Lens_phrase_value

type t = Constant.t

let bool v = Constant.Bool v

let int v = Constant.Int v

let fmt f v = Format.fprintf f "%s" (Constant.to_string v)

let to_value = function
  | Constant.Char c -> Value.Char c
  | Constant.Int a -> Value.Int a
  | Constant.Float f -> Value.Float f
  | Constant.String s -> Value.String s
  | Constant.Bool b -> Value.Bool b

let of_value = function
  | Value.Int a -> Constant.Int a
  | Value.Float f -> Constant.Float f
  | Value.String s -> Constant.String s
  | Value.Bool b -> Constant.Bool b
  | Value.Char c -> Constant.Char c
  | Value.Tuple _ -> failwith "Tuples not supported by Lens_constant.of_value."
