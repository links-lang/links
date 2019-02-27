open CommonTypes

type t = Constant.t

let bool v = Constant.Bool v

let int v = Constant.Int v

let fmt f v = Format.fprintf f "%s" (Constant.to_string v)

let to_value = function
    | Constant.Int a    -> `Int a
    | Constant.Float f  -> `Float f
    | Constant.String s -> `String s
    | Constant.Bool b   -> `Bool b
    | _ -> failwith "Unsupported constant type."

let of_value = function
    | `Int a    -> Constant.Int a
    | `Float f  -> Constant.Float f
    | `String s -> Constant.String s
    | `Bool b   -> Constant.Bool b
    | _ -> failwith "Unsupported value type."
