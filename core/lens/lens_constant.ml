type t = Constant.constant

let bool v = `Bool v

let int v = `Int v

let fmt f v = Format.fprintf f "%s" (Constant.string_of_constant v)

let to_value = function
    | `Int a -> `Int a
    | `Float f -> `Float f
    | `String s -> `String s
    | `Bool b -> `Bool b
    | _ -> failwith "Unsupported constant type."

let of_value = function
    | `Int a -> `Int a
    | `Float f -> `Float f
    | `String s -> `String s
    | `Bool b -> `Bool b
    | _ -> failwith "Unsupported value type."

