(*pp deriving *)
type num = Num.num
type constant =
    [ `Float  of float
    | `Int    of num
    | `String of string
    | `Bool   of bool
    | `Char   of char ]
      deriving (Show)

let constant_type = function
  | `Float _  -> `Primitive `Float
  | `Int _    -> `Primitive `Int
  | `Bool _   -> `Primitive `Bool
  | `Char _   -> `Primitive `Char
  | `String _ ->  Types.string_type

let string_of_constant =
  (* This function is actually specific to database query generation;
     it should be moved to the database module(s). *)
  function
    | `Bool value -> string_of_bool value
    | `Int value -> Num.string_of_num value
    | `Char c -> "'"^ Char.escaped c ^"'" 
    | `String s -> "\"" ^ s ^ "\""
    | `Float value   -> string_of_float value
