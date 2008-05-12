(*pp deriving *)
type num = Num.num
type constant =
    [ `Float  of float
    | `Int    of num
    | `String of string
    | `Bool   of bool
    | `Char   of char ]
      deriving (Eq, Typeable, Show, Pickle, Shelve)

let constant_type = function
  | `Float _  -> `Primitive `Float
  | `Int _    -> `Primitive `Int
  | `Bool _   -> `Primitive `Bool
  | `Char _   -> `Primitive `Char
  | `String _ ->  Types.string_type
