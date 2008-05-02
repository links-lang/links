(*pp deriving *)
type num = Num.num
type constant =
    [ `Float  of float
    | `Int    of num
    | `String of string
    | `Bool   of bool
    | `Char   of char ]
      deriving (Eq, Typeable, Show, Pickle, Shelve)
