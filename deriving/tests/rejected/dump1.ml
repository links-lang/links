(* private datatypes cannot be instances of dump (because Dump.from_string
   constructs values *)

type p = private F
    deriving (Dump)
