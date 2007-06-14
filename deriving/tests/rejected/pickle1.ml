(* private datatypes cannot be instances of pickle (because unpickle
   constructs values *)

type p = private F
    deriving (Pickle)
