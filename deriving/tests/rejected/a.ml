(* Reject types called 'a' to avoid confusion with the overloaded type parameter *)

type a = A
    deriving (Eq)
