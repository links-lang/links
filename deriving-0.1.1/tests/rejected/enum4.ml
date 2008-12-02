(* Enum for extending polymorphic variant types *)
type t1 = [`A] deriving (Enum)

type t2 = [`B|t1] deriving (Enum)
