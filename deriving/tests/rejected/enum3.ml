(* Enum for polymorphic variant types with arguments *)

type t = [`A of int | `B] deriving (Enum)
