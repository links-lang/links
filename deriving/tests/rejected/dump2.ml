(* records with mutable fields cannot be instances of Dump
   (because it doesn't preserve sharing *)

type t = { x : int; mutable y : int ; z : int }
    deriving (Dump)
