(*pp deriving *)

(* Test for eq instance generation *)

type 'a t1 = A | B of int | C of 'a t1 | D of ('a t1 * int) | E of 'a t1 * int
    deriving (Eq)

type t2 = int t1
    deriving (Eq)

let _ = (Eq_t2.eq : t2 -> t2 -> bool)
