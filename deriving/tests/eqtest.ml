(*pp deriving *)

(* Test for eq instance generation *)
type 'a t1 = A | B of int | C of 'a t1 | D of ('a t1 * int) | E of 'a t1 * int
    deriving (Eq)

type t2 = int t1
    deriving (Eq)

let _ = (Eq_t2.eq : t2 -> t2 -> bool)

type 'a t3 = [`A of 'a]
    deriving (Eq)

type t4 = [`B| t4 t3]
    deriving (Eq)

type t5 = [`C] deriving (Eq)
type t6 = [`D | t5] deriving (Eq)

type t7 = { x : int; y : t6 option } deriving (Eq)


(* mutable fields *)
type 'a mynonref = {contents : 'a}
    deriving (Eq) 

type 'a myref = {mutable contents : 'a}
    deriving (Eq) 
