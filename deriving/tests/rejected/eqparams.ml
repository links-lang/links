(* All types in a group must have the same parameters *)

type 'a t1 = int
and ('a,'b) t2 = int
and t3 = int
    deriving (Eq)
