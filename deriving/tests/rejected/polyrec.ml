(* non-regular datatype *) 

type 'a seq = Nil | Cons of 'a * ('a * 'a) seq
  deriving (Eq)
