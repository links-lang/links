(* Polymorphic variant definitions within polymorphic record field
   types *)
type r = {
  (* I think this could be supported without too much difficulty, but
     it doesn't have much benefit *)
  x : 'a. [`Foo of 'a] 
    
} deriving (Eq)
