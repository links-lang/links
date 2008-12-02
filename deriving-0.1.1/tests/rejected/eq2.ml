(* Eq for records with polymorphic fields *)
type r4 = {
  l1 : 'a . 'a list
} deriving (Eq)
