(*pp deriving *)

type t1 = F deriving (Typeable)
type t2 = int deriving (Typeable)

type t3 = [`T of int] deriving (Typeable)
type t4 = [`T of t2] deriving (Typeable)


type t5 = [`T of t5]
    deriving (Typeable)

let main () = 
  prerr_endline (string_of_bool 
                   (Typeable.TypeRep.eq
                      (Typeable_t5.typeRep ())
                      (Typeable_t5.typeRep ())))
