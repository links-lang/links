(*pp deriving *)

type t1 = F deriving (Typeable)
type t2 = int deriving (Typeable)

type t3 = [`T of int] deriving (Typeable)
type t4 = [`T of t2] deriving (Typeable)


type t5 = [`T of t5]
    deriving (Typeable)

type t6 = [`T of t6]
    deriving (Typeable)

type t7 = [`T1 of int | `T2 of t8]
and t8 = [`T3 of t7]
    deriving (Typeable)

type t9 = [`T1 of int | `T2 of [`T3 of t9 ]]
    deriving (Typeable)

let main = 
  assert (Typeable.TypeRep.eq
            (Typeable_t5.type_rep ())
            (Typeable_t5.type_rep ()))
