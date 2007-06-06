(*pp derivingpp *)

type sum deriving (Eq) (*(Pickle, Eq, Show, Typeable, Shelve)*)
(*type nullsum deriving (Enum, Bounded, Eq, Typeable, Shelve)
type r1 deriving (Pickle, Eq, Show, Typeable, Shelve, Functor)
type r2 deriving (Pickle, Eq, Show, Typeable, Shelve)
type r3 deriving (Pickle, Eq, Show, Typeable, Shelve)
type r4
type label
type funct
type intseq deriving (Pickle, Eq, Show, Typeable, Shelve, Functor)
type 'a seq deriving (Pickle, Eq, Show, Functor, Typeable, Shelve)
type uses_seqs deriving (Pickle, Eq, Show, Typeable, Shelve)
type 'a nested
type obj
type poly0 deriving (Enum, Bounded, Show, Eq, Typeable, Shelve)
type poly1 deriving (Pickle, Eq, Show)
type poly2 deriving (Pickle, Eq, Show)
type poly3 deriving (Pickle, Eq, Show, Typeable, Shelve) 
type poly3b deriving (Pickle, Eq, Show, Typeable, Shelve) 
type poly4
type poly5
type 'a poly7
and 'a poly8 deriving (Pickle, Eq, Show, Functor, Typeable, Shelve)
type poly10 deriving (Pickle, Eq, Show, Functor, Typeable, Shelve)
type mutrec_a
and mutrec_b
and mutrec_c
and mutrec_d deriving (Pickle, Eq, Show, Typeable, Shelve)
type ('a,'b) pmutrec_a
and ('a,'b) pmutrec_b
and ('a,'b) pmutrec_c
and ('a,'b) pmutrec_d deriving (Pickle, Eq, Show, Functor, Typeable, Shelve)
type 'a ff1 deriving (Show, Eq, Pickle, Functor, Typeable, Shelve)
type ('a,'b) ff2 deriving (Pickle, Eq, Show, Functor, Typeable, Shelve)
type tup0 deriving (Pickle, Eq, Show, Typeable, Shelve)
type tup2 deriving (Pickle, Eq, Show, Typeable, Shelve)
type tup3 deriving (Pickle, Eq, Show, Typeable, Shelve)
type withref deriving (Pickle, Eq, Show, Typeable(*, Shelve*))
module M : sig 
  type t deriving (Pickle, Eq, Show)
end
module P : sig 
  type 'a t
end
type 'a constrained deriving (Functor)
*)
