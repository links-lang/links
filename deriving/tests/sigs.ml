(*pp deriving *)

(* Deriving a signature with types exposed *)
module T :
sig 
  type sum = S0 | S1 of int | S2 of int * float | S3 of int * float * bool | Sunit of unit | Stup of (int * float) | Stup1 of (int)
    deriving (Dump, Eq, Show, Typeable, Pickle)

  type nullsum = N0 | N1 | N2 | N3
      deriving (Enum, Bounded, Eq, Typeable, Pickle)

  type r1 = {
    r1_l1 : int;
    r1_l2 : int;
  } deriving (Dump, Eq, Show, Typeable, Pickle, Functor)

  type r2 = {
    mutable r2_l1 : int;
    mutable r2_l2 : int;
  } deriving (Eq, Show, Typeable, Pickle)

  type r3 = {
    r3_l1 : int;
    mutable r3_l2 : int;
  } deriving (Eq, Show, Typeable, Pickle)

  type r4 = {
    r4_l1 : 'a . 'a list
  } 
  type label = x:int -> int

  type funct = int -> int

  type intseq = INil | ICons of int * intseq
    deriving (Dump, Eq, Show, Typeable, Pickle, Functor)

  type 'a seq = Nil | Cons of 'a * 'a seq
    deriving (Dump, Eq, Show, Functor, Typeable, Pickle)

  type uses_seqs = (intseq * float seq) 
      deriving (Dump, Eq, Show, Typeable, Pickle)

  type obj = < x : int >

  type poly0 = [`T0 | `T1 | `T2 | `T3]
      deriving (Enum, Bounded, Show, Eq, Typeable, Pickle)

  type poly1 = [`T0 | `T1 of int]
      deriving (Dump, Eq, Show)

  type poly2 = P of int * [`T0 | `T1 of int] * float
    deriving (Dump, Eq, Show)

  type poly3 = [`Nil | `Cons of int * 'c] as 'c
      deriving (Dump, Eq, Show, Typeable, Pickle) 

  type poly3b = int * ([`Nil | `Cons of int * 'c] as 'c) * [`F]
      deriving (Dump, Eq, Show, Typeable, Pickle) 

  type 'a poly7 = Foo of [`F of 'a]
  and 'a poly8 = { x : [`G of [`H of [`I of 'a poly7]]] }
      deriving (Dump, Eq, Show, Functor, Typeable, Pickle)

  type poly10 = [`F | poly3]
      deriving (Dump, Eq, Show, Functor, Typeable, Pickle)

  type mutrec_a = mutrec_c
  and mutrec_b = { l1 : mutrec_c ; l2 : mutrec_a }
  and mutrec_c = S of int * mutrec_a | N
  and mutrec_d = [`T of mutrec_b]
      deriving (Dump, Eq, Show, Typeable, Pickle)

  type ('a,'b) pmutrec_a = ('a,'b) pmutrec_c
  and ('a,'b) pmutrec_b = { pl1 : ('a,'b) pmutrec_c ; pl2 : ('a,'b) pmutrec_a }
  and ('a,'b) pmutrec_c = SS of 'a * ('a,'b) pmutrec_a * 'b
  and ('a,'b) pmutrec_d = [`T of ('a,'b) pmutrec_b]
      deriving (Dump, Eq, Show, Functor, Typeable, Pickle)

  type 'a ff1 = F of 'a * 'a | G of int deriving (Show, Eq, Dump, Functor, Typeable, Pickle)
  type ('a,'b) ff2 = F1 of ('a,'b) ff2 | F2 of 'a seq * int * 'b option
    deriving (Dump, Eq, Show, Functor, Typeable, Pickle)

  type tup0 = unit
      deriving (Dump, Eq, Show, Typeable, Pickle)
  type tup2 = int * float
      deriving (Dump, Eq, Show, Typeable, Pickle)
  type tup3 = int * float * bool
      deriving (Dump, Eq, Show, Typeable, Pickle)
  type tup4 = int * int * bool * unit
      deriving (Dump, Eq, Show, Typeable, Pickle, Bounded)

  type withref = WR of int * (int ref)
    deriving (Eq, Show, Typeable, Pickle)

  module M : sig 
    type t deriving (Show, Eq, Dump)
  end

  module P : sig 
    type 'a t (* deriving (Show) *)
  end

  type 'a constrained = [`F of 'a] constraint 'a = int
      deriving (Functor) 
      
  type p1 = private P1 
      deriving (Show, Eq)
      
  module Private : sig
    type p2 = private Q deriving (Show, Eq, Dump)
  end
    
  type t = int 
      deriving (Eq, Enum, Bounded, Dump, Show, Typeable, Pickle, Functor)
end
  = Defs

(* Deriving a signature with types made abstract *)
module T_opaque :
sig 
  type sum deriving (Dump, Eq, Show, Typeable, Pickle)
  type nullsum deriving (Enum, Bounded, Eq, Typeable, Pickle)
  type r1 deriving (Dump, Eq, Show, Typeable, Pickle, Functor)
  type r2 deriving (Eq, Show, Typeable, Pickle)
  type r3 deriving (Eq, Show, Typeable, Pickle)
  type r4 
  type label
  type funct
  type intseq deriving (Dump, Eq, Show, Typeable, Pickle, Functor)
  type 'a seq deriving (Dump, Eq, Show, Functor, Typeable, Pickle)
  type uses_seqs deriving (Dump, Eq, Show, Typeable, Pickle)
  type obj
  type poly0 deriving (Enum, Bounded, Show, Eq, Typeable, Pickle)
  type poly1 deriving (Dump, Eq, Show)
  type poly2 deriving (Dump, Eq, Show)
  type poly3 deriving (Dump, Eq, Show, Typeable, Pickle) 
  type poly3b deriving (Dump, Eq, Show, Typeable, Pickle) 
  type 'a poly7 
  and 'a poly8 deriving (Dump, Eq, Show, Functor, Typeable, Pickle)
  type poly10 deriving (Dump, Eq, Show, Functor, Typeable, Pickle)
  type mutrec_a 
  and mutrec_b 
  and mutrec_c 
  and mutrec_d deriving (Dump, Eq, Show, Typeable, Pickle)
  type ('a,'b) pmutrec_a 
  and ('a,'b) pmutrec_b 
  and ('a,'b) pmutrec_c 
  and ('a,'b) pmutrec_d deriving (Dump, Eq, Show, Functor, Typeable, Pickle)
  type 'a ff1 deriving (Show, Eq, Dump, Functor, Typeable, Pickle)
  type ('a,'b) ff2 deriving (Dump, Eq, Show, Functor, Typeable, Pickle)
  type tup0 deriving (Dump, Eq, Show, Typeable, Pickle)
  type tup2 deriving (Dump, Eq, Show, Typeable, Pickle)
  type tup3 deriving (Dump, Eq, Show, Typeable, Pickle)
  type tup4 deriving (Dump, Eq, Show, Typeable, Pickle, Bounded)
  type withref deriving (Eq, Show, Typeable, Pickle)
  module M : sig type t deriving (Show, Eq, Dump) end
  module P : sig type 'a t end
  type 'a constrained constraint 'a = int deriving (Functor) 
  type p1 deriving (Show, Eq)
  module Private : sig type p2 end
  type t deriving (Eq, Enum, Bounded, Dump, Show, Typeable, Pickle, Functor)
end
  = Defs


(* A signature with no deriving (to make sure that the types are still
   compatible) *)
module T_no_deriving :
sig 
  type sum = S0 | S1 of int | S2 of int * float | S3 of int * float * bool | Sunit of unit | Stup of (int * float) | Stup1 of (int)

  type nullsum = N0 | N1 | N2 | N3

  type r1 = {
    r1_l1 : int;
    r1_l2 : int;
  } 

  type r2 = {
    mutable r2_l1 : int;
    mutable r2_l2 : int;
  } 

  type r3 = {
    r3_l1 : int;
    mutable r3_l2 : int;
  } 

  type r4 = {
    r4_l1 : 'a . 'a list
  } 
  type label = x:int -> int

  type funct = int -> int

  type intseq = INil | ICons of int * intseq

  type 'a seq = Nil | Cons of 'a * 'a seq

  type uses_seqs = (intseq * float seq) 

  type obj = < x : int >

  type poly0 = [`T0 | `T1 | `T2 | `T3]

  type poly1 = [`T0 | `T1 of int]

  type poly2 = P of int * [`T0 | `T1 of int] * float

  type poly3 = [`Nil | `Cons of int * 'c] as 'c

  type poly3b = int * ([`Nil | `Cons of int * 'c] as 'c) * [`F]

  type 'a poly7 = Foo of [`F of 'a]
  and 'a poly8 = { x : [`G of [`H of [`I of 'a poly7]]] }

  type poly10 = [`F | poly3]

  type mutrec_a = mutrec_c
  and mutrec_b = { l1 : mutrec_c ; l2 : mutrec_a }
  and mutrec_c = S of int * mutrec_a | N
  and mutrec_d = [`T of mutrec_b]

  type ('a,'b) pmutrec_a = ('a,'b) pmutrec_c
  and ('a,'b) pmutrec_b = { pl1 : ('a,'b) pmutrec_c ; pl2 : ('a,'b) pmutrec_a }
  and ('a,'b) pmutrec_c = SS of 'a * ('a,'b) pmutrec_a * 'b
  and ('a,'b) pmutrec_d = [`T of ('a,'b) pmutrec_b]

  type 'a ff1 = F of 'a * 'a | G of int
  type ('a,'b) ff2 = F1 of ('a,'b) ff2 | F2 of 'a seq * int * 'b option

  type tup0 = unit
  type tup2 = int * float
  type tup3 = int * float * bool
  type tup4 = int * int * bool * unit
  type withref = WR of int * (int ref)

  module M : sig 
    type t
  end

  module P : sig 
    type 'a t
  end

  type 'a constrained = [`F of 'a] constraint 'a = int
      
  type p1 = private P1 
      
  module Private : sig
    type p2 = private Q
  end
    
  type t = int 
end
  = Defs
