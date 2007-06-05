(*pp derivingpp *)
(* Test for pickle instance generation *)

(* 1. sums (nullary, unary, and n-ary) *)
type sum = S0 | S1 of int | S2 of int * float | S3 of int * float * bool | Sunit of unit | Stup of (int * float) | Stup1 of (int)
  deriving (Pickle, Eq, Show, Typeable, Shelve)

type nullsum = S0 | S1 | S2 | S3
    deriving (Enum, Bounded, Eq, Typeable, Shelve)

(* 2. records with mutable and immutable fields (and various combinations) *)
type r1 = {
  l1 : int;
  l2 : int;
} deriving (Pickle, Eq, Show, Typeable, Shelve)

type r2 = {
  mutable l1 : int;
  mutable l2 : int;
} deriving (Pickle, Eq, Show, Typeable, Shelve)

type r3 = {
  l1 : int;
  mutable l2 : int;
} deriving (Pickle, Eq, Show, Typeable, Shelve)

(* 3. polymorphic records *)
type r4 = {
  l1 : 'a . 'a list
} (* deriving (Pickle, Eq, Show, Typeable, Shelve) *)

(* 4. label types *)
type label = x:int -> int
  (*  deriving (Pickle, Eq, Show) *)

(* 5. function types  *)
type funct = int -> int
  (* deriving (Pickle, Eq, Show) *)

(* 6. recursive types *)
type intseq = INil | ICons of int * intseq
  deriving (Pickle, Eq, Show, Typeable, Shelve)

type 'a seq = Nil | Cons of 'a * 'a seq
  deriving (Pickle, Eq, Show, Functor, Typeable, Shelve)

(* 7. applied type constructors (nullary, n-ary) *)
type uses_seqs = (intseq * float seq) 
    deriving (Pickle, Eq, Show, Typeable, Shelve)

(* 8. polymorphic recursion (should fail) *)
type 'a nested = NNil | NCons of 'a * ('a * 'a ) nested
  (*deriving (Pickle, Eq, Show, Functor)*)

(* 9. object and class types *)
type obj = < x : int >
    (*deriving (Pickle, Eq, Show, Typeable, Shelve)*)

(* 10. class types *)
class c = object end
  (* deriving (Pickle, Eq, Show, Typeable, Shelve) *)

(* 11. polymorphic variants (nullary, unary tags, extending complex type expressions, defined inline) *)
type poly0 = [`T0 | `T1 | `T2 | `T3]
    deriving (Enum, Bounded)

type poly1 = [`T0 | `T1 of int]
    deriving (Pickle, Eq, Show)

type poly2 = P of int * [`T0 | `T1 of int] * float
    deriving (Pickle, Eq, Show)

(* 12. `as'-recursion *)
type poly3 = [`Nil | `Cons of int * 'c] as 'c
    deriving (Pickle, Eq, Show) 
type poly3b = int * ([`Nil | `Cons of int * 'c] as 'c) * [`F]
    deriving (Pickle, Eq, Show) 

(* 13. <, >, =, > < polymorphic variants *)
type poly4 = private [< `A]
    (* deriving (Pickle, Eq, Show) *)

type poly5 = private [> `A]
    (* deriving (Pickle, Eq, Show, Typeable, Shelve) *)

(*type poly6 = [< `A > `B]*)
    (* deriving (Pickle, Eq, Show, Typeable, Shelve) *)

type 'a poly7 = Foo of [`F of 'a]
and 'a poly8 = { x : [`G of [`H of [`I of 'a poly7]]] }
    deriving (Pickle, Eq, Show, Functor)

(*
type poly9 = [`F | [`G]]
    deriving (Pickle, Eq, Show)
  currently broken.

*)

(* 14. mutually recursive types (monomorphic, polymorphic) *)
type mutrec_a = mutrec_c
and mutrec_b = { l1 : mutrec_c ; l2 : mutrec_a }
and mutrec_c = S of int * mutrec_a
and mutrec_d = [`T of mutrec_b]
    deriving (Pickle, Eq, Show)

type ('a,'b) pmutrec_a = ('a,'b) pmutrec_c
and ('a,'b) pmutrec_b = { l1 : ('a,'b) pmutrec_c ; l2 : ('a,'b) pmutrec_a }
and ('a,'b) pmutrec_c = S of 'a * ('a,'b) pmutrec_a * 'b
and ('a,'b) pmutrec_d = [`T of ('a,'b) pmutrec_b]
    deriving (Pickle, Eq, Show, Functor)

(* 15. polymorphic types *)
type 'a ff1 = F of 'a * 'a | G of int deriving (Show, Eq, Pickle, Functor, Typeable, Shelve)
type ('a,'b) ff2 = F1 of ('a,'b) ff2 | F2 of 'a seq * int * 'b option
  deriving (Pickle, Eq, Show, Functor, Typeable, Shelve)

(* 16. tuples *)
type tup0 = unit
    deriving (Pickle, Eq, Show, Typeable, Shelve)
type tup2 = int * float
    deriving (Pickle, Eq, Show, Typeable, Shelve)
type tup3 = int * float * bool
    deriving (Pickle, Eq, Show, Typeable, Shelve)

(* 17. underscore (?) *)
(* TODO *)

(* 18. type constraints *)
(* TODO *)

(* 19. type equations *)
(* TODO *)

(* 20. references *)
type withref = WR of int * (int ref)
  deriving (Pickle, Eq, Show, Typeable(*, Shelve*))

