(*pp derivingpp *)
(* Test for pickle instance generation *)

(* 1. sums (nullary, unary, and n-ary) *)
type sum = S0 | S1 of int | S2 of int * float | S3 of int * float * bool | Sunit of unit | Stup of (int * float) | Stup1 of (int)
  deriving (Pickle, Eq, Show)

(* 2. records with mutable and immutable fields (and various combinations) *)
type r1 = {
  l1 : int;
  l2 : int;
} deriving (Pickle, Eq, Show)

type r2 = {
  mutable l1 : int;
  mutable l2 : int;
} deriving (Pickle, Eq, Show)

type r3 = {
  l1 : int;
  mutable l2 : int;
} deriving (Pickle, Eq, Show)

(* 3. polymorphic records *)
type r4 = {
  l1 : 'a . 'a list
} (* deriving (Pickle, Eq, Show) *)

(* 4. label types *)
type label = x:int -> int
  (*  deriving (Pickle, Eq, Show) *)

(* 5. function types  *)
type funct = int -> int
  (* deriving (Pickle, Eq, Show) *)

(* 6. recursive types *)
type intseq = INil | ICons of int * intseq
  deriving (Pickle, Eq, Show)

type 'a seq = Nil | Cons of 'a * 'a seq
  deriving (Pickle, Eq, Show, Functor)

(* 7. applied type constructors (nullary, n-ary) *)
type uses_seqs = (intseq * float seq) 
    deriving (Pickle, Eq, Show)

(* 8. polymorphic recursion (should fail) *)
type 'a nested = NNil | NCons of 'a * ('a * 'a ) nested
  (*deriving (Pickle, Eq, Show, Functor)*)

(* 9. object and class types *)
type obj = < x : int >
    (*deriving (Pickle, Eq, Show)*)

(* 10. class types *)
class c = object end
  (* deriving (Pickle, Eq, Show) *)

(* 11. polymorphic variants (nullary, unary tags, extending complex type expressions, defined inline) *)
type poly1 = [`T0 | `T1 of int]
    deriving (Pickle, Eq, Show)

type poly2 = P of int * [`T0 | `T1 of int] * float
    deriving (Pickle, Eq, Show)

(* 12. `as'-recursion *)
type poly3 = [`Nil | `Cons of int * 'c] as 'c
    (*deriving (Pickle, Eq, Show) TODO! *) 

(* 13. <, >, =, > < polymorphic variants *)
type poly4 = private [< `A]
    (* deriving (Pickle, Eq, Show) *)

type poly5 = private [> `A]
    (* deriving (Pickle, Eq, Show) *)

(*type poly6 = [< `A > `B]*)
    (* deriving (Pickle, Eq, Show) *)

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
type 'a ff1 = F of 'a * 'a | G of int deriving (Show, Eq, Pickle, Functor)
type ('a,'b) ff2 = F1 of ('a,'b) ff2 | F2 of 'a seq * int * 'b option
  deriving (Pickle, Eq, Show, Functor)

(* 16. tuples *)
type tup0 = unit
    deriving (Pickle, Eq, Show)
type tup2 = int * float
    deriving (Pickle, Eq, Show)
type tup3 = int * float * bool
    deriving (Pickle, Eq, Show)

(* 17. underscore (?) *)
(* TODO *)

(* 18. type constraints *)
(* TODO *)

(* 19. type equations *)
(* TODO *)

(* 20. references *)
type withref = WR of int * (int ref)
  deriving (Pickle, Eq, Show)
