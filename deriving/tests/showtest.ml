(*pp derivingpp *)
(* Test for show instance generation *)

(* 1. sums (nullary, unary, and n-ary) *)
type sum = S0 | S1 of int | S2 of int * float | S3 of int * float * bool | Sunit of unit | Stup of (int * float) | Stup1 of (int)
  deriving (Show)

(* 2. records with mutable and immutable fields (and various combinations) *)
type r1 = {
  l1 : int;
  l2 : int;
} deriving (Show)

type r2 = {
  mutable l1 : int;
  mutable l2 : int;
} deriving (Show)

type r3 = {
  l1 : int;
  mutable l2 : int;
} deriving (Show)

(* 3. polymorphic records *)
type r4 = {
  l1 : 'a . 'a list
} (* deriving (Show) *)

(* 4. label types *)
type label = x:int -> int
  (*  deriving (Show) *)

(* 5. function types  *)
type funct = int -> int
  (* deriving (Show) *)

(* 6. recursive types *)
type intseq = INil | ICons of int * intseq
  deriving (Show)

type 'a seq = Nil | Cons of 'a * 'a seq
  deriving (Show)

(* 7. applied type constructors (nullary, n-ary) *)
type uses_seqs = (intseq * float seq) 
    deriving (Show)

(* 8. polymorphic recursion (should fail) *)
type 'a nested = NNil | NCons of 'a * ('a * 'a ) nested
  (*deriving (Show)*)

(* 9. object and class types *)
type obj = < x : int >
    (*deriving (Show)*)

(* 10. class types *)
class c = object end
  (* deriving (Show) *)

(* 11. polymorphic variants (nullary, unary tags, extending complex type expressions, defined inline) *)
type poly1 = [`T0 | `T1 of int]
    deriving (Show)

type poly2 = P of int * [`T0 | `T1 of int] * float
    deriving (Show)

(* 12. `as'-recursion *)
type poly3 = [`Nil | `Cons of int * 'c] as 'c
    (*deriving (Show) TODO! *) 

(* 13. <, >, =, > < polymorphic variants *)
type poly4 = private [< `A]
    (* deriving (Show) *)

type poly5 = private [> `A]
    (* deriving (Show) *)

(*type poly6 = [< `A > `B]*)
    (* deriving (Show) *)

(* 14. mutually recursive types (monomorphic, polymorphic) *)
type mutrec_a = mutrec_c
and mutrec_b = { l1 : mutrec_c ; l2 : mutrec_a }
and mutrec_c = S of int * mutrec_a
and mutrec_d = [`T of mutrec_b]
    deriving (Show)

type ('a,'b) pmutrec_a = ('a,'b) pmutrec_c
and ('a,'b) pmutrec_b = { l1 : ('a,'b) pmutrec_c ; l2 : ('a,'b) pmutrec_a }
and ('a,'b) pmutrec_c = S of 'a * ('a,'b) pmutrec_a * 'b
and ('a,'b) pmutrec_d = [`T of ('a,'b) pmutrec_b]
    deriving (Show)

(* 15. polymorphic types *)
(* TODO *)

(* 16. tuples *)
type tup0 = unit
    deriving (Show)
type tup2 = int * float
    deriving (Show)
type tup3 = int * float * bool
    deriving (Show)

(* 17. underscore (?) *)
(* TODO *)

(* 18. type constraints *)
(* TODO *)

(* 19. type equations *)
(* TODO *)

(* 20. references *)
type withref = WR of int * (int ref)
  deriving (Show)
