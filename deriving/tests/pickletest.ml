(*pp derivingpp *)
(* Test for pickle instance generation *)

(* 1. sums (nullary, unary, and n-ary) *)
type sum = S0 | S1 of int | S2 of int * float | S3 of int * float * bool | Sunit of unit | Stup of (int * float) | Stup1 of (int)
  deriving (Pickle)

(* 2. records with mutable and immutable fields (and various combinations) *)
type r1 = {
  l1 : int;
  l2 : int;
} deriving (Pickle)

type r2 = {
  mutable l1 : int;
  mutable l2 : int;
} deriving (Pickle)

type r3 = {
  l1 : int;
  mutable l2 : int;
} deriving (Pickle)

(* 3. polymorphic records *)
type r4 = {
  l1 : 'a . 'a list
} (* deriving (Pickle) *)

(* 4. label types *)
type label = x:int -> int
  (*  deriving (Pickle) *)

(* 5. function types  *)
type funct = int -> int
  (* deriving (Pickle) *)

(* 6. recursive types *)
type intseq = INil | ICons of int * intseq
  deriving (Pickle)

type 'a seq = Nil | Cons of 'a * 'a seq
  deriving (Pickle)

(* 7. applied type constructors (nullary, n-ary) *)
type uses_seqs = (intseq * float seq) 
    deriving (Pickle)

(* 8. polymorphic recursion (should fail) *)
type 'a nested = NNil | NCons of 'a * ('a * 'a ) nested
  (*deriving (Pickle)*)

(* 9. object and class types *)
type obj = < x : int >
    (*deriving (Pickle)*)

(* 10. class types *)
class c = object end
  (* deriving (Pickle) *)

(* 11. polymorphic variants (nullary, unary tags, extending complex type expressions, defined inline) *)
type poly1 = [`T0 | `T1 of int]
    deriving (Pickle)

type poly2 = P of int * [`T0 | `T1 of int] * float
    deriving (Pickle)

(* 12. `as'-recursion *)
type poly3 = [`Nil | `Cons of int * 'c] as 'c
    (*deriving (Pickle) TODO! *) 

(* 13. <, >, =, > < polymorphic variants *)
type poly4 = private [< `A]
    (* deriving (Pickle) *)

type poly5 = private [> `A]
    (* deriving (Pickle) *)

(*type poly6 = [< `A > `B]*)
    (* deriving (Pickle) *)

(* 14. mutually recursive types (monomorphic, polymorphic) *)
type mutrec_a = mutrec_c
and mutrec_b = { l1 : mutrec_c ; l2 : mutrec_a }
and mutrec_c = S of int * mutrec_a
and mutrec_d = [`T of mutrec_b]
    deriving (Pickle)

type ('a,'b) pmutrec_a = ('a,'b) pmutrec_c
and ('a,'b) pmutrec_b = { l1 : ('a,'b) pmutrec_c ; l2 : ('a,'b) pmutrec_a }
and ('a,'b) pmutrec_c = S of 'a * ('a,'b) pmutrec_a * 'b
and ('a,'b) pmutrec_d = [`T of ('a,'b) pmutrec_b]
    deriving (Pickle)

(* 15. polymorphic types *)
(* TODO *)

(* 16. tuples *)
type tup0 = unit
    deriving (Pickle)
type tup2 = int * float
    deriving (Pickle)
type tup3 = int * float * bool
    deriving (Pickle)

(* 17. underscore (?) *)
(* TODO *)

(* 18. type constraints *)
(* TODO *)

(* 19. type equations *)
(* TODO *)

(* 20. references *)
type withref = WR of int * (int ref)
  deriving (Pickle)
