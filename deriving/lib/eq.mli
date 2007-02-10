(* A module for SML-style equality, i.e. where equality of mutables is
   an identity test and equality of immutables is structural equality.
   (Leibniz equality, I think.)
*)

module type Eq =
sig
  type a
  val eq : a -> a -> bool
end

module Eq_defaults (E : Eq) : Eq with type a = E.a
  

module Eq_immutable (S : sig type a end) : Eq with type a = S.a
module Eq_mutable (S : sig type a end) : Eq with type a = S.a

module Eq_int            : Eq with type a = int
module Eq_num            : Eq with type a = Num.num
module Eq_bool           : Eq with type a = bool
module Eq_float          : Eq with type a = float
module Eq_unit           : Eq with type a = unit
module Eq_char           : Eq with type a = char
module Eq_string         : Eq with type a = string
module Eq_ref (E : Eq)   : Eq with type a = E.a ref
module Eq_array (E : Eq) : Eq with type a = E.a array
module Eq_list (E : Eq)  : Eq with type a = E.a list

module Eq_2 (E1 : Eq) (E2 : Eq) :
  Eq with type a = E1.a * E2.a
module Eq_3 (E1 : Eq) (E2 : Eq) (E3 : Eq) :
  Eq with type a = E1.a * E2.a * E3.a
module Eq_4 (E1 : Eq) (E2 : Eq) (E3 : Eq) (E4 : Eq) :
  Eq with type a = E1.a * E2.a * E3.a * E4.a
module Eq_5 (E1 : Eq) (E2 : Eq) (E3 : Eq) (E4 : Eq) (E5 : Eq) :
  Eq with type a = E1.a * E2.a * E3.a * E4.a * E5.a
module Eq_6 (E1 : Eq) (E2 : Eq) (E3 : Eq) (E4 : Eq) (E5 : Eq) (E6 : Eq) :
  Eq with type a = E1.a * E2.a * E3.a * E4.a * E5.a * E6.a
module Eq_7 (E1 : Eq) (E2 : Eq) (E3 : Eq) (E4 : Eq) (E5 : Eq) (E6 : Eq) (E7 : Eq) :
  Eq with type a = E1.a * E2.a * E3.a * E4.a * E5.a * E6.a * E7.a
module Eq_8 (E1 : Eq) (E2 : Eq) (E3 : Eq) (E4 : Eq) (E5 : Eq) (E6 : Eq) (E7 : Eq) (E8 : Eq) :
  Eq with type a = E1.a * E2.a * E3.a * E4.a * E5.a * E6.a * E7.a * E8.a
module Eq_9 (E1 : Eq) (E2 : Eq) (E3 : Eq) (E4 : Eq) (E5 : Eq) (E6 : Eq) (E7 : Eq) (E8 : Eq) (E9 : Eq) :
  Eq with type a = E1.a * E2.a * E3.a * E4.a * E5.a * E6.a * E7.a * E8.a * E9.a
