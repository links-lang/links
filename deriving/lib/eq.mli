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
module Eq_option (E : Eq): Eq with type a = E.a option
module Eq_map_s_t (E : Eq) (M : Map.S) : Eq with type a = E.a M.t
