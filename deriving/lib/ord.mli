(* Total ordering on types. *)

module type Eq =
sig
  type a
  val eq : a -> a -> bool
end

module Defaults (E : Eq) : Eq with type a = E.a

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
module Eq_set_s_t  (S : Set.S) : Eq with type a = S.t

module Eq_6 (A1 : Eq) (A2 : Eq) (A3 : Eq) (A4 : Eq) (A5 : Eq) (A6 : Eq) 
  : Eq with type a = A1.a * A2.a * A3.a * A4.a * A5.a * A6.a
module Eq_5 (A1 : Eq) (A2 : Eq) (A3 : Eq) (A4 : Eq) (A5 : Eq)
  : Eq with type a = A1.a * A2.a * A3.a * A4.a * A5.a
module Eq_4 (A1 : Eq) (A2 : Eq) (A3 : Eq) (A4 : Eq)
  : Eq with type a = A1.a * A2.a * A3.a * A4.a
module Eq_3 (A1 : Eq) (A2 : Eq) (A3 : Eq)
  : Eq with type a = A1.a * A2.a * A3.a
module Eq_2 (A1 : Eq) (A2 : Eq)
  : Eq with type a = A1.a * A2.a
