(* A module for SML-style equality, i.e. where equality of mutables is
   physical equality and equality of immutables is structural equality.
*)

type 'a eq = {
  eq : 'a -> 'a -> bool
}

val eq : 'a eq -> 'a -> 'a -> bool

val eq_immutable : 'a eq
val eq_mutable : 'a eq

val eq_int            : int eq
val eq_num            : Num.num eq
val eq_bool           : bool eq
val eq_float          : float eq
val eq_unit           : unit eq
val eq_char           : char eq
val eq_string         : string eq
val eq_ref : 'a eq -> 'a ref eq
val eq_array : 'a eq  -> 'a array eq
val eq_list : 'a eq  -> 'a list eq
val eq_option : 'a eq -> 'a option eq

module Eq_map_s_t (M : Map.S) :
sig
  val eq : 'a eq -> 'a M.t eq
end

module Eq_set_s_t (S : Set.S) :
sig
  val eq : S.t eq
end

val eq_6 : 'a1 eq -> 'a2 eq -> 'a3 eq -> 'a4 eq -> 'a5 eq -> 'a6 eq -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) eq
val eq_5 : 'a1 eq -> 'a2 eq -> 'a3 eq -> 'a4 eq -> 'a5 eq -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5) eq
val eq_4 : 'a1 eq -> 'a2 eq -> 'a3 eq -> 'a4 eq -> ('a1 * 'a2 * 'a3 * 'a4) eq
val eq_3 : 'a1 eq -> 'a2 eq -> 'a3 eq-> ('a1 * 'a2 * 'a3) eq
val eq_2 : 'a1 eq -> 'a2 eq -> ('a1 * 'a2) eq
