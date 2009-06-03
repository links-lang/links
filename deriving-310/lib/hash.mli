(* A module for hashing.
*)
type state
type 'a hash = {
  hash : 'a -> state -> state ;
  _Eq  : 'a Eq.eq ;
  (* Constraint:
     eq x y => eq (hash x) (hash y)
  *)
}

val hash : 'a hash -> ?depth:int -> 'a -> int
val eq : 'a hash -> 'a -> 'a -> bool

val hash_int    : int hash
val hash_num    : Num.num hash
val hash_bool   : bool hash
val hash_float  : float hash
val hash_unit   : unit hash
val hash_char   : char hash
val hash_string : string hash
val hash_ref    : 'a hash -> 'a ref hash
val hash_array  : 'a hash -> 'a array hash
val hash_list   : 'a hash -> 'a list hash
val hash_option : 'a hash -> 'a option hash

module Hash_map_s_t (M : Map.S) :
sig
  val hash : 'a hash -> 'a M.t hash
end

module Hash_set_s_t (S : Set.S) :
sig
  val hash : S.t hash
end

val hash_6 : 'a1 hash -> 'a2 hash -> 'a3 hash -> 'a4 hash -> 'a5 hash -> 'a6 hash -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) hash
val hash_5 : 'a1 hash -> 'a2 hash -> 'a3 hash -> 'a4 hash -> 'a5 hash -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5) hash
val hash_4 : 'a1 hash -> 'a2 hash -> 'a3 hash -> 'a4 hash -> ('a1 * 'a2 * 'a3 * 'a4) hash
val hash_3 : 'a1 hash -> 'a2 hash -> 'a3 hash-> ('a1 * 'a2 * 'a3) hash
val hash_2 : 'a1 hash -> 'a2 hash -> ('a1 * 'a2) hash
