(* Finite map : dynamic |-> t *)

open Typeable

module Comp (T : Typeable) (E : Eq.Eq with type a = T.a) :
sig
  type a = T.a
  val eq : dynamic -> dynamic -> bool
end

module DynMap :
sig
  type comparator = dynamic -> dynamic -> bool
  type 'a t
   val empty : 'a t
   val is_empty : 'a t -> bool
   val add : dynamic -> 'a -> comparator -> 'a t -> 'a t 
   val mem : dynamic -> 'a t -> bool 
   val find : dynamic -> 'a t -> 'a option
(*   val find : dynamic -> t -> dynamic *)
(*   val remove : key -> t -> t *)
   val iter : (dynamic -> 'a -> unit) -> 'a t -> unit
(*   val fold : (key -> dynamic -> 'b -> 'b) -> t -> 'b -> 'b *)
(*   val compare : (dynamic -> dynamic -> int) -> t -> t -> int *)
(*   val equal : (dynamic -> dynamic -> bool) -> t -> t -> bool *)
end
