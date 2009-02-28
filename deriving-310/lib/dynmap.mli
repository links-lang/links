(* Finite map : dynamic |-> t *)

open Typeable

val comp : 'a typeable -> 'a Eq.eq -> dynamic -> dynamic -> bool

module DynMap :
sig
  type comparator = dynamic -> dynamic -> bool
  type 'a t
   val empty : 'a t
   val add : dynamic -> 'a -> comparator -> 'a t -> 'a t 
   val mem : dynamic -> 'a t -> bool 
   val find : dynamic -> 'a t -> 'a option
   val iter : (dynamic -> 'a -> unit) -> 'a t -> unit
end
