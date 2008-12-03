type id
type repr
type output_state
  
include Monad.Monad_state_type with type state = output_state
val repr_of_string : string -> repr
val make_repr : ?constructor:int -> id list -> repr
val allocate_store_return : Typeable.dynamic -> Dynmap.DynMap.comparator -> repr -> id m

val doShelveB : id m -> Buffer.t -> unit
val doShelveS : id m -> string

