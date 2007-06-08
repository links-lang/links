(*pp derivingpp *)
type id deriving (Show)
type repr
type output_state
  
include Monad.Monad_state_type with type state = output_state
val repr_of_string : string -> repr
val string_of_repr : repr -> string
val make_repr : ?constructor:int -> id list -> repr
val allocate_id : Typeable.dynamic -> Dynmap.DynMap.comparator -> (id * bool) m
val store_repr : id -> repr -> unit m

val doShelveB : id m -> Buffer.t -> unit
val doShelveS : id m -> string

module Input : 
sig
  type input_state
  include Monad.Monad_state_type with type state = input_state
  val ctor_repr : repr -> int option * id list
  val find_by_id : id -> (repr * Typeable.dynamic option) m
  val update_map : id -> Typeable.dynamic -> unit m
  module Whizzy (T : Typeable.Typeable) : sig
    val whizzySum : (int * id list -> T.a m) -> id -> T.a m
    val whizzyNoCtor : (id list -> T.a m) -> (id -> T.a m)
  end
end

module Do (S : sig
             type a
             val shelve : a -> id m
             val unshelve : id -> a Input.m
           end) : sig
  type a = S.a
  val doShelve : a -> string
  val doUnshelve : string -> a
end
