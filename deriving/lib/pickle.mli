open Typeable
open Hash

type id

(* representation of values of user-defined types *)
module Repr : sig
  type t
  val make : ?constructor:int -> id list -> t
end 

type write_state
type read_state

(* Utilities for serialization *)
val allocate : 'a typeable -> 'a hash ->
                'a -> (id -> write_state -> write_state) -> write_state -> id * write_state
val store_repr : id -> Repr.t -> write_state -> write_state

(* Utilities for deserialization *)
val sum    : 'a typeable -> (int * id list -> read_state -> 'a * read_state)  -> id -> read_state -> 'a * read_state
val tuple  : 'a typeable -> (id list -> read_state -> 'a * read_state)        -> id -> read_state -> 'a * read_state
val record : 'a typeable -> ('a -> id list ->read_state -> read_state) -> int -> id -> read_state -> 'a * read_state

exception UnpicklingError of string
exception UnknownTag of int * string

type 'a pickle = {
  _Typeable : 'a typeable ;
  _Hash     : 'a hash ;
  pickle    : 'a -> write_state -> id * write_state ;
  unpickle  : id -> read_state -> 'a * read_state
}

val to_buffer    : 'a pickle -> Buffer.t -> 'a -> unit
val to_string    : 'a pickle -> 'a -> string
val to_channel   : 'a pickle -> out_channel -> 'a -> unit

val from_stream  : 'a pickle -> char Stream.t -> 'a
val from_string  : 'a pickle -> string -> 'a
val from_channel : 'a pickle -> in_channel -> 'a

val pickle_unit  : unit pickle
val pickle_bool  : bool pickle
val pickle_int   : int pickle
val pickle_char  : char pickle
val pickle_float : float pickle
val pickle_num   : Num.num pickle
val pickle_string : string pickle
val pickle_option : 'a pickle -> 'a option pickle
val pickle_list   : 'a pickle -> 'a list pickle
val pickle_ref    : 'a pickle -> 'a ref pickle

val pickle_from_dump : 'a Dump.dump -> 'a hash -> 'a typeable -> 'a pickle

val pickle_6 : 'a1 pickle -> 'a2 pickle -> 'a3 pickle -> 'a4 pickle -> 'a5 pickle -> 'a6 pickle -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) pickle
val pickle_5 : 'a1 pickle -> 'a2 pickle -> 'a3 pickle -> 'a4 pickle -> 'a5 pickle -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5) pickle
val pickle_4 : 'a1 pickle -> 'a2 pickle -> 'a3 pickle -> 'a4 pickle -> ('a1 * 'a2 * 'a3 * 'a4) pickle
val pickle_3 : 'a1 pickle -> 'a2 pickle -> 'a3 pickle -> ('a1 * 'a2 * 'a3) pickle
val pickle_2 : 'a1 pickle -> 'a2 pickle -> ('a1 * 'a2) pickle
