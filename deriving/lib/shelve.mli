type id

(* representation of values of user-defined types *)
module Repr : sig
  type t
  val make : ?constructor:int -> id list -> t
end 

(* Utilities for serialization *)
module Write : sig
  type s
  include Monad.Monad_state_type with type state = s
  module Utils (T : Typeable.Typeable) (E : Eq.Eq with type a = T.a) : sig
    val allocate : T.a -> (id -> unit m) -> id m
    val store_repr : id -> Repr.t -> unit m
  end
end

(* Utilities for deserialization *)
module Read : sig
  type s
  include Monad.Monad_state_type with type state = s
  module Utils (T : Typeable.Typeable) : sig
    val sum    : (int * id list -> T.a m)  -> (id -> T.a m)
    val tuple  : (id list -> T.a m)        -> (id -> T.a m)
    val record : (T.a -> id list -> T.a m) -> int -> (id -> T.a m)
  end
end

exception UnshelvingError of string
exception UnknownTag of int * string

module type Shelve =
sig
  type a
  module T : Typeable.Typeable with type a = a
  module E : Eq.Eq with type a = a
  val shelve : a -> id Write.m
  val unshelve : id -> a Read.m
  val shelveS : a -> string
  val unshelveS : string -> a
end

module Shelve_defaults
  (S : sig
     type a
     module T : Typeable.Typeable with type a = a
     module E : Eq.Eq with type a = a
     val shelve : a -> id Write.m
     val unshelve : id -> a Read.m
   end) : Shelve with type a = S.a

module Shelve_unit  : Shelve with type a = unit
module Shelve_bool  : Shelve with type a = bool
module Shelve_int   : Shelve with type a = int
module Shelve_char  : Shelve with type a = char
module Shelve_float : Shelve with type a = float
module Shelve_num   : Shelve with type a = Num.num
module Shelve_string : Shelve with type a = string
module Shelve_option (V0 : Shelve) : Shelve with type a = V0.a option
module Shelve_list (V0 : Shelve)  : Shelve with type a = V0.a list
module Shelve_ref (S : Shelve) : Shelve with type a = S.a ref

module Shelve_from_pickle
  (P : Pickle.Pickle)
  (E : Eq.Eq with type a = P.a)
  (T : Typeable.Typeable with type a = P.a)
  : Shelve with type a = P.a
