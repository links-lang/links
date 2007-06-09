type 'a m = 'a Shelvehelper.m
type 'a n = 'a Shelvehelper.Input.m
type id = Shelvehelper.id

exception UnshelvingError of string

module type Shelve =
sig
  type a
  module T : Typeable.Typeable with type a = a
  module E : Eq.Eq with type a = a
  val shelve : a -> id m
  val unshelve : id -> a n
  val shelveS : a -> string
  val unshelveS : string -> a
end

module Shelve_defaults
  (S : sig
     type a
     module T : Typeable.Typeable with type a = a
     module E : Eq.Eq with type a = a
     val shelve : a -> id m
     val unshelve : id -> a n
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
