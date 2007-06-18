module type Pickle =
  sig
    type a
    val pickle : Buffer.t -> a -> unit
    val unpickle : char Stream.t -> a
    val pickleS : a -> string
    val unpickleS : string -> a
  end

module Defaults
  (P : sig
     type a
     val pickle : Buffer.t -> a -> unit
     val unpickle : char Stream.t -> a
   end) : Pickle with type a = P.a

exception Unpickling_failure of string

module Pickle_int32     : Pickle with type a = Int32.t
module Pickle_int64     : Pickle with type a = Int64.t
module Pickle_nativeint : Pickle with type a = Nativeint.t
module Pickle_int       : Pickle with type a = int
module Pickle_char      : Pickle with type a = char
module Pickle_string    : Pickle with type a = string
module Pickle_float     : Pickle with type a = float
module Pickle_num       : Pickle with type a = Num.num
module Pickle_bool      : Pickle with type a = bool
module Pickle_unit      : Pickle with type a = unit
module Pickle_list   (P : Pickle) : Pickle with type a = P.a list
module Pickle_option (P : Pickle) : Pickle with type a = P.a option

module Pickle_unpicklable (P : sig type a val tname : string end) 
  : Pickle with type a = P.a
module Pickle_via_marshal (P : sig type a end) 
  : Pickle with type a = P.a
