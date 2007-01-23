module type Pickle =
  sig
    type a
    val pickle : Buffer.t -> a -> unit
    val unpickle : char Stream.t -> a
    val pickleS : a -> string
    val unpickleS : string -> a
  end
module type SimplePickle =
  sig
    type a
    val pickle : Buffer.t -> a -> unit
    val unpickle : char Stream.t -> a
  end
module Pickle_defaults (P : SimplePickle) : Pickle with type a = P.a

exception Unpickling_failure of string

module Pickle_int32 : Pickle with type a = Int32.t
module Pickle_int64 : Pickle with type a = Int64.t
module Pickle_nativeint : Pickle with type a = Nativeint.t
module Pickle_int : Pickle with type a = int
module Pickle_char : Pickle with type a = char
module Pickle_string : Pickle with type a = string
module Pickle_float : Pickle with type a = float
module Pickle_num : Pickle with type a = Num.num
module Pickle_list  (P : SimplePickle) 
 : Pickle with type a = P.a list
module Pickle_ref (P : SimplePickle) 
 : Pickle with type a = P.a ref
module Pickle_option (P : SimplePickle) 
 : Pickle with type a = P.a option
module Pickle_array (P : SimplePickle) 
 : Pickle with type a = P.a array
module Pickle_bool : Pickle with type a = bool
module Pickle_unit : Pickle with type a = unit
module Pickle_0 : Pickle with type a = unit
module Pickle_1 (P1 : SimplePickle) 
 : Pickle with type a = P1.a
module Pickle_2 (P1 : SimplePickle) (P2 : SimplePickle) 
  : Pickle with type a = P1.a * P2.a
module Pickle_3 (P1 : SimplePickle) (P2 : SimplePickle) (P3 : SimplePickle) 
 : Pickle with type a = P1.a * P2.a * P3.a
module Pickle_4 (P1 : SimplePickle) (P2 : SimplePickle) (P3 : SimplePickle) (P4 : SimplePickle) 
 : Pickle with type a = P1.a * P2.a * P3.a * P4.a
module Pickle_5 (P1 : SimplePickle) (P2 : SimplePickle) (P3 : SimplePickle) (P4 : SimplePickle) (P5 : SimplePickle) 
  : Pickle with type a = P1.a * P2.a * P3.a * P4.a * P5.a
module Pickle_6 (P1 : SimplePickle) (P2 : SimplePickle) (P3 : SimplePickle) (P4 : SimplePickle) (P5 : SimplePickle) (P6 : SimplePickle) 
  : Pickle with type a = P1.a * P2.a * P3.a * P4.a * P5.a * P6.a
module Pickle_7 (P1 : SimplePickle) (P2 : SimplePickle) (P3 : SimplePickle) (P4 : SimplePickle) (P5 : SimplePickle) (P6 : SimplePickle) (P7 : SimplePickle) 
  : Pickle with type a = P1.a * P2.a * P3.a * P4.a * P5.a * P6.a * P7.a
module Pickle_8 (P1 : SimplePickle) (P2 : SimplePickle) (P3 : SimplePickle) (P4 : SimplePickle) (P5 : SimplePickle) (P6 : SimplePickle) (P7 : SimplePickle) (P8 : SimplePickle) 
  : Pickle with type a = P1.a * P2.a * P3.a * P4.a * P5.a * P6.a * P7.a * P8.a
module Pickle_9 (P1 : SimplePickle) (P2 : SimplePickle) (P3 : SimplePickle) (P4 : SimplePickle) (P5 : SimplePickle) (P6 : SimplePickle) (P7 : SimplePickle) (P8 : SimplePickle) (P9 : SimplePickle) 
  : Pickle with type a = P1.a * P2.a * P3.a * P4.a * P5.a * P6.a * P7.a *P8.a * P9.a
module Pickle_unpicklable (P : sig type a val tname : string end) 
  : Pickle with type a = P.a
module Pickle_via_marshal (P : sig type a end) 
  : Pickle with type a = P.a
