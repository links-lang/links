module Tag : 
sig
  type tag
  val fresh : unit -> tag
end

type typeRep = TypeRep of (Tag.tag * typeRep list)

module TypeRep : Map.OrderedType

type dynamic

module type Typeable =
sig
  type a
  val typeRep : typeRep
  val hasType : dynamic -> bool
  val cast : dynamic -> a option
  val makeDynamic : a -> dynamic
end

module Typeable_defaults (T : (sig
                                 type a
                                 val typeRep : typeRep
                               end))
  : Typeable with type a = T.a

module Typeable_unit : Typeable with type a = unit
module Typeable_2 (S1:Typeable)(S2:Typeable)
  : Typeable with type a = S1.a * S2.a
module Typeable_3 (S1:Typeable)(S2:Typeable)(S3:Typeable)
  : Typeable with type a = S1.a * S2.a * S3.a
module Typeable_4 (S1:Typeable)(S2:Typeable)(S3:Typeable)(S4:Typeable)
  : Typeable with type a = S1.a * S2.a * S3.a * S4.a
module Typeable_5 (S1:Typeable)(S2:Typeable)(S3:Typeable)(S4:Typeable)(S5:Typeable)
  : Typeable with type a = S1.a * S2.a * S3.a * S4.a * S5.a
module Typeable_6 (S1:Typeable)(S2:Typeable)(S3:Typeable)(S4:Typeable)(S5:Typeable)(S6:Typeable)
  : Typeable with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a
module Typeable_7 (S1:Typeable)(S2:Typeable)(S3:Typeable)(S4:Typeable)(S5:Typeable)(S6:Typeable)(S7:Typeable)
  : Typeable with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a
module Typeable_8 (S1:Typeable)(S2:Typeable)(S3:Typeable)(S4:Typeable)(S5:Typeable)(S6:Typeable)(S7:Typeable)(S8 :Typeable)
  : Typeable with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a * S8.a
module Typeable_9 (S1:Typeable)(S2:Typeable)(S3:Typeable)(S4:Typeable)(S5:Typeable)(S6:Typeable)(S7:Typeable)(S8 :Typeable)(S9:Typeable) 
  : Typeable with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a * S8.a * S9.a

module Typeable_list (A:Typeable) : Typeable with type a = A.a list

module Primitive_typeable (T : sig type t end): Typeable with type a = T.t 

module Typeable_int : Typeable with type a = int
module Typeable_float : Typeable with type a = int
module Typeable_bool : Typeable with type a = int
module Typeable_string : Typeable with type a = int
