module TypeRep :
sig
  type t
  type delayed = unit -> t
  val compare : t -> t -> int
  val eq : t -> t -> bool
  val mkFresh : string -> delayed list -> delayed
  val mkTuple : delayed list -> delayed
  val mkPolyv : (string * delayed option) list -> delayed list -> delayed
end

exception CastFailure of string

type dynamic
val tagOf : dynamic -> TypeRep.t

module type Typeable =
sig
  type a
  val type_rep : unit -> TypeRep.t
  val has_type : dynamic -> bool
  val cast : dynamic -> a option
  val throwing_cast : dynamic -> a
  val make_dynamic : a -> dynamic
  val mk : a -> dynamic
end

module Defaults (T : (sig
                        type a
                        val type_rep : unit -> TypeRep.t
                      end))
  : Typeable with type a = T.a

module Typeable_list   (A : Typeable) : Typeable with type a = A.a list
module Typeable_option (A : Typeable) : Typeable with type a = A.a option
module Typeable_ref    (A : Typeable) : Typeable with type a = A.a ref

(*module Primitive_typeable (T : sig type t end): Typeable with type a = T.t *)

module Typeable_unit   : Typeable with type a = unit
module Typeable_int    : Typeable with type a = int
module Typeable_num    : Typeable with type a = Num.num
module Typeable_float  : Typeable with type a = float
module Typeable_bool   : Typeable with type a = bool
module Typeable_string : Typeable with type a = string
module Typeable_char   : Typeable with type a = char

