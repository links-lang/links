module TypeRep :
sig
  type t
  type delayed = t lazy_t
  val compare : t -> t -> int
  val eq : t -> t -> bool
  val mkFresh : string -> delayed list -> delayed
  val mkTuple : delayed list -> delayed
  val mkPolyv : (string * delayed option) list -> delayed list -> delayed
end

exception CastFailure of string

type dynamic
val tagOf : dynamic -> TypeRep.t

type 'a typeable = { type_rep : TypeRep.delayed }

val has_type      : 'a typeable -> dynamic -> bool
val cast          : 'a typeable -> dynamic -> 'a option
val throwing_cast : 'a typeable -> dynamic -> 'a
val make_dynamic  : 'a typeable -> 'a -> dynamic
val mk            : 'a typeable -> 'a -> dynamic
val type_rep      : 'a typeable -> TypeRep.t

val typeable_list   : 'a typeable -> 'a list typeable
val typeable_option : 'a typeable -> 'a option typeable
val typeable_ref    : 'a typeable -> 'a ref typeable

val primitive_typeable : string -> 'a typeable

val typeable_unit   : unit typeable
val typeable_int    : int typeable
val typeable_num    : Num.num typeable
val typeable_float  : float typeable
val typeable_bool   : bool typeable
val typeable_string : string typeable
val typeable_char   : char typeable

val typeable_6 : 'a1 typeable -> 'a2 typeable -> 'a3 typeable -> 'a4 typeable -> 'a5 typeable -> 'a6 typeable -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) typeable
val typeable_5 : 'a1 typeable -> 'a2 typeable -> 'a3 typeable -> 'a4 typeable -> 'a5 typeable -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5) typeable
val typeable_4 : 'a1 typeable -> 'a2 typeable -> 'a3 typeable -> 'a4 typeable -> ('a1 * 'a2 * 'a3 * 'a4) typeable
val typeable_3 : 'a1 typeable -> 'a2 typeable -> 'a3 typeable -> ('a1 * 'a2 * 'a3) typeable
val typeable_2 : 'a1 typeable -> 'a2 typeable -> ('a1 * 'a2) typeable
