(*pp deriving *)
(** Primitive instanecs for bounded **)
module Bounded = struct 
module type Bounded = sig
  type a
  val minBound : a
  val maxBound : a
end

module Bounded_integer(B : sig type t
                               val max_int : t
                               val min_int : t
                       end) : Bounded with type a = B.t =
struct 
  type a = B.t
  let minBound = B.min_int
  let maxBound = B.max_int
end
module Bounded_int32 = Bounded_integer(Int32)
module Bounded_int64 = Bounded_integer(Int64)
module Bounded_nativeint = Bounded_integer(Nativeint)
module Bounded_int = struct
  type a = int
  let minBound = Pervasives.min_int
  let maxBound = Pervasives.max_int
end
module Bounded_bool = struct
  type a = bool
  let minBound = false
  let maxBound = true
end
module Bounded_char = struct
  type a = char
  let minBound = Char.chr 0
  let maxBound = Char.chr 0xff (* Is this guaranteed? *)
end
module Bounded_unit = struct
  type a = unit
  let minBound = ()
  let maxBound = ()
end 
end
include Bounded
type open_flag = Pervasives.open_flag  =
                 | Open_rdonly
                 | Open_wronly
                 | Open_append
                 | Open_creat
                 | Open_trunc
                 | Open_excl
                 | Open_binary
                 | Open_text
                 | Open_nonblock
                     deriving (Bounded)

type fpclass = Pervasives.fpclass =
               | FP_normal
               | FP_subnormal
               | FP_zero
               | FP_infinite
               | FP_nan
                   deriving (Bounded)
