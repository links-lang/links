(*pp deriving *)
(** Primitive instanecs for bounded **)
module Bounded = struct 

type 'a bounded = {
  min_bound : 'a ;
  max_bound : 'a 
}

module Bounded_integer(B : sig type t
                               val max_int : t
                               val min_int : t
                       end) =
struct 
  let bounded = { min_bound = B.min_int ;
                  max_bound = B.max_int }
end
let bounded_int32 = let module B = Bounded_integer(Int32) in B.bounded
let bounded_int64 = let module B = Bounded_integer(Int64) in B.bounded
let bounded_nativeint = let module B = Bounded_integer(Nativeint) in B.bounded
let bounded_int = {
  max_bound = Pervasives.max_int ;
  min_bound = Pervasives.min_int ;
}

let bounded_bool = {
  min_bound = false ;
  max_bound = true 
}
let bounded_char = {
  min_bound = (Char.chr 0) ;
  max_bound = (Char.chr 0xff) (* Is this guaranteed? *)
}

let bounded_unit = {
  min_bound = () ;
  max_bound = ()
}
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
