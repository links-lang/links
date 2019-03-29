type t = Lens of Sort.t [@@deriving show]

val sort : t -> Sort.t

val equal : t -> t -> bool
