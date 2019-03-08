type t = Lens of Lens_sort.t [@@deriving show]

val sort : t -> Lens_sort.t

val equal : t -> t -> bool
