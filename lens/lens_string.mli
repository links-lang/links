type t = string [@@deriving show]

include module type of String with type t := t

module Set : Lens_set.S with type elt = string

module Map : Lens_map.S with type key = string
