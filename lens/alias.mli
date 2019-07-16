open Lens_utility

type t = string [@@deriving show, eq]

module Map : sig
  include Lens_map.S with type key = t
end

module Set : sig
  include Lens_set.S with type elt = t

  module Set : sig
    include Lens_set.S with type elt = t

    val is_disjoint : t -> (unit, elt) result
  end

  module List : sig
    type elt = t

    type t = elt list

    val is_disjoint : t -> (unit, elt) result
  end

  val pp_pretty : Format.formatter -> t -> unit
end
