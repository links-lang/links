type t = string
  [@@deriving show]

module Map : sig
  include Lens_map.S with type key = t
end

module Set : sig
  include Lens_set.S with type elt = t

  val pp_pretty : Format.formatter -> t -> unit
end
