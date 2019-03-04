type t = string
  [@@deriving show]

module Map : sig
  include Map.S with type key = t

  val find : 'a t -> key:key -> 'a option

  val from_alist : (key * 'a) list -> 'a t
end

module Set : sig
  include module type of Lens_utility.String.Set

  val pp_pretty : Format.formatter -> t -> unit
end
