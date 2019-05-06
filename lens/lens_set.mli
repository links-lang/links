module type OrderedShow = sig
  type t [@@deriving show]

  include Set.OrderedType with type t := t
end

module type S = Lens_set_intf.S

module Make (Ord : OrderedShow) : S with type elt = Ord.t
