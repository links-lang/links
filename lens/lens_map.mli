module type OrderedShow = sig
  type t [@@deriving show]

  include Map.OrderedType with type t := t
end

module type S = Lens_map_intf.S

module Make (Ord : OrderedShow) : S with type key = Ord.t
