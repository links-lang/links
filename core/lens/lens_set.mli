open Utility

module type OrderedType = OrderedShow
module type S = Lens_set_intf.S
module Make (Ord : OrderedType) : S with type elt = Ord.t
