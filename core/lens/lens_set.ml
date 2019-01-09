open Utility

module type OrderedType = OrderedShow
module type S = Lens_set_intf.S
module Make (Ord : OrderedShow) = struct
  include Set.Make (Ord : OrderedType)

  module Seq = Lens_seq

  let of_seq s = Seq.fold_left (fun set v -> add v set) empty s
  let to_seq s = elements s |> Lens_list.to_seq
end
