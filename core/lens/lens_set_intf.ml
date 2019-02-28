open Utility

module Seq = Lens_seq

module type S = sig
  include Set.S

  val of_seq : elt Seq.t -> t
  val to_seq : t -> elt Seq.t
end
