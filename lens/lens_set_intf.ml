module Seq = Lens_seq

module type S = sig
  include Set.S

  val pp : t Lens_format.fmt_fn

  val show : t -> string

  val of_seq : elt Seq.t -> t

  val to_seq : t -> elt Seq.t
end
