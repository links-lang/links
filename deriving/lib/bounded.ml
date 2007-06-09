(** Bounded **)
module type Bounded = sig
  type a
  val minBound : a
  val maxBound : a
end
