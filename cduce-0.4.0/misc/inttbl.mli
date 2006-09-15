module type S = sig
  type key
  type 'a t

  val create: unit -> 'a t
  val fold: 'a t -> (key -> 'a -> 'b -> 'b) -> 'b -> 'b
  val clear: 'a t -> unit
  val add: 'a t -> key -> 'a -> unit
  val find: 'a t -> key -> 'a
  val mem: 'a t -> key -> bool
  val remove: 'a t -> key -> unit
end

include S with type key = int
