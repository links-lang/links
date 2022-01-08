module type S = sig
  val count : unit -> int

  val insert : Lens.Phrase.Value.t list -> unit

  val insert_ints : int list list -> unit

  val drop : unit -> unit

  val drop_if_cleanup : unit -> unit

  val drop_if_exists : unit -> unit

  val set_lens : Lens.Value.t -> unit

  val lens : unit -> Lens.Value.t
end
