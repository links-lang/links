open Sexplib

type t = string [@@deriving show]

include module type of String with type t := t

module Set : Lens_set.S with type elt = string

module Map : sig
  include Lens_map.S with type key = string

  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t

  val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
end
