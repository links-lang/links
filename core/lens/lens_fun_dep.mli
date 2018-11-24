open Utility

module Alias = Lens_alias

type t [@@deriving show]

val make : Alias.Set.t -> Alias.Set.t -> t

(** Get the left side of a functional dependency (i.e. left of X -> Y returns X)*)
val left : t -> Alias.Set.t

(** Get the right side of a functional dependency (i.e. left of X -> Y returns Y)*)
val right : t -> Alias.Set.t

(** Convert two sets of alias lists into a functional dependency *)
val of_lists : Alias.t list * Alias.t list -> t

(** Construct a single functional dependency from a set of columns and a key *)
val key_fd : keys:Alias.t list -> cols:Alias.t list -> t

module Compare : sig
  type elt = t [@@deriving show]
  type t = elt [@@deriving show]

  val compare : t -> t -> int
end

module Set : sig
  include Set.S with type elt = t

  (** Construct a set of functional dependencies of lists of lists of functional dependencies *)
  val of_lists : (Alias.t list * Alias.t list) list -> t

  (** Remove all functional dependencies where the left side is contained by [cols] *)
  val remove_defines : t -> cols:Alias.Set.t -> t

  (** Generate a single functional dependency as a set from the given keys and columns *)
  val key_fds :  keys:Alias.t list -> cols:Alias.t list -> t

  (** Get a root functional dependency *)
  val root_fd : t -> elt option

  (** Get the transitive closure of a functional dependency *)
  val transitive_closure : t -> cols:Alias.Set.t -> Alias.Set.t
end

module Tree : sig
  type elt = Alias.Set.t [@@deriving show]
  type t = | FDNode of elt * (t list) [@@deriving show]

  val pp_pretty : Format.formatter -> t -> unit

  val of_fds : Set.t -> t option
end
