[@@@ocamlformat "doc-comments=before"]

open Lens_utility

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

module Check_error : sig
  type t =
    | UnboundColumns of Alias.Set.t
        (** Error thrown when there are references to columns
            in functional dependencies which don't exist. *)
    | ProbablyCycle of Alias.Set.t
        (** Error thrown when the algorithm assumes that some
            columns have not been included because there is
            some cycle with them. *)
    | FunDepNotTreeForm of Alias.Set.t
        (** Error thrown when *)
  [@@deriving show]
end

module Remove_defines_error : sig
  type t =
    | DefiningFDNotFound of Alias.Set.t
  [@@deriving show]
end

module Compare : sig
  type elt = t [@@deriving show]

  type t = elt [@@deriving show]

  val compare : t -> t -> int
end

module Set : sig
  include Set.S with type elt = t

  val pp_pretty : t Format.fmt_fn

  val show_pretty : t -> string

  (** Construct a set of functional dependencies of lists of lists of functional dependencies *)
  val of_lists : (Alias.t list * Alias.t list) list -> t

  (** Remove all functional dependencies where the left side is contained by [cols] *)
  val remove_defines : t -> cols:Alias.Set.t -> (t, Remove_defines_error.t) result

  (** Generate a single functional dependency as a set from the given keys and columns *)
  val key_fds : keys:Alias.t list -> cols:Alias.t list -> t

  (** Get a root functional dependency *)
  val root_fds : t -> elt list

  (** Get the functional dependency that defines the columns [cols] *)
  val defining_fd : t -> cols:Alias.Set.t -> elt

  (** Get the transitive closure of a functional dependency *)
  val transitive_closure : t -> cols:Alias.Set.t -> Alias.Set.t

  val checked_fds_of_lists :
       (Alias.t list * Alias.t list) list
    -> columns:Alias.Set.t
    -> (t, Check_error.t) result

  (** Get all columns which are defined by other columns. *)
  val outputs : t -> Alias.Set.t
end

module Tree : sig
  type elt = Alias.Set.t [@@deriving show]

  type node = FDNode of elt * t
  and t = node list [@@deriving show]

  val pp_pretty : Format.formatter -> t -> unit

  val show_pretty : t -> string

  val of_fds : Set.t -> columns:Alias.Set.t -> (t, Check_error.t) result
end
