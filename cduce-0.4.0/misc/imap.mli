(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(** Compact maps from integers to values. **)
type 'a t

val empty: 'a t

val create: (int * 'a) array -> 'a t
  (** The integer keys must be pairwise disjoint. *)

val create_default: 'a -> (int * 'a) array -> 'a t
  (** Same as [create] but add necessary bindings so that
      [find_lower] returns a default value when the key is not
      in the set of the bindings which are provided. *)

val merge: 'a t -> 'a t -> 'a t
  (** Merge two maps, with a priority to the second one in case of conflict.
      Complexity linear in the size of the result. *)

val find: 'a t -> int -> 'a
  (** Find the value associated to a key, or raise [Not_found]. *)

val find_default: 'a t -> 'a -> int -> 'a
  (** Find the value associated to a key, or return a default value. *)

val find_lower: 'a t -> int -> 'a
  (** Find the value associated to the largest key smaller than
      or equal to the integer. It is assumed that such a key exists. *)

val find: 'a t -> int -> 'a
  (** Find the value associated to a key, or raise [Not_found]. *)

val cardinal: 'a t -> int
  (** Number of keys in the map. *)

val elements: 'a t -> (int * 'a) list

val map: ('a -> 'b) -> 'a t -> 'b t

val map_elements: (int -> 'a -> 'b) -> 'a t -> 'b list

val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int

val hash: ('a -> int) -> 'a t -> int

val remove: 'a t -> int -> 'a t

val iter: (int -> 'a -> unit) -> 'a t -> unit
