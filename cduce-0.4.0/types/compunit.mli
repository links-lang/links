(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

type t

val compare: t -> t -> int
val hash: t -> int
val equal: t -> t -> bool

val pervasives: t

val enter: unit -> unit
val current: unit -> t
val leave: unit -> unit

val set_hash: t -> int -> int -> unit
val get_hash: t -> int * int


val wrap: ('a -> 'b) -> ('a -> 'b)


(* Detect collision of descriptors *)

val register: t -> string -> unit
