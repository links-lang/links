(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

module V : sig
  include Custom.T
  val print : Format.formatter -> t -> unit
  val mk: string -> t
  val from_int: int -> t
  val from_bigint: Big_int.big_int -> t
  val to_string: t -> string

  val is_int: t -> bool
  val get_int: t -> int
  val get_bigint: t -> Big_int.big_int
  val is_zero: t -> bool

  val add: t -> t -> t
  val mult: t -> t -> t
  val sub: t -> t -> t
  val div: t -> t -> t
  val modulo: t -> t -> t
  val succ: t -> t
  val pred: t -> t
  val negat: t -> t

  val lt: t -> t -> bool
  val gt: t -> t -> bool

  val zero : t
  val one : t
  val minus_one : t

  val from_int32: Int32.t -> t
  val from_int64: Int64.t -> t
  val to_int32: t -> Int32.t
  val to_int64: t -> Int64.t
end


include Custom.T
val print : t -> (Format.formatter -> unit) list

val empty : t
val any   : t
val cup   : t -> t -> t
val cap   : t -> t -> t
val diff  : t -> t -> t

  (** closed interval *)
val bounded  : V.t -> V.t -> t

  (** left opened interval (i.e. * -- something ) *)
val left : V.t -> t

  (** right opened interval (i.e. something -- * ) *)
val right : V.t -> t

val atom : V.t -> t

val disjoint : t -> t -> bool
val is_empty : t -> bool
val contains : V.t -> t -> bool
val sample : t -> V.t
val single : t -> V.t
  (* raise Not_found if empty.
     raise Exit if empty and not singleton *)


val add : t -> t -> t
val mul : t -> t -> t
val sub : t -> t -> t
val div : t -> t -> t
val modulo : t -> t -> t
val negat : t -> t


val int32: t
val int64: t
