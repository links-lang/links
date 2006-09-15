(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Encodings

module V : sig
  include Custom.T
  type value = Ns.QName.t
(*  include Upool.S with type value = Ns.QName.t  *)
  val mk: value -> t
  val value: t -> value
  val print_quote: Format.formatter -> t -> unit
  val mk_ascii: string -> t
  val get_ascii: t -> string
  val print: Format.formatter -> t -> unit
  val to_string: t -> string
end

include Custom.T
val print : t -> (Format.formatter -> unit) list

val empty : t
val any   : t

val cup : t -> t -> t
val cap : t -> t -> t
val diff : t -> t -> t
val atom : V.t -> t
val any_in_ns : Ns.Uri.t -> t

val contains : V.t -> t -> bool
val disjoint : t -> t -> bool
val is_empty : t -> bool
val print_tag : t -> (Format.formatter -> unit) option

val single : t -> V.t

type 'a map
val mk_map: (t * 'a) list -> 'a map
val get_map: V.t -> 'a map -> 'a

