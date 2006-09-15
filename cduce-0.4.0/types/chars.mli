(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

module V : sig
  include Custom.T

  val mk_int: int -> t
  val mk_char: char -> t
  val to_int: t -> int
  val to_char: t -> char
  val print : Format.formatter -> t -> unit
  val print_in_string : Format.formatter -> t -> unit
end

include Custom.T
val print : t -> (Format.formatter -> unit) list

val empty : t
val any   : t
val cup   : t -> t -> t
val cap   : t -> t -> t
val diff  : t -> t -> t
val char_class  : V.t-> V.t-> t
val atom  : V.t-> t
val mk_classes : (int * int) list -> t

val disjoint : t -> t -> bool
val is_empty : t -> bool
val contains : V.t-> t -> bool
val sample : t -> V.t
val is_char : t -> V.t option
val single : t -> V.t


type 'a map
val mk_map: (t * 'a) list -> 'a map
val get_map: V.t-> 'a map -> 'a
