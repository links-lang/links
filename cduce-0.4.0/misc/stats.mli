(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

type verbosity = Quiet | Summary | Details
val set_verbosity: verbosity -> unit

val gettimeofday: (unit -> float) ref

val register: verbosity -> (Format.formatter -> unit) -> unit
val dump: Format.formatter -> unit

module Counter: sig
  type t
    
  val create: string -> t
  val incr: t -> unit
  val add: t -> int -> unit
  val print: Format.formatter -> t -> unit
end

module Timer: sig
  type t
    
  val create: string -> t
  val start: t -> unit
  val stop: t -> 'a -> 'a
  val print: Format.formatter -> t -> unit
end


module InOut: sig
  val enter: string -> unit
  val leave: string -> unit
  val wrap: string -> ('a -> 'b) -> 'a -> 'b
end
