(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

module type S =
sig
  include Custom.T
  type elem

  val get: t -> (elem list * elem list) list
  val get': t -> (elem list * (elem list) list) list

  val empty : t
  val full  : t
  val cup   : t -> t -> t
  val cap   : t -> t -> t
  val diff  : t -> t -> t
  val atom  : elem -> t

  val iter: (elem-> unit) -> t -> unit

  val compute: empty:'b -> full:'b -> cup:('b -> 'b -> 'b) 
    -> cap:('b -> 'b -> 'b) -> diff:('b -> 'b -> 'b) ->
    atom:(elem -> 'b) -> t -> 'b

(*
  val print: string -> (Format.formatter -> elem -> unit) -> t ->
    (Format.formatter -> unit) list
*)

  val trivially_disjoint : t -> t -> bool
end

module type MAKE = functor (X : Custom.T) -> S with type elem = X.t

module Make : MAKE

(*
module type S' = sig
  include S
  type bdd = False | True | Br of elem * t * t
  val br: t -> bdd
end

module MakeBdd(X : Custom.T) : S' with type elem = X.t

module type S'' = sig
  include S
  val dnf: (elem list -> (elem list) list -> unit) -> t -> unit
end

module Make2(X : Custom.T) : S'' with type elem = X.t
*)
