(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

type 'a typed_int
external int: 'a typed_int -> int = "%identity"

module type S = sig
  type token
  type value
  include Custom.T with type t = token typed_int
  exception Not_unique of value * value

  val dummy: t
  val min: t -> t -> t
  val mk: value -> t
  val value: t -> value

  val extract: unit -> value array
  val intract: value array -> unit

  val from_int: int -> t
end

module Make(X : Custom.T) : S with type value = X.t
