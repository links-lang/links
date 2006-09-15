(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(* Decompilation of regular expressions *)

type 'a regexp = 
  | Empty
  | Epsilon
  | Seq of 'a regexp * 'a regexp
  | Alt of 'a regexp * 'a regexp
  | Star of 'a regexp
  | Plus of 'a regexp
  | Trans of 'a

module type S = sig
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val hash: t -> int
end

module type TABLE = sig
  type key
  type 'a t
  val create: int -> 'a t
  val add: 'a t -> key -> 'a -> unit
  val find: 'a t -> key -> 'a
end

module Decompile(X : TABLE)(S : S)
: sig
  val decompile: (X.key -> (S.t * X.key) list * bool) -> X.key -> S.t regexp
end
