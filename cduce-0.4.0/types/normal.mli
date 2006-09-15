(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

module type S =
sig
  type t

  val any: t
  val empty: t
  val cup: t -> t -> t
  val cap: t -> t -> t
  val diff: t -> t -> t
  val is_empty: t -> bool
end

type 'a bool = ('a list * 'a list) list
module Make(X1 : S)(X2 : S) :
sig
  type t = (X1.t * X2.t) list

  val normal: t -> t
    (* normalized form: 
         (t1,t2),...,(s1,s2) ==>  t1 & s1 = 0 
         (t1,t2)  => t1 <> 0, t2 <> 0
    *)

  val boolean_normal: (X1.t * X2.t) bool -> t
    (* return a normalized form *)

  val boolean: (X1.t * X2.t) bool -> t

  val pi1: t -> X1.t
  val pi2: t -> X2.t
  val pi2_restricted: X1.t -> t -> X2.t
end
