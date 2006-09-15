(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

type t = (Value.t * string) list
exception Failed of t


val print: Format.formatter -> t -> unit
val to_string: t -> string

val simplify: t -> t
val explain: Auto_pat.state -> Value.t -> t option
val check: Auto_pat.state -> Value.t -> unit
  (** Same, but raise [Failed]. *)

val do_check: Auto_pat.state -> Value.t -> Value.t
  (** Same, but raise a CDuce exception. *)

val check_failure: Auto_pat.state -> Value.t -> Value.t
  (** Raise [Failure ...]. *)
