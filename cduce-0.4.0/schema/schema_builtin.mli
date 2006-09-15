(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Encodings
open Schema_types

exception Error of string
type t

val is: Ns.QName.t -> bool
val get: Ns.QName.t -> t
val iter: (Ns.QName.t -> t -> unit) -> unit
val of_st: simple_type_definition -> t

val simple_type: t -> simple_type_definition
val cd_type: t -> Types.t
val validate: t -> Utf8.t -> Value.t

val string_of_time_type: (Ns.Label.t * Value.t) list -> Utf8.t

val any_simple_type: t
val string: t
