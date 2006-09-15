(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

type t

val create: bool -> t
val ppf: t -> Format.formatter
val get: t -> string
val mark: t -> string -> unit
val markup: t -> string -> (Format.formatter -> unit) -> unit
val is_html: t -> bool

