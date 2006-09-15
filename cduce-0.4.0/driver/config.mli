(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

val init_all: unit -> unit
val register: string -> string -> (unit -> unit) -> unit
val descrs: unit -> (string * string) list
val inhibit: string -> unit


(* Last registered features are initialized last (and thus take priority) *)
