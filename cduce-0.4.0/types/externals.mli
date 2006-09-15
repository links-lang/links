(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

type ext_info

val get: unit -> ext_info option

val register: ref (bool -> string -> Types.Node.t list -> (int * Types.t))
val ext_info: ref (unit -> ext_info)

val resolve: string -> Types.Node.t list -> (int * Types.t)
val typ: string -> Types.Node.t list -> Types.t
