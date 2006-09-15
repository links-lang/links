(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

exception Escape of exn

val toplevel: bool ref
val verbose: bool ref

val extra_specs: (string * Arg.spec * string) list ref

val script : Format.formatter -> Format.formatter -> char Stream.t -> bool
val topinput : Format.formatter -> Format.formatter -> char Stream.t -> bool

val dump_env : Format.formatter -> unit

val compile: string -> string option -> unit
val compile_run: string -> unit
val run: string -> unit

val print_exn: Format.formatter -> exn -> unit


val eval: string -> (Atoms.V.t option * Value.t) list
  (* Can be used from CDuce units *)
