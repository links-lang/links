(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Ident

exception InconsistentCrc of U.t
exception InvalidObject of string
exception CannotOpen of string
exception NoImplementation of U.t

val name: Compunit.t -> U.t
val run_loaded: bool ref

val obj_path: string list ref

val compile_save: bool -> U.t -> string -> string -> unit
val compile_run: bool -> U.t -> string -> unit
val load_run: U.t -> unit
val run: Compunit.t -> unit

val prepare_stub: string -> unit
val ocaml_stub: string -> 
  Types.t array * (Value.t array -> unit) * 
    Value.t array *
    (unit -> unit)

val stub_ml : (string -> Typer.t -> Compile.env -> 
		 Externals.ext_info option -> (Types.t array -> string) ->
		   unit) ref


val register_static_external: string -> Value.t -> unit
val get_builtins: unit -> string list


val make_wrapper: (string -> unit) ref
