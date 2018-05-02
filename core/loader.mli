type envs = Var.var Env.String.t * Types.typing_environment

type program = Ir.binding list * Ir.computation * Types.datatype

(* Filename of an external dependency *)
type ext_dep = string

type source = {
  envs: envs;
  program: program;
  external_dependencies: ext_dep list
}

val read_file_source : envs -> string -> source
val load_file : envs -> string -> source

val print : string -> unit
