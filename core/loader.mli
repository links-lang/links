

(* Filename of an external FFI dependency *)
type ext_dep = string

type source = {
  program: Sugartypes.binding;
  external_dependencies: ext_dep list
}


val load_source_file : string -> source
val load_source_files_and_dependencies : string list -> source list

val print : string -> unit
