type envs = Var.var Env.String.t * Types.typing_environment
type program = Ir.binding list * Ir.computation * Types.datatype

val read_file_source : envs -> string -> envs * program
val load_file : envs -> string -> envs * program

val print_cache : string -> unit
val precompile_cache : envs -> string -> unit

val cache : string -> string -> (unit -> 'a) -> 'a

(* Globally accessible root filename of the whole program cache *)

(* Caching function for whole-program caching *)
val activate_wpcache : string -> unit
val wpcache : string -> (unit -> 'a) -> 'a
