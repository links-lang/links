(* JavaScript code generation *)

val generate_program_defs : 
  Syntax.program -> Utility.StringSet.t -> string list

val generate_program : ?onload:string -> 
  Syntax.program -> string

val make_boiler_page : 
  ?onload:string -> 
  ?body:string ->
  string list -> string

val script_tag : ?base:string -> string -> string

val test : unit -> unit 
val run_tests : unit -> unit 
