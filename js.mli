(* JavaScript code generation *)

val generate_program_defs : 
  Syntax.expression list -> string list -> string list

val generate_program : ?onload:string -> 
  Syntax.expression list -> Syntax.expression -> string

val make_boiler_page : 
  ?onload:string -> 
  ?body:string ->
  string list -> string

val script_tag : ?base:string -> string -> string

val test : unit -> unit 
val run_tests : unit -> unit 
