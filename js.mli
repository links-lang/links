(* JavaScript code generation *)

val generate_program_defs : 
  string list -> 
  Syntax.definition list -> Utility.StringSet.t -> string list

val generate_program : ?onload:string -> 
  string list -> 
  Syntax.program -> string

val make_boiler_page : 
  ?onload:string -> 
  ?body:string ->
  ?head:string ->
  string list -> string

val ext_script_tag : ?base:string -> string -> string
