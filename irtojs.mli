(* JavaScript code generation *)

val generate_program_defs : 
  (Ir.closures * Var.var Env.String.t * Types.typing_environment) ->
  Ir.binding list -> string list

val generate_program_page : ?cgi_env:(string * string) list -> ?onload:string -> 
  (Ir.closures * Var.var Env.String.t * Types.typing_environment) ->
  Ir.program -> string

val make_boiler_page : 
  ?cgi_env:(string * string) list ->
  ?onload:string -> 
  ?body:string ->
  ?head:string ->
  string list -> string

val ext_script_tag : ?base:string -> string -> string
