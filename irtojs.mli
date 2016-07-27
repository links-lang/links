(* JavaScript code generation *)

val generate_program_defs : 
  (Var.var Env.String.t * Types.typing_environment) ->
  Ir.binding list -> string list

val generate_program_page : ?cgi_env:(string * string) list -> ?onload:string ->
  (Var.var Env.String.t * Types.typing_environment) ->
  Ir.program ->  string

val generate_real_client_page : ?cgi_env:(string * string) list -> ?onload:string ->
  (Var.var Env.String.t * Types.typing_environment) ->
  Ir.binding list -> (Value.env * Value.t) -> string

val make_boiler_page : 
  ?cgi_env:(string * string) list ->
  ?onload:string -> 
  ?body:string ->
  ?html:string ->
  ?head:string ->
 string list -> string

val ext_script_tag : ?base:string -> string -> string
