(* JavaScript code generation *)

val generate_real_client_page : ?cgi_env:(string * string) list ->
  (Var.var Env.String.t * Types.typing_environment) ->
  Ir.binding list -> (Value.env * Value.t) ->
  Webserver_types.websocket_url option -> Loader.ext_dep list -> string

val make_boiler_page :
  ?cgi_env:(string * string) list ->
  ?onload:string ->
  ?body:string ->
  ?html:string ->
  ?head:string ->
  ?external_files:string list -> string list -> string

val ext_script_tag : ?base:string -> string -> string
