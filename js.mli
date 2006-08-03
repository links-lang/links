(* JavaScript code generation *)

val generate_program : Syntax.expression list -> Syntax.expression -> string
(* val generate_program_defs : Syntax.expression list -> string *)
val generate_program_defs_script_tag : Syntax.expression list -> Result.xmlitem
val test : unit -> unit 
val run_tests : unit -> unit 

val get_js_lib_url : unit -> string

val jsheader : bool -> Result.xmlitem list

(* val expr_to_js : Syntax.expression -> string *)
val tl_gen : Syntax.expression -> string

val compile_event_handler_to_str : string list -> (string * Syntax.expression) -> (string * string)
