(** Interface to the parser.*)

type ('result, 'intermediate) grammar

(* Grammar for types *)
val datatype    :  (Types.datatype, Sugartypes.datatype) grammar
(* Grammar for interactive shell *)
val interactive : (Sugartypes.sentence', Sugartypes.sentence) grammar
(* Grammar for programs stored in files etc. *)
val program : (Syntax.untyped_program,
               (Sugartypes.binding list * Sugartypes.phrase option)) grammar

type context
val fresh_context : unit -> context

val parse_string  : ?pp:string
                  -> ?in_context:context
                  -> ('a,'b) grammar
                  -> string
                  -> 'a * ('b * (Sugartypes.position -> Syntax.position))
val parse_file    : ?pp:string
                  -> ?in_context:context
                  -> ('a,'b) grammar
                  -> string
                  -> 'a * ('b * (Sugartypes.position -> Syntax.position))
val parse_channel : ?interactive:(unit -> unit)
                  -> ?in_context:context
                  -> ('a,'b) grammar
                  -> (in_channel * string) 
                  -> 'a * ('b * (Sugartypes.position -> Syntax.position))
