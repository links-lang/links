(** Interface to the parser.*)

type 'result grammar

(* Grammar for types *)
val datatype    :  Sugartypes.datatype grammar
(* Grammar for interactive shell *)
val interactive : Sugartypes.sentence grammar
(* Grammar for programs stored in files etc. *)
val program : (Sugartypes.binding list * Sugartypes.phrase option) grammar

type context
val fresh_context : unit -> context

type position_context = SourceCode.source_code

val parse_string  : ?pp:string
                  -> ?in_context:context
                  -> 'a grammar
                  -> string
                  -> 'a * position_context
val parse_file    : ?pp:string
                  -> ?in_context:context
                  -> 'a grammar
                  -> string
                  -> 'a * position_context
val parse_channel : ?interactive:(unit -> unit)
                  -> ?in_context:context
                  -> 'a grammar
                  -> (in_channel * string)
                  -> 'a * position_context

val parse_readline : string
                   -> ?in_context:context
                   -> 'a grammar
                   -> ('a * position_context)
