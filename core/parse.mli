(** Interface to the parser.*)
open Scanner

module LinksLexer : (LexerSig with type token         = Parser.token and
                                   type lexer_context = Lexer.lexer_context)

(* Grammar for types *)
val datatype    :  Sugartypes.Datatype.with_pos LinksLexer.grammar
(* Grammar for interactive shell *)
val interactive : Sugartypes.sentence LinksLexer.grammar
(* Grammar for programs stored in files etc. *)
val program : (Sugartypes.binding list * Sugartypes.phrase option)
                LinksLexer.grammar

val parse_string  : ?pp:string
                  -> ?in_context:LinksLexer.lexer_context
                  -> 'a LinksLexer.grammar
                  -> string
                  -> 'a * position_context
val parse_file    : ?pp:string
                  -> ?in_context:LinksLexer.lexer_context
                  -> 'a LinksLexer.grammar
                  -> string
                  -> 'a * position_context
val parse_channel : ?interactive:(unit -> unit)
                  -> ?in_context:LinksLexer.lexer_context
                  -> 'a LinksLexer.grammar
                  -> (in_channel * string)
                  -> 'a * position_context

val parse_readline : string
                   -> ?in_context:LinksLexer.lexer_context
                   -> 'a LinksLexer.grammar
                   -> ('a * position_context)
