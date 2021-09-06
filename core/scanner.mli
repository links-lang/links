type position_context = SourceCode.source_code

module type LexerSig = sig
  type token
  type lexer_context
  type 'a grammar = (Lexing.lexbuf -> token) -> Lexing.lexbuf -> 'a

  val lexer : lexer_context
           -> newline_hook:(unit -> unit)
           -> (Lexing.lexbuf -> token)

  val fresh_context     : unit -> lexer_context
end

module Scanner (Lex : LexerSig) : sig
  val read : context:Lex.lexer_context
          -> ?nlhook:(unit -> unit)
          -> parse:('result Lex.grammar)
          -> infun:(bytes -> int -> int)
          -> name:string
          -> unit
          -> 'result * position_context

  val normalize_context : Lex.lexer_context option -> Lex.lexer_context
end
