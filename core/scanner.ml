open Utility
open SourceCode
open Lexing

type position_context = SourceCode.source_code

module type LexerSig = sig
  type token
  type lexer_context
  type 'a grammar = (Lexing.lexbuf -> token) -> Lexing.lexbuf -> 'a

  val lexer : lexer_context
           -> newline_hook:(unit -> unit)
           -> (Lexing.lexbuf -> token)
  val fresh_context : unit -> lexer_context
end

module Scanner (Lex : LexerSig) = struct
  let read : context:Lex.lexer_context
          -> ?nlhook:(unit -> unit)
          -> parse:('result Lex.grammar)
          -> infun:(bytes -> int -> int)
          -> name:string
          -> unit
          -> 'result * source_code =
  fun ~context ?nlhook ~parse ~infun ~name () ->
    let code = new source_code in
    let lexbuf = {(from_function (code#parse_into infun))
                 with lex_curr_p={pos_fname=name; pos_lnum=1; pos_bol=0; pos_cnum=0}} in
    try
      let p = parse (Lex.lexer context ~newline_hook:(from_option identity nlhook)) lexbuf in
      (p, code)
    with
    | Parser.Error ->
          let line, column = code#find_line lexbuf.lex_curr_p in
            raise
              (Errors.RichSyntaxError
                 {Errors.filename = name;
                  Errors.linespec = string_of_int lexbuf.lex_curr_p.pos_lnum;
                  Errors.message = "";
                  Errors.linetext = line;
                  Errors.marker = String.make column ' ' ^ "^" })
      | Sugartypes.ConcreteSyntaxError (pos, msg) ->
          let start, finish = Position.start pos, Position.finish pos in
          let linespec =
            if start.pos_lnum = finish.pos_lnum
            then string_of_int start.pos_lnum
            else (string_of_int start.pos_lnum  ^ "..."
                  ^ string_of_int finish.pos_lnum) in
          let line = code#extract_line_range (start.pos_lnum-1) finish.pos_lnum in
          let _, column = code#find_line finish in
            raise
              (Errors.RichSyntaxError
                 {Errors.filename = name;
                  Errors.linespec = linespec;
                  Errors.message = msg;
                  Errors.linetext = line;
                  Errors.marker = String.make column ' ' ^ "^"})
      | Lexer.LexicalError (lexeme, position) ->
          let line, column = code#find_line position in
            raise
              (Errors.RichSyntaxError
                 {Errors.filename = name;
                  Errors.linespec = string_of_int position.pos_lnum;
                  Errors.message = "Unexpected character : " ^ lexeme;
                  Errors.linetext = line;
                  Errors.marker = String.make column ' ' ^ "^"})

  let normalize_context = function
    | None   -> Lex.fresh_context ()
    | Some c -> c
end
