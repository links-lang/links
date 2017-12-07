open Lexing

(*
   This module is a copied, pasted and hacked version of parse.ml
*)

type 'a grammar = (Lexing.lexbuf -> XmlParser.token) -> Lexing.lexbuf -> 'a

class source_code = SourceCode.source_code

(* Read and parse Links source code from the source named `name' via
   the function `infun'.
*)
let read : context:XmlLexer.lexer_context
        -> ?nlhook:(unit -> unit)
        -> parse:('intermediate grammar)
        -> infun:(bytes -> int -> int)
        -> name:string
        -> 'result * source_code =
fun ~context ?nlhook ~parse ~infun ~name ->
  let code = new source_code in
  let lexbuf = {(from_function (code#parse_into infun))
                 with lex_curr_p={pos_fname=name; pos_lnum=1; pos_bol=0; pos_cnum=0}} in
   try
      let p = parse (XmlLexer.lexer context ~newline_hook:(Utility.from_option Utility.identity nlhook)) lexbuf in
        (p, code)

    with
      | Parsing.Parse_error ->
          let line, column = code#find_line lexbuf.lex_curr_p in
            raise
              (Errors.RichSyntaxError
                 {Errors.filename = name;
                  Errors.linespec = string_of_int lexbuf.lex_curr_p.pos_lnum;
                  Errors.message = "";
                  Errors.linetext = line;
                  Errors.marker = String.make column ' ' ^ "^" })
      | Sugartypes.ConcreteSyntaxError (msg, (start, finish, _)) ->
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
      | XmlLexer.LexicalError (lexeme, position) ->
          let line, column = code#find_line position in
            raise
              (Errors.RichSyntaxError
                 {Errors.filename = name;
                  Errors.linespec = string_of_int position.pos_lnum;
                  Errors.message = "Unexpected character : " ^ lexeme;
                  Errors.linetext = line;
                  Errors.marker = String.make column ' ' ^ "^"})

(* Given a string, return a function suitable for input to
   Lexing.from_function that reads characters from the string.
*)
let reader_of_string string =
  let current_pos = ref 0 in
    fun buffer nchars ->
      let nchars = min nchars (String.length string - !current_pos) in
        StringLabels.blit ~src:string ~src_pos:!current_pos ~dst:buffer ~dst_pos:0 ~len:nchars;
        current_pos := !current_pos + nchars;
        nchars

let xml : (Value.xmlitem) grammar = XmlParser.xml

(* We parse in a "context", which is an environment with
   respect to which any parse-time resolution takes place.
*)
(* type context = XmlLexer.lexer_context *)
let fresh_context = XmlLexer.fresh_context

let normalize_context = function
  | None -> fresh_context ()
  | Some c -> c

let parse_string ?in_context:context grammar string =
  let context = normalize_context context in
    read ?nlhook:None ~parse:grammar ~infun:(reader_of_string string) ~name:"<string>" ~context

let parse_xml s = fst (Errors.display_fatal (parse_string xml) s)
