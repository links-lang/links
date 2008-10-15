open Utility
open Lexing

(* NB : For now, positions are resolved eagerly.  It might be better
   to resolve them lazily, i.e. keep the source around and only find
   the text corresponding to an expression when an error actually
   occurs.
*)

type 'a grammar = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a

class source_code = SourceCode.source_code

(* Read and parse Links source code from the source named `name' via
   the function `infun'.
*)
let read : context:Lexer.lexer_context
        -> ?nlhook:(unit -> unit)
        -> parse:('intermediate grammar)
        -> infun:(string -> int -> int)
        -> name:string
        -> 'result * source_code =
fun ~context ?nlhook ~parse ~infun ~name ->
  let code = new source_code in
  let lexbuf = {(from_function (code#parse_into infun))
                 with lex_curr_p={pos_fname=name; pos_lnum=1; pos_bol=0; pos_cnum=0}} in
    try
      let p = parse (Lexer.lexer context ~newline_hook:(from_option identity nlhook)) lexbuf in
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
      | Lexer.LexicalError (lexeme, position) ->
          let line, column = code#find_line position in
            raise
              (Errors.RichSyntaxError
                 {Errors.filename = name;
                  Errors.linespec = string_of_int position.pos_lnum;
                  Errors.message = "Unexpected character : " ^ lexeme;
                  Errors.linetext = line;
                  Errors.marker = String.make column ' ' ^ "^"})

(* Given an input channel, return a function suitable for input to
   Lexing.from_function that reads characters from the channel.
*)
let reader_of_channel channel buffer = input channel buffer 0
  
(* Given a string, return a function suitable for input to
   Lexing.from_function that reads characters from the string.
*)
let reader_of_string ?pp string = 
  let string = match pp with
    | None -> string
    | Some command -> Utility.filter_through ~command string in
  let current_pos = ref 0 in
    fun buffer nchars ->
      let nchars = min nchars (String.length string - !current_pos) in
        StringLabels.blit ~src:string ~src_pos:!current_pos ~dst:buffer ~dst_pos:0 ~len:nchars;
        current_pos := !current_pos + nchars;
        nchars

let interactive : Sugartypes.sentence grammar = Parser.interactive
let program : (Sugartypes.binding list * Sugartypes.phrase option) grammar = Parser.file
let datatype : Sugartypes.datatype grammar = Parser.just_datatype

let normalize_pp = function
  | "" -> None
  | pp -> Some pp

(* We parse in a "context", which at the moment means the set of
   operator precedences, but more generally is an environment with
   respect to which any parse-time resolution takes place.
*)
type context = Lexer.lexer_context
let fresh_context = Lexer.fresh_context

let normalize_context = function
  | None -> fresh_context ()
  | Some c -> c

let default_preprocessor = (Settings.get_value Basicsettings.pp) 

(** Public functions: parse some data source containing Links source
    code and return a list of ASTs. 

    We use Lexing.from_function in every case rather than
    Lexing.from_channel, Lexing.from_string etc. so that we can
    intercept and retain the code that has been read (in order to give
    better error messages).
**)
let parse_string ?(pp=default_preprocessor) ?in_context:context grammar string =
  let pp = normalize_pp pp
  and context = normalize_context context in 
    read ?nlhook:None ~parse:grammar ~infun:(reader_of_string ?pp string) ~name:"<string>" ~context

let parse_channel ?interactive ?in_context:context grammar (channel, name) =
  let context = normalize_context context in
    read ?nlhook:interactive ~parse:grammar ~infun:(reader_of_channel channel) ~name:name ~context

let parse_file ?(pp=default_preprocessor) ?in_context:context grammar filename =
  match normalize_pp pp with
    | None -> parse_channel ?in_context:context grammar (open_in filename, filename)
    | Some pp ->
        Utility.call_with_open_infile filename
          (fun channel ->
             let context = normalize_context context in
             read ~nlhook:ignore
                  ~parse:grammar
                  ~infun:(reader_of_string ~pp (String.concat "\n" (Utility.lines channel)))
                  ~name:filename 
                  ~context)

type position_context = SourceCode.source_code

