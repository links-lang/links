open Utility
open Lexing

(* NB : For now, positions are resolved eagerly.  It might be better
   to resolve them lazily, i.e. keep the source around and only find
   the text corresponding to an expression when an error actually
   occurs.
*)

(* Initial estimates for input size *)
let default_lines = 100
and default_chars = 8000

let trim_initial_newline s =
  let len = String.length s in
  if len > 0 && s.[0] = '\n' then StringLabels.sub s ~pos:1 ~len:(len-1)
  else s

class source_code =
object (self)
  val lines = 
    let tbl = Hashtbl.create default_lines in
      Hashtbl.add tbl 0 0;
      tbl
  val text = Buffer.create default_chars

  (* Return the portion of source code that falls between two positions *)
  method extract_substring (start : position) (finish : position) =
    if start == dummy_pos || finish == dummy_pos then
      "*** DUMMY POSITION ****"
    else
      Buffer.sub text start.pos_cnum (finish.pos_cnum - start.pos_cnum)

  (* Return some lines of the source code *)
  method extract_line_range (startline : int) (finishline : int) =
    try 
      let start  = Hashtbl.find lines startline
      and finish = (if finishline = Hashtbl.length lines
                    (* handle the last line of input *)
                    then Buffer.length text
                    else Hashtbl.find lines finishline)
      in
        trim_initial_newline (Buffer.sub text (start) (finish - start))
    with NotFound _ -> "<unknown>"
      
  (* Return one line of the source code *)
  method extract_line (line : int) =
    self#extract_line_range (line - 1) line

  (* Given a function `infun' as required by Lexing.from_function,
     return another such function that stores the text read in `code'.
  *)
  method parse_into (infun : string -> int -> int) : string -> int -> int =
    fun buffer nchars ->
      let nchars = infun buffer nchars in
        List.iter (fun linepos ->
                     Hashtbl.add lines 
                       (Hashtbl.length lines)
                       (linepos + Buffer.length text))
          (Utility.find_char (StringLabels.sub buffer ~pos:0 ~len:nchars) '\n');
        Buffer.add_substring text buffer 0 nchars;
        nchars

  (* Retrieve the last line of source code read. *)
  method find_line (pos : position) : (string * int) =
    (self#extract_line pos.pos_lnum, 
     pos.pos_cnum - Hashtbl.find lines (pos.pos_lnum -1) - 1)

  (* Create a `lookup function' that given start and finish positions
     returns an Syntax.position
  *)
  method lookup =
    fun (start, finish) ->
      (start, 
       self#extract_line start.pos_lnum,
       self#extract_substring start finish)
end

type 'a parser_ = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a
type ('a,'b) desugarer = (source_code -> 'a -> 'b)

(* Read and parse Links source code from the source named `name' via
   the function `infun'.
*)
let read : context:Lexer.lexer_context
        -> ?nlhook:(unit -> unit)
        -> parse:('intermediate parser_)
        -> desugarer:('intermediate, 'result) desugarer
        -> infun:(string -> int -> int)
        -> name:string
        -> 'result * ('intermediate * (Sugartypes.pposition -> Syntax.position)) =
fun ~context ?nlhook ~parse ~desugarer ~infun ~name ->
  let code = new source_code in
  let lexbuf = {(from_function (code#parse_into infun))
                 with lex_curr_p={pos_fname=name; pos_lnum=1; pos_bol=0; pos_cnum=0}} in
    try
      let p = parse (Lexer.lexer context ~newline_hook:(from_option identity nlhook)) lexbuf in
        (desugarer code p, (p, code#lookup))
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
      | Sugar.ConcreteSyntaxError (msg, (start, finish)) ->
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

type ('result, 'intermediate) grammar = {
    desugar : ('intermediate, 'result) desugarer;
    parse : 'intermediate parser_
  }

let interactive : (Sugartypes.sentence', Sugartypes.sentence) grammar = { 
    desugar = 
    (fun code s -> match s with 
       | `Definitions phrases -> `Definitions (Sugar.desugar_definitions code#lookup phrases)
       | `Expression phrase   -> `Expression (Sugar.desugar_expression code#lookup phrase)
       | `Directive directive -> `Directive directive);
    parse =  Parser.interactive
  }
  
let program : (Syntax.untyped_program,
               (Sugartypes.binding list * Sugartypes.phrase option)) grammar = {
  desugar = (fun code (defs, body) ->
               let body =
                 if Settings.get_value Basicsettings.web_mode then
                   opt_map (fun body ->
                              let _, pos = body in
                                `FnAppl ((`Var "renderPage", pos), [body]), pos) body
                 else
                   body in
               let pos = code#lookup in
                 Syntax.Program
                   (Sugar.desugar_definitions pos defs,
                    opt_app (Sugar.desugar_expression pos) (Syntax.unit_expression (`U Syntax.dummy_position)) body));
  parse = Parser.file
}

let datatype : (Types.datatype, Sugartypes.datatype) grammar = {
    desugar =  (fun _ -> Sugar.desugar_datatype);
    parse = Parser.just_datatype
  }

let normalize_pp = function
  | None
  | Some "" -> None
  | pp -> pp

(* We parse in a "context", which at the moment means the set of
   operator precedences, but more generally is an environment with
   respect to which any parse-time resolution takes place.
*)
type context = Lexer.lexer_context
let fresh_context = Lexer.fresh_context

let normalize_context = function
  | None -> fresh_context ()
  | Some c -> c

    (** Public functions: parse some data source containing Links source
        code and return a list of ASTs. 

        We use Lexing.from_function in every case rather than
        Lexing.from_channel, Lexing.from_string etc. so that we can
        intercept and retain the code that has been read (in order to give
        better error messages).
    **)
let parse_string ?pp ?in_context:context grammar string =
  let pp = normalize_pp pp 
  and context = normalize_context context in 
    read ?nlhook:None ~parse:grammar.parse ~desugarer:grammar.desugar ~infun:(reader_of_string ?pp string) ~name:"<string>" ~context

let parse_channel ?interactive ?in_context:context grammar (channel, name) =
  let context = normalize_context context in
    read ?nlhook:interactive ~parse:grammar.parse ~desugarer:grammar.desugar ~infun:(reader_of_channel channel) ~name:name ~context

let parse_file ?pp ?in_context:context grammar filename =
  match normalize_pp pp with
    | None -> parse_channel ?in_context:context grammar (open_in filename, filename)
    | Some pp ->
        Utility.call_with_open_infile filename
          (fun channel ->
             let context = normalize_context context in
             read ~nlhook:ignore
                  ~parse:grammar.parse
                  ~desugarer:grammar.desugar
                  ~infun:(reader_of_string ~pp (String.concat "\n" (Utility.lines channel)))
                  ~name:filename 
                  ~context)
