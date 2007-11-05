open Utility
open Lexing

(* NB : For now, positions are resolved eagerly.  It might be better
   to resolve them lazily, i.e. keep the source around and only find
   the text corresponding to an expression when an error actually
   occurs.
*)

type source_code = {
  (* offsets of line start positions in `text' *)
  lines : (int, int) Hashtbl.t;
  (* the text itself *)
  text : Buffer.t;
}

(* Initial estimates for input size *)
let default_lines = 100
and default_chars = 8000

let trim_initial_newline s =
  let len = String.length s in
  if len > 0 && s.[0] == '\n' then StringLabels.sub s ~pos:1 ~len:(len-1)
  else s

let code_create () =
  let tbl = Hashtbl.create default_lines in
    Hashtbl.add tbl 0 0;
    {lines = tbl; text = Buffer.create default_chars}

(* Return the portion of soure code that falls between two positions *)
let extract_substring
    (code : source_code)
    (start : position)
    (finish : position) : string =
  if start == dummy_pos || finish == dummy_pos then
    "*** DUMMY POSITION ****"
  else
    Buffer.sub code.text start.pos_cnum (finish.pos_cnum - start.pos_cnum)

(* Return some lines of the source code *)
let extract_line_range
    (code : source_code)
    (startline : int)
    (finishline : int) : string =
  try 
    let start  = Hashtbl.find code.lines startline
    and finish = (if finishline == Hashtbl.length code.lines
                  then (* handle the last line of input *)
                    Buffer.length code.text
                  else
                    Hashtbl.find code.lines finishline)
    in
      trim_initial_newline (Buffer.sub code.text (start) (finish - start))
  with Not_found -> "<unknown>"

(* Return one line of the source code *)
let extract_line
    (code : source_code)
    (line : int) : string =
  extract_line_range code (line-1) line
      
(* Given a function `infun' as required by Lexing.from_function,
   return another such function that stores the text read in `code'.
*)
let parse_into
    (code : source_code)
    (infun : string -> int -> int) : string -> int -> int =
  fun buffer nchars ->
    let nchars = infun buffer nchars in
      List.iter (fun linepos ->
                  Hashtbl.add code.lines 
                    (Hashtbl.length code.lines)
                    (linepos + Buffer.length code.text))
        (Utility.find_char (StringLabels.sub buffer ~pos:0 ~len:nchars) '\n');
      Buffer.add_substring code.text buffer 0 nchars;
      nchars

(* Retrieve the last line of source code read. *)
let find_line (code : source_code) (pos : position) : (string * int) =
  (extract_line code pos.pos_lnum, 
   pos.pos_cnum - Hashtbl.find code.lines (pos.pos_lnum -1) - 1)

(* Create a `lookup function' that given start and finish positions
   returns an Syntax.position
*)
let lookup code =
  fun (start, finish) ->
    (start, 
     extract_line code start.pos_lnum,
     extract_substring code start finish)

type 'a parser_ = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a
type ('a,'b) desugarer = (source_code -> 'a -> 'b)

(* Read and parse Links source code from the source named `name' via
   the function `infun'.
*)
let read : parse:('intermediate parser_)
        -> desugarer:('intermediate, 'result) desugarer
        -> infun:(string -> int -> int)
        -> name:string
        -> ?nlhook:(unit -> unit)
        -> 'result * ('intermediate * (Sugartypes.pposition -> Syntax.position)) =
fun ~parse ~desugarer ~infun ~name ?nlhook ->
  let code = code_create () in
  let lexbuf = {(from_function (parse_into code infun))
                 with lex_curr_p={pos_fname=name; pos_lnum=1; pos_bol=0; pos_cnum=0}} in
    try
      let p = parse (Lexer.lexer (from_option identity nlhook)) lexbuf in
        (desugarer code p, (p, lookup code))
    with 
      | Parsing.Parse_error -> 
          let line, column = find_line code lexbuf.lex_curr_p in
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
          let line = extract_line_range code (start.pos_lnum-1) finish.pos_lnum in
          let _, column = find_line code finish in
            raise 
              (Errors.RichSyntaxError
                 {Errors.filename = name;
                  Errors.linespec = linespec;
                  Errors.message = msg;
                  Errors.linetext = line;
                  Errors.marker = String.make column ' ' ^ "^"})
      | Lexer.LexicalError (lexeme, position) ->
          let line, column = find_line code position in
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
       | `Definitions phrases -> `Definitions (Sugar.desugar_definitions (lookup code) phrases)
       | `Expression phrase   -> `Expression (Sugar.desugar_expression (lookup code) phrase)
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
                                `FnAppl ((`Var "renderPagelet", pos), [body]), pos) body
                 else
                   body in
               let pos = lookup code in
                 Syntax.Program
                   (Sugar.desugar_definitions pos defs,
                    opt_app (Sugar.desugar_expression pos) (Syntax.unit_expression (`U Syntax.dummy_position)) body));
  parse = Parser.file
}

let datatype : (Types.assumption, Sugartypes.datatype) grammar = {
    desugar =  (fun _ -> Sugar.desugar_datatype);
    parse = Parser.just_datatype
  }

let normalize_pp = function
  | None
  | Some "" -> None
  | pp -> pp

(** Public functions: parse some data source containing Links source
    code and return a list of ASTs. 

    We use Lexing.from_function in every case rather than
    Lexing.from_channel, Lexing.from_string etc. so that we can
    intercept and retain the code that has been read (in order to give
    better error messages).
**)
let parse_string ?pp grammar string =
  let pp = normalize_pp pp in
    read ~parse:grammar.parse ~desugarer:grammar.desugar ~infun:(reader_of_string ?pp string) ~name:"<string>" ?nlhook:None

let parse_channel ?interactive grammar (channel, name) =
  read ~parse:grammar.parse ~desugarer:grammar.desugar ~infun:(reader_of_channel channel) ~name:name ?nlhook:interactive

let parse_file ?pp grammar filename =
  match normalize_pp pp with
    | None -> parse_channel grammar (open_in filename, filename)
    | Some pp ->
        Utility.call_with_open_infile filename
          (fun channel ->
             read ~parse:grammar.parse
                  ~desugarer:grammar.desugar
                  ~infun:(reader_of_string ~pp (String.concat "\n" (Utility.lines channel)))
                  ~name:filename 
                  ?nlhook:None)
