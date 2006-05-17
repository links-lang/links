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
  Buffer.sub code.text start.pos_cnum (finish.pos_cnum - start.pos_cnum)

(* Return a line of source code *)
let extract_line
    (code : source_code)
    (line : int) : string =
  try 
    let start  = Hashtbl.find code.lines (line-1)
    and finish = Hashtbl.find code.lines line in
      trim_initial_newline (Buffer.sub code.text (start) (finish - start))
  with Not_found -> "<unknown>"
      
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

(* Read and parse Links source code from the source named `name' via
   the function `infun'.
*)
let read parse desugarer (infun : string -> int -> int) (name : string)  =
  let code = code_create () in
  let lexbuf = {(from_function (parse_into code infun))
 		with lex_curr_p={pos_fname=name; pos_lnum=1; pos_bol=0; pos_cnum=0}} in
    try
      desugarer code (parse (Lexer.lexer ()) lexbuf)
(*        List.map (desugar (lookup code)) (parse (Lexer.lexer ()) lexbuf)*)
    with 
      | Parsing.Parse_error -> 
	  let line, position = find_line code lexbuf.lex_curr_p in
	    raise
	      (Errors.SyntaxError
		 ("*** Parse error: " ^ name ^ ":"
		  ^ string_of_int lexbuf.lex_curr_p.pos_lnum
		  ^"\n   " ^ line ^ "\n"
		  ^ String.make (position + 3) ' ' ^ "^"))
      | Sugar.ParseError (msg, (start, finish)) ->
	  let linespec = 
	    if start.pos_lnum = finish.pos_lnum 
	    then string_of_int start.pos_lnum
	    else (string_of_int start.pos_lnum  ^ "..."
		  ^ string_of_int finish.pos_lnum) in
	  let line, position = find_line code finish in
	    raise 
	      (Errors.SyntaxError
		 ("*** Parse error: " ^ name ^ ":"
		  ^ linespec ^ "\n"
		  ^ msg ^ "\n   " ^ line ^ "\n"
		  ^ String.make (position + 3) ' ' ^ "^"))
      | Lexer.LexicalError (lexeme, position) ->
	  let line = extract_line code position.pos_lnum in
	    raise
	      (Errors.SyntaxError
		 ("*** Lexical error: " ^ name ^ ":"
		  ^ string_of_int position.pos_lnum
		  ^ "\nIn line:\n   " ^ line ^ "\n"
		  ^ "Unexpected character : " ^ lexeme))


(* Given an input channel, return a function suitable for input to
   Lexing.from_function that reads characters from the channel.
*)
let reader_of_channel channel =
  fun buffer nchars -> input channel buffer 0 nchars

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



(** Public functions: parse some data source containing Links source
    code and return a list of ASTs. 

    We use Lexing.from_function in every case rather than
    Lexing.from_channel, Lexing.from_string etc. so that we can
    intercept and retain the code that has been read (in order to give
    better error messages).
**)

(* Parse a string containing Links code.
   Return a list of ASTs representing definitions and expressions.
*)
let parse_string string = 
  read Parser.parse_links 	(fun code s -> List.map (Sugar.desugar (lookup code)) s) (reader_of_string string) "<string>"
    
(* Read and parse Links code from an input channel.
   Return a list of ASTs representing definitions and expressions.
*)
let parse_channel (channel, name) =
  read Parser.parse_links (fun code s -> List.map (Sugar.desugar (lookup code)) s) (reader_of_channel channel) name

(* Open, read and parse a file containing Links code.
   Return a list of ASTs representing definitions and expressions.
*)
let parse_file filename = 
  parse_channel (open_in filename, filename)


(* Parse a kind *)
let parse_kind string = 
  Sugar.desugar_assumption (Sugar.generalize (Parser.just_kind (Lexer.lexer ()) (from_string (string ^ ";"))))

(* Parse a sentence *)
let parse_sentence (channel, name) =
  read Parser.sentence (fun code -> function
                          | Left  phrases   -> Left (List.map (Sugar.desugar (lookup code)) phrases)
                          | Right directive -> Right directive)
    (reader_of_channel channel) name
