open Utility
open Lexing

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
  method private extract_substring (start : position) (finish : position) =
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
  method private extract_line (line : int) =
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
