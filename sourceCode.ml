(*pp deriving *)
open Utility

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
  method private extract_substring (start : Lexing.position) (finish : Lexing.position) =
    if start == Lexing.dummy_pos || finish == Lexing.dummy_pos then
      "*** DUMMY POSITION ****"
    else
      Buffer.sub text start.Lexing.pos_cnum (finish.Lexing.pos_cnum - start.Lexing.pos_cnum)

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
  method find_line (pos : Lexing.position) : (string * int) =
    (self#extract_line pos.Lexing.pos_lnum, 
     pos.Lexing.pos_cnum - Hashtbl.find lines (pos.Lexing.pos_lnum -1) - 1)

  (* Create a `lookup function' that given start and finish positions
     returns a resolved position
  *)
  method lookup =
    fun (start, finish) ->
      (start, 
       self#extract_line start.Lexing.pos_lnum,
       self#extract_substring start finish)
end

type lexpos = Lexing.position 
module Typeable_lexpos 
  : Typeable.Typeable with type a = lexpos = 
  Typeable.Defaults
    (struct
       type a = lexpos
       let type_rep = Typeable.TypeRep.mkFresh "lexpos" []
     end)
                                             
module Eq_lexpos : Eq.Eq with type a = lexpos = 
struct
  type a = lexpos
  let eq = (==)
end
module LexposType = struct type a = lexpos let tname = "SourceCode.lexpos" end
module Show_lexpos = Show.Show_unprintable (LexposType)
(*module Pickle_lexpos = Pickle_unpicklable (LexposType)*)

module Typeable_source_code 
  : Typeable.Typeable with type a = source_code = 
  Typeable.Defaults
    (struct
       type a = source_code
       let type_rep = Typeable.TypeRep.mkFresh "source_code" []
     end)

module Eq_source_code : Eq.Eq with type a = source_code = 
struct
  type a = source_code
  let eq = (==)
end
module SourceCodePos = struct type a = source_code let tname = "SourceCode.source_code" end
module Show_source_code = Show.Show_unprintable (SourceCodePos)

(** unresolved position *)
(* start * end * code *)
type pos = lexpos * lexpos * source_code option
    deriving (Typeable, Show,  Eq)
let dummy_pos = (Lexing.dummy_pos, Lexing.dummy_pos, None)


(** resolved position *)
(* The start position is only used to generate the filename and the
   line number. Perhaps we should just store these here instead.
*)
(* start * source line * source expression *)
type position = lexpos * string * string
    deriving (Typeable, Show,  Eq)
let dummy_position = Lexing.dummy_pos, "<dummy>", "<dummy>"

(* generic syntax error *)
exception ASTSyntaxError of pos * string

let resolve_pos : pos -> position =
  function
    | (start, finish, Some source_code) -> source_code#lookup(start, finish)
    | (_, _, None) -> dummy_position
