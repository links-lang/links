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
    try
      Buffer.sub text start.Lexing.pos_cnum (finish.Lexing.pos_cnum - start.Lexing.pos_cnum)
    with Invalid_argument _ -> "*** DUMMY POSITION ****"

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
  method parse_into (infun : bytes -> int -> int) : bytes -> int -> int =
    fun buffer nchars ->
    let nchars = infun buffer nchars in
    List.iter (fun linepos ->
        Hashtbl.add lines
          (Hashtbl.length lines)
          (linepos + Buffer.length text))
      (Utility.find_char (Bytes.sub buffer 0 nchars) '\n');
    Buffer.add_subbytes text buffer 0 nchars;
    nchars

  (* Retrieve the last line of source code read. *)
  method find_line (pos : Lexing.position) : (string * int) =
    (self#extract_line pos.Lexing.pos_lnum,
     abs @@ pos.Lexing.pos_cnum - Hashtbl.find lines (pos.Lexing.pos_lnum -1) - 1)

  (* Create a `lookup function' that given start and finish positions
     returns a resolved position
  *)
  method lookup =
    fun (start, finish) ->
    (start,
     self#extract_line start.Lexing.pos_lnum,
     self#extract_substring start finish)
end

module Lexpos = struct
  type t = Lexing.position

  let pp fmt _ =
    (** Supress lexpos output**)
    Utility.format_omission fmt

  let show v = Format.asprintf "%a" pp v
end


module Position = struct
  type t = {
    start : Lexpos.t;
    finish : Lexpos.t;
    code : source_code option;
  }

  let pp fmt _ = Utility.format_omission fmt
  let show v = Format.asprintf "%a" pp v

  let make ~start ~finish ~code =
    { start; finish; code; }

  let dummy = make ~start:Lexing.dummy_pos ~finish:Lexing.dummy_pos ~code:None

  let start t = t.start

  let finish t = t.finish

  let code t = t.code

  let map_code t ~f =
    let code = f t.code in
    { t with code }

  let traverse t ~o ~f_start ~f_finish ~f_code =
    let o = f_start o t.start in
    let o = f_finish o t.finish in
    let o = f_code o t.code in
    o

  let traverse_map t ~o ~f_start ~f_finish ~f_code =
    let o, start = f_start o t.start in
    let o, finish = f_finish o t.finish in
    let o, code = f_code o t.code in
    o, make ~start ~finish ~code

  (* generic syntax error *)
  exception ASTSyntaxError of t * string

  module Resolved = struct
    type unresolved = t

    (** resolved position *)
    (* The start position is only used to generate the filename and the
       line number. Perhaps we should just store these here instead.
    *)
    (* start * source line * source expression *)
    type t = {
      start : Lexpos.t;
      source_line : string;
      source_expression : string;
    }
    [@@deriving show]

    let dummy = {
      start = Lexing.dummy_pos;
      source_line = "<dummy>";
      source_expression = "<dummy>";
    }

    let resolve pos =
      match code pos with
      | Some source_code ->
        let start, source_line, source_expression =
          source_code#lookup(start pos, finish pos) in
        { start; source_line; source_expression }
      | None -> dummy

    let start (t : t) = t.start
    let source_line t = t.source_line
    let source_expression t = t.source_expression
  end

  let resolve_expression t = Resolved.resolve t |> Resolved.source_expression

  let resolve_start_expr t =
    let r = Resolved.resolve t in
    Resolved.start r, Resolved.source_expression r

  let pp fmt _ = Utility.format_omission fmt
end

module WithPos = struct
  module Legacy = struct
      type 'a with_pos = {
      node : 'a;
      pos  : Position.t;
    }
    [@@deriving show]

    let make ?(pos = Position.dummy) node = { node; pos }

    let with_pos pos node = make ~pos node
    let with_dummy_pos node = make node
    let tuple_of_with_pos t = t.node, t.pos
  end

  open Legacy

  type 'a t = 'a with_pos [@@deriving show]

  let make ?(pos = Position.dummy) node = { node; pos }

  let node t = t.node

  let pos t = t.pos

  let map t ~f =
    let { node; pos } = t in
    let node = f node in
    make ~pos node

  let map2 t ~f_pos ~f_node =
    let pos = f_pos t.pos in
    let node = f_node t.node in
    make ~pos node

  let traverse t ~o ~f_pos ~f_node =
    let o = f_pos o t.pos in
    let o = f_node o t.node in
    o
end
