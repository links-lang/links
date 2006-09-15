(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(* TODO: handle encodings of the input for pretty printing
   fragments of code *)

type source = [ `None | `File of string | `Stream | `String of string 
	      | `Buffer of Buffer.t ]
type loc = source * int * int
type precise = [ `Full | `Char of int ]

let merge_loc ((s1,i1,j1) as loc1) ((s2,i2,j2) as loc2) =
  if s1 = s2 then 
    if i1 = -1 then loc2 else if i2 = -1 then loc1 else 
      (s1, min i1 i2, max j1 j2)
  else loc1

let source = ref `None
let source_stack = ref []
let push_source s = source_stack := !source :: !source_stack; source := s
let pop_source () = 
  match !source_stack with
    | [] -> assert false
    | s::rem -> source_stack := rem; source := s

let current_dir () =
  match !source with
    | `File s -> Filename.dirname s
    | _ -> ""

exception Location of loc * precise * exn
exception Generic of string

let raise_loc i j exn = raise (Location ((!source,i,j),`Full,exn))
let raise_generic s = raise (Generic s)
let raise_loc_generic loc s = raise (Location (loc, `Full, Generic s))

let noloc = (`None,-1,-1)
let nopos = (-1,-1)

let viewport = ref (Html.create false)
let set_viewport v = viewport := v
let get_viewport () = !viewport

(* Note: this is incorrect. Directives #utf8,... should
   not be recognized inside comments and strings !
   The clean solution is probably to have the real lexer
   count the lines. *)

let get_line_start enc lb i =
  let rec count line start = lexer
    | '\n' | "\n\r" | '\r' ->
	if (Ulexing.lexeme_start lb >= i) then (line, start)
	else
	aux (line + 1) (Ulexing.lexeme_end lb)
    | "#utf8" ->
	enc := Ulexing.Utf8;
	aux line start
    | "#ascii" ->
	enc := Ulexing.Ascii;
	aux line start
    | "#latin1" ->
	enc := Ulexing.Latin1;
	aux line start
    | eof ->
	(line, start)
    | _ ->
	aux line start
  and aux line start =
    if (Ulexing.lexeme_start lb >= i) then (line, start)
    else count line start lb
  in
  aux 1 0

let get_line_number src i =
  let enc = ref Ulexing.Latin1 in
  let ic = open_in_bin src in
  let lb = Ulexing.from_var_enc_channel enc ic in
  let r = get_line_start enc lb i in
  close_in ic;
  r

let get_line_number_str src i =
  let enc = ref Ulexing.Latin1 in
  let lb = Ulexing.from_var_enc_string enc src in
  get_line_start enc lb i

let print_precise ppf = function
  | `Full -> ()
  | `Char i -> Format.fprintf ppf "Char %i of the string:@\n" i

let print_loc ppf ((src,i,j),w) =  
  match src with
    | `None -> () (*Format.fprintf ppf "somewhere (no source defined !)"*)
    | `Stream | `String _ ->
	Format.fprintf ppf "At chars %i-%i:@\n%a" i j print_precise w
    | `Buffer b ->
(*	let b = Buffer.contents b in
	let (l1,start1) = get_line_number_str b i in *)
	Format.fprintf ppf "Characters %i-%i:@\n%a"
	  i j
	  print_precise w

    | `File fn ->
	let (l1,start1) = get_line_number fn i in
	Format.fprintf ppf "File \"%s\", line %i, characters %i-%i:@\n%a"
	  fn l1 (i - start1) (j - start1)
	  print_precise w

let extr s i j =
  try
    let n = min (String.length s) j - i in
    if n <= 0 then "" else String.sub s i n
  with e -> failwith (Printf.sprintf "Location.extr len=%i i=%i j=%i"
			(String.length s) i j )

let dump_loc ((src,i,j),w) =
  let v = get_viewport () in
  match (src, Html.is_html v) with
    | (`String s, true) ->
	if (i < 0) then
	  Html.markup v "b" (fun ppf -> Format.fprintf ppf "GHOST LOCATION@.")
	else
	  Html.markup v "i" (fun ppf -> Format.fprintf ppf "%s" (extr s i j))
    | _ -> ()

let rec beg_of_line s i =
  if (i <= 0) || (s.[i-1] = '\n') || (s.[i-1] = '\r')
  then i else beg_of_line s (i - 1)

let rec end_of_line s i =
  if (i >= String.length s) || (s.[i] = '\n') || (s.[i] = '\r')
  then i else end_of_line s (i + 1)

let html_hilight ((src,i,j),w) =
  let v = get_viewport () in
  match (src, Html.is_html v) with
    | `String s, true ->
	if (i < 0) then
	  Html.markup v "b" 
	    (fun ppf -> Format.fprintf ppf "GHOST LOCATION@.")
	else
	  let i0 = beg_of_line s i in
	  let j0 = end_of_line s j in
	  Html.markup v "i"
	    (fun ppf ->
	       Format.fprintf ppf "%s" (extr s i0 i);
	       Html.mark v "<font color=\"red\"><b>";
	       Format.fprintf ppf "%s" (extr s i j);
	       Html.mark v "</b></font>";
	       Format.fprintf ppf "%s@." (extr s j j0);
	    )
    | _ -> ()
	

type 'a located = { loc : loc; descr : 'a }

let mk (i,j) x = { loc = (!source,i,j); descr = x }
let mk_loc loc x = { loc = loc; descr = x }
let mknoloc x = { loc = noloc; descr = x }
let loc_of_pos (i,j) = (!source,i,j)

let protected = ref false
let set_protected p = protected := p
let is_protected () = !protected

let protect_op op =
  if (!protected) then
    raise 
      (Generic (op ^ ": operation not authorized in the web prototype"))
