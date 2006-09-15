(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

type t = { 
  ppf : Format.formatter; buf : Buffer.t; html : bool;
  mutable marker : int; mutable marks : (int * string) list
}

let create html = 
  let buf = Buffer.create 1024 in
  { ppf = Format.formatter_of_buffer buf;
    buf = buf;
    html = html;
    marker = 0; marks = [] }

let ppf x = x.ppf

let mark x s =
  if x.html then (
    let m  = x.marker in
    x.marker <- m + 1;
    x.marks <- (m, s) :: x.marks;
    Format.pp_print_as x.ppf 0 ("\000" ^ (string_of_int m) ^ "\000")
  )

let markup x s p =
  if x.html then (
    mark x ("<"^s^">");
    p x.ppf;
    mark x ("</"^s^">");
  ) else
    p x.ppf

let get x =
  Format.pp_print_flush x.ppf ();
  let s = Buffer.contents x.buf in
  Buffer.clear x.buf;
  let rec aux i =
    if i = String.length s then ()
    else match s.[i] with
      | '\000' ->
	  let j = 
	    try String.index_from s (i+1) '\000' 
	    with Not_found -> assert false in
	  let m = int_of_string (String.sub s (i+1) (j-i-1)) in
	  let m = List.assq m x.marks in
	  Buffer.add_string x.buf m;
	  aux (j+1)
      | '<' ->
	  Buffer.add_string x.buf "&lt;";
	  aux (i+1)
      | '&' ->
	  Buffer.add_string x.buf "&amp;";
	  aux (i+1)
      | ('\000'..'\008' | '\011' | '\012' | '\013'..'\031' | '\127') as c ->
	  Buffer.add_string x.buf (Printf.sprintf "&#%i;" (Char.code c));
	  aux (i+1)
      | c ->
	  Buffer.add_char x.buf c;
	  aux (i+1)
  in
  aux 0;
  let s = Buffer.contents x.buf in
  Buffer.clear x.buf;
  x.marker <- 0;
  x.marks <- [];
  s

let is_html x = x.html
