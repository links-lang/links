(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(* Loading XML documents *)

open Value
open Ident
open Encodings

let keep_ns = ref true

type buf =
    { mutable buffer : string;
      mutable pos : int;
      mutable length : int }

let txt = { buffer = String.create 1024; pos = 0; length = 1024 }

let resize txt n  =
  let new_len = txt.length * 2 + n in
  let new_buf = String.create new_len in
  String.unsafe_blit txt.buffer 0 new_buf 0 txt.pos;
  txt.buffer <- new_buf;
  txt.length <- new_len

let add_string txt s =
  let len = String.length s in
  let new_pos = txt.pos + len in
  if new_pos > txt.length then resize txt len;
  String.unsafe_blit s 0 txt.buffer txt.pos len;
  txt.pos <- new_pos

let rec only_ws s i =
  (i = 0) ||
  (let i = pred i in match (String.unsafe_get s i) with
     | ' ' | '\t' | '\n' | '\r' -> only_ws s i
     | _ -> false) 


let string s q =
  let s = Utf8.mk s in
  String_utf8 (Utf8.start_index s,Utf8.end_index s, s, q)


let attrib att = 
  (* TODO: better error message *)
  let att = List.map (fun (n,v) -> Upool.int n, string_utf8 v) att in
  Imap.create (Array.of_list att)

let elem ns tag att child =
  if !keep_ns then
    XmlNs (Atom tag, Record (attrib att), child, ns)
  else
    Xml (Atom tag, Record (attrib att), child)

type stack = 
  | Element of Value.t * stack
  | Start of 
      Ns.table * Atoms.V.t * (Ns.Label.t * Utf8.t) list * Ns.table * stack
  | String of string * stack
  | Empty

let stack = ref Empty
let ns_table = ref Ns.empty_table

let rec create_elt accu = function
  | String (s,st) -> create_elt (string s accu) st
  | Element (x,st) -> create_elt (Pair (x,accu)) st
  | Start (ns,name,att,old_table,st) -> 
      stack := Element (elem ns name att accu, st);
      ns_table := old_table
  | Empty -> assert false

let start_element_handler name att =
  if not (only_ws txt.buffer txt.pos) then 
    stack := String (String.sub txt.buffer 0 txt.pos, !stack); 
  txt.pos <- 0;

  let (table,name,att) = Ns.process_start_tag !ns_table name att in
  stack := Start (table,Atoms.V.mk name,att,!ns_table, !stack);
  ns_table := table

let end_element_handler _ =
  let accu =
    if only_ws txt.buffer txt.pos 
    then nil 
    else string (String.sub txt.buffer 0 txt.pos) nil in
  txt.pos <- 0; 
  create_elt accu !stack

let text_handler = add_string txt


let xml_parser = ref (fun s -> failwith "No XML parser available")


let load_xml ?(ns=false) s =
  try
    keep_ns := ns;
    !xml_parser s;
    match !stack with
      | Element (x,Empty) -> stack := Empty; x
      | _ -> Value.failwith' "No XML stream to parse"
  with e -> stack := Empty; txt.pos <-0; 
    match e with 
      | Ns.UnknownPrefix _ -> Value.failwith' "Unknown namespace prefix"
      | e -> raise e

      

let load_html s =
  let rec val_of_doc q = function
    | Nethtml.Data data -> 
	if (only_ws data (String.length data)) then q else string data q
    | Nethtml.Element (tag, att, child) -> 
	let att = List.map (fun (n,v) -> (Label.mk (Ns.empty, U.mk n), U.mk v)) att in
	Pair (elem Ns.empty_table (Atoms.V.mk (Ns.empty,U.mk tag) )
		att (val_of_docs child), q)
  and val_of_docs = function
    | [] -> nil
    | h::t -> val_of_doc (val_of_docs t) h
  in	

  Location.protect_op "load_html";
  let parse src = Nethtml.parse_document ~dtd:Nethtml.relaxed_html40_dtd src in
  let doc = 
    if Url.is_url s then
      parse (Lexing.from_string (Url.load_url s))
    else
      let ic = open_in s in
      let doc = 
	try parse (Lexing.from_channel ic) 
	with exn -> close_in ic; raise exn in
      close_in ic;
      doc
  in
  let doc = Nethtml.decode ~subst:(fun _ -> "???") doc in
  let doc = Nethtml.map_list 
	      (Netconversion.convert ~in_enc:`Enc_iso88591
		 ~out_enc:`Enc_utf8) doc in
  val_of_docs doc
