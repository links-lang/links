(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(* Print XML documents *)

(* The write_*_function are inspired from Pxp_aux.ml *)

open Netconversion

let write_markup_string ~to_enc buf s =
  let s' = if to_enc = `Enc_utf8 then s
  else convert
    ~in_enc:`Enc_utf8
    ~out_enc:to_enc
    ~subst:(fun n -> 
	      failwith ("Cannot represent code point " ^ string_of_int n))
    s
  in
  Buffer.add_string buf s'

let write_data_string ~to_enc buf s =
  let write_part i len =
    if (len > 0) then
      if to_enc = `Enc_utf8 
      then Buffer.add_substring buf s i len
      else
	let s' = 
	  convert
            ~in_enc:`Enc_utf8
            ~out_enc:to_enc
            ~subst:(fun n -> "&#" ^ string_of_int n ^ ";")
	    ~range_pos:i ~range_len:len s
	in
	Buffer.add_string buf s'
  in
  let i = ref 0 in
  for k = 0 to String.length s - 1 do
    match s.[k] with
      | ('&' | '<' | '>' | '"' | '%') as c ->
          write_part !i (k - !i);
          begin match c with
              '&' -> Buffer.add_string buf "&amp;"
            | '<' -> Buffer.add_string buf "&lt;"
            | '>' -> Buffer.add_string buf "&gt;"
            | '"' -> Buffer.add_string buf "&quot;"
            | '%' -> Buffer.add_string buf "&#37;"  (* reserved in DTDs *)
            | _   -> assert false
          end;
          i := k+1
      | _ -> ()
  done;
  write_part !i (String.length s - !i)


(*************)


open Value
open Ident
module U = Encodings.Utf8

let exn_print_xml = CDuceExn (Pair (
				Atom (Atoms.V.mk_ascii "Invalid_argument"),
				string_latin1 "print_xml"))

let blank = U.mk " "
let true_literal = U.mk "true"
let false_literal = U.mk "false"

  (* @raise exn_print_xml in case of failure. Rationale: schema printing is
   * the last attempt to print a value, others have already failed *)
let rec schema_value ?(recurs=true) ~wds v = match v with
  | Abstract ("float",f) ->
      wds (U.mk (string_of_float (Obj.magic f : float)))
  | Record _ as v ->
      (try
        wds (Schema_builtin.string_of_time_type (Value.get_fields v))
      with Schema_builtin.Error _ -> raise exn_print_xml)
  | Integer i -> wds (U.mk (Intervals.V.to_string i))
  | v when Value.equal v Value.vtrue -> wds true_literal
  | v when Value.equal v Value.vfalse -> wds false_literal
  | Pair _ as v when recurs -> schema_values ~wds v
  | String_utf8 _ as v -> wds (fst (get_string_utf8 v))
  | _ -> raise exn_print_xml

and schema_values ~wds v =
  match v with
  | Pair (hd, Atom a) when a = Sequence.nil_atom ->
      schema_value ~recurs:false ~wds hd
  | Pair (hd, tl) ->
      schema_value ~recurs:false ~wds hd;
      wds blank;
      schema_values ~wds tl
  | _ -> raise exn_print_xml

let string_of_xml ~utf8 ns_table v = 
  let to_enc = if utf8 then `Enc_utf8 else `Enc_iso88591 in

  let buffer = Buffer.create 127 in
  let printer = Ns.Printer.printer ns_table in

  let wms = write_markup_string ~to_enc buffer
  and wds s = write_data_string ~to_enc buffer (U.get_str s)
  in
  let write_att (n,v) =
    wms (" " ^ (Ns.Printer.attr printer (Label.value n)) ^ "=\""); wds v; wms "\"" in
  let write_xmlns (pr,ns) =
    let pr = U.get_str pr in
    if pr = "" then wms " xmlns"
    else (wms " xmlns:"; wms pr);
    wms "=\"";
    wds (Ns.Uri.value ns);
    wms "\"" in

  let element_start q xmlns attrs = 
    wms ("<" ^ (Ns.Printer.tag printer (Atoms.V.value q))); 
    List.iter write_xmlns xmlns;
    List.iter write_att attrs; 
    wms ">"
  and empty_element q xmlns attrs = 
    wms ("<" ^ (Ns.Printer.tag printer (Atoms.V.value q))); 
    List.iter write_xmlns xmlns;
    List.iter write_att attrs; 
    wms "/>"
  and element_end q = 
    wms ("</" ^ (Ns.Printer.tag printer (Atoms.V.value q)) ^ ">")
  and document_start () = 
(*    wms ("<?xml version='1.0' encoding='" ^
	 Netconversion.string_of_encoding to_enc ^
	 "'?>\n") *)
    ()
  in

  let rec register_elt = function
    | Xml (Atom q, Record attrs, content) 
    | XmlNs (Atom q, Record attrs, content, _) ->
	Imap.iter
	  (fun n _ -> Ns.Printer.register_qname printer 
	     (Label.value (Label.from_int n)))
	  attrs;
	Ns.Printer.register_qname printer (Atoms.V.value q);
	register_content content
    | _ -> ()
  and register_content = function
    | String_utf8 (_,_,_,q)
    | String_latin1 (_,_,_,q) -> register_content q
    | Pair (x, q) -> register_elt x; register_content q
    | Concat (x,y) -> register_content x; register_content y
    | _ -> () 
  in
  register_elt v;

  let rec print_elt xmlns = function
    | Xml (Atom tag, Record attrs, content)
    | XmlNs (Atom tag, Record attrs, content, _) ->
	let attrs = Imap.map_elements
		      (fun n v -> 
                         if is_str v then begin
                           let (s,q) = get_string_utf8 v in
                           match q with
                             | Atom a when a = Sequence.nil_atom -> 
                                 (Label.from_int n), s
                             | _ -> raise exn_print_xml
                         end else begin
                           let buf = Buffer.create 20 in
                           let wds s = Buffer.add_string buf (U.get_str s) in
                           schema_value ~wds v;
                           (Label.from_int n, U.mk (Buffer.contents buf))
                         end
		      ) attrs in
	(match content with
	  | Atom a when a = Sequence.nil_atom -> empty_element tag xmlns attrs
	  | _ ->
	      element_start tag xmlns attrs;
	      print_content content;
	      element_end tag)
    | _ -> raise exn_print_xml
  and print_content v =
    let (s,q) = get_string_utf8 v in
    wds s;
    match q with
      | Pair ((Xml _ | XmlNs _) as x, q) -> print_elt [] x; print_content q
      | Atom a when a = Sequence.nil_atom -> ()
      | v -> schema_value ~wds v
  in
  document_start ();
  print_elt (Ns.Printer.prefixes printer) v;
  Buffer.contents buffer

let print_xml ~utf8 ns_table s =
  let s = string_of_xml ~utf8 ns_table s in
  if utf8 then string_utf8 (U.mk s) else string_latin1 s
 
