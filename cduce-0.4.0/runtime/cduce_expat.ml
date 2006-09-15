(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

let buflen = 1024
let buf = String.create buflen

let load_from_file p s =
  let ic = 
    try open_in s
    with exn ->
      let msg = 
	Printf.sprintf "load_xml, file \"%s\": %s" s (Printexc.to_string exn)
      in
      raise (Location.Generic msg)
  in
  let rec loop () =
    let n = input ic buf 0 buflen in
    if (n > 0) then (Expat.parse_sub p buf 0 n; loop ()) 
  in
  try
    loop(); 
    Expat.final p;
    close_in ic
  with exn -> close_in ic; raise exn

let rec push p s =
  Expat.set_external_entity_ref_handler p 
    (fun ctx base sys pub -> 
       let s = Url.local s sys in
       let p = Expat.external_entity_parser_create p ctx None in
       push p s);
  try
    if Url.is_url s then Expat.parse p (Url.load_url s)
    else load_from_file p s
  with Expat.Expat_error e -> 
    let msg =
      Printf.sprintf
	"load_xml,%s at line %i, column %i: %s"
	s
	(Expat.get_current_line_number p)
	(Expat.get_current_column_number p)
	(Expat.xml_error_to_string e)
    in
    Value.failwith' msg

let rec load_expat se ee txt s =
  let p = Expat.parser_create None in
  Expat.set_start_element_handler p se;
  Expat.set_end_element_handler p ee;
  Expat.set_character_data_handler p txt;
  ignore (Expat.set_param_entity_parsing p Expat.ALWAYS);
  push p s

let use () = Load_xml.xml_parser := 
  load_expat Load_xml.start_element_handler Load_xml.end_element_handler
    Load_xml.text_handler

let () = 
  Config.register 
    "expat" 
    "Expat XML parser"
    use

let () = 
  Schema_xml.xml_parser := 
    (fun uri f g -> load_expat f (fun _ -> g ()) (fun _ -> ()) uri)
