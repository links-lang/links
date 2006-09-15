(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Pxp_yacc
open Pxp_lexer_types
open Pxp_types
open Pxp_ev_parser
open Pxp_reader

let pxp_handle_event = function
  | E_start_tag (name,att,_,_) -> Load_xml.start_element_handler name att
  | E_char_data data -> Load_xml.text_handler data 
  | E_end_tag (_,_) -> Load_xml.end_element_handler ()
  | _ -> ()

let pxp_config = 
  { default_config with 
      (* warner = new warner; *)
      encoding = `Enc_utf8;
      store_element_positions = false;
      drop_ignorable_whitespace = true
  }

let utf8_bom = "\239\187\191"

let of_string s =
  let enc,s =
    if (String.length s >= 3) && (String.sub s 0 3 = utf8_bom)
    then Some `Enc_utf8, (String.sub s 3 (String.length s - 3))
    else None,s in
  enc, new Netchannels.input_string s

let of_file fn =
  let ic = open_in_bin fn in
  let buf = String.create 3 in
  really_input ic buf 0 3;
  let e = if buf = utf8_bom then Some `Enc_utf8 else (seek_in ic 0; None) in
  e, new Netchannels.input_channel ic

let channel_of_id rid =
  let url =
    match rid.rid_system_base, rid.rid_system with
      | Some base, Some rel -> Url.local base rel
      | _, Some rel -> rel
      | _ -> raise Not_competent
  in
  let enc,ch = 
    if Url.is_url url 
    then of_string (Url.load_url url)
    else of_file url
  in
  ch, enc, Some { rid with rid_system = Some url }

let alt = new resolve_to_any_obj_channel ~channel_of_id ()

let src_of_uri uri = XExtID (System uri,None,alt)

let load_pxp handlers uri =
  try
    let mgr = create_entity_manager pxp_config (src_of_uri uri) in
    process_entity pxp_config 
      (`Entry_document[`Extend_dtd_fully]) mgr handlers;
  with exn ->
    Value.failwith' (Pxp_types.string_of_exn exn)

let use () = Load_xml.xml_parser := load_pxp pxp_handle_event

let () = 
  Config.register 
    "pxp" 
    "PXP XML parser"
    use

let () = 
  Schema_xml.xml_parser := 
    (fun uri f g ->
       load_pxp 
	 (function
	    | E_start_tag (name,att,_,_) -> f name att
	    | E_end_tag (_,_) -> g ()
	    | _ -> ())
	 uri)
