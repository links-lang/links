(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(*
  XML Schema validator

  Usage:   validate <schema_document> [<instance_document> ...]

  Exit codes:
    0     validation ok
    1     wrong invocation
    2     error validating schema document
    3     error validating instance document
*)

open Printf
open Pxp_document

open Schema_common
open Schema_types

exception Usage

let debug = true
let debug_print s = if debug then prerr_endline s

let main () =
  let schema_file =
    try
      (match Sys.argv.(1) with
      | "--help" | "-help" -> raise Usage
      | fname -> fname)
    with Invalid_argument _ -> raise Usage
  in
  debug_print "Parsing schema document ...";
  let schema = Schema_parser.schema_of_uri schema_file in
(*
  for i = 2 to Array.length Sys.argv - 1 do
    let instance_stream = Schema_xml.pxp_stream_of_file Sys.argv.(i) in
    let first_element_name =
      let rec aux s =
        match Stream.peek s with
        | Some (Pxp_yacc.E_start_tag (name, _, _)) -> name
        | _ -> Stream.junk s; aux s
      in
      aux instance_stream
    in
    (try
      let first_element_decl =
        (try
          List.find (fun (name,_,_) -> name = first_element_name)
            schema.elements
        with Not_found ->
          raise (XSI_validation_error (sprintf "No declaration found in schema \
            for element '%s'" first_element_name)))
      in
      debug_print "Creating validator for root element ...";
      let validator =
        Schema_validator.validator_of_elt_decl first_element_decl
      in
      debug_print "Validating ...";
      let value = Schema_validator.validate ~validator instance_stream in
      debug_print "Printing CDuce value ...";
      Value.print Format.std_formatter value;
      debug_print "All done!"
    with XSI_validation_error msg ->
      print_endline (sprintf "Validation error on '%s': %s" Sys.argv.(i) msg);
      flush stdout)
  done
*)
  ()

let _ =
  try
    main ()
  with
  | Usage ->
      prerr_endline
        "Usage:   validate <schema_document> [ <instance_document> ...  ]";
      exit 1
  | XSD_validation_error msg ->
      prerr_endline ("Error validating schema document:\n" ^ msg);
      exit 2
  | XSI_validation_error msg ->
      prerr_endline ("Error validating instance document:\n" ^ msg);
      exit 3
  | Pxp_types.At _ as exc ->
      prerr_endline ("PXP error: " ^ Pxp_types.string_of_exn exc);
      exit 4
  
