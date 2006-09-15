(* To test PXP pull parsers *)

open Pxp_yacc
open Pxp_lexer_types
open Pxp_types
open Printf


(* dump_event: dumps a single parsing event *)

let dump_event e = ()

(*
let dump_event =
  function
      E_start_doc(v,sa,dtd) ->
	printf "E_start_doc version=%s standalone=%b\n" v sa
    | E_end_doc ->
	printf "E_end_doc\n"
    | E_start_tag(name,attlist,_) ->
	printf "E_start_tag %s %s\n" name 
	  (String.concat " " (List.map (fun (n,v) -> n ^ "=" ^ v) attlist))
    | E_end_tag(name,_) ->
	printf "E_end_tag %s\n" name
    | E_char_data data ->
	printf "E_char_data %s\n" data
    | E_pinstr(target,data) ->
	printf "E_pinstr %s %s\n" target data
    | E_comment data ->
	printf "E_comment %s\n" data
    | E_position(ent,line,col) ->
	printf "E_position %s line=%d col=%d\n" ent line col
    | E_error e ->
	printf "E_error %s\n" (Pxp_types.string_of_exn e)
    | E_end_of_stream ->
	printf "E_end_of_stream\n"
*)


(* parse: prints the events while parsing the passed string *)

let pull s =
  let config = default_config in
  let mgr = create_entity_manager config (from_file s) in
  let next_event = 
    create_pull_parser config (`Entry_document [`Extend_dtd_fully]) mgr in
  let event = ref (Some E_end_of_stream) in
  while !event <> None do
    event := next_event();
    match !event with
	Some e -> dump_event e
      | None -> () 
  done


let push s =
  process_entity
    default_config
    (`Entry_document[(* `Extend_dtd_fully *)])
    (create_entity_manager default_config (from_file s))
    dump_event

let () = push Sys.argv.(1)
