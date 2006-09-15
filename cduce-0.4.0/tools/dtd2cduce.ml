open Netcgi
exception Timeout


(* TODO: 
    - clever factorizations of content model and attribute specifs
         (e.g.  type XHTML_inlien = [ ( Char | ... ) ])
    - better pretty-printing
*)
open Printf
open Pxp_yacc
open Pxp_lexer_types
open Pxp_types

let mixed_table : ('a,unit) Hashtbl.t = Hashtbl.create 127
let regexp_table : ('a,unit) Hashtbl.t = Hashtbl.create 127

let import_dtd ppf name src =
  let rec regexp ppf = function
    | Optional re -> Format.fprintf ppf "%a?" regexp re
    | Repeated re -> Format.fprintf ppf "%a*" regexp re
    | Repeated1 re -> Format.fprintf ppf "%a+" regexp re
    | Seq (re1 :: res) -> 
	Format.fprintf ppf "(@[%a" regexp re1;
	List.iter (fun re -> Format.fprintf ppf "@ %a" regexp re) res;
	Format.fprintf ppf "@])"
    | Alt (re1 :: res) -> 
	Format.fprintf ppf "(@[%a" regexp re1;
	List.iter (fun re -> Format.fprintf ppf "@ | %a" regexp re) res;
	Format.fprintf ppf "@])"
    | Child s -> Format.fprintf ppf "%s" (name s)
    | _ -> assert false
  in
  let content ppf = function
    | Unspecified | Any -> Format.fprintf ppf "Any*"
    | Empty -> Format.fprintf ppf ""
    | Mixed l -> 
	(try 
	   Hashtbl.find mixed_table l;
	   Format.fprintf ppf "MIXED:CACHED!"; raise Not_found
	 with Not_found ->
(*	   Hashtbl.add mixed_table l ();  *)
	   let l = List.map 
		     (function 
			| MPCDATA -> "Char"
			| MChild s -> name s) l in
	   Format.fprintf ppf "( %s )*" (String.concat " | " l))
    | Regexp r -> 
	(try 
	   Hashtbl.find regexp_table r;
	   Format.fprintf ppf "REGEXP:CACHED!"; raise Not_found
	 with Not_found ->
(*	   Hashtbl.add regexp_table r (); *)
	   regexp ppf r
	)
  in    
  let att_type ppf = function
    | A_enum l -> 
	Format.fprintf ppf "(";
	ignore 
	  (List.fold_left 
	     (fun first s -> 
		if not first then Format.fprintf ppf " | ";
		Format.fprintf ppf "\"%s\"" s; false) true l);
	Format.fprintf ppf ")"
    | _ -> Format.fprintf ppf "String"
  in
  let attrib ppf e =
    List.iter
      (fun a ->
	 let (at,ad) = e # attribute a in
	 match ad with
	   | D_fixed _ -> ()
	   | _ ->
	       Format.fprintf ppf " %s=%s%a"
		 a
		 (if ad = D_required then "" else "?")
		 att_type at;
      )
      (e # attribute_names)
  in
  let elt ppf e = 
    Format.fprintf ppf "type @[<2>%s =@ @[<3><%s%a>[@ @[%a@]@ ]@]@];;@\n" 
      (name (e # name))
      (e # name)
      attrib e
      content (e # content_model)
  in      

  let dtd = 
    parse_dtd_entity { default_config with encoding = `Enc_utf8 } src in
  Format.fprintf ppf 
    "(* This file has been automatically by dtd2cduce *)@\n";
  List.iter (fun x -> elt ppf (dtd # element x)) (dtd # element_names)

let main (cgi : Netcgi.std_activation) =
  try
    cgi # set_header
      ~content_type:"text/plain; charset=\"iso-8859-1\""
      ();
    let dtd = cgi # argument_value "dtd" in
    let prefix = cgi # argument_value "prefix" in
    import_dtd Format.str_formatter (fun s -> prefix ^ s) 
      (from_string dtd);
    let res = Format.flush_str_formatter () in
    cgi # output # output_string res;
    cgi # output # commit_work();
  with exn ->
    cgi # output # rollback_work();
    cgi # set_header 
      ~content_type:"text/plain; charset=\"iso-8859-1\""
      ();
    let s = Pxp_types.string_of_exn exn in
    cgi # output # output_string "ERROR:\n";
    cgi # output # output_string s;
    cgi # output # output_string "\n";
    cgi # output # commit_work()


let () =
  match Array.length Sys.argv with
    | 3 ->
	let name s = Sys.argv.(1) ^ s in
	import_dtd Format.std_formatter name (from_file Sys.argv.(2))
    | 1 ->
	let operating_type = Netcgi.buffered_transactional_optype in
	let cgi = new Netcgi.std_activation ~operating_type () in
	ignore (Unix.alarm 20);
	Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout));
	main cgi;
	cgi # finalize ()
   | _ ->
       prerr_endline "Usage: dtd2cduce <prefix> <.dtd file>";
       exit 2
