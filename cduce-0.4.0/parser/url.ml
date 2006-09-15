(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

let start_with s p =
  let l = String.length p in
  let n = String.length s in
  if (n >= l) && (String.sub s 0 l = p)
  then Some (String.sub s l (n - l))
  else None

let is_url s = 
  try let _ = Neturl.extract_url_scheme s in true
  with Neturl.Malformed_URL -> false
	
let no_load_url s =
  let msg =  
    Printf.sprintf 
      "Error \"%s\": \nTo fetch external URLs, you need to compile CDuce with curl and/or netclient" s 
  in
  raise (Location.Generic msg) 

let url_loader = ref no_load_url

type kind = File of string | Uri of string | String of string

let kind s = 
  match start_with s "string:" with
    | None -> if is_url s then Uri s else File s
    | Some s -> String s

let local s1 s2 =
  match (kind s1, kind s2) with
    | File _, File _ -> 
	let url1 = Neturl.file_url_of_local_path s1 in
	let url2 = 
	  Neturl.parse_url 
	    ~base_syntax:(Neturl.url_syntax_of_url url1) 
	    s2 in
	Neturl.local_path_of_file_url(
	  Neturl.ensure_absolute_url ~base:url1 url2
	)
    | _, (String _ | Uri _) | (String _, File _) ->
	s2
    | Uri _, File _ -> 
	let url1 = Neturl.parse_url s1 in
	let url2 = 
	  Neturl.parse_url 
	    ~base_syntax:(Neturl.url_syntax_of_url url1) 
	    s2 in
	Neturl.string_of_url (Neturl.ensure_absolute_url ~base:url1 url2)

let load_file fn =
  try
    let ic = open_in fn in
    let len = in_channel_length ic in
    let s = String.create len in
    really_input ic s 0 len;
    close_in ic;
    s
  with exn -> 
    Value.failwith' (Printf.sprintf "load_file: %s"
		       (Printexc.to_string exn))

let load_url s =
  match start_with s "string:" with
    | None -> if is_url s then !url_loader s else load_file s
    | Some s -> s
