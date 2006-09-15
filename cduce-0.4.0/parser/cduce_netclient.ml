(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

let error msg =
  Value.failwith' (Printf.sprintf "Netclient error. %s" msg)

let load_url s =
  match  Neturl.extract_url_scheme s with
    | "http" -> 
	(try Http_client.Convenience.http_get s
	 with 
	   | Http_client.Bad_message s ->
	       let msg = Printf.sprintf "Bad HTTP answer: %s" s in
	       error msg
	   | Http_client.Http_error (n,s) ->
	       let msg = Printf.sprintf "HTTP error %i: %s" n s in
	       error msg
	   | Http_client.No_reply ->
	       error "No reply"
	   | Http_client.Http_protocol exn ->
	       let msg = Printexc.to_string exn in
	       error msg
	)
    | "file" ->
	error
	  "FIXME: write in url.ml the code so that netclient \
                    handle file:// protocol"
    | sc -> 
	let msg = 
	  Printf.sprintf "Netclient does not handle the %s protocol" sc
	in
	error msg

let () = 
  Config.register 
    "netclient" 
    "Load external URLs with netclient"
    (fun () -> Url.url_loader := load_url)
