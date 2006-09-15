(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

let load_url s =
  try
    let buff = Buffer.create 4096 in 
    let conn = Curl.init () in
    Curl.set_url conn s;
    Curl.set_writefunction conn (Buffer.add_string buff);
    Curl.perform conn;
    Buffer.contents buff
  with Curl.CurlException (code, n, msg) ->
    Value.failwith' (Printf.sprintf "Curl error for url `%s' %i: %s" s n msg)


let () = 
  Config.register 
    "curl" 
    "Load external URLs with curl"
    (fun () -> Url.url_loader := load_url)
