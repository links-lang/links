(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

exception Timeout

let header = "Content-Type: text/plain\n\n"

let cut w s =
  let b= Buffer.create 1024 in
  let rec aux i x =
    if i < String.length s then
      match s.[i] with
	| '\n' -> Buffer.add_char b '\n'; aux (i + 1) 0
	| '\r' -> aux (i + 1) 0
	| '<' ->
	    let rec tag i =
	      Buffer.add_char b s.[i];
	      if (s.[i] = '>') then aux (i + 1) x else tag (i + 1) in
	    tag i
	| c -> 
	    let x = 
	      if x = w then (Buffer.add_string b "\\\n:"; 2) 
	      else (x + 1) in
	    Buffer.add_char b c; 
	    if c = '&' then
	      let rec ent i =
		Buffer.add_char b s.[i];
		if (s.[i] = ';') then aux (i + 1) x else ent (i + 1) in
	      ent (i + 1)
	    else
	      aux (i + 1) x
  in
  aux 0 0;
  Buffer.contents b

let () =
  let exec src =
    ignore (Unix.alarm 10);
    Sys.set_signal Sys.sigalrm 
      (Sys.Signal_handle (fun _ -> raise (Cduce.Escape Timeout)));
    let v = Location.get_viewport () in
    let ppf = Html.ppf v 
    and input = Stream.of_string src in
    Format.pp_set_margin ppf 60;
    Location.push_source (`String src);
    Location.set_protected true;
    Config.init_all ();
    let ok = Cduce.script ppf ppf input in
    if ok then Format.fprintf ppf "@\nOk.@\n";
    Html.get v
  in

  Location.set_viewport (Html.create true);
  let prog = Buffer.create 1024 in
  (try while true do Buffer.add_string prog (read_line ()); Buffer.add_string prog "\n" done;
   with End_of_file -> ());
  let prog = Buffer.contents prog in
  let res = try exec prog with Timeout -> "Timeout reached !" in
  let res = cut 60 res in
  print_string header;
  print_endline "<pre>";
  print_endline res;
  print_endline "</pre>"

