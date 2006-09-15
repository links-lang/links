(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(* TODO:
   - HTML design, logo
*)

open Netcgi
exception Timeout

let operating_type = Netcgi.buffered_transactional_optype
let cgi = new Netcgi.std_activation ~operating_type ()

let fatal_error title s =
  cgi # output # rollback_work();
  cgi # set_header 
    ~content_type:"text/html; charset=\"iso-8859-1\""
    ~cache:`No_cache 
    ();
  cgi # output # output_string ("<h1>" ^ title ^ "</h1>");
  cgi # output # output_string s;
  cgi # output # output_string "\n";
  cgi # output # commit_work();
  cgi # finalize ();
  exit 0


(* Loading examples *)

let example code = 
  try List.assoc code Examples.examples
  with Not_found -> ""

let begin_table = "<div class=\"box\">"
let end_table = "</div>"

let (|||) p x = p x; p
let (||=) p () = ()

let html_header p =
  p "
<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html>
<head>
  <meta content=\"text/html; charset=iso-8859-1\" 
        http-equiv=\"Content-Type\"/>
  <link type=\"text/css\" href=\"/cduce.css\" rel=\"stylesheet\"/>
  <title>CDuce online prototype</title>
</head>
<body>
 <div class=\"title\"> <h1>CDuce online prototype</h1> </div>
 <div id=\"Sidelog\">
   <div class=\"box\">
    <ul>
     <li><a href=\"http://www.cduce.org/\">Main page</a></li>
     <li><a href=\"http://www.cduce.org/manual.html\">User's manual</a></li>
     <li><a href=\"http://www.cduce.org/memento.html\">Quick Reference</a></li>
    </ul>
   </div>
";

    p ||| "
<div class=\"box\">
<br/><center><b style=\"font-size:120&#37;; color: #008000\">Sample programs</b></center>
<p>
You can start from one of the predefined examples below or try 
with you own program...</p>
" ||| Examples.present ||| "</div></div><div id=\"Content\">" 
  ||= ()


let html_form p content =
  p "
<div class=\"box\">
 <h2>Input</h2>
   <form name=\"main\" method=\"post\" action=\"/cgi-bin/cduce\">
   <p><input type=\"submit\" name=\"exec\" value=\"Submit to CDuce\"/>
   <input type=\"button\" value=\"Clear\" onClick=\"main.prog.value=''\"/>
   <input type=\"reset\" value=\"Revert changes\"/>
";

  p ||| "</p><p><textarea name=\"prog\" cols=\"80\" rows=\"35\">"
    ||| content
    ||| "</textarea></p></form></div>"
    ||= ()


let html_footer p =
  p "</div></body></html>"


let cmds = [ "exec", `Exec;
	     "example", `Example;
	   ]

let cut p w s =
  let rec aux i x =
    if i < String.length s then
      match s.[i] with
	| '\n' -> p '\n'; aux (i + 1) 0
	| '\r' -> aux (i + 1) 0
	| '<' ->
	    let rec tag i =
	      p s.[i];
	      if (s.[i] = '>') then aux (i + 1) x else tag (i + 1) in
	    tag i
	| c -> 
	    let x = if x = w then (p '\\'; p '\n'; p ':'; 2) else (x + 1) in
	    p c; 
	    if c = '&' then
	      let rec ent i =
		p s.[i];
		if (s.[i] = ';') then aux (i + 1) x else ent (i + 1) in
	      ent (i + 1)
	    else
	      aux (i + 1) x
  in
  aux 0 0

let main (cgi : Netcgi.std_activation) =
  let p = cgi # output # output_string in
  let clicked s = cgi # argument_value s <> "" in
  try
    cgi # set_header ();

    let cmd = 
      try snd (List.find (fun (x,y) -> clicked x) cmds)
      with Not_found -> `New in

    let dialog content = html_form p content in

    let exec src =
      let v = Location.get_viewport () in
      let ppf = Html.ppf v
      and input = Stream.of_string src in
      Location.push_source (`String src);
      Location.set_protected true;
      
      let ok = Cduce.script ppf ppf input in
      if ok then Format.fprintf ppf "@\nOk.@\n";
      let res = Html.get v in
      p "<div class=\"box\"><h2>Results</h2><pre>"; 
      cut (cgi # output # output_char) 80 res;  
      p "</pre></div>";
      dialog src
    in

    Location.set_viewport (Html.create true);
    html_header p;
    let prog = cgi # argument_value "prog" in
    (match cmd with
       | `Exec -> exec prog
       | `Example -> dialog (example (cgi # argument_value "example"))
       | `New -> dialog ""
    );
    p ("
<div class=\"box\"><h2>About the prototype</h2>
<p>
CDuce is under active development; some features may not work properly.
<p><a href='mailto:Alain.Frisch@ens.fr'>Webmaster</a></p>
<p>Prototype version "^ <:symbol<cduce_version>> ^",
 built on "^ <:symbol<build_date>> ^".</p></div>");
    html_footer p;
    cgi # output # commit_work()
  with
      exn ->
	let msg = 
	  match exn with
	    | Unix.Unix_error (e,f,arg) ->
		"System error: " ^ (Unix.error_message e) ^ 
		"; function " ^ f ^ 
		"; argument " ^ arg
	    | Timeout ->
		"Timeout reached ! This prototype limits computation time ..."
	    | exn ->
	      Printexc.to_string exn
	in
	fatal_error "Internal software error!" msg

let () =
  ignore (Unix.alarm 20);
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout));
  Random.self_init ();
  main cgi;
  cgi # finalize ()

