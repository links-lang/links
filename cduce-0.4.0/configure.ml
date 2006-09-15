(*
#use "topfind";;
#require "findlib";;
*)

open Printf

let not_distrib = Sys.file_exists "Makefile.distrib"

let usage () =
  print_string "\
Configuration of CDuce.

Usage: ./configure [OPTION]...

Defaults for the options are specified in brackets.

Options:
 --help                display this help and exit

Optional features:
 --with-FEATURE        force support for FEATURE [default: autodetect]
 --without-FEATURE     disable support for FEATURE [default: autodetect]

 Available features:
 ocamlopt              use ocamlopt instead of ocamlc to build CDuce
 pxp_wlex              use wlexers for parsing utf8 with PXP [default: false]
 pxp                   support for the PXP XML parser
 expat                 support for the expat XML parser
 curl                  support for the libcurl library
 netclient             support for the netclient library

OCaml/CDuce interface:
 --mliface=DIR         build the interface with the OCaml sources in DIR

Installation directories:
 --prefix=PREFIX       install files in PREFIX [/usr/local]
 --bindir=DIR          install user executables in DIR [PREFIX/bin]
 --mandir=DIR          install man documentation in DIR [PREFIX/man]
 --docdir=DIR          install the rest of the doc in DIR [PREFIX/doc/cduce]
";
if not_distrib then print_string "
 --wprefix=WPREFIX     root directory of the web-server [/var/www]
 --cgidir=DIR          install the cgi-bin interpreter in DIR [WPREFIX/cgi-bin]
 --htmldir=DIR         install the website in DIR [WPREFIX/html]
 --sessiondir=DIR      store the open sessions of the cgi-bin in DIR 
                       [/tmp/cduce_sessions]
"

let features = 
  [ "ocamlopt", ref `auto;
    "mliface", ref `auto;
    "pxp", ref `auto;
    "expat", ref `auto;
    "curl", ref `auto;
    "netclient", ref `auto;
    "pxp_wlex", ref `no ]
  
let vars = 
  [ "prefix", ref "/usr/local";
    "bindir", ref "";
    "mandir", ref "";
    "docdir", ref "";

    "wprefix", ref "/var/www";
    "cgidir", ref "";
    "htmldir", ref "";

    "sessiondir", ref "/tmp/cduce_sessions";
    "mliface", ref ""
  ]


let src_dirs = ["/usr/src"; "/usr/local/src"; "/tmp"]
	
let fatal s = printf "*** Fatal error: %s\n" s; exit 1
let warning s = printf "* Warning: %s\n" s

let log s =
  let oc = open_out_gen [ Open_append; Open_creat ] 0o600 "configure.log" in
  output_string oc s;
  output_char oc '\n';
  close_out oc;
  flush stdout

let command s =
  log ("==> " ^ s);
  Sys.command (sprintf "%s 2>> configure.log" s) = 0

let start_with s p =
  let ls = String.length s and lp = String.length p in
  if (ls >= lp) && (String.sub s 0 lp = p)
  then Some (String.sub s lp (ls - lp)) else None
    
let parse_arg s =
  if s = "--help" then (usage (); exit 0)
  else
    match start_with s "--with-" with
      | Some f -> (List.assoc f features) := `yes
      | None ->
    match start_with s "--without-" with
      | Some f -> (List.assoc f features) := `no
      | None ->
    let i = String.index s '=' in
    if i < 2 then raise Not_found;
    let n = String.sub s 2 (i - 2) in
    let v = String.sub s (succ i) (String.length s - i - 1) in
    (List.assoc n vars) := v

let () =
  print_endline "Configuring CDuce for compilation...\n";
  (try for i = 1 to Array.length Sys.argv - 1 do parse_arg Sys.argv.(i) done
  with Not_found -> usage (); fatal "Incorrect command line");
  (try Sys.remove "configure.log" with Sys_error _ -> ());
  ignore (Sys.command "uname -a >> configure.log")

let print s = print_string s; flush stdout

let check_feature f p =
  printf "%s: " f;
  match !(List.assoc f features) with
    | `no -> 
	print "disabled\n"; false
    | `yes -> 
	print "checking... ";
	if p () 
	then (print "ok\n"; true) 
	else (print "failed !\n"; fatal "Required feature is not available")
    | `auto -> 
	print "autodetecting... ";
	if p () 
	then (print "enabled\n"; true)
	else (print "disabled\n"; false)

let native = 
  check_feature "ocamlopt" (fun () -> command "ocamlfind ocamlopt")

let check_pkg p () =
  try
(*    ignore (Findlib.package_property
      [ (if native then "native" else "bytecode") ]
      p "archive"); *)
    command 
      (sprintf 
	 "ocamlfind ocaml%s -package %s -linkpkg -o configure.try && rm -f configure.try"
	 (if native then "opt" else "c")
	 p)
  with Not_found -> false

let need_pkg p =
  printf "Checking for package %s... " p; flush stdout;
  if not (check_pkg p ()) 
  then (print "failed !\n"; fatal "Required package is not available")
  else (print "ok\n")

let dir ?def d = 
  let s = !(List.assoc d vars) in
  if s <> "" then s
  else match def with 
    | Some x -> x 
    | None -> fatal (sprintf "%s cannot be empty" d)


let exe = match Sys.os_type with 
  | "Cygwin" ->
      print "Cygwin detected... executable will have .exe extension"; ".exe" 
  | _ -> ""

let check_mliface dir =
  Sys.file_exists (Filename.concat dir "typing/types.ml") 

let ocaml_stdlib () =
  if (Sys.command "ocamlc -where > ocaml_stdlib" <> 0) then
    fatal "Can't run ocamlc to get OCaml standard library path";
  let ic = open_in "ocaml_stdlib" in
  let s = input_line ic in
  close_in ic;
  Sys.remove "ocaml_stdlib";
  s

let ml_interface = 
  let dir1 = !(List.assoc "mliface" vars) in
  let dirs = [] in
  let dirs = if dir1 = "" then dirs else dir1 :: dirs in
  print "ocaml sources... ";
  let rec loop = function
    | [] -> 
	print "not found (the interface will not be built)\n";
	None
    | d::dirs ->
	if check_mliface d then
	  (print ("found: " ^ d ^ "\n"); Some d)
	else loop dirs
  in
  loop dirs
  
let pxp = check_feature "pxp" (check_pkg "pxp")
let expat = check_feature "expat" (check_pkg "expat")
let curl = check_feature "curl" (check_pkg "curl")
let netclient = check_feature "netclient" (check_pkg "netclient")
let pxp_wlex = check_feature "pxp_wlex" (check_pkg "pxp-wlex-utf8")
let prefix = dir "prefix"
let bindir = dir ~def:(prefix^"/bin") "bindir"
let mandir = dir ~def:(prefix^"/man") "mandir"
let docdir = dir ~def:(prefix^"/doc/cduce") "docdir"
let wprefix = dir "wprefix"
let cgidir = dir ~def:(wprefix^"/cgi-bin") "cgidir"
let htmldir = dir ~def:(wprefix^"/html") "htmldir"
let sessiondir = dir "sessiondir"

let curl,netclient = 
  match curl,netclient with
    | true,true -> 
	warning "Both netclient and curl are available. Will use curl.";
	true,false
    | false,false ->
	warning "No package for loading external URLs.";
	false,false
    | c,n -> c,n

let pxp,expat = 
  match pxp,expat with
    | true,true -> 
	warning "Both PXP and expat are available. Will build both and use expat by default.";
	true,true
    | false,false ->
	warning "No package for parsing XML documents.";
	false,false
    | c,n -> c,n

let required_packages = ["camlp4"; "num"; "pcre"; "ulex"; "cgi"; "netstring"]

let has_forpack =
  print "testing for -for-pack option: ";
  if Sys.command "ocamlc -for-pack foo 2> /dev/null" = 0 then
    (print "available\n"; true)
  else
    (print "not available\n"; false)

let () =
  List.iter need_pkg required_packages;
  if pxp then (
    need_pkg "pxp-engine";
    need_pkg "pxp-lex-iso88591";
    if not pxp_wlex then need_pkg "pxp-lex-utf8";
  );

  print "Creating Makefile.conf...\n";

  let out = open_out "Makefile.conf" in
  fprintf out "# This file has been generated by the configure script\n";
  fprintf out "NATIVE=%b\n" native;
  (match ml_interface with
    | Some d -> fprintf out "ML_INTERFACE=true\nOCAML_SRC=%s\n" d
    | None -> fprintf out "ML_INTERFACE=false\n");
  fprintf out "PXP=%b\n" pxp;
  fprintf out "EXPAT=%b\n" expat;
  fprintf out "CURL=%b\n" curl;
  fprintf out "NETCLIENT=%b\n" netclient;
  fprintf out "PXP_WLEX=%b\n" pxp_wlex;
  fprintf out "BINDIR=%s\n" bindir;
  fprintf out "MANDIR=%s\n" mandir;
  fprintf out "DOCDIR=%s\n" docdir;
  fprintf out "CGI_DIR=%s\n" cgidir;
  fprintf out "HTML_DIR=%s\n" htmldir;
  fprintf out "SESSION_DIR=%s\n" sessiondir;
  fprintf out "EXE=%s\n" exe;
  fprintf out "PROFILE=false\n";
  fprintf out "FORPACK=%b\n" has_forpack;
  close_out out
