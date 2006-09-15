(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Ident

let out_dir  = ref [] (* directory of the output file *)
let src  = ref []
let args = ref []

let compile = ref false
let run = ref false
let script = ref false
let mlstub = ref false
let topstub = ref false

let version () =
  Printf.eprintf "CDuce, version %s\n" <:symbol<cduce_version>>;
  Printf.eprintf "built on %s\n" <:symbol<build_date>>;
  Printf.eprintf "using OCaml %s compiler\n" <:symbol<ocaml_compiler>>;
  Printf.eprintf "Supported features: \n";
  List.iter (fun (n,d) -> Printf.eprintf "- %s: %s\n" n d) (Config.descrs ());
  exit 0

let specs =
  [ "--compile", Arg.Set compile,
             "compile the given CDuce file";
    "-c", Arg.Set compile,
      "       same as --compile";
    "--run", Arg.Set run,
         "    execute the given .cdo files";
    "--verbose", Arg.Set Cduce.verbose,
             "(for --compile) show types of exported values";
    "--obj-dir",  Arg.String (fun s -> out_dir := s :: !out_dir),
             "(for --compile) directory for the compiled .cdo file";
    "-I", Arg.String (fun s -> Librarian.obj_path := s::!Librarian.obj_path),
      "       add one directory to the lookup path for .cdo and .cmi files";
    "--stdin", Arg.Unit (fun () -> src := "" :: !src),
           "  read CDuce script on standard input";
    "--arg", Arg.Rest (fun s -> args := s :: !args),
         "    following arguments are passed to the CDuce program";
    "--script", Arg.Rest (fun s -> 
			    if not !script then (script := true;
						src := s :: !src)
			    else args := s :: !args),
            " the first argument after is the source, then the arguments";
    "--no", Arg.String Config.inhibit,
        "     disable a feature (cduce -v to get a list of features)";
    "--debug", Arg.Unit (fun () -> Stats.set_verbosity Stats.Summary),
           "  print profiling/debugging information";
    "-v", Arg.Unit version,
      "       print CDuce version, and list built-in optional features";
    "--version", Arg.Unit version,
             "print CDuce version, and list built-in optional features";
    "--mlstub", Arg.Set mlstub,
            " produce stub ML code from a compiled unit";
    "--topstub", Arg.Set topstub,
             "produce stub ML code for a toplevel from a primitive file";
 ]

let ppf = Format.std_formatter
let ppf_err = Format.err_formatter

let err s =
  prerr_endline s;
  exit 1

let mode () =
  Arg.parse (specs @ !Cduce.extra_specs) (fun s -> src := s :: !src) 
    "Usage:\ncduce [OPTIONS ...] [FILE ...] [--arg argument ...]\n\nOptions:";
  if (!mlstub) then (
    match !src with [x] -> `Mlstub x | _ ->
      err "Please specify one .cdo file"
  ) else if (!topstub) then (
    match !src with [x] -> `Topstub x | _ ->
      err "Please specify one primitive file"
  ) else match (!compile,!out_dir,!run,!src,!args) with
    | false, _::_, _,  _, _   -> 
	err "--obj-dir option can be used only with --compile"
    | false, [], false, [],  args   -> `Toplevel args
    | false, [], false, [x], args   -> `Script (x,args)
    | false, [], false, _, _        ->
	err "Only one CDuce program can be executed at a time"
    | true,  [o], false, [x], []     -> `Compile (x,Some o) 
    | true,  [], false, [x], []     -> `Compile (x,None) 
    | true,  [], false, [], []      ->
	err "Please specify the CDuce program to be compiled"
    | true,  [], false, _, []       ->
	err "Only one CDuce program can be compiled at a time"
    | true,  _, false, _, []        ->
	err "Please specify only one output directory"
    | true,  _, false, _, _        ->
	err "No argument can be passed to programs at compile time"
    | false, _, true,  [x], args   -> `Run (x,args)
    | false, _, true,  [], _       ->
	err "Please specifiy the CDuce program to be executed"
    | false, _, true,   _, _       ->
	err "Only one CDuce program can be executed at a time"
    | true, _, true,   _,  _       ->
	err "The options --compile and --run are incompatible"
	


let bol = ref true

let outflush s =
  output_string stdout s;
  flush stdout

let toploop () =
  let restore = 
    try 
      let tcio = Unix.tcgetattr Unix.stdin in
      Unix.tcsetattr 
	Unix.stdin Unix.TCSADRAIN { tcio with Unix.c_vquit = '\004' };
      fun () -> Unix.tcsetattr Unix.stdin Unix.TCSADRAIN tcio
    with Unix.Unix_error (_,_,_) -> 
      fun () -> ()
  in
  let quit () = 
    outflush "\n";
    restore ();
    exit 0
  in
  Format.fprintf ppf "        CDuce version %s\n@." <:symbol<cduce_version>>;
  Sys.set_signal Sys.sigquit (Sys.Signal_handle (fun _ -> quit ()));
  Sys.catch_break true;
  Cduce.toplevel := true;
  Librarian.run_loaded := true;
  let buf_in = Buffer.create 1024 in
  Location.push_source (`Buffer buf_in);
  let read _i =
    if !bol then 
      if !Ulexer.in_comment then outflush "* " else outflush "> ";
    try 
      let c = input_char stdin in
      Buffer.add_char buf_in c;
      bol := c = '\n';
      Some c
    with Sys.Break -> quit () 
  in
  let input = Stream.from read in
  let rec loop () =
    outflush "# ";
    bol := false;
    Buffer.clear buf_in;
    ignore (Cduce.topinput ppf ppf_err input);
    while (input_char stdin != '\n') do () done;
    loop () in
  (try loop () with End_of_file -> ());
  restore ()

let argv args = 
  Value.sequence (List.rev_map Value.string_latin1 args)

let main () = 
  at_exit (fun () -> Stats.dump Format.std_formatter);
  Location.set_viewport (Html.create false);
  match mode () with
    | `Toplevel args ->
	Config.init_all ();
	Builtin.argv := argv args;
	toploop ()
    | `Script (f,args) ->
	Config.init_all ();
	Builtin.argv := argv args;
	Cduce.compile_run f
    | `Compile (f,o) ->
	Config.init_all ();
	Cduce.compile f o
    | `Run (f,args) ->
	Config.init_all ();
	Builtin.argv := argv args;
	Cduce.run f
    | `Mlstub f ->
	Config.init_all ();
	Librarian.prepare_stub f
    | `Topstub f ->
	Config.init_all ();
	!Librarian.make_wrapper f
