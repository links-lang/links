open Getopt
open Utility
open Debug

(*
 Whether to run the interactive loop
 (default is true)
*)
let interacting = Settings.add_bool ("interacting", true, false)

(* Whether we're in web mode or not *)
let web_mode = Settings.add_bool ("web_mode", false, false)

(* Whether to print types *)
let printing_types = Settings.add_bool ("printing_types", true, true)

(* Prompt in interactive mode *)
let ps1 = "links> "

(* Builtin environments *)
let stdenvs = [], Library.type_env

(* shell directives *)
let rec directives = 
  [
    "directives", 
    ((fun _ -> 
        List.iter (fun (n, (_, h)) -> Printf.fprintf stderr " @%-20s : %s\n" n h) directives),
     "list available directives");
     
    "settings",
    ((fun _ -> 
        List.iter (Printf.fprintf stderr " %s\n") (Settings.print_settings ())),
     "print available settings");
    
    "set",
    ((function (name::value::_) -> Settings.parse_and_set_user (name, value)
        | _ -> prerr_endline "syntax : @set name value"),
     "change the value of a setting");
    
    "builtins",
    ((fun _ ->
        List.iter (fun (n, k) ->
                     Printf.fprintf stderr " %-16s : %s\n" 
                       n (Types.string_of_datatype (snd k)))
          Library.type_env),
     "list builtin functions and values");

    "quit",
    ((fun _ -> exit 0), "exit the interpreter");
    ]
let execute_directive name args = 
  try fst (List.assoc name directives) args; flush stderr
  with Not_found -> Printf.fprintf stderr "unknown directive : %s\n" name; flush stderr

(* Run unit tests *)
let run_tests () = 
  Settings.set_value interacting false;
(*   Optimiser.test () ; *)
(*   Js.test () *)
  Js.run_tests ()

(* Print a result, including its type if `printing_types' is true. *)
let print_result rtype result = 
  print_string (Result.string_of_result result);
  print_endline (if Settings.get_value(printing_types) then
		   Types.with_mailbox_typing (Settings.get_value(Types.show_mailbox_annotations))
		     (fun () -> 
			" : "^ Types.string_of_datatype rtype)
                 else "")

(* Read Links source code, then type, optimize and run it. *)
let evaluate ?(handle_errors=Errors.display_errors_fatal stderr) parse (valenv, typeenv) input = 
  handle_errors
    (fun input ->
       let exprs =          Performance.measure "parse" parse input in 
       let typeenv, exprs = Performance.measure "type_program" (Inference.type_program typeenv) exprs in
       let exprs =          Performance.measure "optimise_program" Optimiser.optimise_program (typeenv, exprs) in
       let exprs = List.map Syntax.labelize exprs in
       let valenv, result = Performance.measure "run_program" (Interpreter.run_program valenv) exprs in
         print_result (Syntax.node_datatype (last exprs)) result;
         (valenv, typeenv), result
    ) input


(* Interactive loop *)
let rec interact envs = 
let evaluate ?(handle_errors=Errors.display_errors_fatal stderr) parse (valenv, typeenv) input = 
  handle_errors
    (fun input ->
       match Performance.measure "parse" parse input with 
         | Left exprs -> 
             let typeenv, exprs = Performance.measure "type_program" (Inference.type_program typeenv) exprs in
             let exprs =          Performance.measure "optimise_program" Optimiser.optimise_program (typeenv, exprs) in
             let exprs = List.map Syntax.labelize exprs in
             let valenv, result = Performance.measure "run_program" (Interpreter.run_program valenv) exprs in
               print_result (Syntax.node_datatype (last exprs)) result;
               (valenv, typeenv)
         | Right (directive : Sugar.directive) -> 
             begin
               (Utility.uncurry execute_directive) directive;
               (valenv, typeenv)
             end)
    input
in
let error_handler = Errors.display_errors stderr (fun _ -> envs) in
    print_string ps1; flush stdout; 
    interact (evaluate ~handle_errors:error_handler Parse.parse_sentence envs (stdin, "<stdin>"))

(** testenv
    Test whether an environment variable is set.
    TBD: Move me to Utility *)
let testenv env_var = 
  try 
    let _ = Sys.getenv env_var in true
  with Not_found -> false

let run_file filename = 
  if testenv "REQUEST_METHOD" then
    (Settings.set_value interacting false;
     Settings.set_value web_mode true;
     Webif.serve_request filename)
  else
    (ignore(evaluate Parse.parse_file stdenvs filename);
    ())

let set setting value = Some (fun () -> Settings.set_value setting value)

let evaluate_string v =
  (Settings.set_value interacting false;
   ignore(evaluate Parse.parse_string stdenvs v))

let load_settings filename =
  let file = open_in filename in

  let strip_comment s = 
    if String.contains s '#' then
      let i = String.index s '#' in
	String.sub s 0 i
    else
      s in

  let is_empty s =
    let empty = ref true in
      String.iter (fun c ->
		     if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) then
		       empty := false) s;
      !empty in

  let parse_line n s =
    let s = strip_comment s in
      if not (is_empty s) then
	(* ignore 'empty' lines *)
	begin
	  if String.contains s '=' then
	    begin
	      let i = String.index s '=' in
	      let name = String.sub s 0 i in
	      let value = String.sub s (i+1) ((String.length s) - (i+1))
	      in
		Settings.parse_and_set (name, value)
	    end
	  else
	    failwith ("Error in configuration file (line "^string_of_int n^"): '"^s^"'\n"^
			"Configuration options must be of the form <name>=<value>")
	end in
    
  let rec parse_lines n =
    try
      parse_line n (input_line file);
      parse_lines (n+1)
    with
	End_of_file -> close_in file
  in
    parse_lines 1

let options : opt list = 
    [
      ('d',     "debug",               set Debug.debugging_enabled true, None);
      ('O',     "optimize",            set Optimiser.optimising true,    None);
      (noshort, "measure-performance", set Performance.measuring true,   None);
      ('n',     "no-types",            set printing_types false,         None);
      ('e',     "evaluate",            None,                             Some evaluate_string);
      (noshort, "config",              None,                             Some load_settings)
(* [DEACTIVATED] *)
(*
      ('t',     "run-tests",           Some run_tests,                   None);
*)    ]

(* main *)
let _ =
  Errors.display_errors_fatal stderr (parse_cmdline options) run_file;
  if Settings.get_value(interacting) then interact stdenvs
