open Getopt
open Utility
open Debug

let load_file filename =
  let ast_program = Parse.parse_file Parse.program filename in
  let typingenv, exprs = Inference.type_program Library.typing_env ast_program in
  let exprs = List.map Syntax.labelize exprs in
    typingenv, exprs

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
let stdenvs = [], Library.typing_env

(* shell directives *)
let ignore_envs fn envs arg = let _ = fn arg in envs
let rec directives = lazy (* lazy so we can have applications on the rhs *)
  [
    "directives", 
    (ignore_envs 
       (fun _ -> 
          List.iter (fun (n, (_, h)) -> Printf.fprintf stderr " @%-20s : %s\n" n h) (Lazy.force directives)),
     "list available directives");
    
    "settings",
    (ignore_envs
       (fun _ -> 
          List.iter (Printf.fprintf stderr " %s\n") (Settings.print_settings ())),
     "print available settings");
    
    "set",
    (ignore_envs
       (function (name::value::_) -> Settings.parse_and_set_user (name, value)
          | _ -> prerr_endline "syntax : @set name value"),
     "change the value of a setting");
    
    "builtins",
    (ignore_envs 
       (fun _ ->
          List.iter (fun (n, k) ->
                       Printf.fprintf stderr " %-16s : %s\n" 
                         n (Types.string_of_datatype (snd k)))
          Library.type_env),
     "list builtin functions and values");

    "quit",
    (ignore_envs (fun _ -> exit 0), "exit the interpreter");

    "typeenv",
    ((fun ((_, (typeenv, _)) as envs) _ ->
        List.iter (fun (v, k) ->
                     Printf.fprintf stderr " %-16s : %s\n"
                       v (Types.string_of_datatype (snd k)))
          (List.filter (not -<- (flip List.mem_assoc Library.type_env) -<- fst) typeenv);
        envs),
    "display the current type environment");

    "env",
    ((fun ((valenv, _) as envs) _ ->
        List.iter (fun (v, k) ->
                     Printf.fprintf stderr " %-16s : %s\n"
                       v (Result.string_of_result k))
          (List.filter (not -<- (flip List.mem_assoc !Library.value_env) -<- fst)  valenv);
     envs),
     "display the current value environment");

    "load",
    ((fun envs args ->
        match args with
          | [filename] ->
              let library_types, libraries =
                (Errors.display_errors_fatal stderr load_file "prelude.links") in 
              let libraries, _ = Interpreter.run_program [] libraries in
              let program = Parse.parse_file Parse.program filename in
              let typingenv, exprs = Inference.type_program library_types program in
              let exprs = Optimiser.optimise_program (typingenv, exprs) in
                (fst ((Interpreter.run_program libraries) (List.map Syntax.labelize exprs)), typingenv)
          | _ -> prerr_endline "syntax: @load \"filename\""; envs),
     "load in a Links source file, replacing the current environment");
  ]

let execute_directive (name, args) valenv typingenv = 
  let envs = 
    (try fst (List.assoc name (Lazy.force directives)) (valenv, typingenv) args; 
     with Not_found -> 
       Printf.fprintf stderr "unknown directive : %s\n" name;
       (valenv, typingenv))
  in
    flush stderr;
    envs
    

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
let evaluate ?(handle_errors=Errors.display_errors_fatal stderr) parse (valenv, typingenv) input = 
  handle_errors
    (fun input ->
       let exprs =          Performance.measure "parse" parse input in 
       let typingenv, exprs = Performance.measure "type_program" (Inference.type_program typingenv) exprs in
       let exprs =          Performance.measure "optimise_program" Optimiser.optimise_program (typingenv, exprs) in
       let exprs = List.map Syntax.labelize exprs in
       let valenv, result = Performance.measure "run_program" (Interpreter.run_program valenv) exprs in
         print_result (Syntax.node_datatype (last exprs)) result;
         (valenv, typingenv), result
    ) input

(* Read Links source code, then type and optimize it. *)
let just_optimise parse (valenv, typingenv) input = 
  Settings.set_value interacting false;
  let parse = parse Parse.program in
  let exprs = Performance.measure "parse" parse input in 
  let typingenv, exprs = Performance.measure "type_program" (Inference.type_program typingenv) exprs in
  let exprs = Performance.measure "optimise_program" Optimiser.optimise_program (typingenv, exprs) in
    print_endline (mapstrcat "\n" Syntax.string_of_expression exprs)

(* Interactive loop *)
let rec interact envs =
  let evaluate ?(handle_errors=Errors.display_errors_fatal stderr) parse (valenv, typingenv) input = 
    handle_errors
      (fun input ->
         match Performance.measure "parse" parse input with 
           | Left exprs -> 
               let typingenv, exprs = Performance.measure "type_program" (Inference.type_program typingenv) exprs in
               let exprs = Performance.measure "optimise_program" Optimiser.optimise_program (typingenv, exprs) in
               let exprs = List.map Syntax.labelize exprs in
               let valenv, result = Performance.measure "run_program" (Interpreter.run_program valenv) exprs in
                 print_result (Syntax.node_datatype (last exprs)) result;
                 (valenv, typingenv)
           | Right (directive : Sugartypes.directive) -> 
               execute_directive directive valenv typingenv)
      input
  in
  let error_handler = Errors.display_errors stderr (fun _ -> envs) in
    print_string ps1; flush stdout; 
    interact (evaluate ~handle_errors:error_handler (Parse.parse_channel Parse.interactive) envs (stdin, "<stdin>"))
      
 let run_file libraries envs filename = 
  Settings.set_value interacting false;
  match Utility.getenv "REQUEST_METHOD" with 
    | Some _ -> 
        (Settings.set_value web_mode true;
         Webif.serve_request libraries envs filename)
    | None ->
        ignore (evaluate (Parse.parse_file Parse.program) envs filename)

let evaluate_string v =
  (Settings.set_value interacting false;
   ignore(evaluate (Parse.parse_string Parse.program) stdenvs v))

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

let set setting value = Some (fun () -> Settings.set_value setting value)

let options : opt list = 
    [
      ('d',     "debug",               set Debug.debugging_enabled true, None);
      ('O',     "optimize",            set Optimiser.optimising true,    None);
      (noshort, "measure-performance", set Performance.measuring true,   None);
      ('n',     "no-types",            set printing_types false,         None);
      ('e',     "evaluate",            None,                             Some evaluate_string);
      (noshort, "config",              None,                             Some load_settings);

      (* Modes to just optimise a program and print the result. I'm
         not crazy about these option letters*)
      ('o',     "print-optimize",      None,                             Some (just_optimise Parse.parse_file stdenvs));
      ('q',     "print-optimize-expr", None,                             Some (just_optimise Parse.parse_string stdenvs));
    ]

let welcome_note = Settings.add_string ("welcome_note", "Welcome to Links", false)

(* main *)
let _ =
  let file_list = ref [] in
  Errors.display_errors_fatal stderr (parse_cmdline options) (push file_list);
  (* load prelude *)
  let library_types, libraries =
    (Errors.display_errors_fatal stderr load_file "prelude.links") in 
  (* TBD: accumulate type/value environment so that "interact" has access *)
  ListLabels.iter ~f:(run_file libraries ([], library_types)) !file_list;
  if Settings.get_value(interacting) then
    begin
      print_endline (Settings.get_value(welcome_note));
      let libraries, _ = Interpreter.run_program [] libraries in
      interact (libraries, library_types)
    end

