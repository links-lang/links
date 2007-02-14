open Performance
open Getopt
open Utility
open List

let load_file filename =
  let typingenv, exprs = Loader.read_file_cache filename in
(*   let exprs = map Syntax.labelize exprs in *)
    typingenv, exprs

(**
 Whether to run the interactive loop
 (default is true)
*)
let interacting = Settings.add_bool ("interacting", true, `System)

(** [true] if we're in web mode *)
let web_mode = Settings.add_bool ("web_mode", false, `System)

(** Set this to [true] to print types when printing results. *)
let printing_types = Settings.add_bool ("printing_types", true, `User)

(** Name of the file containing the prelude code. *)
let prelude = Settings.add_string ("prelude", "prelude.links", `System)

(** The prompt used for interactive mode *)
let ps1 = "links> "

(* Types of built-in primitives *)
let stdenvs = ref([], Library.typing_env)

let ignore_envs fn envs arg = let _ = fn arg in envs
(** Definition of the various repl directives *)
let rec directives = lazy (* lazy so we can have applications on the rhs *)
  [
    "directives", 
    (ignore_envs 
       (fun _ -> 
          iter (fun (n, (_, h)) -> Printf.fprintf stderr " @%-20s : %s\n" n h) (Lazy.force directives)),
     "list available directives");
    
    "settings",
    (ignore_envs
       (fun _ -> 
          iter (Printf.fprintf stderr " %s\n") (Settings.print_settings ())),
     "print available settings");
    
    "set",
    (ignore_envs
       (function (name::value::_) -> Settings.parse_and_set_user (name, value)
          | _ -> prerr_endline "syntax : @set name value"),
     "change the value of a setting");
    
    "builtins",
    (ignore_envs 
       (fun _ ->
          iter (fun (n, k) ->
                       Printf.fprintf stderr " %-16s : %s\n" 
                         n (Inferencetypes.string_of_datatype (snd k)))
          Library.type_env),
     "list builtin functions and values");

    "quit",
    (ignore_envs (fun _ -> exit 0), "exit the interpreter");

    "typeenv",
    ((fun ((_, (typeenv, _)) as envs) _ ->
        iter (fun (v, k) ->
                     Printf.fprintf stderr " %-16s : %s\n"
                       v (Inferencetypes.string_of_datatype (snd k)))
          (filter (not -<- (flip mem_assoc Library.type_env) -<- fst) typeenv);
        envs),
    "display the current type environment");

    "env",
    ((fun ((valenv, _) as envs) _ ->
        iter (fun (v, k) ->
                     Printf.fprintf stderr " %-16s : %s\n"
                       v (Result.string_of_result k))
          (filter (not -<- (flip mem_assoc !Library.value_env) -<- fst)  valenv);
     envs),
     "display the current value environment");

    "load",
    ((fun envs args ->
        match args with
          | [filename] ->
              let library_types, libraries =
                (Errors.display_fatal load_file (Settings.get_value prelude)) in 
              let libraries, _ = Interpreter.run_program [] [] libraries in
              let program = Parse.parse_file Parse.program filename in
              let typingenv, exprs = Inference.type_program library_types program in
              let exprs = Optimiser.optimise_program (typingenv, exprs) in
              let exprs = map Syntax.labelize exprs in
                (fst ((Interpreter.run_program libraries []) exprs), typingenv)
          | _ -> prerr_endline "syntax: @load \"filename\""; envs),
     "load in a Links source file, replacing the current environment");
  ]

let execute_directive (name, args) (valenv, typingenv) = 
  let envs = 
    (try fst (assoc name (Lazy.force directives)) (valenv, typingenv) args; 
     with Not_found -> 
       Printf.fprintf stderr "unknown directive : %s\n" name;
       (valenv, typingenv))
  in
    flush stderr;
    envs
    

(** Run unit tests; FIXME: fallen into disrepair. *)
let run_tests () = 
  Settings.set_value interacting false;
(*   Optimiser.test () ; *)
(*   Js.test () *)
  Js.run_tests ()

(** Print a result (including its type if `printing_types' is [true]). *)
let print_result rtype result = 
  print_string (Result.string_of_result result);
  print_endline (if Settings.get_value(printing_types) then
		   Inferencetypes.with_mailbox_typing (Settings.get_value(Inferencetypes.show_mailbox_annotations))
		     (fun () -> 
			" : "^ Inferencetypes.string_of_datatype rtype)
                 else "")

(** type, optimise and evaluate a list of expressions *)
let process_one (valenv, typingenv) exprs = 
  let typingenv, exprs = lazy (Inference.type_program typingenv exprs) 
    <|measure_as|> "type_program" in
  let exprs = lazy (Optimiser.optimise_program (typingenv, exprs))
    <|measure_as|> "optimise_program" in
  let exprs = map Syntax.labelize exprs in
  let valenv, result = lazy (Interpreter.run_program valenv [] exprs)
    <|measure_as|> "run_program" 
  in
    print_result (Syntax.node_datatype (last exprs)) result;
    (valenv, typingenv), result

(* Read Links source code, then type, optimize and run it. *)
let evaluate ?(handle_errors=Errors.display_fatal) parse envs = 
  handle_errors (measure "parse" parse ->- process_one envs)

(* Read Links source code, then type and optimize it. *)
let just_optimise parse (_valenv, typingenv) input = 
  Settings.set_value interacting false;
  let parse = parse Parse.program in
  let exprs = measure "parse" parse input in 
  let typingenv, exprs = measure_l "type_program"
    (lazy (Inference.type_program typingenv exprs)) in
  let exprs = measure_l "optimise_program"
    (lazy (Optimiser.optimise_program (typingenv, exprs))) in
    print_endline (mapstrcat "\n" Syntax.string_of_expression exprs)

(* Interactive loop *)
let rec interact envs =
  let evaluate_replitem parse envs input = 
    Errors.display ~default:(fun _ -> envs)
      (lazy
         (match measure "parse" parse input with 
           | Left exprs      -> fst (process_one envs exprs)
           | Right directive -> execute_directive directive envs)
      )
  in
    print_string ps1; flush stdout; 
    interact (evaluate_replitem (Parse.parse_channel Parse.interactive) envs (stdin, "<stdin>"))

let concat_envs (valenv1, typingenv1) (valenv2, typingenv2) =
  (valenv1 @ valenv2, Inferencetypes.concat_environment typingenv1 typingenv2)
      
let run_file envs filename = 
  Settings.set_value interacting false;
  match Utility.getenv "REQUEST_METHOD" with 
    | Some _ -> 
        (Settings.set_value web_mode true;
         Webif.serve_request envs filename)
    | None ->
        ignore (evaluate (Parse.parse_file Parse.program) envs filename)

(** Note: We have this notion of "stdenvs" (a global mutable list), but
    later on we ignore it when running the repl or loading a file.  We
    need to have a coherent policy for what is "the loaded environment," 
    what modifies it and which environments apply at a given time.
*)

let evaluate_string_in_stdenvs v =
  (Settings.set_value interacting false;
   ignore(evaluate (Parse.parse_string Parse.program) (!stdenvs) v))

let set setting value = Some (fun () -> Settings.set_value setting value)

type action = 
    [ `Evaluate of string
    | `LoadFile of string ]

let cmd_line_actions = ref []

let options : opt list = 
  [
    ('d',     "debug",               set Debug.debugging_enabled true, None);
    ('O',     "optimize",            set Optimiser.optimising true,    None);
    (noshort, "measure-performance", set measuring true,               None);
    ('n',     "no-types",            set printing_types false,         None);
    ('e',     "evaluate",            None,                             Some (fun str -> push cmd_line_actions (`Evaluate str)));
    (noshort, "config",              None,                             Some Settings.load_file);
    (noshort, "dump",                None,                             Some Loader.dump_cached);

    (* Modes to just optimise a program and print the result. I'm
       not crazy about these option letters *)
      ('o',     "print-optimize",      None,                             Some (just_optimise Parse.parse_file (!stdenvs)));
      ('q',     "print-optimize-expr", None,                             Some (just_optimise Parse.parse_string (!stdenvs)));
    ]

let welcome_note = Settings.add_string ("welcome_note", 
"  _    __ __   __ __   __ ____
 | |   | |  \\  | |  | / // .__|
 | |   | | , \\ | |  V  /|. `-.
 | |___| | |\\ \\  |     \\ _`.  |
 |_____|_|_| \\___|__|\\__|____/
Welcome to Links version 0.3.7 (Gogarloch)", `System)

(** main *)
let _ =
  let file_list = ref [] in
  Errors.display_fatal_l (lazy (parse_cmdline options (push file_list)));
  (* load prelude: *)
  let prelude_types, prelude =
    (Errors.display_fatal load_file (Settings.get_value prelude)) in

  let (prelude_compiled, _) = Interpreter.run_program [] [] prelude in
    (let (stdvalenv, stdtypeenv) = !stdenvs in
       stdenvs := 
         (stdvalenv @ prelude_compiled,
          Inferencetypes.concat_environment stdtypeenv prelude_types));
    Utility.for_each !cmd_line_actions
      (function `Evaluate str -> evaluate_string_in_stdenvs str);
  (* TBD: accumulate type/value environment so that "interact" has access *)
  ListLabels.iter ~f:(run_file (prelude_compiled, prelude_types)) !file_list;
  if Settings.get_value(interacting) then
    begin
      print_endline (Settings.get_value(welcome_note));
      interact (prelude_compiled, prelude_types)
    end

