open Performance
open Getopt
open Utility
open List
open Basicsettings

(** The prompt used for interactive mode *)
let ps1 = "links> "

(* Types of built-in primitives.  TODO: purge. *)
let stdenvs = ref([], Library.typing_env)

(** Definition of the various repl directives *)
let rec directives = 
  let ignore_envs fn envs arg = let _ = fn arg in envs in lazy
  (* lazy so we can have applications on the rhs *)
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
                         n (Types.string_of_datatype (snd k)))
          Library.type_env),
     "list builtin functions and values");

    "quit",
    (ignore_envs (fun _ -> exit 0), "exit the interpreter");

    "typeenv",
    ((fun ((_, (typeenv, _)) as envs) _ ->
        iter (fun (v, k) ->
                     Printf.fprintf stderr " %-16s : %s\n"
                       v (Types.string_of_datatype (snd k)))
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
                (Errors.display_fatal Loader.read_file_cache (Settings.get_value prelude_file)) in 
              let libraries, _ = Interpreter.run_program [] [] libraries in
              let program = Parse.parse_file Parse.program filename in
              let typingenv, exprs = Inference.type_program library_types program in
              let exprs = Optimiser.optimise_program (typingenv, exprs) in
              let exprs = map Syntax.labelize exprs in
                (fst ((Interpreter.run_program libraries []) exprs), typingenv)
          | _ -> prerr_endline "syntax: @load \"filename\""; envs),
     "load in a Links source file, replacing the current environment");

    "withtype",
    ((fun (_, ((tenv, alias_env):Types.typing_environment) as envs) args ->
        match args with 
          [] -> prerr_endline "syntax: @withtype type"; envs
          | _ -> let _,t = Parse.parse_string Parse.datatype (String.concat " " args) in
              ListLabels.iter tenv
                ~f:(fun (id,(_,t')) -> 
                      if id <> "_MAILBOX_" then
                        (try begin
                           let ttype = Types.string_of_datatype t' in
                           let fresh_envs = Types.make_fresh_envs t' in
                           let t' = Instantiate.instantiate_datatype fresh_envs t' in 
                             Inference.unify alias_env (t,t');
                             Printf.fprintf stderr " %s : %s\n" id ttype
                         end with _ -> ()))
              ; envs),
     "search for functions that match the given type");

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
    
(** Print a result (including its type if `printing_types' is [true]). *)
let print_result rtype result = 
  print_string (Result.string_of_result result);
  print_endline (if Settings.get_value(printing_types) then
		   " : "^ Types.string_of_datatype rtype
                 else "")

(** type, optimise and evaluate a list of expressions *)
let process_one ?(printer=print_result) (valenv, typingenv) exprs = 
  let typingenv, exprs = lazy (Inference.type_program typingenv exprs) 
    <|measure_as|> "type_program" in
  let exprs = lazy (Optimiser.optimise_program (typingenv, exprs))
    <|measure_as|> "optimise_program" in
  let exprs = map Syntax.labelize exprs in
  let valenv, result = lazy (Interpreter.run_program valenv [] exprs)
    <|measure_as|> "run_program" 
  in
    printer (Syntax.node_datatype (last exprs)) result;
    (valenv, typingenv), result, exprs

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
let interact envs =
  let make_dotter ps1 = 
    let dots = String.make (String.length ps1 - 1) '.' ^ " " in
      fun _ -> 
        print_string dots;
        flush stdout in
  let rec interact envs =
    let evaluate_replitem parse envs input = 
      Errors.display ~default:(fun _ -> envs)
        (lazy
           (match measure "parse" parse input with 
              | `Definitions [] -> envs
              | `Definitions defs -> 
                  let (valenv, _ as envs), _, exprs = process_one ~printer:(fun _ _ -> ()) envs defs  
                  in ListLabels.iter exprs
                       ~f:(function
                             | Syntax.Define (name, _, _, _) as d -> 
                                 prerr_endline (name
                                                ^" = "^
                                                Result.string_of_result (List.assoc name valenv)
                                                ^" : "^ 
                                                Types.string_of_datatype (Syntax.node_datatype d))
                             | _ -> () (* non-value definition (type, fixity, etc.) *));
                    envs
              | `Expression expr -> 
                  let envs, _, _ = process_one envs [expr] in envs
              | `Directive directive -> execute_directive directive envs))
    in
      print_string ps1; flush stdout; 
      interact (evaluate_replitem (Parse.parse_channel ~interactive:(make_dotter ps1) Parse.interactive) envs (stdin, "<stdin>"))
  in 
    Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> raise Sys.Break));
    interact envs

let run_file prelude envs filename = 
  Settings.set_value interacting false;
  match Utility.getenv "REQUEST_METHOD" with 
    | Some _ -> 
        (Settings.set_value web_mode true;
         Webif.serve_request prelude envs filename)
    | None ->
        ignore (evaluate (Parse.parse_file Parse.program) envs filename)

let evaluate_string_in envs v =
  (Settings.set_value interacting false;
   ignore(evaluate (Parse.parse_string Parse.program) envs v))

let cmd_line_actions = ref []

let run_tests tests () = 
  begin
    Test.run tests;
    exit 0
  end

let options : opt list = 
  let set setting value = Some (fun () -> Settings.set_value setting value) in
  [
    ('d',     "debug",               set Debug.debugging_enabled true, None);
    ('O',     "optimize",            set Optimiser.optimising true,    None);
    (noshort, "measure-performance", set measuring true,               None);
    ('n',     "no-types",            set printing_types false,         None);
    ('e',     "evaluate",            None,                             Some (fun str -> push cmd_line_actions (`Evaluate str)));
    (noshort, "config",              None,                             Some Settings.load_file);
    (noshort, "dump",                None,                             Some Loader.dump_cached);
    (noshort, "working-tests",               Some (run_tests Tests.working_tests),                   None);
    (noshort, "broken-tests",                Some (run_tests Tests.broken_tests),                   None);
    (noshort, "failing-tests",               Some (run_tests Tests.known_failures),                   None);
    ('q',     "print-optimize-expr", None,                             Some (just_optimise Parse.parse_string (!stdenvs)));

    ]

let main () =
  let file_list = ref [] in
  Errors.display_fatal_l (lazy (parse_cmdline options (push file_list)));
  (* load prelude: *)
  let prelude_types, prelude =
    (Errors.display_fatal Loader.read_file_cache (Settings.get_value prelude_file)) in

  let (prelude_compiled, _) = Interpreter.run_program [] [] prelude in
    (let (stdvalenv, stdtypeenv) = !stdenvs in
       stdenvs := 
         (stdvalenv @ prelude_compiled,
          Types.concat_environment stdtypeenv prelude_types));
    Utility.for_each !cmd_line_actions
      (function `Evaluate str -> evaluate_string_in !stdenvs str);
  (* TBD: accumulate type/value environment so that "interact" has access *)
  ListLabels.iter ~f:(run_file prelude (prelude_compiled, prelude_types)) !file_list;
  if Settings.get_value(interacting) then
    begin
      print_endline (Settings.get_value(welcome_note));
      interact (prelude_compiled, prelude_types)
    end

let _ = main ()

