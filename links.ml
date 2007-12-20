open Performance
open Getopt
open Utility
open List
open Basicsettings

(** The prompt used for interactive mode *)
let ps1 = "links> "

(* Types of built-in primitives.  TODO: purge. *)
let stdenvs = ref([], Library.typing_env)

type envs = Result.environment * Types.typing_environment

(** Definition of the various repl directives *)
let rec directives 
    : (string
     * ((envs ->  string list -> envs)
        * string)) list Lazy.t =

  let ignore_envs fn (envs : envs) arg = let _ = fn arg in envs in lazy
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
          StringSet.iter (fun n ->
                       Printf.fprintf stderr " %-16s : %s\n" 
                         n (Types.string_of_datatype (Env.String.lookup Library.type_env n)))
            (Env.String.domain Library.type_env)),
     "list builtin functions and values");

    "quit",
    (ignore_envs (fun _ -> exit 0), "exit the interpreter");

    "typeenv",
    ((fun ((_, (typeenv, _)) as envs) _ ->
        StringSet.iter (fun k ->
                Printf.fprintf stderr " %-16s : %s\n"
                  k (Types.string_of_datatype (Env.String.lookup typeenv k)))
          (StringSet.diff (Env.String.domain typeenv) (Env.String.domain Library.type_env));
        envs),
     "display the current type environment");

    "env",
    ((fun ((valenv, _) as envs) _ ->
        iter (fun (v, k) ->
                     Printf.fprintf stderr " %-16s : %s\n"
                       v (Result.string_of_result k))
          (filter (not -<- Library.is_primitive -<- fst)  valenv);
     envs),
     "display the current value environment");

    "load",
    ((fun (envs : Result.environment * Types.typing_environment) args ->
        match args with
          | [filename] ->
              let library_types, libraries =
                (Errors.display_fatal Loader.read_file_cache (Settings.get_value prelude_file)) in 
              let libraries, _ = Interpreter.run_program [] [] libraries in
              let sugar, pos_context = Parse.parse_file ~pp:(Settings.get_value pp) Parse.program filename in
              let resolve = Parse.retrieve_code pos_context in
              let (bindings, expr), _, _ = Frontend.Pipeline.program Library.typing_env resolve sugar in
              let defs = Sugar.desugar_definitions resolve bindings in
              let expr = opt_map (Sugar.desugar_expression resolve) expr in
              let program = Syntax.Program (defs, from_option (Syntax.unit_expression (`U Syntax.dummy_position)) expr) in
              let (typingenv : Types.typing_environment), program = Inference.type_program library_types program in
              let program = Optimiser.optimise_program (typingenv, program) in
              let program = Syntax.labelize program in
                (fst ((Interpreter.run_program libraries []) program), (typingenv : Types.typing_environment))
          | _ -> prerr_endline "syntax: @load \"filename\""; envs),
     "load in a Links source file, replacing the current environment");

    "withtype",
    ((fun (_, ((tenv, alias_env):Types.typing_environment) as envs) args ->
        match args with 
          [] -> prerr_endline "syntax: @withtype type"; envs
          | _ -> let t = DesugarDatatype.read_datatype (String.concat " " args) in
              StringSet.iter
                (fun id -> 
                      if id <> "_MAILBOX_" then
                        (try begin
                           let t' = Env.String.lookup tenv id in
                           let ttype = Types.string_of_datatype t' in
                           let fresh_envs = Types.make_fresh_envs t' in
                           let t' = Instantiate.datatype fresh_envs t' in 
                             Inference.unify alias_env (t,t');
                             Printf.fprintf stderr " %s : %s\n" id ttype
                         end with _ -> ()))
                (Env.String.domain tenv)
              ; envs),
     "search for functions that match the given type");

  ]

let execute_directive (name, args) (valenv, typingenv) = 
  let envs = 
    (try fst (assoc name (Lazy.force directives)) (valenv, typingenv) args; 
     with NotFound _ -> 
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

(** type, optimise and evaluate a program *)
let process_program ?(printer=print_result) (valenv, typingenv) 
    ((program : Syntax.untyped_program),
     (sugar : (Sugartypes.binding list * Sugartypes.phrase option) * (Sugartypes.position -> Syntax.position))) = 

  let typingenv, program = lazy (Inference.type_program typingenv program) 
    <|measure_as|> "type_program" in
  let program = lazy (Optimiser.optimise_program (typingenv, program))
    <|measure_as|> "optimise_program" in
  let Syntax.Program (_, body) as program = Syntax.labelize program in
  let valenv, result = lazy (Interpreter.run_program valenv [] program)
    <|measure_as|> "run_program" 
  in
    printer (Syntax.node_datatype body) result;
    (valenv, typingenv), result, program

(* Read Links source code, then type, optimize and run it. *)
let evaluate ?(handle_errors=Errors.display_fatal) parse program = 
  handle_errors (measure "parse" parse ->- process_program program)

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
              | (`Definitions [] : Sugartypes.sentence'), _ -> envs
              | `Definitions defs, ((`Definitions sugar : Sugartypes.sentence), lookup) -> 
                  let (valenv, _ as envs), _, (Syntax.Program (defs', body) as program) =
                    process_program
                      ~printer:(fun _ _ -> ())
                      envs
                      ((Syntax.Program (defs, Syntax.unit_expression (`U Syntax.dummy_position))),
                       ((sugar, None), lookup))
                  in
                    ListLabels.iter defs'
                       ~f:(function
                             | Syntax.Define (name, _, _, _) as d -> 
                                 prerr_endline (name
                                                ^" = "^
                                                Result.string_of_result (List.assoc name valenv)
                                                ^" : "^ 
                                                Types.string_of_datatype (Syntax.def_datatype d))
                             | _ -> () (* non-value definition (type, fixity, etc.) *));
                    envs
              | `Expression expr, (`Expression sexpr, lookup) -> 
                  let envs, _, _ = process_program envs ((Syntax.Program ([], expr)), (([], Some sexpr), lookup)) in envs 
              | `Directive directive, _ -> execute_directive directive envs))
    in
      print_string ps1; flush stdout; 

      let parse_thingy input = 
        let sugar, pos_context = Parse.parse_channel ~interactive:(make_dotter ps1) Parse.interactive input in
        let resolve = Parse.retrieve_code pos_context in
        let sentence, _, _ = Frontend.Pipeline.interactive Library.typing_env resolve sugar in
        let sentence' = match sentence with
          | `Definitions defs -> `Definitions (Sugar.desugar_definitions resolve defs)
          | `Expression e -> `Expression (Sugar.desugar_expression resolve e) 
          | `Directive d -> `Directive d in
          sentence', (sentence, resolve) in
      interact (evaluate_replitem parse_thingy envs (stdin, "<stdin>"))
  in 
    Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> raise Sys.Break));
    interact envs

let run_file prelude envs filename = 
  Settings.set_value interacting false;
  begin match Utility.getenv "REQUEST_METHOD" with 
    | Some _ -> 
        Settings.set_value web_mode true
    | None -> ()
  end;
  if Settings.get_value web_mode then 
    Webif.serve_request prelude envs filename
  else 
      let parse_thingy filename = 
        let sugar, pos_context = Parse.parse_file ~pp:(Settings.get_value pp) Parse.program filename in
        let resolve = Parse.retrieve_code pos_context in
        let (bindings, expr), _, _ = Frontend.Pipeline.program Library.typing_env resolve sugar in
        let defs = Sugar.desugar_definitions resolve bindings in
        let expr' = opt_map (Sugar.desugar_expression resolve) expr in
        let program = Syntax.Program (defs, from_option (Syntax.unit_expression (`U Syntax.dummy_position)) expr') in
          program, ((bindings, expr), resolve) in
        ignore (evaluate parse_thingy envs filename)

let evaluate_string_in envs v =
  (Settings.set_value interacting false;

      let parse_thingy s = 
        let sugar, pos_context = Parse.parse_string ~pp:(Settings.get_value pp) Parse.program s in
        let resolve = Parse.retrieve_code pos_context in
        let (bindings, expr), _, _ = Frontend.Pipeline.program Library.typing_env resolve sugar in
        let defs = Sugar.desugar_definitions resolve bindings in
        let expr' = opt_map (Sugar.desugar_expression resolve) expr in
        let program = Syntax.Program (defs, from_option (Syntax.unit_expression (`U Syntax.dummy_position)) expr') in
          program, ((bindings, expr), resolve) in

   ignore(evaluate parse_thingy envs v))

let cmd_line_actions = ref []

let run_tests tests () = 
  begin
    Test.run tests;
    exit 0
  end

let config_file = (ref None : string option ref)

let options : opt list = 
  let set setting value = Some (fun () -> Settings.set_value setting value) in
  [
    ('d',     "debug",               set Debug.debugging_enabled true, None);
    ('w',     "web-mode",            set web_mode true,                None);
    ('O',     "optimize",            set Optimiser.optimising true,    None);
    (noshort, "measure-performance", set measuring true,               None);
    ('n',     "no-types",            set printing_types false,         None);
    ('e',     "evaluate",            None,                             Some (fun str -> push_back (`Evaluate str) cmd_line_actions));
    (noshort, "config",              None,                             Some (fun name -> config_file := Some name));
    (noshort, "dump",                None,                             Some Loader.dump_cached);
    (noshort, "test",                Some (fun _ -> SqlcompileTest.test(); exit 0),     None);
    (noshort, "working-tests",       Some (run_tests Tests.working_tests),                  None);
    (noshort, "broken-tests",        Some (run_tests Tests.broken_tests),                   None);
    (noshort, "failing-tests",       Some (run_tests Tests.known_failures),                 None);
    (noshort, "pp",                  None,                             Some (Settings.set_value pp));
    ]

let main () =
  config_file := (try Some (Unix.getenv "LINKS_CONFIG") with _ -> !config_file);
  let file_list = ref [] in
  Errors.display_fatal_l (lazy (parse_cmdline options (fun i -> push_back i file_list)));
  (match !config_file with None -> () 
     | Some file -> Settings.load_file file);
  (* load prelude: *)
  let prelude_types, (Syntax.Program (prelude, _) as prelude_program) =
    (Errors.display_fatal Loader.read_file_cache (Settings.get_value prelude_file)) in

  (* make sure the fresh type variable counter does not clash with any
     type variables from the prelude

     BUG: we should really do something similar for term variables
  *)
  let _ =
    let max_or (set : Types.TypeVarSet.t) (m : int) = 
      if Types.TypeVarSet.is_empty set then
        m
      else max m (Types.TypeVarSet.max_elt set) in
    let max_prelude_tvar =
      List.fold_right max_or 
        (Env.String.range (Env.String.map Types.free_bound_type_vars (fst prelude_types)))
        (Types.TypeVarSet.max_elt (Syntax.free_bound_type_vars_program prelude_program)) in
      Types.bump_variable_counter max_prelude_tvar
  in
    
  let prelude_compiled = Interpreter.run_defs [] [] prelude in
    (let (stdvalenv, stdtypeenv) = !stdenvs in
       stdenvs := 
         (stdvalenv @ prelude_compiled,
          Types.concat_typing_environment stdtypeenv prelude_types));
    Utility.for_each !cmd_line_actions
      (function `Evaluate str -> evaluate_string_in !stdenvs str);
  (* TBD: accumulate type/value environment so that "interact" has access *)
  ListLabels.iter ~f:(run_file prelude (prelude_compiled, prelude_types)) !file_list;
  if Settings.get_value(interacting) then
    begin
      print_endline (Settings.get_value(welcome_note));
      interact (prelude_compiled, prelude_types)
    end

let _ = 
  prerr_endline "it's me";
  flush stderr;
  main ()
