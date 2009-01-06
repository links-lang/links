open Performance
open Getopt
open Utility
open List
open Basicsettings

(** The prompt used for interactive mode *)
let ps1 = "links> "

type envs = Value.env * Ir.var Env.String.t * Types.typing_environment

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
                            let t = Env.String.lookup Lib.type_env n in
                              Printf.fprintf stderr " %-16s : %s\n" 
                                n (Types.string_of_datatype t))
            (Env.String.domain Lib.type_env)),
     "list builtin functions and values");

    "quit",
    (ignore_envs (fun _ -> exit 0), "exit the interpreter");

    "typeenv",
    ((fun ((_, _, {Types.var_env = typeenv}) as envs) _ ->
        StringSet.iter (fun k ->
                          let t = Env.String.lookup typeenv k in
                            Printf.fprintf stderr " %-16s : %s\n" k (Types.string_of_datatype t))
          (StringSet.diff (Env.String.domain typeenv) (Env.String.domain Lib.type_env));
        envs),
     "display the current type environment");

(* TODO: adapt env and load to the new IR *)

(*     "env", *)
(*     ((fun ((valenv, _) as envs) _ -> *)
(*         iter (fun (v, k) -> *)
(*                      Printf.fprintf stderr " %-16s : %s\n" *)
(*                        v (Result.string_of_result k)) *)
(*           (filter (not -<- Library.is_primitive -<- fst)  valenv); *)
(*      envs), *)
(*      "display the current value environment"); *)

(*     "load", *)
(*     ((fun (envs : Result.environment * Types.typing_environment) args -> *)
(*         match args with *)
(*           | [filename] -> *)
(*               let library_types, libraries = *)
(*                 (Errors.display_fatal Oldloader.load_file Library.typing_env (Settings.get_value prelude_file)) in  *)
(*               let libraries, _ = Interpreter.run_program [] [] libraries in *)
(*               let sugar, pos_context = Parse.parse_file Parse.program filename in *)
(*               let (bindings, expr), _, _ = Frontend.Pipeline.program Library.typing_env pos_context sugar in *)
(*               let defs = Sugar.desugar_definitions bindings in *)
(*               let expr = opt_map Sugar.desugar_expression expr in *)
(*               let program = Syntax.Program (defs, from_option (Syntax.unit_expression (`U Syntax.dummy_position)) expr) in *)
(*               let (typingenv : Types.typing_environment), program = Inference.type_program library_types program in *)
(*               let program = Optimiser.optimise_program (typingenv, program) in *)
(*               let program = Syntax.labelize program in *)
(*                 (fst ((Interpreter.run_program libraries []) program), (typingenv : Types.typing_environment)) *)
(*           | _ -> prerr_endline "syntax: @load \"filename\""; envs), *)
(*      "load in a Links source file, replacing the current environment"); *)

    "withtype",
    ((fun (_, _, {Types.var_env = tenv; Types.tycon_env = aliases} as envs) args ->
        match args with 
          [] -> prerr_endline "syntax: @withtype type"; envs
          | _ -> let t = DesugarDatatypes.read ~aliases (String.concat " " args) in
              StringSet.iter
                (fun id -> 
                      if id <> "_MAILBOX_" then
                        (try begin
                           let t' = Env.String.lookup tenv id in
                           let ttype = Types.string_of_datatype t' in
                           let fresh_envs = Types.make_fresh_envs t' in
                           let t' = Instantiate.datatype fresh_envs t' in 
                             Unify.datatypes (t,t');
                             Printf.fprintf stderr " %s : %s\n" id ttype
                         end with _ -> ()))
                (Env.String.domain tenv)
              ; envs),
     "search for functions that match the given type");

  ]

let execute_directive (name, args) (valenv, nenv, typingenv) = 
  let envs = 
    (try fst (assoc name (Lazy.force directives)) (valenv, nenv, typingenv) args;
     with NotFound _ -> 
       Printf.fprintf stderr "unknown directive : %s\n" name;
       (valenv, nenv, typingenv))
  in
    flush stderr;
    envs

(** Print a value (including its type if `printing_types' is [true]). *)
let print_value rtype value = 
  print_string (Value.string_of_value value);
  print_endline (if Settings.get_value(printing_types) then
		   " : "^ Types.string_of_datatype rtype
                 else "")

(** optimise and evaluate a program *)
let process_program ?(printer=print_value) (valenv, nenv, tyenv) (program, t) =
  (* TODO: the optimise part *)
(*
  print_string ((Ir.Show_program.show program)^"\n");
  print_endline;
*)
  let closures = Ir.ClosureTable.program (Var.varify_env (nenv, tyenv.Types.var_env)) program in
  let valenv = Value.with_closures valenv closures in

  let valenv, v = lazy (Evalir.run_program valenv program)
    <|measure_as|> "run_program"
  in
    printer t v;
    (valenv, nenv, tyenv), v

(* Read Links source code, then optimise and run it. *)
let evaluate ?(handle_errors=Errors.display_fatal) parse (_, nenv, tyenv as envs) =
    handle_errors (measure "parse" parse (nenv, tyenv) ->- process_program envs)

(* Interactive loop *)
let interact envs =
  let make_dotter ps1 =
    let dots = String.make (String.length ps1 - 1) '.' ^ " " in
      fun _ ->
        print_string dots;
        flush stdout in
  let rec interact envs =
    let evaluate_replitem parse envs input =
      let valenv, nenv, tyenv = envs in
        Errors.display ~default:(fun _ -> envs)
          (lazy
             (match measure "parse" parse input with
                | `Definitions ([], _), tyenv -> valenv, nenv, tyenv
                | `Definitions (defs, nenv'), tyenv ->
                    let (valenv, _, _ as envs), _ =
                      process_program
                        ~printer:(fun _ _ -> ())
                        envs
                        ((defs, `Return (`Extend (StringMap.empty, None))), Types.unit_type) in
                    let () =
                      Env.String.fold
                        (fun name var () ->
                           let v = Value.find var valenv in
                           let t = Env.String.lookup tyenv.Types.var_env name in
                             prerr_endline (name
                                            ^" = "^Value.string_of_value v
                                            ^" : "^Types.string_of_datatype t); ())
                        nenv'
                        ()
                    in
                      (valenv,
                       Env.String.extend nenv nenv',
                       tyenv)
                | `Expression (e, t), tyenv ->
                    let envs, _ = process_program envs (e, t) in
                      envs
                | `Directive directive, _ -> execute_directive directive envs))
    in
      print_string ps1; flush stdout;

      let _, nenv, tyenv = envs in

      let parse_and_desugar input =
        let sugar, pos_context = Parse.parse_channel ~interactive:(make_dotter ps1) Parse.interactive input in
        let sentence, t, tyenv' = Frontend.Pipeline.interactive tyenv pos_context sugar in
        let sentence' = match sentence with
          | `Definitions defs ->
              let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in
              let defs, nenv = Sugartoir.desugar_definitions (nenv, tenv, tyenv.Types.effect_row) defs in
(*                 Debug.print ("defs: "^Ir.Show_computation.show (defs, `Return (`Extend (StringMap.empty, None)))); *)
                `Definitions (defs, nenv)
          | `Expression e     ->
              let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in
              let e = Sugartoir.desugar_expression (nenv, tenv, tyenv.Types.effect_row) e in
(*                Debug.print ("e: "^Ir.Show_computation.show e); *)
                `Expression (e, t)
          | `Directive d      -> `Directive d
        in
          sentence', tyenv'
      in
        interact (evaluate_replitem parse_and_desugar envs (stdin, "<stdin>"))
  in
    Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> raise Sys.Break));
    interact envs

(* Given an environment mapping source names to IR names return
   the inverse environment mapping IR names to source names.

   It is silly that this takes an Env.String.t but returns an
   IntMap.t. String_of_ir should really take an Env.Int.t instead of
   an IntMap.t. Perhaps invert_env should be hidden or inlined inside
   string_of_ir too.
*)
let invert_env env =
  Env.String.fold
    (fun name var env ->
       if IntMap.mem var env then
         failwith ("(invert_env) duplicate variable in environment")
       else
         IntMap.add var name env)
    env IntMap.empty

(* Read Links source code and pretty-print the IR *)
let print_ir ?(handle_errors=Errors.display_fatal) parse (_, nenv, tyenv as envs) =
  let printer (valenv, nenv, typingenv) (program, t) =
    print_endline (Ir.Show_program.show program ^ "\n");
    print_endline (Ir.string_of_ir (invert_env nenv) program) in
  handle_errors (measure "parse" parse (nenv, tyenv) ->- printer envs)

let run_file prelude envs filename =
  Settings.set_value interacting false;
  let parse_and_desugar (nenv, tyenv) filename =
    let (nenv, tyenv), (globals, (locals, main), t) =
      Errors.display_fatal Loader.load_file (nenv, tyenv) filename
    in
      ((globals @ locals, main), t)
  in
    if Settings.get_value web_mode then
      Webif.serve_request envs prelude filename
    else
      if Settings.get_value pretty_print_ir then
        print_ir parse_and_desugar envs filename
      else
        ignore (evaluate parse_and_desugar envs filename)

let evaluate_string_in envs v =
  let parse_and_desugar (nenv, tyenv) s = 
    let sugar, pos_context = Parse.parse_string ~pp:(Settings.get_value pp) Parse.program s in
    let program, t, _ = Frontend.Pipeline.program tyenv pos_context sugar in

    let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in

    let globals, (locals, main), _nenv = Sugartoir.desugar_program (nenv, tenv, tyenv.Types.effect_row) program in
      ((globals @ locals, main), t)
  in
    (Settings.set_value interacting false;
     if Settings.get_value pretty_print_ir then
       print_ir parse_and_desugar envs v
     else
       ignore (evaluate parse_and_desugar envs v))

let load_prelude () = 
  let (nenv, tyenv), (globals, _, _) =
    (Errors.display_fatal
       Loader.load_file (Lib.nenv, Lib.typing_env) (Settings.get_value prelude_file)) in
  let () = Lib.prelude_tyenv := Some tyenv in
  let () = Lib.prelude_nenv := Some nenv in

  let closures = Ir.ClosureTable.bindings (Var.varify_env (Lib.nenv, Lib.typing_env.Types.var_env)) globals in
  let valenv = Evalir.run_defs (Value.with_closures Value.empty_env closures) globals in
  let envs =
    (valenv,
     Env.String.extend Lib.nenv nenv,
     Types.extend_typing_environment Lib.typing_env tyenv)
  in
    globals, envs

let run_tests tests () = 
  begin
(*    Test.run tests;*)
    exit 0
  end

let to_evaluate : string list ref = ref []
let to_precompile : string list ref = ref []

let config_file   : string option ref = ref None
let options : opt list = 
  let set setting value = Some (fun () -> Settings.set_value setting value) in
  [
    ('d',     "debug",               set Debug.debugging_enabled true, None);
    ('w',     "web-mode",            set web_mode true,                None);
(*    ('O',     "optimize",            set Optimiser.optimising true,    None);*)
    (noshort, "measure-performance", set measuring true,               None);
    ('n',     "no-types",            set printing_types false,         None);
    ('p',     "print-ir",            set pretty_print_ir true,         None);
    ('e',     "evaluate",            None,                             Some (fun str -> push_back str to_evaluate));
    (noshort, "config",              None,                             Some (fun name -> config_file := Some name));
    (noshort, "dump",                None,
     Some(fun filename -> Loader.print_cache filename;  
            Settings.set_value interacting false));
    (noshort, "precompile",          None,                             Some (fun file -> push_back file to_precompile));
(*     (noshort, "working-tests",       Some (run_tests Tests.working_tests),                  None); *)
(*     (noshort, "broken-tests",        Some (run_tests Tests.broken_tests),                   None); *)
(*     (noshort, "failing-tests",       Some (run_tests Tests.known_failures),                 None); *)
    (noshort, "pp",                  None,                             Some (Settings.set_value pp));
    ]

let main () =
  begin match Utility.getenv "REQUEST_METHOD" with 
    | Some _ -> Settings.set_value web_mode true
    | None -> ()
  end;
  config_file := (try Some (Unix.getenv "LINKS_CONFIG") with _ -> !config_file);
  let file_list = ref [] in
  Errors.display_fatal_l (lazy 
     (parse_cmdline options (fun i -> push_back i file_list)));
  (match !config_file with None -> () 
     | Some file -> Settings.load_file file);

  let prelude, ((_valenv, nenv, tyenv) as envs) = load_prelude () in
    
  let () = Utility.for_each !to_evaluate (evaluate_string_in envs) in
    (* TBD: accumulate type/value environment so that "interact" has access *)

  let () = Utility.for_each !to_precompile (Loader.precompile_cache (nenv, tyenv)) in
  let () = if !to_precompile <> [] then Settings.set_value interacting false in
          
  let () = Utility.for_each !file_list (run_file prelude envs) in       
    if Settings.get_value interacting then
      let () = print_endline (Settings.get_value welcome_note) in
        interact envs

let _ =
  main ()
