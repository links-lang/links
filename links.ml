open Notfound

open Performance
open Getopt
open Utility
open List

module BS = Basicsettings

(** The prompt used for interactive mode *)
let ps1 = "links> "

type envs = Value.env * Ir.var Env.String.t * Types.typing_environment

(** Print a value (including its type if `printing_types' is [true]). *)
let print_value rtype value =
  print_string (Value.pprint_value value);
  print_endline (if Settings.get_value(BS.printing_types) then
		   " : "^ Types.string_of_datatype rtype
                 else "")

(** optimise and evaluate a program *)
let process_program ?(printer=print_value) (valenv, nenv, tyenv) (program, t) =
  let tenv = (Var.varify_env (nenv, tyenv.Types.var_env)) in

  (* TODO: optimisation *)

  (* We need to be careful here as, for instance, running ElimDeadDefs
     on the prelude would lead to lots of functions being deleted that
     might actually be used in the program itself.

     Actually, this code isn't currently called on the
     prelude. However, it does cause problems in the interactive loop
     as if you define a function it immediately gets optimised away as
     it isn't yet used!

     In order to resolve the problem we could either simply disable
     optimisation for the interactive loop, or we could more usefully
     disable optimisation of the top level definitions by returning a
     tuple containing all of them. That way we could also optimise the
     prelude.
  *)

  let optimise_program program =
    let program = Ir.ElimDeadDefs.program tenv program in
    let program = Ir.Inline.program tenv program in
    program
  in

  let program =
    if Settings.get_value BS.optimise
    then measure "optimise" optimise_program program
    else program
  in

  let program = Closures.program tenv Lib.primitive_vars program in
  (* Debug.print ("Closure converted program: " ^ Ir.Show_program.show program); *)
  BuildTables.program tenv Lib.primitive_vars program;

  let valenv, v = lazy (Evalir.run_program valenv program) <|measure_as|> "run_program" in
  lazy (printer t v) <|measure_as|> "print";
  valenv, v

let process_program ?(printer=print_value) (valenv, nenv, tyenv) (program, t) =
  lazy (process_program ~printer (valenv, nenv, tyenv) (program, t)) <|measure_as|> "process_program"

(** Read Links source code, then optimise and run it. *)
let evaluate ?(handle_errors=Errors.display_fatal) parse (_, nenv, tyenv as envs) =
  let evaluate_inner x =
    let (program, t), (nenv', tyenv') = parse (nenv, tyenv) x in

    let valenv, v = process_program envs (program, t) in
    (valenv,
     Env.String.extend nenv nenv',
     Types.extend_typing_environment tyenv tyenv'), v
  in
  let evaluate_inner x =   lazy (evaluate_inner x) <|measure_as|> "evaluate" in
  handle_errors evaluate_inner



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
          iter (fun (n, (_, h)) -> Printf.fprintf stderr " @%-20s : %s\n" n h)
            (Lazy.force directives)),
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
          Env.String.fold
            (fun k s () ->
               Printf.fprintf stderr "typename %s = %s\n" k
                 (Types.string_of_tycon_spec s))
            (Lib.typing_env.Types.tycon_env) ();
          StringSet.iter (fun n ->
                            let t = Env.String.lookup Lib.type_env n in
                              Printf.fprintf stderr " %-16s : %s\n"
                                n (Types.string_of_datatype t))
            (Env.String.domain Lib.type_env)),
     "list builtin functions and values");

    "quit",
    (ignore_envs (fun _ -> exit 0), "exit the interpreter");

    "typeenv",
    ((fun ((_, _, {Types.var_env = typeenv; Types.tycon_env = tycon_env}) as envs) _ ->
        StringSet.iter
          (fun k ->
             let t = Env.String.lookup typeenv k in
               Printf.fprintf stderr " %-16s : %s\n" k
                 (Types.string_of_datatype t))
          (StringSet.diff (Env.String.domain typeenv)
             (Env.String.domain Lib.type_env));
        envs),
     "display the current type environment");

    "tyconenv",
    ((fun ((_, _, {Types.tycon_env = tycon_env}) as envs) _ ->
        StringSet.iter (fun k ->
                          let s = Env.String.lookup tycon_env k in
                            Printf.fprintf stderr " %s = %s\n" k
                              (Types.string_of_tycon_spec s))
          (StringSet.diff (Env.String.domain tycon_env) (Env.String.domain Lib.typing_env.Types.tycon_env));
        envs),
     "display the current type alias environment");

    "env",
    ((fun ((valenv, nenv, _tyenv) as envs) _ ->
        Env.String.fold
          (fun name var () ->
             if not (Lib.is_primitive name) then
               Printf.fprintf stderr " %-16s : %s\n"
                 name (Value.string_of_value (Value.find var valenv)))
          nenv ();
        envs),
     "display the current value environment");

    "load",
    ((fun (envs) args ->
        match args with
          | [filename] ->
              let parse_and_desugar (nenv, tyenv) filename =
                let (nenv, tyenv), (globals, (locals, main), t) =
                  Errors.display_fatal (Loader.load_file (nenv, tyenv)) filename
                in
                  ((globals @ locals, main), t), (nenv, tyenv) in
              let envs, _ = evaluate parse_and_desugar envs filename in
                envs
          | _ -> prerr_endline "syntax: @load \"filename\""; envs),
     "load in a Links source file, extending the current environment");

    "withtype",
    ((fun (_, _, {Types.var_env = tenv; Types.tycon_env = aliases} as envs) args ->
        match args with
          [] -> prerr_endline "syntax: @withtype type"; envs
          | _ -> let t = DesugarDatatypes.read ~aliases (String.concat " " args) in
              StringSet.iter
                (fun id ->
                   try begin
                     let t' = Env.String.lookup tenv id in
                     let ttype = Types.string_of_datatype t' in
                     let fresh_envs = Types.make_fresh_envs t' in
                     let t' = Instantiate.datatype fresh_envs t' in
                       Unify.datatypes (t,t');
                       Printf.fprintf stderr " %s : %s\n" id ttype
                   end with _ -> ())
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

(** Interactive loop *)
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
             (match parse input with
                | `Definitions (defs, nenv'), tyenv' ->
                    let valenv, _ =
                      process_program
                        ~printer:(fun _ _ -> ())
                        envs
                        ((defs, `Return (`Extend (StringMap.empty, None))),
                         Types.unit_type) in

                      Env.String.fold (* TBD: Make Env.String.foreach. *)
                        (fun name spec () ->
                             prerr_endline (name ^" = "^
                                            Types.string_of_tycon_spec spec); ())
                        (tyenv'.Types.tycon_env)
                        ();

                      Env.String.fold
                        (fun name var () ->
                           let v, t =
                             (* function values are bound in a global
                                table, whereas other values are bound
                                in the value environment *)
                             match Tables.lookup Tables.fun_defs var with
                             | None ->
                               let v = Value.find var valenv in
                               let t = Env.String.lookup tyenv'.Types.var_env name in
                               v, t
                             | Some (finfo, _, None, location) ->
                               let v =
                                 match location with
                                 | `Server | `Unknown ->
                                   `FunctionPtr (var, None)
                                 | `Client ->
                                   `ClientFunction (Js.var_name_binder (var, finfo)) in
                               let t = Var.info_type finfo in
                               v, t in
                           prerr_endline(name
                                         ^" = "^Value.string_of_value v
                                         ^" : "^Types.string_of_datatype t))
                        nenv'
                        ();

                      (valenv,
                       Env.String.extend nenv nenv',
                       Types.extend_typing_environment tyenv tyenv')
                | `Expression (e, t), _ ->
                    let valenv, _ = process_program envs (e, t) in
                      valenv, nenv, tyenv
                | `Directive directive, _ -> execute_directive directive envs))
    in
      print_string ps1; flush stdout;

      let _, nenv, tyenv = envs in

      let parse_and_desugar input =
        let sugar, pos_context = Parse.parse_channel ~interactive:(make_dotter ps1) Parse.interactive input in
        let sentence, t, tyenv' = Frontend.Pipeline.interactive tyenv pos_context sugar in
          (* FIXME: What's going on here? Why is this not part of
             Frontend.Pipeline.interactive?*)
        let sentence' = match sentence with
          | `Definitions defs ->
              let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in
              let defs, nenv' = Sugartoir.desugar_definitions (nenv, tenv, tyenv.Types.effect_row) defs in
                `Definitions (defs, nenv')
          | `Expression e     ->
              let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in
              let e = Sugartoir.desugar_expression (nenv, tenv, tyenv.Types.effect_row) e in
                `Expression (e, t)
          | `Directive d      -> `Directive d
        in
          sentence', tyenv'
      in
        interact (evaluate_replitem parse_and_desugar envs (stdin, "<stdin>"))
  in
    Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> raise Sys.Break));
    interact envs

(** Given an environment mapping source names to IR names, return
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

let run_file prelude envs filename =
  Settings.set_value BS.interacting false;
  let parse_and_desugar (nenv, tyenv) filename =
    let (nenv, tyenv), (globals, (locals, main), t) =
      Errors.display_fatal (Loader.load_file (nenv, tyenv)) filename
    in
      ((globals @ locals, main), t), (nenv, tyenv)
  in
    if Settings.get_value BS.web_mode then
       Webif.serve_request envs prelude filename
    else
      ignore (evaluate parse_and_desugar envs filename)


let run_file prelude envs filename =
  lazy (run_file prelude envs filename) <|measure_as|> ("run_file "^filename)

let evaluate_string_in envs v =
  let parse_and_desugar (nenv, tyenv) s =
    let sugar, pos_context = Parse.parse_string ~pp:(Settings.get_value BS.pp) Parse.program s in
    let program, t, _ = Frontend.Pipeline.program tyenv pos_context sugar in

    let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in

    let globals, (locals, main), _nenv = Sugartoir.desugar_program (nenv, tenv, tyenv.Types.effect_row) program in
    ((globals @ locals, main), t), (nenv, tyenv)
  in
    (Settings.set_value BS.interacting false;
     ignore (evaluate parse_and_desugar envs v))

let load_prelude () =
  let (nenv, tyenv), (globals, _, _) =
    (Errors.display_fatal
       (Loader.load_file (Lib.nenv, Lib.typing_env)) (Settings.get_value BS.prelude_file))
  in

  let tyenv = Lib.patch_prelude_funs tyenv in

  Lib.prelude_tyenv := Some tyenv;
  Lib.prelude_nenv := Some nenv;

  let tenv = (Var.varify_env (Lib.nenv, Lib.typing_env.Types.var_env)) in

  let globals = Closures.bindings tenv Lib.primitive_vars globals in
  (* Debug.print ("Prelude after closure conversion: " ^ Ir.Show_program.show (globals, `Return (`Extend (StringMap.empty, None)))); *)
  BuildTables.bindings tenv Lib.primitive_vars globals;

  let valenv = Evalir.run_defs Value.empty_env globals in
  let envs =
    (valenv,
     Env.String.extend Lib.nenv nenv,
     Types.extend_typing_environment Lib.typing_env tyenv)
  in
    globals, envs

(*Impure so caching is painful *)
let cache_load_prelude () =
  let (nenv, tyenv), (globals, _, _) =
    (Errors.display_fatal
       (Loader.wpcache "prelude.ir")
	  (fun () -> Loader.read_file_source (Lib.nenv, Lib.typing_env) (Settings.get_value BS.prelude_file)))
  in

  let tyenv = Lib.patch_prelude_funs tyenv in

  Lib.prelude_tyenv := Some tyenv;
  Lib.prelude_nenv := Some nenv;

  Loader.wpcache "prelude.closures" (fun () ->
    (* TODO: either scrap whole program caching or add closure
       conversion code here *)
    let valenv = Evalir.run_defs Value.empty_env globals in
    let envs =
      (valenv,
       Env.String.extend Lib.nenv nenv,
       Types.extend_typing_environment Lib.typing_env tyenv)
    in
    globals, envs)



let run_tests tests () =
  begin
(*    Test.run tests;*)
    exit 0
  end

let to_evaluate : string list ref = ref []
let to_precompile : string list ref = ref []

let set_web_mode() = (
  (* When forcing web mode using the command-line argument, default the
     required CGI environment variables to a GET request with no params--
     i.e. start running with the main expression. *)
  if not(is_some(getenv "REQUEST_METHOD")) then
    Unix.putenv "REQUEST_METHOD" "GET";
  if not(is_some(getenv "QUERY_STRING")) then
    Unix.putenv "QUERY_STRING" "";
  Settings.set_value BS.web_mode true
  )

let config_file   : string option ref = ref None
let options : opt list =
  let set setting value = Some (fun () -> Settings.set_value setting value) in
  [
    ('d',     "debug",               set Debug.debugging_enabled true, None);
    ('w',     "web-mode",            Some set_web_mode,                None);
    (noshort, "optimise",            set BS.optimise true,                None);
    (noshort, "measure-performance", set measuring true,               None);
    ('n',     "no-types",            set BS.printing_types false,         None);
    ('e',     "evaluate",            None,                             Some (fun str -> push_back str to_evaluate));
    (noshort, "config",              None,                             Some (fun name -> config_file := Some name));
    (noshort, "dump",                None,
     Some(fun filename -> Loader.print_cache filename;
            Settings.set_value BS.interacting false));
    (noshort, "precompile",          None,                             Some (fun file -> push_back file to_precompile));
(*     (noshort, "working-tests",       Some (run_tests Tests.working_tests),                  None); *)
(*     (noshort, "broken-tests",        Some (run_tests Tests.broken_tests),                   None); *)
(*     (noshort, "failing-tests",       Some (run_tests Tests.known_failures),                 None); *)
    (noshort, "pp",                  None,                             Some (Settings.set_value BS.pp));
    ]

let file_list = ref []

let main () =
  let prelude, ((_valenv, nenv, tyenv) as envs) = measure "prelude" load_prelude () in

  for_each !to_evaluate (evaluate_string_in envs);
    (* TBD: accumulate type/value environment so that "interact" has access *)

  lazy (for_each
          !to_precompile
          (Errors.display_fatal (Loader.precompile_cache (nenv, tyenv)))) <|measure_as|> "precompile";
  if !to_precompile <> [] then Settings.set_value BS.interacting false;
  for_each !file_list (run_file prelude envs);
  if Settings.get_value BS.interacting then
    begin
      print_endline (Settings.get_value BS.welcome_note);
      interact envs
    end


(* jcheney:
   Implementation of "cache_whole_program" setting.

   Alternative version of main that aggressively caches whole
   program (prelude plus source), including closures and
   HTML/JS generated by irtojs.

   This avoids expensive re-computation of :
   - irtojs conversion (per page load)
   - and closures (per page load and XHR call).

   Thus, in the common case of a client-mode program, we just
   return the cached html right away, instead of recomputing it.

   Assumes web mode and single source file.
 *)

let whole_program_caching_main () =
  Debug.print ("Whole program caching mode activated.");

  if Settings.get_value BS.interacting
  then Settings.set_value BS.interacting false;

  if !to_precompile <> []
  then failwith "Cannot precompile in whole program caching mode";

  if !to_evaluate <> []
  then failwith "Cannot evaluate in whole program caching mode";

  (* caching_main assumes exactly one source file *)
  let file_list = ref [] in
  Errors.display_fatal_l (lazy
			    (parse_cmdline options
			       (fun i -> push_back i file_list)));
  if(length (!file_list) <> 1)
  then failwith "Whole program caching mode expects exactly one source file";

  let filename = hd (!file_list) in
  let _ = Loader.activate_wpcache filename in

  let prelude, envs = measure "prelude" cache_load_prelude ()
  in
   Webif.serve_request envs prelude filename

let _ =
(* parse common cmdline arguments and settings *)
  begin match Utility.getenv "REQUEST_METHOD" with
    | Some _ -> Settings.set_value BS.web_mode true
    | None -> ()
  end;

  config_file := (try Some (Unix.getenv "LINKS_CONFIG") with _ -> !config_file);

  Errors.display_fatal_l (lazy
     (parse_cmdline options (fun i -> push_back i file_list)));

  (match !config_file with None -> ()
     | Some file -> Settings.load_file file);

  if Settings.get_value BS.cache_whole_program
  then whole_program_caching_main ()
  else main()
