open Webserver

open Performance
open Utility
open List

module BS = Basicsettings
module Eval = Evalir.Eval(Webserver)
module Webif = Webif.WebIf(Webserver)

(** The prompt used for interactive mode *)
let ps1 = "links> "

type envs = Value.env * Ir.var Env.String.t * Types.typing_environment

(** Print a value (including its type if `printing_types' is [true]). *)
let print_value rtype value =
  if Settings.get_value Basicsettings.web_mode || not (Settings.get_value Basicsettings.print_pretty || Settings.get_value Basicsettings.interacting)
  then begin
      print_string (Value.string_of_value value);
      print_endline (if Settings.get_value(BS.printing_types) then
		       " : "^ Types.string_of_datatype rtype
                     else "")
    end
  else
    let (width, _height) = try ANSITerminal.size () with _ -> (80, 24) in
    let open Format in
    pp_set_margin std_formatter width;
    pp_set_tags std_formatter (Settings.get_value Basicsettings.print_colors);
    pp_set_mark_tags std_formatter (Settings.get_value Basicsettings.print_colors);
    pp_set_formatter_tag_functions
      std_formatter
      {mark_open_tag = (function
                        | "constructor" -> "\x1b[32m"
                        | "recordlabel" -> "\x1b[35m"
                        (* | "string" -> "\x1b[36m" *)
                        | _ -> "");
       mark_close_tag  = (fun _ -> "\x1b[39m");
       print_open_tag  = ignore;
       print_close_tag = ignore;
      };
    fprintf std_formatter "@[%a@;<1 4>: %s@]"
            Value.p_value value
            (if Settings.get_value(BS.printing_types) then
	       Types.string_of_datatype rtype
             else "");
    pp_print_newline std_formatter ()

(** optimise and evaluate a program *)
let process_program ?(printer=print_value) (valenv, nenv, tyenv) (program, t) external_files =
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
  BuildTables.program tenv Lib.primitive_vars program;
  let (globals, _) = program in
  Webserver.init (valenv, nenv, tyenv) globals external_files;

  let valenv, v = lazy (Eval.run_program valenv program) |>measure_as<| "run_program" in
  lazy (printer t v) |>measure_as<| "print";
  valenv, v

let process_program ?(printer=print_value) (valenv, nenv, tyenv) (program, t) external_files =
  lazy (process_program ~printer (valenv, nenv, tyenv) (program, t) external_files) |>measure_as<| "process_program"

(** Read Links source code, then optimise and run it. *)
let evaluate ?(handle_errors=Errors.display_fatal) parse (_, nenv, tyenv as envs) =
  let evaluate_inner x =
    let (program, t), (nenv', tyenv'), external_files = parse (nenv, tyenv) x in

    let valenv, v = process_program envs (program, t) external_files in
    (valenv,
     Env.String.extend nenv nenv',
     Types.extend_typing_environment tyenv tyenv'), v
  in
  let evaluate_inner x = lazy (evaluate_inner x) |>measure_as<| "evaluate" in
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
       (function (name::value::_) -> Settings.parse_and_set_user (name, value) false
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
    ((fun ((_, _, { Types.var_env = typeenv; _ }) as envs) _ ->
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
    ((fun ((_, _, {Types.tycon_env = tycon_env; _ }) as envs) _ ->
        StringSet.iter (fun k ->
                          let s = Env.String.lookup tycon_env k in
                            Printf.fprintf stderr " %s = %s\n" k
                              (Types.string_of_tycon_spec s))
          (StringSet.diff (Env.String.domain tycon_env) (Env.String.domain Lib.typing_env.Types.tycon_env));
        envs),
     "display the current type alias environment");

    "env",
    ((fun ((_valenv, nenv, tyenv) as envs) _ ->
        Env.String.fold
          (fun name var () ->
            if not (Lib.is_primitive name) then
              let ty = (Types.string_of_datatype ~policy:Types.Print.default_policy ~refresh_tyvar_names:true
                        -<- Env.String.lookup tyenv.Types.var_env) name in
              let name =
                if Settings.get_value Debug.debugging_enabled
                then Printf.sprintf "%s(%d)" name var
                else name
              in
               Printf.fprintf stderr " %-16s : %s\n"
                 name ty)
          nenv ();
        envs),
     "display the current value environment");

    "load",
    ((fun (envs) args ->
        match args with
          | [filename] ->
              let parse_and_desugar (nenv, tyenv) filename =
                let source =
                  Loader.load_file (nenv, tyenv) filename
                in
                  let open Loader in
                  let (nenv, tyenv) = source.envs in
                  let (globals, (locals, main), t) = source.program in
                  let external_files = source.external_dependencies in
                  ((globals @ locals, main), t), (nenv, tyenv), external_files in
              let envs, _ = evaluate parse_and_desugar envs filename in
                envs
          | _ -> prerr_endline "syntax: @load \"filename\""; envs),
     "load in a Links source file, extending the current environment");

    "dload",
    ((fun envs args ->
      match args with
      | [filename] ->
         begin try
             Dynlink.loadfile (Sys.expand filename)
           with
           | Dynlink.Error e -> prerr_endline (Printf.sprintf "dynamic linking error: %s" (Dynlink.error_message e))
           | Sys.Unknown_environment_variable _ -> prerr_endline (Printf.sprintf "dynamic linking error: file %s not found." filename)
           end;
         envs
      | _ -> prerr_endline "syntax: @dload \"filename.cmxs\""; envs),
     "dynamically load in a Links extension");

    "withtype",
    ((fun (_, _, {Types.var_env = tenv; Types.tycon_env = aliases; _} as envs) args ->
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
  (* Ensure we retain history *)
  let history_path = Basicsettings.Readline.readline_history_path () in
  ignore (LNoise.history_load ~filename:history_path);
  ignore (LNoise.history_set ~max_length:100);
  let rec interact envs =
    let evaluate_replitem parse envs =
      let _, nenv, tyenv = envs in
        Errors.display ~default:(fun _ -> envs)
          (lazy
             (match parse () with
                | `Definitions (defs, nenv'), tyenv' ->
                    let valenv, _ =
                      process_program
                        ~printer:(fun _ _ -> ())
                        envs
                        ((defs, `Return (`Extend (StringMap.empty, None))),
                         Types.unit_type) [] in

                      Env.String.fold (* TBD: Make Env.String.foreach. *)
                        (fun name spec () ->
                             print_endline (name ^" = "^
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
                               let v = Value.Env.find var valenv in
                               let t = Env.String.lookup tyenv'.Types.var_env name in
                               v, t
                             | Some (finfo, _, None, location) ->
                               let v =
                                 match location with
                                 | `Server | `Unknown ->
                                   `FunctionPtr (var, None)
                                 | `Client ->
                                   `ClientFunction (Js.var_name_binder (var, finfo))
                                 | `Native -> assert false in
                               let t = Var.info_type finfo in v, t
                             | _ -> assert false
                           in
                           print_endline(name
                                         ^" = "^Value.string_of_value v
                                         ^" : "^Types.string_of_datatype t))
                        nenv'
                        ();

                      (valenv,
                       Env.String.extend nenv nenv',
                       Types.extend_typing_environment tyenv tyenv')
                | `Expression (e, t), _ ->
                    let valenv, _ = process_program envs (e, t) [] in
                      valenv, nenv, tyenv
                | `Directive directive, _ -> try execute_directive directive envs with _ -> envs))
    in
      let use_linenoise = Settings.get_value Basicsettings.Readline.native_readline in
      begin
        if not use_linenoise then
          (print_string ps1; flush stdout)
        else ()
      end;
      let _, nenv, tyenv = envs in

      let parse_and_desugar () =
        let sugar, pos_context =
          if use_linenoise then
            Parse.parse_readline ps1 Parse.interactive
          else
            let make_dotter ps1 =
              let dots = String.make (String.length ps1 - 1) '.' ^ " " in
              fun _ -> print_string dots; flush stdout in
            Parse.parse_channel ~interactive:(make_dotter ps1) Parse.interactive (stdin, "<stdin>")
          in
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
        interact (evaluate_replitem parse_and_desugar envs)
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
  Webserver.set_prelude prelude;
  let parse_and_desugar (nenv, tyenv) filename =
    let source =
      Errors.display_fatal (Loader.load_file (nenv, tyenv)) filename
    in
      let open Loader in
      let (nenv, tyenv) = source.envs in
      let (globals, (locals, main), t) = source.program in
      let external_files = source.external_dependencies in
      ((globals @ locals, main), t), (nenv, tyenv), external_files
  in
    if Settings.get_value BS.web_mode then
       Webif.serve_request envs prelude filename
    else
      ignore (evaluate parse_and_desugar envs filename)


let run_file prelude envs filename =
  lazy (run_file prelude envs filename) |>measure_as<| ("run_file "^filename)

let evaluate_string_in envs v =
  let parse_and_desugar (nenv, tyenv) s =
    let sugar, pos_context = Parse.parse_string ~pp:(Settings.get_value BS.pp) Parse.program s in
    let (program, t, _), _ = Frontend.Pipeline.program tyenv pos_context sugar in

    let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in

    let globals, (locals, main), _nenv = Sugartoir.desugar_program (nenv, tenv, tyenv.Types.effect_row) program in
    ((globals @ locals, main), t), (nenv, tyenv), []
  in
    (Settings.set_value BS.interacting false;
     ignore (evaluate parse_and_desugar envs v))

let load_prelude () =
  let open Loader in
  let source =
    (Errors.display_fatal
       (Loader.load_file (Lib.nenv, Lib.typing_env)) (Settings.get_value BS.prelude_file))
  in
  let (nenv, tyenv) = source.envs in
  let (globals, _, _) = source.program in

  let tyenv = Lib.patch_prelude_funs tyenv in

  Lib.prelude_tyenv := Some tyenv;
  Lib.prelude_nenv := Some nenv;

  let tenv = (Var.varify_env (Lib.nenv, Lib.typing_env.Types.var_env)) in

  let globals = Closures.bindings tenv Lib.primitive_vars globals in
  (* Debug.print ("Prelude after closure conversion: " ^ Ir.Show_program.show (globals, `Return (`Extend (StringMap.empty, None)))); *)
  BuildTables.bindings tenv Lib.primitive_vars globals;

  let valenv = Eval.run_defs Value.Env.empty globals in
  let envs =
    (valenv,
     Env.String.extend Lib.nenv nenv,
     Types.extend_typing_environment Lib.typing_env tyenv)
  in
    globals, envs

(*Impure so caching is painful *)
let cache_load_prelude () =
  let open Loader in
  let source =
    (Errors.display_fatal
       (Loader.wpcache "prelude.ir")
	  (fun () -> read_file_source (Lib.nenv, Lib.typing_env) (Settings.get_value BS.prelude_file))) in
  let (nenv, tyenv) = source.envs in
  let (globals, _, _) = source.program in

  let tyenv = Lib.patch_prelude_funs tyenv in
  Lib.prelude_tyenv := Some tyenv;
  Lib.prelude_nenv := Some nenv;

  Loader.wpcache "prelude.closures" (fun () ->
    (* TODO: either scrap whole program caching or add closure
       conversion code here *)
    let valenv = Eval.run_defs Value.Env.empty globals in
    let envs =
      (valenv,
       Env.String.extend Lib.nenv nenv,
       Types.extend_typing_environment Lib.typing_env tyenv)
    in
    globals, envs)


let to_evaluate : string list ref = ParseSettings.to_evaluate
let to_precompile : string list ref = ParseSettings.to_precompile
let file_list : string list ref = ParseSettings.file_list

let main () =
  let prelude, ((_valenv, nenv, tyenv) as envs) = measure "prelude" load_prelude () in

  for_each !to_evaluate (evaluate_string_in envs);
    (* TBD: accumulate type/value environment so that "interact" has access *)

  lazy (for_each
          !to_precompile
          (Errors.display_fatal (Loader.precompile_cache (nenv, tyenv)))) |>measure_as<| "precompile";
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
  let open Getopt in
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
			    (parse_cmdline ParseSettings.options
			       (fun i -> push_back i file_list)));
  if(length (!file_list) <> 1)
  then failwith "Whole program caching mode expects exactly one source file";

  let filename = hd (!file_list) in
  let _ = Loader.activate_wpcache filename in

  let prelude, envs = measure "prelude" cache_load_prelude ()
  in
   Webif.serve_request envs prelude filename

let _ =
  (match !ParseSettings.print_cache with
   | (true, Some filename) -> Loader.print_cache filename;
                              Settings.set_value BS.interacting false
   | _                     -> ());
  if !ParseSettings.print_keywords
  then (List.iter (fun (k,_) -> print_endline k) Lexer.keywords; exit 0);

(* parse common cmdline arguments and settings *)
  begin match Utility.getenv "REQUEST_METHOD" with
    | Some _ -> Settings.set_value BS.web_mode true
    | None -> ()
  end;

  (* Load database drivers *)
  Dyn_db_hack.load ();

  if Settings.get_value BS.cache_whole_program
  then whole_program_caching_main ()
  else main()
