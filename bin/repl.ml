open Links_core
open Utility
open List
open Sugartypes
open CommonTypes

module BS = Basicsettings

(** The prompt used for interactive mode *)
let ps1 = "links> "

type envs = Driver.evaluation_env


(** Print a value (including its type if `printing_types' is [true]). *)
let print_value rtype value =
  if Settings.get_value Basicsettings.web_mode || not (Settings.get_value Basicsettings.print_pretty)
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
    begin [@alert "-deprecated"] pp_set_formatter_tag_functions
      std_formatter
      {mark_open_tag = (function
                        | "constructor" -> "\x1b[32m"
                        | "recordlabel" -> "\x1b[35m"
                        (* | "string" -> "\x1b[36m" *)
                        | _ -> "");
       mark_close_tag  = (fun _ -> "\x1b[39m");
       print_open_tag  = ignore;
       print_close_tag = ignore;
      }
    end;
    fprintf std_formatter "@[%a@;<1 4>: %s@]"
            Value.p_value value
            (if Settings.get_value(BS.printing_types) then
           Types.string_of_datatype rtype
             else "");
    pp_print_newline std_formatter ()

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
                          Printf.fprintf stderr " %s = %s\n"
                            (Module_hacks.Name.prettify k)
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
                else (Module_hacks.Name.prettify name)
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
              let r = Driver.evaluate true parse_and_desugar envs filename in
                print_value r.Driver.result_type r.Driver.result_value;
                r.Driver.result_env
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


let evaluate_parse_result envs parse_result =
  let _, nenv, tyenv = envs in
  match parse_result with
    | `Definitions (defs, nenv'), tyenv' ->
        let valenv, _ =
          Driver.process_program
            true
            envs
            (defs, Ir.Return (Ir.Extend (StringMap.empty, None)))
            [] in

          Env.String.fold (* TBD: Make Env.String.foreach. *)
            (fun name spec () ->
              Printf.printf "%s = %s\n%!"
                (Module_hacks.Name.prettify name)
                (Types.string_of_tycon_spec spec))
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
                      | Location.Server | Location.Unknown ->
                        `FunctionPtr (var, None)
                      | Location.Client ->
                        `ClientFunction (Js.var_name_binder (var, finfo))
                      | Location.Native -> assert false in
                    let t = Var.info_type finfo in v, t
                  | _ -> assert false
                in
                Printf.printf "%s = %s : %s\n%!"
                  (Module_hacks.Name.prettify name)
                  (Value.string_of_value v)
                  (Types.string_of_datatype t))
            nenv'
            ();

          (valenv,
            Env.String.extend nenv nenv',
            Types.extend_typing_environment tyenv tyenv')
    | `Expression (e, t), _ ->
        let valenv, v = Driver.process_program true envs e [] in
          print_value t v;
          valenv, nenv, tyenv
    | `Directive directive, _ -> try execute_directive directive envs with _ -> envs


(** Interactive loop *)
let interact envs =
  Settings.set_value Basicsettings.interactive_mode true;
  (* Ensure we retain history *)
  let history_path = Basicsettings.Readline.readline_history_path () in
  ignore (LNoise.history_load ~filename:history_path);
  ignore (LNoise.history_set ~max_length:100);
  let rec interact envs =
    let evaluate_replitem parse envs =
        Errors.display ~default:(fun _ -> envs)
          (lazy (evaluate_parse_result envs (parse ())))
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
          | Definitions defs ->
              let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in
              let defs, nenv' = Sugartoir.desugar_definitions (nenv, tenv, tyenv.Types.effect_row) defs in
                `Definitions (defs, nenv')
          | Expression e     ->
              let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in
              let e = Sugartoir.desugar_expression (nenv, tenv, tyenv.Types.effect_row) e in
                `Expression (e, t)
          | Directive d      -> `Directive d
        in
          sentence', tyenv'
      in
        interact (evaluate_replitem parse_and_desugar envs)
  in
    Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> raise Sys.Break));
    interact envs
