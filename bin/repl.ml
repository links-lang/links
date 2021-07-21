open Links_core
open Utility
open List
open CommonTypes

(** Set this to [true] to print types when printing results. *)
let printing_types =
  Settings.(flag ~default:true "printing_types"
            |> synopsis "Print types when printing results"
            |> convert parse_bool
            |> sync)

(* Pretty print values (outside web mode) *)
let print_pretty
  = Settings.(flag ~default:true "print_pretty"
              |> synopsis "Pretty print values (outside web mode)"
              |> convert parse_bool
              |> sync)

let print_colors
  = Settings.(flag "print_colours"
              |> convert parse_bool
              |> sync)

let print_setting_description : out_channel -> Settings.Reflection.t -> unit
  = fun oc descr ->
  let show_option = function None -> "(none)" | Some s -> s in
  let show_type = function
    | `Flag -> "flag"
    | `Option -> "option"
    | `MultiOption -> "multi option"
  in
  let open Settings.Reflection in
  Printf.fprintf oc "%13s: %s\n" "name" descr.name;
  Printf.fprintf oc "%13s: %s\n" "type" (show_type descr.kind);
  Printf.fprintf oc "%13s: %s\n" "default value" (show_option descr.default);
  Printf.fprintf oc "%13s: %s\n" "current value" (show_option descr.current_value);
  (match descr.value_hint with
   | None -> ()
   | Some hint ->
      Printf.fprintf oc "%15s(value description: %s)\n" "" hint);
  Printf.fprintf oc "%13s: %s\n" "synopsis" (show_option descr.synopsis)

module BS = Basicsettings

(** The prompt used for interactive mode *)
let ps1 = "links> "

(** Print a value (including its type if `printing_types' is [true]). *)
let print_value rtype value =
  if Settings.get Webserver_types.webs_running || not (Settings.get print_pretty)
  then begin
      print_string (Value.string_of_value value);
      print_endline (if Settings.get printing_types then
               " : "^ Types.string_of_datatype rtype
                     else "")
    end
  else
    let (width, _height) = try ANSITerminal.size () with _ -> (80, 24) in
    let open Format in
    pp_set_margin std_formatter width;
    pp_set_tags std_formatter (Settings.get print_colors);
    pp_set_mark_tags std_formatter (Settings.get print_colors);
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
            (if Settings.get printing_types then
           Types.string_of_datatype rtype
             else "");
    pp_print_newline std_formatter ()


(** Interactive loop *)
let welcome_note =
" _     _ __   _ _  __  ___\n\
 / |   | |  \\ | | |/ / / ._\\\n\
 | |   | | , \\| |   /  \\  \\\n\
 | |___| | |\\ \\ | |\\ \\ _\\  \\\n\
 |_____|_|_| \\__|_| \\_|____/\n\
Welcome to Links version " ^ (Utility.val_of (Settings.get BS.version)) ^ "\n"
let welcome_note = Settings.(option ~default:(Some welcome_note) ~readonly:true "welcome_note"
                             |> privilege `System
                             |> to_string from_string_option
                             |> sync)

let rec directives : (string * ((Context.t -> string list -> Context.t) * string)) list Lazy.t
  = let perform f context args =
      ignore (f args); context
    in
    (* lazy so we can have applications on the rhs *)
    lazy
      [ "directives",
        (perform (fun _ ->
             iter (fun (n, (_, h)) -> Printf.fprintf stderr " @%-20s : %s\n" n h)
               (Lazy.force directives)),
         "list available directives");

        "settings",
        (perform (fun _ ->
             (Settings.print_settings stderr)),
         "print available settings");

        "set",
        (perform (function
             | (name::value::_) -> Settings.parse_and_set_user name value
             | _ -> prerr_endline "syntax : @set name value"),
         "change the value of a setting");

        "builtins",
        (perform (fun _ ->
             Env.String.fold
               (fun k s () ->
                 Printf.fprintf stderr "typename %s = %s\n" k
                   (Types.string_of_tycon_spec s))
               (Lib.typing_env.Types.tycon_env) ();
             StringSet.iter (fun n ->
                 let t = Env.String.find n Lib.type_env in
                 Printf.fprintf stderr " %-16s : %s\n"
                   n (Types.string_of_datatype t))
               (Env.String.domain Lib.type_env)),
         "list builtin functions and values");

        "quit",
        (perform (fun _ -> exit 0),
         "exit the interpreter");

        "typeenv",
        ((fun context  _ ->
          let typeenv =
            let tenv = Context.typing_environment context in
            tenv.Types.var_env
          in
          StringSet.iter
            (fun k ->
              let t = Env.String.find k typeenv in
              Printf.fprintf stderr " %-16s : %s\n" k
                (Types.string_of_datatype t))
            (StringSet.diff (Env.String.domain typeenv)
               (Env.String.domain Lib.type_env));
          context),
         "display the current type environment");

        "tyconenv",
        ((fun context _ ->
          let tycon_env =
            let tenv = Context.typing_environment context in
            tenv.Types.tycon_env
          in
          StringSet.iter (fun k ->
              let s = Env.String.find k tycon_env in
              Printf.fprintf stderr " %s = %s\n"
                (Module_hacks.Name.prettify k)
                (Types.string_of_tycon_spec s))
            (StringSet.diff (Env.String.domain tycon_env) (Env.String.domain Lib.typing_env.Types.tycon_env));
          context),
         "display the current type alias environment");

        "env",
        ((fun context _ ->
          let nenv =
            Context.name_environment context
          in
          let tyenv =
            Context.typing_environment context
          in
          Env.String.fold
            (fun name var () ->
              if not (Lib.is_primitive name) then
                let ty = (Types.string_of_datatype ~policy:Types.Policy.default_policy ~refresh_tyvar_names:true
                          -<- (fun name -> Env.String.find name tyenv.Types.var_env)) name in
                let name =
                  if Settings.get Debug.enabled
                  then Printf.sprintf "%s(%d)" name var
                  else (Module_hacks.Name.prettify name)
                in
                Printf.fprintf stderr " %-16s : %s\n"
                  name ty)
            nenv ();
          context),
         "display the current value environment");

        "load",
        ((fun context args ->
          match args with
          | [filename] ->
             Errors.display
               ~default:(fun _ -> context)
               (lazy
                  (let (context', datatype, value) =
                     Driver.Phases.whole_program context filename
                   in
                   print_value datatype value; context'))
          | _ -> prerr_endline "syntax: @load \"filename\""; context),
         "load in a Links source file, extending the current environment");

        "dload",
        ((fun context args ->
          match args with
          | [filename] ->
             begin try
                 Dynlink.loadfile (Sys.expand filename)
               with
               | Dynlink.Error e -> prerr_endline (Printf.sprintf "dynamic linking error: %s" (Dynlink.error_message e))
               | Sys.Unknown_environment_variable _ -> prerr_endline (Printf.sprintf "dynamic linking error: file %s not found." filename)
             end; context
          | _ -> prerr_endline "syntax: @dload \"filename.cmxs\""; context),
        "dynamically load in a Links extension");

        "withtype",
        ((fun context args ->
          let tenv, aliases =
            let tyenv = Context.typing_environment context in
            tyenv.Types.var_env, tyenv.Types.tycon_env
          in
          match args with
          | [] -> prerr_endline "syntax: @withtype type"; context
          | _ -> let t = DesugarDatatypes.read ~aliases (String.concat " " args) in
                 StringSet.iter
                   (fun id ->
                     try begin
                         let t' = Env.String.find id tenv in
                         let ttype = Types.string_of_datatype t' in
                         let fresh_envs = Types.make_fresh_envs t' |> Types.combine_per_kind_envs in
                         let t' = Instantiate.datatype fresh_envs t' in
                         Unify.datatypes (t,t');
                         Printf.fprintf stderr " %s : %s\n" id ttype
                       end with _ -> ())
                   (Env.String.domain tenv)
                 ; context),
         "search for functions that match the given type");

        "help",
        ((fun context args ->
          (match args with
           | [setting_name] ->
              (try
                 print_setting_description stderr (Settings.Reflection.reflect setting_name)
               with Settings.Unknown_setting setting_name ->
                 Printf.fprintf stderr "Unknown setting '%s'\n%!" setting_name);
           | _ -> prerr_endline "syntax: @help setting");
          context),
         "print the documentation of a given setting") ]

let execute_directive context (name, args) =
  let context =
    try
      let f = fst (assoc name (Lazy.force directives)) in
      f context args
    with NotFound _ ->
      Printf.fprintf stderr "unknown directive : %s\n" name;
      context
  in
  flush stderr; context

let handle previous_context current_context = function
  | `Definitions _defs ->
     let tycon_env' =
       let tenv  = Context.typing_environment previous_context in
       let tenv' = Context.typing_environment current_context in
       let tycon_env, tycon_env' =
         Types.(tenv.tycon_env, tenv'.tycon_env)
       in
       Env.String.fold
         (fun name def new_tycons ->
           (* This is a bit of a hack, but it will have to do until names become hygienic. *)
           if not (Env.String.has name tycon_env) || not (Env.String.find name tycon_env == def)
           then Env.String.bind name def new_tycons
           else new_tycons)
         tycon_env' Env.String.empty
     in
     Env.String.fold
       (fun name spec () ->
         Printf.printf "%s = %s\n%!"
           (Module_hacks.Name.prettify name)
           (Types.string_of_tycon_spec spec))
       tycon_env' ();
     let diff previous_context current_context =
       let new_vars =
         let nenv, nenv' =
           ( Context.name_environment previous_context
           , Context.name_environment current_context )
         in
         let vars, vars' =
           ( Env.String.fold (fun _ var vars -> IntSet.add var vars) nenv IntSet.empty
           , Env.String.fold (fun _ var vars -> IntSet.add var vars) nenv' IntSet.empty )
         in
         IntSet.diff vars' vars
       in
       let nenv' =
         let nenv = Context.name_environment current_context in
         Env.String.filter (fun _ var -> IntSet.mem var new_vars) nenv
       in
       nenv'
     in
     let nenv' = diff previous_context current_context in
     let var_env' =
       let tenv = Context.typing_environment current_context in
       tenv.Types.var_env
     in
     let valenv = Context.value_environment current_context in
     Env.String.fold
       (fun name var () ->
         let v, t =
           (* Function values are bound in a global table, whereas
              other values are bound in the value environment. *)
           match Tables.lookup Tables.fun_defs var with
           | None ->
              let v = Value.Env.find var valenv in
              let t = Env.String.find name var_env' in
              v, t
           | Some (finfo, _, None, location) ->
              let v =
                match location with
                | Location.Server | Location.Unknown ->
                   `FunctionPtr (var, None)
                | Location.Client ->
                   `ClientFunction (Js.var_name_var var)
              in
              let t = Var.info_type finfo in v, t
           | _ -> assert false
         in
         Printf.printf "%s = %s : %s\n%!"
           (Module_hacks.Name.prettify name)
           (Value.string_of_value v)
           (Types.string_of_datatype t))
       nenv'
       (); current_context
  | `Expression (datatype, value) ->
     print_value datatype value;
     current_context
  | `Directive directive ->
     execute_directive current_context directive

let interact : Context.t -> unit
  = fun context ->
  let module I = Driver.Phases.Interactive in
  let print_error exn =
    Printf.fprintf stderr "%s\n%!" (Errors.format_exception exn)
  in
  Settings.set BS.interactive_mode true;
  Printf.printf "%s%!" (val_of (Settings.get welcome_note));
  let rec loop context =
    Parse.Readline.prepare_prompt ps1;
    match I.readline ps1 context with
    | exception exn ->
       print_error exn; loop context
    | I.({ sentence; context = context' }) ->
       let context'' =
         try handle context context' sentence
         with exn -> print_error exn; context'
       in
       loop context''
  in
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> raise Sys.Break));
  loop context
