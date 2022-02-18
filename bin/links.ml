open Links_core
open Utility

module BS = Basicsettings

let to_evaluate
  = Settings.(multi_option "evaluate"
              |> synopsis "Evaluates an expression"
              |> privilege `System
              |> hint "<expression>"
              |> hidden
              |> convert (fun s -> [s])
              |> CLI.(add (short 'e' <&> long "evaluate"))
              |> sync)

let _ =
  let print_keywords _ =
    List.iter (fun (kw, _) -> Printf.printf "%s\n%!" kw) Lexer.keywords;
    exit 0
  in
  Settings.(flag "print_keywords"
            |> synopsis "Print keywords and exit"
            |> privilege `System
            |> action print_keywords
            |> hidden
            |> show_default false
            |> CLI.(add (long "print-keywords"))
            |> sync)

let _ =
  let show_help _ =
    Printf.fprintf stdout "usage: %s [options] [source-files [-- arguments]]\n\n" (Filename.basename Sys.executable_name);
    Printf.fprintf stdout "Options are:\n";
    Settings.print_cli_options stdout;
    flush stderr; exit 0
  in
  Settings.(flag "help"
            |> synopsis "Print help message and exit"
            |> privilege `System
            |> action show_help
            |> hidden
            |> show_default false
            |> convert parse_bool
            |> CLI.(add (short 'h' <&> long "help"))
            |> sync)


let print_simple datatype value =
  let oc = stdout in
  if Settings.get Repl.printing_types
  then Printf.fprintf oc
         "%s : %s\n%!"
         (Value.string_of_value value)
         (Types.string_of_datatype datatype)
  else Printf.fprintf oc "%s\n%!" (Value.string_of_value value)

let handle_errors comp =
  Errors.display ~default:(fun _ -> exit 1) comp

let process_file context file =
  let (context', datatype, value) =
    handle_errors (lazy (Driver.Phases.whole_program context file))
  in
  print_simple datatype value; context'

let process_expr context expr_string =
  let (context', datatype, value) =
    handle_errors (lazy (Driver.Phases.evaluate_string context expr_string))
  in
  print_simple datatype value; context'

let isolate
  = Settings.(flag ~default:true "isolation"
              |> synopsis "Run file and expression arguments in isolation"
              |> privilege `System
              |> convert parse_bool
              |> CLI.(add (long "isolate"))
              |> sync)

let for_each : Context.t -> (Context.t -> string -> Context.t) -> string list -> Context.t
  = fun context f xs ->
  List.fold_left
    (fun context x ->
      let context' = f context x in
      if Settings.get isolate
      then context
      else context')
    context xs

let run : string list -> string list -> unit
  = fun file_list to_evaluate ->
    let context =
      handle_errors (lazy (Driver.Phases.initialise ()))
    in
    let context' =
      for_each context process_expr to_evaluate
    in
    let open Basicsettings.System in
    match Settings.get mode with
    | None | Some Interactive ->
      let context'' =
        for_each context' process_file file_list
      in
      begin match file_list, to_evaluate with
        | [], [] -> Repl.interact context''
        | _, _ -> ()
      end
    | Some Web ->
      ignore (for_each context' process_file file_list)
    | Some Compile ->
      (* TODO(dhil): The following might behave unexpectedly if
         |file_list| > 1 as the output file is repeatedly
         overwritten. We need a better design here. *)
      ignore(for_each context'
        (fun context file ->
           let output_file = val_of (Settings.get output_file) in
           handle_errors (lazy (Driver.Phases.compile_js_only context file output_file)); context)
        file_list)

let main () =
  (* Attempt to synchronise all settings. If any setting commands are
     left unhandled, then error and exit. *)
  Settings.ensure_all_synchronised ();

  let file_list = Settings.get_anonymous_arguments () in
  let to_evaluate = Settings.get to_evaluate in
  run file_list to_evaluate

let _ = main()
