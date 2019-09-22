open Links_core
open Performance
open Utility

module BS = Basicsettings
module TP = TypePrinter.BySetting

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


let print_simple rtype value =
  print_string (Value.string_of_value value);
  print_endline
    (if Settings.get (Repl.printing_types) then
          " : " ^ TP.string_of_datatype rtype
        else
          "")

let process_filearg prelude envs file =
  let result = Driver.NonInteractive.run_file prelude envs file in
  print_simple result.Driver.result_type result.Driver.result_value

let process_exprarg envs expr =
  let result = Driver.NonInteractive.evaluate_string_in envs expr in
  print_simple result.Driver.result_type result.Driver.result_value

let main () =
  (* Attempt to synchronise all settings. If any setting commands are
     left unhandled, then error and exit. *)
  Settings.ensure_all_synchronised ();

  let file_list = Settings.get_anonymous_arguments () in
  let to_evaluate = Settings.get to_evaluate in

  let prelude, envs = measure "prelude" Driver.NonInteractive.load_prelude () in

  for_each to_evaluate (process_exprarg envs);
    (* TBD: accumulate type/value environment so that "interact" has access *)

  for_each file_list (process_filearg prelude envs);
  match file_list, to_evaluate with
  | [], [] -> Repl.interact envs
  | _, _ -> ()

let _ =
  (* Determine whether web mode should be enabled. *)
  begin match Utility.getenv "REQUEST_METHOD" with
    | Some _ -> Settings.set BS.web_mode true
    | None -> ()
  end;

  main()
