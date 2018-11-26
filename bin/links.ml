open Links_core
open Performance
open Utility

module BS = Basicsettings


(** Ensure the settings were parsed correctly *)
let _ = ParseSettings.validate_settings ()



let to_evaluate : string list ref = ParseSettings.to_evaluate
let file_list : string list ref = ParseSettings.file_list

let main () =
  let prelude, ((_valenv, _, _) as envs) = measure "prelude" Driver.NonInteractive.load_prelude () in

  for_each !to_evaluate (Driver.NonInteractive.evaluate_string_in envs);
    (* TBD: accumulate type/value environment so that "interact" has access *)

  for_each !file_list (Driver.NonInteractive.run_file prelude envs);
  let should_start_repl = !to_evaluate = [] && !file_list = [] in
  if should_start_repl then
    begin
      print_endline (Settings.get_value BS.welcome_note);
      Repl.interact envs
    end



let _ =
  if !ParseSettings.print_keywords
  then (List.iter (fun (k,_) -> print_endline k) Lexer.keywords; exit 0);

(* parse common cmdline arguments and settings *)
  begin match Utility.getenv "REQUEST_METHOD" with
    | Some _ -> Settings.set_value BS.web_mode true
    | None -> ()
  end;

  main()

