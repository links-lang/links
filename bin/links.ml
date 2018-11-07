open Links_core
open Webserver

open Performance
open Utility

module BS = Basicsettings
module Eval = Evalir.Eval(Webserver)
module Webif = Webif.WebIf(Webserver)

(** Ensure the settings were parsed correctly *)
let _ = ParseSettings.validate_settings ()



let load_prelude () =
  (if Settings.get_value Basicsettings.Ir.show_lib_function_env then
    (Debug.print "lib.ml mappings:";
    Env.String.iter (fun name var -> Debug.print (string_of_int var ^ " -> " ^ name ^ " :: " ^
      Types.string_of_datatype (Env.String.lookup Lib.typing_env.Types.var_env name ) )) Lib.nenv));

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

  let globals = Backend.transform_prelude tenv globals in

  let valenv = Eval.run_defs Value.Env.empty globals in
  let envs =
    (valenv,
     Env.String.extend Lib.nenv nenv,
     Types.extend_typing_environment Lib.typing_env tyenv)
  in
    globals, envs



let to_evaluate : string list ref = ParseSettings.to_evaluate
let file_list : string list ref = ParseSettings.file_list

let main () =
  let prelude, ((_valenv, _, _) as envs) = measure "prelude" load_prelude () in

  for_each !to_evaluate (Evaluation.evaluate_string_in envs);
    (* TBD: accumulate type/value environment so that "interact" has access *)

  for_each !file_list (Evaluation.run_file prelude envs);
  if Settings.get_value BS.interacting then
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

