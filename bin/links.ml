open Links_core
open Performance
open Utility

module BS = Basicsettings


(** Ensure the settings were parsed correctly *)
let _ = ParseSettings.validate_settings ()



let to_evaluate : string list ref = ParseSettings.to_evaluate
let file_list : string list ref = ParseSettings.file_list


let print_simple rtype value =
  print_string (Value.string_of_value value);
  print_endline
    (if Settings.get_value (BS.printing_types) then
          " : " ^ Types.string_of_datatype rtype
        else
          "")

let prelude_evaluation_environment () =
  let empty_eval_env = Evaluation_env.empty in
  let lib_eval_env =
    {empty_eval_env with
      Evaluation_env.tyenv = FrontendTypeEnv.import_module Lib.BuiltinModules.lib Lib.typing_env;
      Evaluation_env.nenv = Lib.nenv} in

  let prelude_path = (Settings.get_value BS.prelude_file) in
  let (prelude_eval_env, _, _) =
    Driver.run_single_file false lib_eval_env prelude_path in

  let patched_tyenv = Lib.patch_prelude_funs prelude_eval_env.Evaluation_env.tyenv in
  let prelude_opened_tyenv = FrontendTypeEnv.import_module Lib.BuiltinModules.prelude  patched_tyenv in

  {prelude_eval_env with Evaluation_env.tyenv = prelude_opened_tyenv}






let process_arg_files arg_files eval_env : Evaluation_env.t =
  Driver.run_files_and_dependencies false eval_env arg_files

let process_arg_exprs exprs initial_env : Evaluation_env.t =
  List.fold_left
    (fun env expr ->
    let result_env, result_value, result_type = Driver.run_string false env expr in
    print_simple result_type result_value;
    result_env)
  initial_env
  exprs



let main () =
  let prelude_eval_env = measure "prelude" prelude_evaluation_environment () in

  let argfiles_eval_env = process_arg_files !file_list prelude_eval_env in

  let arg_expr_eval_env = process_arg_exprs !to_evaluate argfiles_eval_env in

  let should_start_repl = !to_evaluate = [] && !file_list = [] in
  if should_start_repl then
    begin
      print_endline (Settings.get_value BS.welcome_note);
      Repl.interact arg_expr_eval_env
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

