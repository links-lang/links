open Webserver
open Performance
open Utility

module BS = Basicsettings
module Eval = Evalir.Eval(Webserver)
module Webif = Webif.WebIf(Webserver)

(** optimise and evaluate a program *)
let process_program ?printer (valenv, nenv, tyenv) (program, t) external_files =
  let tenv = (Var.varify_env (nenv, tyenv.Types.var_env)) in

  let (globals, _) = Backend.transform_program tenv program in

  (if Settings.get_value BS.typecheck_only then exit 0);

  Webserver.init (valenv, nenv, tyenv) globals external_files;

  let valenv, v = lazy (Eval.run_program valenv program) |>measure_as<| "run_program" in
  (match printer with
    | None -> ()
    | Some p -> lazy (p t v) |>measure_as<| "print");
  valenv, v


let process_program ?printer (valenv, nenv, tyenv) (program, t) external_files =
  lazy (process_program ?printer (valenv, nenv, tyenv) (program, t) external_files) |>measure_as<| "process_program"



(** Read Links source code, then optimise and run it. *)
let evaluate ?printer ?(handle_errors=Errors.display_fatal) parse (_, nenv, tyenv as envs) =
  let evaluate_inner x =
    let (program, t), (nenv', tyenv'), external_files = parse (nenv, tyenv) x in

    let valenv, v = process_program ?printer envs (program, t) external_files in
    (valenv,
     Env.String.extend nenv nenv',
     Types.extend_typing_environment tyenv tyenv'), v
  in
  let evaluate_inner x = lazy (evaluate_inner x) |>measure_as<| "evaluate" in
  handle_errors evaluate_inner




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
