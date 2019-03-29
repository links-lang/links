open Links_core
open Webserver
open Performance
open Utility

module BS = Basicsettings
module Eval = Evalir.Eval(Webserver)
module Webif = Webif.WebIf(Webserver)

type evaluation_env =   Value.env (* maps int identifiers to their values *)
                      * Ir.var Env.String.t (* map string identifiers to int identifiers *)
                      * Types.typing_environment (* typing info, using string identifiers *)

type evaluation_result =
  {
    result_env : evaluation_env;
    result_value : Value.t;
    result_type : Types.datatype
  }

(** optimise and evaluate a program *)
let process_program
      (interacting : bool)
      (envs : evaluation_env)
      (program : Ir.program)
      external_files
          : (Value.env * Value.t) =
  let (valenv, nenv, tyenv) = envs in
  let tenv = (Var.varify_env (nenv, tyenv.Types.var_env)) in

  let perform_optimisations = Settings.get_value BS.optimise && not interacting in

  let (globals, _main) as post_backend_pipeline_program =
    Backend.transform_program perform_optimisations tenv program in


  (if Settings.get_value BS.typecheck_only then exit 0);

  Webserver.init (valenv, nenv, tyenv) globals external_files;

  lazy (Eval.run_program valenv post_backend_pipeline_program) |>measure_as<| "run_program"


let process_program  interacting envs program external_files =
  lazy (process_program  interacting envs program external_files) |>measure_as<| "process_program"

let die_on_exception f x =
  Errors.display ~default:(fun _ -> exit 1) (lazy (f x))

let die_on_exception_unless_interacting is_interacting f x =
  let handle exc =
    if is_interacting then
      raise exc
    else
      exit 1 in
  Errors.display ~default:handle (lazy (f x))


(** Read Links source code, then optimise and run it. *)
let evaluate
      ?(handle_errors=die_on_exception_unless_interacting)
      interacting
      parse_fun
      (envs : evaluation_env)
      filename
          : evaluation_result =
  let (_, nenv, tyenv) = envs in
  let evaluate_inner f =
    let (program, t), (nenv', tyenv'), external_files = parse_fun (nenv, tyenv) f in

    let valenv, v = process_program interacting envs program external_files in
    {
    result_env = (valenv,
      Env.String.extend nenv nenv',
      Types.extend_typing_environment tyenv tyenv');
    result_value = v;
    result_type = t
    }
  in
  let evaluate_inner f = lazy (evaluate_inner f) |>measure_as<| "evaluate" in
  handle_errors interacting evaluate_inner filename


(* For non-REPL use only *)
module NonInteractive =
struct


  let run_file prelude envs filename : evaluation_result =
    Webserver.set_prelude prelude;
    let parse_and_desugar (nenv, tyenv) filename =
      let source =
        die_on_exception_unless_interacting false (Loader.load_file (nenv, tyenv)) filename
      in
        let open Loader in
        let (nenv, tyenv) = source.envs in
        let (globals, (locals, main), t) = source.program in
        let external_files = source.external_dependencies in
        ((globals @ locals, main), t), (nenv, tyenv), external_files
    in
      evaluate false parse_and_desugar envs filename


  let run_file prelude envs filename =
    lazy (run_file prelude envs filename) |>measure_as<| ("run_file "^filename)



  let evaluate_string_in envs  v =
    let parse_and_desugar (nenv, tyenv) s =
      let sugar, pos_context = Parse.parse_string ~pp:(Settings.get_value BS.pp) Parse.program s in
      let (program, t, _), _ = Frontend.Pipeline.program tyenv pos_context sugar in

      let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in

      let globals, (locals, main), _nenv = Sugartoir.desugar_program (nenv, tenv, tyenv.Types.effect_row) program in
      ((globals @ locals, main), t), (nenv, tyenv), []
    in
      evaluate false parse_and_desugar envs v



  (* TODO: Remove special handling of prelude once module processing is in place *)
  let load_prelude () =
    (if Settings.get_value Basicsettings.Ir.show_lib_function_env then
      (Debug.print "lib.ml mappings:";
      Env.String.iter (fun name var -> Debug.print (string_of_int var ^ " -> " ^ name ^ " :: " ^
        Types.string_of_datatype (Env.String.lookup Lib.typing_env.Types.var_env name ) )) Lib.nenv));

    let load_prelude_inner () =
      let open Loader in
      let source =
        (die_on_exception_unless_interacting false
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
   in
   die_on_exception load_prelude_inner ()
end
