open Links_core
open Webserver
open Performance
open Utility

module BS = Basicsettings
module Eval = Evalir.Eval(Webserver)
module Webif = Webif.WebIf(Webserver)

(*type env = Evaluation_env.t
type evaluation_result = env * Value.t*)



let die_on_exception_unless_interacting is_interacting lazy_v =
  let handle exc =
    if is_interacting then
      raise exc
    else
      exit 1 in
  Errors.display ~default:handle lazy_v



(** Evaluates the given IR program
    In particular, all frontend processing, sugar-to-ir translation, and backend processing must be done beforehand. *)
let evaluate_ir
      (_interacting : bool)
      (envs : Evaluation_env.t)
      (program : Ir.program)
      (program_external_files : Loader.ext_dep list) :
        Evaluation_env.t * Value.t =
  let program_bindings, _ = program in
  let combined_globals = program_bindings @ envs.Evaluation_env.globals in
  let combined_ffi_files = program_external_files @ envs.Evaluation_env.external_files in

  let envs_combined =
    {envs with
      Evaluation_env.globals = combined_globals;
      Evaluation_env.external_files = combined_ffi_files} in

  (if Settings.get_value BS.typecheck_only then exit 0);

  Webserver.set_context envs_combined;

  let result_venv, result_value =
    lazy (Eval.run_program envs.Evaluation_env.venv program) |>measure_as<| "run_program" in

  {envs_combined with Evaluation_env.venv = result_venv}, result_value



(* just adds benchmarking and error handling aound evaluate_ir *)
let evaluate_ir  interacting envs program external_files =
  let benchmarked_evaluate_ir =
    lazy (lazy (evaluate_ir  interacting envs program external_files) |>measure_as<| "evaluate_ir") in
  die_on_exception_unless_interacting interacting benchmarked_evaluate_ir






let sugar_program_to_ir
    (interacting : bool)
    (envs : Evaluation_env.t)
    (sugar_program : Sugartypes.program)
    (pos_context : SourceCode.source_code) :
      Evaluation_env.t * Ir.program * Types.datatype =
  let post_frontend_program, typ, new_tyenv =
    Frontend.Pipeline.program
      envs.Evaluation_env.tyenv
      pos_context
      sugar_program in

  let tyenv = envs.Evaluation_env.tyenv in
  let varified_tenv = Var.varify_env (envs.Evaluation_env.nenv, tyenv.FrontendTypeEnv.var_env) in

  let globals, (locals, main), new_nenv =
    Sugartoir.desugar_program
      (envs.Evaluation_env.nenv,
        varified_tenv,
        new_tyenv.FrontendTypeEnv.effect_row)
      post_frontend_program in


  (* FIXME: It's weird that Sugartoir.desugar_program has some explicitly documented
      logic to distinguish between global and local bindings, but the existing behavior was always
      to join both types of bindings here *)
  let ir_program = (globals @ locals), main in


  let perform_optimisations = Settings.get_value BS.optimise && not interacting in
  let post_backend_ir_program =
    Backend.transform_program
      perform_optimisations
      varified_tenv
      ir_program in

  let updated_evaluation_env =
    {envs with
      Evaluation_env.nenv = Env.String.extend envs.Evaluation_env.nenv new_nenv;
      Evaluation_env.tyenv = FrontendTypeEnv.extend_typing_environment envs.Evaluation_env.tyenv new_tyenv;
      } in

  updated_evaluation_env, post_backend_ir_program, typ


  (* Not public, only used by process_single_file and process_files_and_dependencies.
     Performs frontend pipeline, sugar-to-ir translation, and backend pipeline on an already parsed source file *)
  let file_ast_to_ir
      (interacting : bool)
      (envs : Evaluation_env.t)
      (source : Loader.source)
        : Evaluation_env.t * Ir.binding list * Types.datatype =
    let toplevel_module_binding = source.Loader.program in

    let sugar_program = [toplevel_module_binding], None in


    let updated_evaluation_env, post_backend_ir_program, typ =
      sugar_program_to_ir
        interacting
        envs
        sugar_program
        source.Loader.pos_context in

let die_on_exception f x =
  Errors.display ~default:(fun _ -> exit 1) (lazy (f x))

    let post_backend_ir_bindings, _ = post_backend_ir_program in

    updated_evaluation_env, post_backend_ir_bindings, typ


(* just adds benchmarking and error handling around file_ast_to_ir *)
let file_ast_to_ir  interacting envs source =
  let benchmarked_file_ast_to_ir =
    lazy (lazy (file_ast_to_ir  interacting envs source) |>measure_as<| "file_ast_to_ir") in
  die_on_exception_unless_interacting interacting benchmarked_file_ast_to_ir




  (* Runs a single file  without trying to resolve external dependenices
     (i.e., without processing any additional files *)
let run_single_file
    interacting
    envs
    path
      : Evaluation_env.t * Value.t * Types.datatype =
  let source = Loader.load_source_file path in

  let (new_envs, ir_bindings, typ) =
    file_ast_to_ir
      interacting
      envs
      source in

  let ir_program = Ir.program_of_bindings ir_bindings in

  let result_env, result_value =
    (evaluate_ir : bool -> Evaluation_env.t -> Ir.program -> string list -> (Evaluation_env.t * Value.t))
      interacting
      new_envs
      ir_program
      source.Loader.external_dependencies in
  result_env, result_value, typ

(* just adds benchmarking and error handling around run_single_file *)
let run_single_file interacting envs path =
  let benchmarked_run_single_file =
    lazy (lazy (run_single_file  interacting envs path) |>measure_as<| "run_single_file") in
  die_on_exception_unless_interacting interacting benchmarked_run_single_file





(* Runs a list of files in the given order.
    External dependencies are resolved by processing additional files from the include directory
    to satisfy dependencies, if needed. *)
let run_files_and_dependencies
  interacting
  (envs : Evaluation_env.t)
  paths
    : Evaluation_env.t  =


  let sources_and_dependencies =
    Loader.load_source_files_and_dependencies paths in

  (* First, we perform the processing from source to ir on each file *)
  let updated_envs, (ir_bindings_rev : (Ir.binding list * Loader.source) list) =
    List.fold_left
      (fun (cur_env, bss) source ->
        let (new_env, ir_bindings, _typ) =
          file_ast_to_ir
            interacting
            cur_env
            source in
        new_env, ((ir_bindings, source) :: bss))
      (envs, [])
      sources_and_dependencies in
  let ir_bindings = List.rev ir_bindings_rev in

  (* Then we evaluate everything in the necessary order *)
  List.fold_left
    (fun env (ir_bindings, source) ->
      let ir_program =  Ir.program_of_bindings ir_bindings in
      let (new_env, _) =
        evaluate_ir
          interacting
          env
          ir_program
          source.Loader.external_dependencies in
      new_env
      )
    updated_envs
    ir_bindings


(* just adds benchmarking and error handling around run_files_and_dependencies *)
let run_files_and_dependencies interacting envs paths =
  let benchmarked_run_files_and_dependencies =
    lazy (lazy (run_files_and_dependencies  interacting envs paths) |>measure_as<| "run_files_and_dependencies") in
  die_on_exception_unless_interacting interacting benchmarked_run_files_and_dependencies




let run_string
    (interacting : bool)
    (envs : Evaluation_env.t)
    (s : string) :
      Evaluation_env.t * Value.t * Types.datatype =
  let sugar_program, pos_context = Parse.parse_string ~pp:(Settings.get_value BS.pp) Parse.program s in

  let updated_evaluation_env, post_backend_ir_program, typ =
    sugar_program_to_ir
      interacting
      envs
      sugar_program
      pos_context in

  let post_evaluation_env, value =
    evaluate_ir
      interacting
      updated_evaluation_env
      post_backend_ir_program
      [] in

  post_evaluation_env, value, typ



(* just adds benchmarking and error handling around run_string *)
let run_string interacting envs s =
  let benchmarked_run_string =
    lazy (lazy (run_string  interacting envs s) |>measure_as<| "run_string") in
  die_on_exception_unless_interacting interacting benchmarked_run_string
