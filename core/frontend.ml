open Utility

module Pipeline :
sig
  val program :
    Types.typing_environment ->
    SourceCode.source_code ->
    Sugartypes.program ->
    ((Sugartypes.program * Types.datatype * Types.typing_environment) * string list)
  val interactive :
    Types.typing_environment ->
    SourceCode.source_code ->
    Sugartypes.sentence ->
    Sugartypes.sentence * Types.datatype * Types.typing_environment
end
=
struct
  let _show s program =
    Debug.print (s ^ ": " ^ Sugartypes.show_program program);
    program

  let _show_sentence s sentence =
    Debug.print (s ^ ": " ^ Sugartypes.show_sentence sentence);
    sentence

  let session_exceptions = Settings.get_value Basicsettings.Sessions.exceptions_enabled

  let repeat_type_check =
    Settings.get_value
      Basicsettings.TypeSugar.check_frontend_transformations


  let for_side_effects ignored_transformer program  =
    let _ = ignored_transformer program in
    program

  type pre_typing_program_transformer =
    Sugartypes.program -> Sugartypes.program

  type post_typing_program_transformer =
    Types.typing_environment -> Sugartypes.program -> Sugartypes.program


  (* Program transformations before type-checking on programs (i.e., non-REPL mode *)
  let program_pre_typing_transformers
    (pos_context : SourceCode.source_code)
    (prev_tyenv : Types.typing_environment) (* type env before checking current program *)
      : pre_typing_program_transformer list =
    let only_if enabled f x =
      if enabled then f x else x in
    [
      (ResolvePositions.resolve_positions pos_context)#program;
      for_side_effects
        CheckXmlQuasiquotes.checker#program;
      for_side_effects
        (DesugarSessionExceptions.settings_check);
      DesugarModules.desugar_program;
      only_if session_exceptions
        DesugarSessionExceptions.wrap_linear_handlers;
      DesugarLAttributes.desugar_lattributes#program;
      LiftRecursive.lift_funs#program;
      DesugarDatatypes.program prev_tyenv;
    ]


  (* Program transformations after type-checking programs (i.e., non-REPL mode *)
  let program_post_typing_transformers
      : post_typing_program_transformer list =
    let only_if enabled f x y =
      if enabled then f x y else y in
    [
      DesugarCP.desugar_program;
      DesugarInners.desugar_program;
      (*only_if session_exceptions
        DesugarSessionExceptions.insert_toplevel_handlers; TODO*)
      only_if session_exceptions
        DesugarSessionExceptions.desugar_program;
      DesugarProcesses.desugar_program;
      DesugarFors.desugar_program;
      DesugarRegexes.desugar_program;
      DesugarFormlets.desugar_program;
      DesugarPages.desugar_program;
      DesugarFuns.desugar_program;
    ]


let program prev_tyenv pos_context program =
  if Settings.get_value Basicsettings.show_pre_frontend_ast then
    Debug.print ("Pre-Frontend AST:\n" ^ Sugartypes.show_program program);


  let pre_transformers =
    program_pre_typing_transformers pos_context prev_tyenv in
  let apply_pre_tc_transformer program transformer =
    transformer program in
  let pre_tc_program =
    List.fold_left
      apply_pre_tc_transformer
      program
      pre_transformers in

  let post_tc_program, t, cur_tyenv =
    TypeSugar.Check.program prev_tyenv pre_tc_program in


  let apply_post_tc_transformer program transformer =
    let post = transformer prev_tyenv program in
    if repeat_type_check then
      let _ = fst3 (TypeSugar.Check.program prev_tyenv post) in
      post
    else
      post
  in
  let result_program =
    List.fold_left
      apply_post_tc_transformer
      post_tc_program
      program_post_typing_transformers in

  let ffi_files = ModuleUtils.get_ffi_files result_program in

  if Settings.get_value Basicsettings.show_post_frontend_ast then
    Debug.print ("Post-Frontend AST:\n" ^ Sugartypes.show_program result_program);

  (result_program, t, cur_tyenv), ffi_files


  type pre_typing_sentence_transformer =
    Sugartypes.sentence -> Sugartypes.sentence

  type post_typing_sentence_transformer =
    Types.typing_environment -> Sugartypes.sentence -> Sugartypes.sentence

  (* Program transformations before type-checking on sentences (i.e., REPL mode *)
  let interactive_pre_typing_transformers
    (pos_context : SourceCode.source_code)
    (prev_tyenv : Types.typing_environment) (* type env before checking current program *)
      : pre_typing_sentence_transformer list =
    [
      (ResolvePositions.resolve_positions pos_context)#sentence;
      for_side_effects
        CheckXmlQuasiquotes.checker#sentence;
      DesugarModules.desugar_sentence;
      DesugarLAttributes.desugar_lattributes#sentence;
      LiftRecursive.lift_funs#sentence;
      DesugarDatatypes.sentence prev_tyenv;
    ]


  (* Program transformations after type-checking sentences (i.e., REPL mode *)
  let interactive_post_typing_transformers
      : post_typing_sentence_transformer list =
    [
      DesugarCP.desugar_sentence;
      DesugarInners.desugar_sentence;
      DesugarProcesses.desugar_sentence;
      DesugarFors.desugar_sentence;
      DesugarRegexes.desugar_sentence;
      DesugarFormlets.desugar_sentence;
      DesugarPages.desugar_sentence;
      DesugarFuns.desugar_sentence;
    ]


let interactive prev_tyenv pos_context sentence =
  if Settings.get_value Basicsettings.show_pre_frontend_ast then
    Debug.print ("Pre-Frontend AST:\n" ^ Sugartypes.show_sentence sentence);

  let pre_transformers =
    interactive_pre_typing_transformers pos_context prev_tyenv in
  let apply_pre_tc_transformer sentence transformer =
    transformer sentence in
  let pre_tc_sentence =
    List.fold_left
      apply_pre_tc_transformer
      sentence
      pre_transformers in

  let (post_tc_sentence, t, post_tyenv) =
    TypeSugar.Check.sentence prev_tyenv pre_tc_sentence in

  let apply_post_tc_transformer sentence transformer =
    let post = transformer prev_tyenv sentence in
    if repeat_type_check then
      let _ = fst3 (TypeSugar.Check.sentence prev_tyenv post) in
      post
    else
      post
  in
  let result_sentence =
    List.fold_left
      apply_post_tc_transformer
      post_tc_sentence
      interactive_post_typing_transformers in

  if Settings.get_value Basicsettings.show_post_frontend_ast then
    Debug.print ("Post-Frontend AST:\n" ^ Sugartypes.show_sentence result_sentence);

   (result_sentence, t, post_tyenv)


end
