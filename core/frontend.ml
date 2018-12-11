open Utility

module Pipeline :
sig
  val program :
    FrontendTypeEnv.t ->
    SourceCode.source_code ->
    Sugartypes.program ->
    ((Sugartypes.program * Types.datatype * FrontendTypeEnv.t) * string list)
  val interactive :
    FrontendTypeEnv.t ->
    SourceCode.source_code ->
    Sugartypes.sentence ->
    Sugartypes.sentence * Types.datatype * FrontendTypeEnv.t
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

  (* (These functions correspond to 'first' in an arrow) *)
  let after_typing f (a, b, c) = (f a, b, c)
  let _after_alias_expansion f (a, b) = (f a, b)
  let extension enabled f g (a, b, c) =
    if enabled
    then f g (a, b, c)
    else (a, b, c)

  let after_typing_ext enabled = extension enabled after_typing
  let before_typing_ext enabled f x = if enabled then f x else x

  let program_pipeline =
    fun tyenv pos_context program ->
      let program = (ResolvePositions.resolve_positions pos_context)#program program in
      let program = DesugarAlienBlocks.transform_alien_blocks program in
      (* Module-y things *)
      let (program, ffi_files) =
        (program, ModuleUtils.get_ffi_files program)
      in
      let _program = CheckXmlQuasiquotes.checker#program program in
      let () = DesugarSessionExceptions.settings_check program in
      ((( ExperimentalExtensions.check#program
       ->- before_typing_ext session_exceptions DesugarSessionExceptions.wrap_linear_handlers
       ->- DesugarHandlers.desugar_handlers_early#program
       ->- DesugarLAttributes.desugar_lattributes#program
       ->- LiftRecursive.lift_funs#program
       ->- DesugarDatatypes.program tyenv
       ->- uncurry TypeSugar.Check.program
        (*->- after_typing ((FixTypeAbstractions.fix_type_abstractions tyenv)#program ->- snd3)*)
       ->- after_typing ((DesugarCP.desugar_cp tyenv)#program ->- snd3)
       ->- after_typing ((DesugarInners.desugar_inners tyenv)#program ->- snd3)
       ->- after_typing_ext session_exceptions ((DesugarSessionExceptions.insert_toplevel_handlers tyenv)#program ->- snd3)
       ->- after_typing_ext session_exceptions ((DesugarSessionExceptions.desugar_session_exceptions tyenv)#program ->- snd3)
       ->- after_typing ((DesugarProcesses.desugar_processes tyenv)#program ->- snd3)
       ->- after_typing ((DesugarFors.desugar_fors tyenv)#program ->- snd3)
       ->- after_typing ((DesugarRegexes.desugar_regexes tyenv)#program ->- snd3)
       ->- after_typing ((DesugarFormlets.desugar_formlets tyenv)#program ->- snd3)
       ->- after_typing ((DesugarPages.desugar_pages tyenv)#program ->- snd3)
       ->- after_typing ((DesugarFuns.desugar_funs tyenv)#program ->- snd3))
        program), ffi_files)


let program tyenv pos_context program =
  if Settings.get_value Basicsettings.show_pre_frontend_ast then
    Debug.print ("Pre-Frontend AST:\n" ^ Sugartypes.show_program program);

  let ((post_program, _, _), _) as result = program_pipeline tyenv pos_context program in

  if Settings.get_value Basicsettings.show_post_frontend_ast then
    Debug.print ("Post-Frontend AST:\n" ^ Sugartypes.show_program post_program);

  result


  let interactive_pipeline =
    fun tyenv pos_context sentence ->
    let sentence = (ResolvePositions.resolve_positions pos_context)#sentence sentence in
    let _sentence = CheckXmlQuasiquotes.checker#sentence sentence in
      ( ExperimentalExtensions.check#sentence
       ->- DesugarHandlers.desugar_handlers_early#sentence
       ->- DesugarLAttributes.desugar_lattributes#sentence
       ->- LiftRecursive.lift_funs#sentence
       ->- DesugarDatatypes.sentence tyenv
       ->- uncurry TypeSugar.Check.sentence
        (*  ->- after_typing ((FixTypeAbstractions.fix_type_abstractions tyenv)#sentence ->- snd)*)
       ->- after_typing ((DesugarCP.desugar_cp tyenv)#sentence ->- snd)
       ->- after_typing ((DesugarInners.desugar_inners tyenv)#sentence ->- snd)
       ->- after_typing ((DesugarProcesses.desugar_processes tyenv)#sentence ->- snd)
       ->- after_typing ((DesugarFors.desugar_fors tyenv)#sentence ->- snd)
       ->- after_typing ((DesugarRegexes.desugar_regexes tyenv)#sentence ->- snd)
       ->- after_typing ((DesugarFormlets.desugar_formlets tyenv)#sentence ->- snd)
       ->- after_typing ((DesugarPages.desugar_pages tyenv)#sentence ->- snd)
       ->- after_typing ((DesugarFuns.desugar_funs tyenv)#sentence ->- snd)) sentence


let interactive tyenv pos_context sentence =
  if Settings.get_value Basicsettings.show_pre_frontend_ast then
    Debug.print ("Pre-Frontend AST:\n" ^ Sugartypes.show_sentence sentence);

  let (post_sentence, _, _) as result = interactive_pipeline tyenv pos_context sentence in

  if Settings.get_value Basicsettings.show_post_frontend_ast then
    Debug.print ("Post-Frontend AST:\n" ^ Sugartypes.show_sentence post_sentence);

  result


end
