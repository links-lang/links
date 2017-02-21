open Utility

module Pipeline :
sig
  val program :
    Types.typing_environment ->
    SourceCode.source_code ->
    Sugartypes.program ->
    (Sugartypes.program * Types.datatype * Types.typing_environment)
  val interactive :
    Types.typing_environment ->
    SourceCode.source_code ->
    Sugartypes.sentence ->
    Sugartypes.sentence * Types.datatype * Types.typing_environment
end
=
struct
  let _show s program =
    Debug.print (s ^ ": " ^ Sugartypes.Show_program.show program);
    program

  (* (These functions correspond to 'first' in an arrow) *)
  let after_typing f (a, b, c) = (f a, b, c)
  let _after_alias_expansion f (a, b) = (f a, b)

  let program =
    fun tyenv pos_context program ->
      let program = (ResolvePositions.resolve_positions pos_context)#program program in
      (* Module-y things *)
      let program =
        if ModuleUtils.contains_modules program then
          if Settings.get_value Basicsettings.modules then
            let prog_with_deps = Chaser.add_dependencies program in
            DesugarModules.desugarModules prog_with_deps
          else
            failwith ("File contains modules, but modules not enabled. Please set " ^
              "modules flag to true, or run with -m.")
      else program in
      let _program = CheckXmlQuasiquotes.checker#program program in
      (DesugarLAttributes.desugar_lattributes#program
       ->- RefineBindings.refine_bindings#program
       ->- DesugarDatatypes.program tyenv.Types.tycon_env
       ->- TypeSugar.Check.program tyenv
       ->- after_typing ((FixTypeAbstractions.fix_type_abstractions tyenv)#program ->- snd3)
       ->- after_typing ((DesugarCP.desugar_cp tyenv)#program ->- snd3)
       ->- after_typing ((DesugarInners.desugar_inners tyenv)#program ->- snd3)
       ->- after_typing ((DesugarProcesses.desugar_processes tyenv)#program ->- snd3)
       ->- after_typing ((DesugarDbs.desugar_dbs tyenv)#program ->- snd3)
       ->- after_typing ((DesugarFors.desugar_fors tyenv)#program ->- snd3)
       ->- after_typing ((DesugarRegexes.desugar_regexes tyenv)#program ->- snd3)
       ->- after_typing ((DesugarFormlets.desugar_formlets tyenv)#program ->- snd3)
       ->- after_typing ((DesugarPages.desugar_pages tyenv)#program ->- snd3)
       ->- after_typing ((DesugarFuns.desugar_funs tyenv)#program ->- snd3))
        program

  let interactive =
    fun tyenv pos_context sentence ->
      let sentence = (ResolvePositions.resolve_positions pos_context)#sentence sentence in
      let _sentence = CheckXmlQuasiquotes.checker#sentence sentence in
      (DesugarLAttributes.desugar_lattributes#sentence
       ->- RefineBindings.refine_bindings#sentence
       ->- DesugarDatatypes.sentence tyenv
       ->- uncurry TypeSugar.Check.sentence
       ->- after_typing ((FixTypeAbstractions.fix_type_abstractions tyenv)#sentence ->- snd)
       ->- after_typing ((DesugarCP.desugar_cp tyenv)#sentence ->- snd)
       ->- after_typing ((DesugarInners.desugar_inners tyenv)#sentence ->- snd)
       ->- after_typing ((DesugarProcesses.desugar_processes tyenv)#sentence ->- snd)
       ->- after_typing ((DesugarDbs.desugar_dbs tyenv)#sentence ->- snd)
       ->- after_typing ((DesugarFors.desugar_fors tyenv)#sentence ->- snd)
       ->- after_typing ((DesugarRegexes.desugar_regexes tyenv)#sentence ->- snd)
       ->- after_typing ((DesugarFormlets.desugar_formlets tyenv)#sentence ->- snd)
       ->- after_typing ((DesugarPages.desugar_pages tyenv)#sentence ->- snd)
       ->- after_typing ((DesugarFuns.desugar_funs tyenv)#sentence ->- snd)) sentence
end
