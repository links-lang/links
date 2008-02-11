open Utility

module Pipeline :
sig
  val program :
    Types.typing_environment ->
    Types.alias_environment ->
    SourceCode.source_code ->
    Sugartypes.program ->
    (Sugartypes.program * Types.datatype * Types.typing_environment)
  val interactive :
    Types.typing_environment ->
    Types.alias_environment ->
    SourceCode.source_code ->
    Sugartypes.sentence ->
    Sugartypes.sentence * Types.datatype * Types.typing_environment * Types.alias_environment
end
=
struct
  let show s program =
    Debug.print (s ^ ": " ^ Sugartypes.Show_program.show program);
    program

  (* (These functions correspond to 'first' in an arrow) *)
  let after_typing f (a, b, c) = (f a, b, c)
  let after_alias_expansion f (a, b) = (f a, b)

  let program =
    fun tyenv aliases pos_context program ->
      ((ResolvePositions.resolve_positions pos_context)#program
      ->- DesugarLAttributes.desugar_lattributes#program
      ->- RefineBindings.refine_bindings#program
      ->- DesugarDatatypes.program
      ->- (ExpandAliases.expand_aliases aliases)#program ->- snd
      ->- TypeSugar.Check.program tyenv aliases
      ->- after_typing DesugarRegexes.desugar_regexes#program
      ->- after_typing DesugarFormlets.desugar_formlets#program
      ->- after_typing DesugarPages.desugar_pages#program)
      program

  let expand_aliases env sentence =
    let s, sentence = (ExpandAliases.expand_aliases env)#sentence sentence in
      sentence, s#aliases

  let interactive =
    fun tyenv aliases pos_context ->
      ((ResolvePositions.resolve_positions pos_context)#sentence
      ->- DesugarLAttributes.desugar_lattributes#sentence
      ->- RefineBindings.refine_bindings#sentence
      ->- DesugarDatatypes.sentence
      ->- expand_aliases aliases
      ->- after_alias_expansion (TypeSugar.Check.sentence tyenv aliases)
      ->- after_alias_expansion (after_typing DesugarRegexes.desugar_regexes#sentence)
      ->- after_alias_expansion (after_typing DesugarFormlets.desugar_formlets#sentence)
      ->- after_alias_expansion (after_typing DesugarPages.desugar_pages#sentence)
      ->- fun ((s, d, t), a) -> s, d, t, a)
end
