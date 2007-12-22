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
    (Sugartypes.sentence * Types.datatype * Types.typing_environment)
end
=
struct
  let show s program =
    Debug.print (s ^ ": " ^ Sugartypes.Show_program.show program);
    program

  let program =
    fun tyenv pos_context program ->
      ((ResolvePositions.resolve_positions pos_context)#program
      ->- DesugarLAttributes.desugar_lattributes#program
(*      ->- show "before refining bindings"*)
      ->- RefineBindings.refine_bindings#program
(*      ->- show "after refining bindings" *)
      ->- TypeSugar.Check.program tyenv) program

  let interactive =
    fun tyenv pos_context sentence ->
      ((ResolvePositions.resolve_positions pos_context)#sentence
      ->- DesugarLAttributes.desugar_lattributes#sentence
      ->- RefineBindings.refine_bindings#sentence
      ->- TypeSugar.Check.sentence tyenv) sentence
end
