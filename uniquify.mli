type unique_ast

(* Gets the AST component of a uniquified AST *)
val get_ast : unique_ast -> Sugartypes.program

(* Creates a uniquified AST from a plain AST *)
val uniquify_ast : Sugartypes.program -> unique_ast

(* Looks up a uniquified variable from the unique ast, resolves to a plain name *)
val lookup_var : Sugartypes.name -> unique_ast -> Sugartypes.name
