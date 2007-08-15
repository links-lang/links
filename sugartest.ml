(** Temporary interface that wires the front end together *)

type 'a t = string -> 'a

let parse f s =
  let lex = Lexer.lexer (fun _ -> ()) in
    f lex (Lexing.from_string s)

let lookup_pos _ = 
  failwith "lookup_pos"

let interactive : TypeSugar.Typed.sentence t =
  fun input ->
    let parsed : Sugartypes.sentence = parse Parser.interactive input in
      (* TODO: callgraph, type checking *)
      TypeSugar.sentence lookup_pos Library.typing_env parsed

let file  : (TypeSugar.Typed.binding list * TypeSugar.Typed.phrase option) t =
  fun input ->
    let parsed : Sugartypes.binding list * Sugartypes.phrase option = parse Parser.file input in
      (* TODO: callgraph, type checking *)
      TypeSugar.file lookup_pos Library.typing_env parsed

let show_expr_type s = 
    try
      let (_,Some (_,(_,p))) = file s in
        print_endline (Types.string_of_datatype p)
    with Unify.Failure e ->
      prerr_endline ("Unify failure : " ^ e)
