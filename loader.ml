open Utility
open Performance

let read_file_source (filename:string) (nenv, tyenv) =
  let sugar, pos_context =
    lazy (Parse.parse_file Parse.program filename) <|measure_as|> "parse" in
  let program, t, tenv = Frontend.Pipeline.program tyenv pos_context sugar in
  let globals, main, nenv = Sugartoir.desugar_program (nenv, Var.varify_env (nenv, tyenv.Types.var_env)) program in
    (nenv, tenv), (globals, main, t)

(** Load a file.

    TODO: port caching from Oldloader
*)
let load_file : _ -> string -> ((Var.var Env.String.t * Types.typing_environment) * (Ir.binding list * Ir.computation * Types.datatype)) = 
  fun env infile ->
    (* Read & process the source *)
    read_file_source infile env
      
