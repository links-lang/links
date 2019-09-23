open Utility
open Performance

type envs = Var.var Env.String.t * Types.typing_environment
type program = Ir.binding list * Ir.computation * Types.datatype

(* Filename of an external dependency *)
type ext_dep = string

(* Result of loading a file *)
type source = {
  envs: envs;
  program: program;
  external_dependencies: ext_dep list
}



(** Unmarshal an IR program from a file along with naming and typing
    environments and the fresh variable counters.
*)
let read_a filename : ('a) =
  let x, (gc, tc, vc) =
    call_with_open_infile filename ~binary:true Marshal.from_channel
  in
    Utility.gensym_counter := gc;
    Types.type_variable_counter := tc;
    Var.variable_counter := vc;
    x

let read_program filename : (envs * program) = read_a filename

(* measuring only *)
let read_program filename : (envs * program) =
  measure ("read_program "^filename) read_program filename



(** Read source code from a file, parse, infer types and desugar to
    the IR *)
let read_file_source (nenv, tyenv) (filename:string) =
  let sugar, pos_context =
    ModuleUtils.try_parse_file filename in
  (* printf "AST: \n %s \n" (Sugartypes.show_program sugar); *)
  let ((program, t, tenv), ffi_files) = Frontend.Pipeline.program tyenv pos_context sugar in
  let globals, main, nenv =
    Sugartoir.desugar_program
      (nenv,
       Var.varify_env (nenv, tyenv.Types.var_env),
       tyenv.Types.effect_row) program
  in
  {
    envs = (nenv, tenv);
    program = (globals, main, t);
    external_dependencies = ffi_files
  }



(** Loads a named file and prints it as syntax *)
let print filename =
   let _envs, (globals, (locals, main), _t) = read_program filename in
     print_string (Ir.show_program (globals @ locals, main))


let load_file = read_file_source



