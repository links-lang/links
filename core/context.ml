(* Compiler (functional) context/state. *)
(* TODO localise environment within their associated compilation units. *)
type t =
  { typing_environment: Types.typing_environment; (* TODO remove. *)
    name_environment: Ir.var Env.String.t;        (* TODO remove. *)
    value_environment: Value.env;
    variable_environment: Types.datatype Env.Int.t; (* TODO remove. *)
    source_code: SourceCode.source_code;          (* TODO remove. *)
    ffi_files: string list;                      (* TODO remove. *)
    operator_table: (int * Operators.Associativity.t) Utility.StringMap.t option } (* TODO remove. *)

let empty_code = new SourceCode.source_code

let empty =
  { typing_environment = Types.empty_typing_environment;
    variable_environment = Env.Int.empty;
    name_environment = Env.String.empty;
    value_environment = Value.Env.empty;
    source_code = empty_code;
    ffi_files = [];
    operator_table = None }

let typing_environment { typing_environment; _ } = typing_environment
let name_environment { name_environment; _ } = name_environment
let value_environment { value_environment; _ } = value_environment
let source_code { source_code; _ } = source_code
let variable_environment { variable_environment; _ } = variable_environment
let ffi_files { ffi_files; _ } = ffi_files
let operator_table { operator_table; _ } = operator_table
