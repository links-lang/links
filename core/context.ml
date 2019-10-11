(* Compiler (functional) context/state. *)
type t =
  { typing_environment: Types.typing_environment;
    source_code: SourceCode.source_code }

let empty_code = new SourceCode.source_code

let empty =
  { typing_environment = Types.empty_typing_environment;
    source_code = empty_code }

let typing_environment { typing_environment; _ } = typing_environment
let source_code { source_code; _ } = source_code
