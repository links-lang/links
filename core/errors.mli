(* Some of the errors that Links produces, and ways of displaying them. *)
open SourceCode

type synerrspec = {filename : string; linespec : string;
                   message : string; linetext : string;
                   marker : string}

type sugar_error_stage =
  | DesugarFormlets
  | DesugarRegexes
  | CheckQuasiquotes
  | DesugarLAttributes
  | DesugarPages
  | CheckXML
  | DesugarInners
  | DesugarModules


exception RuntimeError of string
exception UndefinedVariable of string
exception InvalidMutualBinding of Position.t
exception Type_error of (Position.t * string)
exception IRTypeError of string
exception MultiplyDefinedMutualNames of
  ((Position.t list) Utility.stringmap)
exception RichSyntaxError of synerrspec
exception DesugaringError of
  { pos: Position.t; stage: sugar_error_stage; message: string }
exception UnboundTyCon of (Position.t * string)
exception InternalError of { filename: string; message: string }
exception TypeApplicationArityMismatch of
  { pos: Position.t; name: string; expected: int; provided: int}
exception TypeApplicationKindMismatch of
  { pos: Position.t; name: string; tyarg_number: int;
    expected: string; provided: string }
exception SettingsError of string
exception DynlinkError of string
exception ModuleError of string * Position.t option
exception MissingBuiltinType of string

val format_exception : exn -> string
val format_exception_html : exn -> string

val display : ?default:(exn -> 'a) -> ?stream:out_channel -> ('a lazy_t) -> 'a

val internal_error : filename:string -> message:string -> exn (* filename in which internal error occurred *)
val desugaring_error: pos:Position.t -> stage:sugar_error_stage -> message:string -> exn
val settings_error: string -> exn
val runtime_error: string -> exn
val dynlink_error: string -> exn
val module_error : ?pos:Position.t -> string -> exn
val disabled_extension : ?pos:Position.t -> ?setting:(string * bool) -> ?flag:string -> string -> exn
val prime_alien : Position.t -> exn
val driver_locate_failure : string -> exn
val illformed_plugin_description : string -> exn
val dependency_load_failure : string -> Dynlink.error -> exn
val load_failure : string -> Dynlink.error -> exn
val client_call_outside_webmode : string -> exn
