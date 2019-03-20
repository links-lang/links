(* Some of the errors that Links produces, and ways of displaying them. *)
open SourceCode

type synerrspec = {filename : string; linespec : string;
                   message : string; linetext : string;
                   marker : string}


exception Runtime_error of string
exception UndefinedVariable of string

exception MultiplyDefinedMutualNames of
  ((Position.t list) Utility.stringmap)
exception InvalidMutualBinding of Position.t
exception Type_error of (Position.t * string)
exception RichSyntaxError of synerrspec
exception SugarError of (Position.t * string)
exception UnboundTyCon of (Position.t * string)
exception InternalError of { filename: string; message: string }
exception TyAppArityMismatch of
  { pos: Position.t; name: string; expected: int; provided: int}
exception TyAppKindMismatch of
  { pos: Position.t; name: string; tyarg_number: int;
    expected: string; provided: string }

val format_exception : exn -> string
val format_exception_html : exn -> string

val display : ?default:(exn -> 'a) -> ?stream:out_channel -> ('a lazy_t) -> 'a
