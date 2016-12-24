(* Some of the errors that Links produces, and ways of displaying them. *)

type synerrspec = {filename : string; linespec : string;
                   message : string; linetext : string;
                   marker : string}


exception Runtime_error of string
exception UndefinedVariable of string

exception MultiplyDefinedToplevelNames of
  ((SourceCode.pos list) Utility.stringmap)
exception Type_error of (SourceCode.pos * string)
exception RichSyntaxError of synerrspec
exception SugarError of (SourceCode.pos * string)

val format_exception : exn -> string
val format_exception_html : exn -> string

val display_fatal : ?stream:out_channel -> ('a -> 'b) -> ('a -> 'b)
val display_fatal_l : ?stream:out_channel -> ('b lazy_t) -> 'b
val display : ?default:(exn -> 'a) -> ?stream:out_channel -> ('a lazy_t) -> 'a
