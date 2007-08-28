(* Some of the errors that Links produces, and ways of displaying them. *)

type synerrspec = {filename : string; linespec : string; 
                   message : string; linetext : string;
                   marker : string}

exception UndefinedVariable of string

exception NoMainExpr
exception ManyMainExprs of Syntax.expression list
exception MultiplyDefinedToplevelNames of 
  ((Syntax.position list) Utility.stringmap)
exception Type_error of (Syntax.position * string)
exception RichSyntaxError of synerrspec


val mistyped_application : Syntax.position ->
  (Syntax.expression * Types.datatype) ->
  (Syntax.expression list * Types.datatype list) ->
  Types.datatype option ->
  'a
               
val mistyped_union : Syntax.position ->
  Syntax.expression ->
  Types.datatype ->
  Syntax.expression ->
  Types.datatype ->
  'a

val mistype : Syntax.position ->
  Syntax.expression * Types.datatype ->
  Types.datatype ->
  'a              

val nested_def : Syntax.position -> string -> 'a
  
  
val letrec_nonfunction : Syntax.position -> 
  (Syntax.expression * Types.datatype) ->
  'a

val invalid_name : Syntax.position -> string -> string -> 'a

val format_exception : exn -> string
val format_exception_html : exn -> string

val display_fatal : ?stream:out_channel -> ('a -> 'b) -> ('a -> 'b)
val display_fatal_l : ?stream:out_channel -> ('b lazy_t) -> 'b
val display : ?default:(exn -> 'a) -> ?stream:out_channel -> ('a lazy_t) -> 'a
