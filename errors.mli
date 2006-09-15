(* Some of the errors that Links produces, and ways of displaying them. *)

type synerrspec = {filename : string; linespec : string; 
                   message : string; linetext : string;
                   marker : string}

exception NoMainExpr
exception ManyMainExprs of Syntax.expression list
exception MultiplyDefinedToplevelNames of 
  ((Syntax.position list) Utility.stringmap)
exception Type_error of (Syntax.position * string)
exception RichSyntaxError of synerrspec

val mistyped_application : Syntax.position ->
  (Inferencetypes.inference_expression * Inferencetypes.datatype) ->
  (Inferencetypes.inference_expression * Inferencetypes.datatype) ->
  (Inferencetypes.inference_expression * Inferencetypes.datatype) option ->
  'a
               
val mistyped_union : Syntax.position ->
  Inferencetypes.inference_expression ->
  Inferencetypes.datatype ->
  Inferencetypes.inference_expression ->
  Inferencetypes.datatype ->
  'a

val mistype : Syntax.position ->
  Inferencetypes.inference_expression * Inferencetypes.datatype ->
  Inferencetypes.datatype ->
  'a              

val nested_def : Syntax.position -> string -> 'a
  
  
val letrec_nonfunction : Syntax.position -> 
  (Inferencetypes.inference_expression * Inferencetypes.datatype) ->
  'a

val invalid_name : Syntax.position -> string -> string -> 'a

val format_exception : exn -> string
val format_exception_html : exn -> string

val display_errors : out_channel -> (unit -> 'a) -> ('b -> 'a) -> ('b -> 'a)
val display_errors_fatal : out_channel -> ('a -> 'b) -> ('a -> 'b)

