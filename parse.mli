(* source -> untyped_expression list 
   where source <- {filename, string, stream}
*)

val parse_string  : string -> Syntax.untyped_expression list
val parse_file    : string -> Syntax.untyped_expression list
val parse_channel : (in_channel * string) -> Syntax.untyped_expression list

val parse_kind : string -> Kind.assumption
val parse_sentence : (in_channel * string) -> Sugar.sentence'

