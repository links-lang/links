(* source -> untyped_expression list 
   where source <- {filename, string, stream}
*)

val parse_string  : string -> Sl_syntax.untyped_expression list
val parse_file    : string -> Sl_syntax.untyped_expression list
val parse_channel : (in_channel * string) -> Sl_syntax.untyped_expression list
