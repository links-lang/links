let read lexbuf name : Sl_syntax.untyped_expression list =
  List.map Sl_sugar.desugar (Sl_parser.parse_links (Sl_lexer.lexer ()) (Sl_lexer.initialize_lexbuf name lexbuf))

(* Parse a string containing Links code.
   Return a list of ASTs representing definitions and expressions.
*)
let parse_string string =
  read (Lexing.from_string string) "<string>"
    
(* Read and parse Links code from an input channel.
   Return a list of ASTs representing definitions and expressions.
*)
let parse_channel (channel, name) =
  read (Lexing.from_channel channel) name

(* Open, read and parse a file containing Links code.
   Return a list of ASTs representing definitions and expressions.
*)
let parse_file filename = 
  parse_channel (open_in filename, filename)
