exception Type_error of (Syntax.position * string)
exception SyntaxError of string

val mistyped_application : Syntax.position ->
  (Inferencetypes.inference_expression * Inferencetypes.inference_type) ->
  (Inferencetypes.inference_expression * Inferencetypes.inference_type) ->
  'a
               
val mistyped_union : Syntax.position ->
  Inferencetypes.inference_expression ->
  Inferencetypes.inference_type ->
  Inferencetypes.inference_expression ->
  Inferencetypes.inference_type ->
  'a

val mistype : Syntax.position ->
  Inferencetypes.inference_expression * Inferencetypes.inference_type ->
  Inferencetypes.inference_type ->
  'a              

val nested_def : Syntax.position -> string -> 'a
  
  
val letrec_nonfunction : Syntax.position -> 
  (Inferencetypes.inference_expression * Inferencetypes.inference_type) ->
  'a

val string_of_pos : (Lexing.position * int * string) -> string


val invalid_name : Syntax.position -> string -> string -> 'a

val format_exception : exn -> string

val display_errors : out_channel -> (unit -> 'a) -> ('b -> 'a) -> ('b -> 'a)
val display_errors_fatal : out_channel -> ('a -> 'b) -> ('a -> 'b)

