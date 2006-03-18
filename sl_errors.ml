(*open Sl_kind *)
open Inferencetypes
open Sl_syntax
open Sl_sugar
open Lexing

exception Type_error of (Sl_syntax.position * string)
exception SyntaxError of string

let mistyped_application pos (fn, fntype) (param, paramtype)
    = match fntype with 
      | `Function _ -> raise (Type_error (pos, "`" ^ prettyprint param
                                            ^"' has type " ^ string_of_kind paramtype
                                            ^" and cannot be passed as an argument to `"^ string_of_expression fn
                                            ^"', which has type "^ string_of_kind fntype))
      | _ -> raise (Type_error (pos,
                                "`"^ string_of_expression fn ^"' is not a function (it has type "^ string_of_kind fntype
                                ^") and cannot be applied to `"^ string_of_expression param ^"'"))                           
               

let mistyped_union pos coll_type l ltype r rtype (* not quite right, e.g. [1] :: [1.] *)
    = raise (Type_error (pos, "Type error in union of "^ string_of_expression l ^" ("^ string_of_kind ltype 
                           ^") and "^ string_of_expression r ^" ("^ string_of_kind rtype ^")"))

let mistype pos (condition, condtype) expected_type
    = raise (Type_error (pos, "`"^ string_of_expression condition
                           ^"' has type "^ string_of_kind condtype
                           ^", but is used in a context where a "^ string_of_kind expected_type
                           ^" is expected"))
               

let nested_def pos var
    = raise (Parse_failure (pos, "Syntax error"
                              ^":\nDefinitions are only allowed at top level, but the definition of `"
                              ^ var ^"' is not at top level."))
  
  
let letrec_nonfunction pos (form, formtype)
    = raise (Parse_failure (pos, "Invalid form:\n  The values bound by letrec (and defrec) must be function forms,"
                              ^"\n  but `" ^ string_of_expression form 
                              ^"' is not a function form"))

let string_of_pos ((pos : Lexing.position), line, expr) = 
  Printf.sprintf "%s:%d: some error" pos.pos_fname pos.pos_lnum


let display_error error line =
  let show_place position = failwith "FIXME (showing place)" in
  match error with
    | Type_error (position, msg) ->
        prerr_endline ("*** " ^ string_of_pos position ^ "\n    " ^  msg);
        show_place position
    | Parse_failure (position, msg) ->
          prerr_endline ("*** " ^ string_of_pos position ^ "\n    " ^ msg);
        show_place position
    | _ -> prerr_endline "Generic error..."; ()

let invalid_name pos name message = 
  raise (Parse_failure (pos, "`"^ name ^"' is an invalid name: " ^ message))


let format_exception = function
  | SyntaxError s -> s
  | Type_error (pos, s) -> "*** Type error at " ^ string_of_pos pos ^ ": \n   " ^ s
  | Sl_result.Runtime_failure s -> "*** Runtime failure: " ^ s
  | Sl_result.Runtime_exception s -> "*** Runtime exception: " ^ s
  | Parse_failure (pos, s) -> "*** Parse failure at " ^ string_of_pos pos ^ ": " ^ s
  | Failure msg -> "*** Fatal error : " ^ msg
  | exn -> "*** Error: " ^ Printexc.to_string exn


let display_errors' default stream (f : 'a -> 'b) (a : 'a) = 
  try 
    f a
  with e ->
    output_string stream (format_exception e ^ "\n");
    flush stream;
    default ()

let display_errors stream default = display_errors' default stream
and display_errors_fatal stream = display_errors' (fun () -> exit 1) stream

