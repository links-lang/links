(*open Kind *)
open Inferencetypes
open Syntax
open Sugar
open Lexing

exception Type_error of (Syntax.position * string)
exception SyntaxError of string

let mistyped_application pos (fn, fntype) (param, paramtype)
    = let ((_, _, fexpr),_,_), ((_, _, pexpr),_,_) = expression_data fn, expression_data param in
      match fntype with 
        | `Function _ -> 
            raise (Type_error (pos, "`" ^ pexpr
                                 ^"' has type " ^ string_of_type paramtype
                                 ^" and cannot be passed as an argument to `"^ fexpr
                                 ^"', which has type "^ string_of_type fntype))
        | _ -> raise (Type_error (pos,
                                  "`"^ fexpr ^"', which has type "^ string_of_type fntype
                                  ^", cannot be applied to `"^ pexpr ^"', of type " ^ string_of_type paramtype))
               

let mistyped_union pos l ltype r rtype (* not quite right, e.g. [1] :: [1.] *)
    = raise (Type_error (pos, "Type error in union of "^ string_of_expression l ^" ("^ string_of_type ltype 
                           ^") and "^ string_of_expression r ^" ("^ string_of_type rtype ^")"))

let mistype pos (condition, condtype) expected_type
    = raise (Type_error (pos, "`"^ string_of_expression condition
                           ^"' has type "^ string_of_type condtype
                           ^", but is used in a context where a "^ string_of_type expected_type
                           ^" is expected"))
               

let nested_def pos var
    = raise (Parse_failure (pos, "Syntax error"
                              ^":\nDefinitions are only allowed at top level, but the definition of `"
                              ^ var ^"' is not at top level."))
  
  
let letrec_nonfunction pos (form, _)
    = raise (Parse_failure (pos, "Invalid form:\n  The values bound by letrec (and defrec) must be function forms,"
                              ^"\n  but `" ^ string_of_expression form 
                              ^"' is not a function form"))

let string_of_pos ((pos : Lexing.position), _, expr) = 
  Printf.sprintf "%s:%d:\nexpression: %s" pos.pos_fname pos.pos_lnum expr


let invalid_name pos name message = 
  raise (Parse_failure (pos, "`"^ name ^"' is an invalid name: " ^ message))


let format_exception = function
  | SyntaxError s -> s
  | Getopt.Error s -> s
  | Type_error ((pos,_,expr), s) -> Printf.sprintf "%s:%d: Type error: %s\nIn expression: %s\n" pos.pos_fname pos.pos_lnum s expr
  | Result.Runtime_failure s -> "*** Runtime failure: " ^ s
  | Result.Runtime_exception s -> "*** Runtime exception: " ^ s
  | Parse_failure ((pos,_,expr), s) -> Printf.sprintf "%s:%d: Syntax error: %s\nIn expression: %s\n" pos.pos_fname pos.pos_lnum s expr
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

