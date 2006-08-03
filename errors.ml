open Inferencetypes
open Syntax
open Sugar
open Lexing

type synerrspec = {filename : string; linespec : string; 
                   message : string; linetext : string;
                   marker : string}
    
exception NoMainExpr
exception ManyMainExprs
exception Type_error of (Syntax.position * string)
exception MultiplyDefinedToplevelNames of (Utility.StringMap.t (Syntax.position list))
exception RichSyntaxError of synerrspec

exception WrongArgumentTypeError of (Syntax.position *
				       string * Inferencetypes.datatype * 
                                       string * Inferencetypes.datatype *
				       (string * Inferencetypes.datatype) option)

exception NonfuncAppliedTypeError of (Syntax.position * string * Inferencetypes.datatype * 
					string * Inferencetypes.datatype *
					(string * Inferencetypes.datatype) option)

let mistyped_application pos (fn, fntype) (param, paramtype) mb =
  let ((_, _, fexpr),_,_) = expression_data fn in
  let ((_, _, pexpr),_,_) = expression_data param in
  let mb = match mb with
    | None -> None
    | Some (exp, mbtype) ->
	let ((_, _, mbexpr),_,_) = expression_data exp in
	  Some (mbexpr, mbtype)
  in  
    match fntype with 
      | `Function _ -> 
          raise (WrongArgumentTypeError(pos, fexpr, fntype, pexpr, paramtype, mb))
      | _ -> raise(NonfuncAppliedTypeError(pos, fexpr, fntype, pexpr, paramtype, mb))
          
          
let mistyped_union pos l ltype r rtype (* not quite right, e.g. [1] :: [1.] *)
    = raise (Type_error (pos, "Type error in union of "^ string_of_expression l ^" ("^ string_of_datatype ltype 
                           ^") and "^ string_of_expression r ^" ("^ string_of_datatype rtype ^")"))

let mistype pos (condition, condtype) expected_type
    = raise (Type_error (pos, "`"^ string_of_expression condition
                           ^"' has type "^ string_of_datatype condtype
                           ^", but is used in a context where a "^ string_of_datatype expected_type
                           ^" is expected"))
               

let nested_def pos var
    = raise (ASTSyntaxError (pos, "Syntax error"
                              ^":\nDefinitions are only allowed at top level, but the definition of `"
                              ^ var ^"' is not at top level."))
  
  
let letrec_nonfunction pos (form, _)
    = raise (ASTSyntaxError (pos, "Invalid form:\n  The values bound by letrec (and defrec) must be function forms,"
                              ^"\n  but `" ^ string_of_expression form 
                              ^"' is not a function form"))

let invalid_name pos name message = 
  raise (ASTSyntaxError (pos, "`"^ name ^"' is an invalid name: " ^ message))

let prefix_lines prefix s =  (* TBD: prepend `prefix' to each line of s *)
  prefix ^ Str.global_replace (Str.regexp "\n") ("\n" ^ prefix) s


(* ARGH! This breaks my structure for discriminating HTML/plaintext errors! *)
let get_mailbox_msg add_code_tags =
  let wrap =
    if add_code_tags then
      (fun s -> "<code>"^s^"</code>")
    else
      (fun s -> s)
  in
    function
      | None -> ""
      | Some (mbexpr, mbtype) ->
	  " (mailbox parameter `"^(wrap mbexpr)^"' of type "^
	    string_of_datatype mbtype ^ ") "
  
let rec format_exception = function
  | RichSyntaxError s ->
      ("*** Parse error: " ^ s.filename ^ ":"
       ^ s.linespec ^ "\n"
       ^ s.message ^ "\n" ^ prefix_lines "   " s.linetext ^ "\n"
       ^ "   " ^ s.marker)
  | Getopt.Error s -> s
  | Type_error ((pos,_,expr), s) -> 
      Printf.sprintf "%s:%d: Type error: %s\nIn expression: %s\n" 
        pos.pos_fname pos.pos_lnum s expr
  | WrongArgumentTypeError(pos, fexpr, fntype, pexpr, paramtype, mb) ->
      let msg = "`" ^ pexpr ^
        "' has type " ^ string_of_datatype paramtype ^ (get_mailbox_msg false mb) ^
        " and cannot be passed to function `"^ fexpr ^
        "', which has type "^ string_of_datatype fntype
      in format_exception(Type_error(pos, msg))
  | NonfuncAppliedTypeError(pos, fexpr, fntype, pexpr, paramtype, mb) ->
      let msg = "`"^ fexpr ^"', which has type "^ string_of_datatype fntype ^
        ", cannot be applied to `"^ pexpr ^"', of type " ^ 
        string_of_datatype paramtype ^ (get_mailbox_msg false mb)
      in format_exception(Type_error(pos, msg))
  | Result.Runtime_error s -> "*** Runtime error: " ^ s
  | ASTSyntaxError ((pos,_,expr), s) -> 
      Printf.sprintf "%s:%d: Syntax error: %s\nIn expression: %s\n" 
        pos.pos_fname pos.pos_lnum s expr
  | Failure msg -> "*** Fatal error : " ^ msg
  | MultiplyDefinedToplevelNames duplicates ->
    let show_pos : Syntax.position -> string = fun ((pos : Lexing.position), _, _) ->
      Printf.sprintf "%s:%d" pos.Lexing.pos_fname pos.Lexing.pos_lnum
    in
      "Duplicate top-level bindings\n" ^
        (Utility.StringMap.fold (fun name positions message ->
                                   message^" "^name^":\n  "^
			             (String.concat "\n  " (List.map show_pos (List.rev positions)))) duplicates "")
  | NoMainExpr -> "Syntax Error: No \"main\" expression at end of file"
  | ManyMainExprs -> "Syntax Error: More than one \"main\" expression at end of file"
  | exn -> "*** Error: " ^ Printexc.to_string exn


let rec format_exception_html = function
  | RichSyntaxError s ->
      ("<h1>Links Syntax Error</h1>\n<p>Syntax error in <code>" ^ s.filename ^ "</code> line "
       ^ s.linespec ^ ":</p><p>"
       ^ s.message ^ "</p><pre>" ^ Utility.xml_escape s.linetext ^ "\n"
       ^ s.marker ^ "</pre>")
  | Getopt.Error s -> s
  | Type_error ((pos,_,expr), s) -> 
      Printf.sprintf ("<h1>Links Type Error</h1>\n<p>Type error at <code>%s</code>:%d:</p> <p>%s. In expression:</p>\n<pre>%s</pre>\n")
        pos.pos_fname pos.pos_lnum s (Utility.xml_escape expr)
  | WrongArgumentTypeError(pos, fexpr, fntype, pexpr, paramtype, mb) ->
      let msg = "<code>" ^ Utility.xml_escape(pexpr) ^ (get_mailbox_msg true mb) ^
        "</code> has type <code>" ^ (Utility.xml_escape(string_of_datatype paramtype)) ^
        "</code> and cannot be passed to function <code>"^ Utility.xml_escape(fexpr) ^
        "</code>, which has type <code>"^ Utility.xml_escape(string_of_datatype fntype) ^ "</code>"
      in
        format_exception_html(Type_error(pos, msg))
  | NonfuncAppliedTypeError(pos, fexpr, fntype, pexpr, paramtype, mb) ->
      let msg = "<code>"^ fexpr ^"</code>, which has type <code>"^ string_of_datatype fntype ^
        "</code>, cannot be applied to <code>"^ pexpr ^"</code>, of type <code>" ^ 
        string_of_datatype paramtype ^ "</code>" ^ (get_mailbox_msg true mb)
      in
        format_exception_html(Type_error(pos, msg))
   
  | MultiplyDefinedToplevelNames duplicates -> 
      let show_pos : Syntax.position -> string = fun ((pos : Lexing.position), _, _) ->
        Printf.sprintf "file <code>%s</code>, line %d" pos.Lexing.pos_fname pos.Lexing.pos_lnum
      in
        "<h1>Links Syntax Error</h1><p>Duplicate top-level bindings:</p><ul>" ^
          (Utility.StringMap.fold (fun name positions message -> message ^ "<li>" ^ 
                                     name ^ ":<ul>" ^
			             (String.concat "\n" 
                                        (List.map (fun l -> "<li>" ^ l ^ "</li>")
                                           (List.map show_pos (List.rev positions))))
                                     ^ "</ul></li>\n")
             duplicates "") ^ "</ul>"
          
  | Result.Runtime_error s -> "<h1>Links Runtime Error</h1> " ^ s
  | ASTSyntaxError ((pos,_,expr), s) -> 
      Printf.sprintf "<h1>Links Syntax Error</h1> Syntax error at <code>%s</code> line %d. %s\nIn expression: %s\n" 
        pos.pos_fname pos.pos_lnum s expr
  | Failure msg -> "<h1>Links Fatal Error</h1>\n" ^ msg
  | NoMainExpr -> "<h1>Links Syntax Error</h1>\nNo \"main\" expression at end of file"
  | ManyMainExprs -> "<h1>Links Syntax Error</h1>\nMoer than one \"main\" expression at end of file"
  | exn -> "<h1>Links Error</h1>\n" ^ Printexc.to_string exn




let display_errors' default stream (f : 'a -> 'b) (a : 'a) = 
  try 
    f a
  with e ->
    output_string stream (format_exception e ^ "\n");
    flush stream;
    default ()

let display_errors stream default = display_errors' default stream
and display_errors_fatal stream = display_errors' (fun () -> exit 1) stream

