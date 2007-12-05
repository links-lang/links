open Types
open Syntax
open Sugar
open Lexing
open Utility

type synerrspec = {filename : string; linespec : string; 
                   message : string; linetext : string;
                   marker : string}

exception UndefinedVariable of string
    
exception NoMainExpr
exception ManyMainExprs of Syntax.expression list
exception Type_error of (Syntax.position * string)
exception MultiplyDefinedToplevelNames of ((Syntax.position list) stringmap)
exception RichSyntaxError of synerrspec

exception WrongArgumentTypeError of (Syntax.position *
				       string * Types.datatype * 
                                       string list * Types.datatype list *
				       Types.datatype option)

exception MistypedSendError of (Syntax.position *
				       string * Types.datatype * 
                                       string list * Types.datatype list *
				       Types.datatype option)

exception NonfuncAppliedTypeError of (Syntax.position * string * Types.datatype *
					string list * Types.datatype list *
					Types.datatype option)

type expression = Syntax.expression
(*type inference_expression = (Syntax.position * datatype * Syntax.label option) Syntax.expression'*)

let mistyped_application pos (fn, fntype) (params, paramtypes) mb =
  let `T ((_, _, fexpr),_,_) = expression_data fn in
  let pexprs = List.map (fun param -> 
                           let `T ((_, _, pexpr),_,_) = expression_data param in
                             pexpr) params 
  in match fn, paramtypes with
      (* Sadly, this doesn't trigger--I think because of metatypevars
         and other stuff that gets in the way of the type. --eekc 5/07 *)
    | Variable("send", _), [`Application("Mailbox", [mbType]); msgType] -> 
        raise(MistypedSendError(pos,fexpr,fntype,pexprs,paramtypes,mb))
    | _ -> 
        match fntype with 
          | `Function _ -> 
              raise(WrongArgumentTypeError(pos, fexpr, fntype, pexprs, paramtypes, mb))
          | _ -> raise(NonfuncAppliedTypeError(pos, fexpr, fntype, pexprs, paramtypes, mb))
              
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
    = raise(ASTSyntaxError
              (pos, "Invalid form:\n  The values bound by letrec must be function forms,"
                 ^"\n  but `" ^ string_of_expression form 
                 ^"' is not a function form"))

let invalid_name pos name message = 
  raise (ASTSyntaxError (pos, "`"^ name ^"' is an invalid name: " ^ message))

let prefix_lines prefix s =  (* TBD: prepend `prefix' to each line of s *)
  prefix ^ Str.global_replace (Str.regexp "\n") ("\n" ^ prefix) s

let indent n str = String.make n ' ' ^ str

let get_mailbox_msg add_code_tags =
  let wrap =
    if add_code_tags then
      (fun s -> "<code>"^s^"</code>")
    else
      (fun s -> s)
  in
    function
      | None -> ""
      | Some mbtype ->
	  " (mailbox type "^ string_of_datatype mbtype ^ ") "

let rec format_exception = function
  | RichSyntaxError s ->
      ("*** Parse error: " ^ s.filename ^ ":"
       ^ s.linespec ^ "\n"
       ^ s.message ^ "\n" ^ prefix_lines "   " s.linetext ^ "\n"
       ^ "   " ^ s.marker)
  | Getopt.Error s -> s
  | Type_error ((pos,_,expr), s) -> 
      Printf.sprintf "%s:%d: Type error: %s\nIn expression: %s.\n" 
        pos.pos_fname pos.pos_lnum s expr
  | WrongArgumentTypeError(pos, fexpr, fntype, pexpr, paramtype, mb) ->
      let msg = "The expressions `" ^ 
        String.concat "', `" pexpr ^ "' have types\n    " ^ 
        mapstrcat "\n" (indent 2 -<- string_of_datatype) paramtype ^ 
        (get_mailbox_msg false mb)^
        "\nand cannot be passed to function `"^ fexpr ^
        "', which has type\n    "^ string_of_datatype fntype
      in format_exception(Type_error(pos, msg))
  | NonfuncAppliedTypeError(pos, fexpr, fntype, pexpr, paramtype, mb) ->
      let msg = "The expression `"^ fexpr ^"', which has type\n    "^ 
        string_of_datatype fntype ^
        ", cannot be applied to `"^ String.concat ", " pexpr ^"'of types\n    " ^
        mapstrcat ", " string_of_datatype paramtype ^ (get_mailbox_msg false mb)
      in format_exception(Type_error(pos, msg))
  | Result.Runtime_error s -> "*** Runtime error: " ^ s
  | ASTSyntaxError ((pos,_,expr), s) -> 
      Printf.sprintf "%s:%d: Syntax error: %s\nIn expression: %s\n" 
        pos.pos_fname pos.pos_lnum s expr
  | Sugar.RedundantPatternMatch (pos,_,expr) -> 
      Printf.sprintf "%s:%d: Redundant pattern match:\nIn expression: %s\n" 
        pos.pos_fname pos.pos_lnum expr
  | PatternDuplicateNameError((pos,_,expr), name, pattern) -> 
      Printf.sprintf
        "%s:%d: Syntax Error: Duplicate name `%s' in pattern\n  %s\nIn expression: %s" 
        pos.pos_fname pos.pos_lnum name (xml_escape pattern) (xml_escape expr)
  | Failure msg -> "*** Fatal error : " ^ msg
  | MultiplyDefinedToplevelNames duplicates ->
      "Duplicate top-level bindings\n" ^
        StringMap.fold (fun name positions message ->
                          message^" "^name^":\n  "^
			    (mapstrcat "\n  " show_pos (List.rev positions)))
          duplicates ""
  | NoMainExpr -> "Syntax Error: No \"main\" expression at end of file"
  | ManyMainExprs _ -> "Syntax Error: More than one \"main\" expression at end of file"
  | Sys.Break -> "Caught interrupt"
  | exn -> "*** Error: " ^ Printexc.to_string exn

let rec format_exception_html = function
  | RichSyntaxError s ->
      ("<h1>Links Syntax Error</h1>\n<p>Syntax error in <code>" ^ s.filename ^ "</code> line "
       ^ s.linespec ^ ":</p><p>"
       ^ s.message ^ "</p><pre>" ^ xml_escape s.linetext ^ "\n"
       ^ s.marker ^ "</pre>")
  | Getopt.Error s -> s
  | Type_error ((pos,_,expr), s) -> 
      Printf.sprintf ("<h1>Links Type Error</h1>\n<p>Type error at <code>%s</code>:%d:</p> <p>%s</p><p>In expression:</p>\n<pre>%s</pre>\n")
        pos.pos_fname pos.pos_lnum s (xml_escape expr)
  | MistypedSendError(pos, fexpr, fntype, pexpr,
                      ([`Application("Mailbox", [mbType]); msgType] as paramtypes),
                      mb)
    ->
      let msg = "The expressions <code class=\"typeError\">" ^ 
        mapstrcat "\n" (indent 2 -<- xml_escape) pexpr ^ (get_mailbox_msg true mb) ^
        "</code> have type <code class=\"typeError\">" ^ 
        xml_escape (mapstrcat ", " string_of_datatype paramtypes) ^
        "</code> and cannot be passed to function <code class=\"typeError\">"^ xml_escape(fexpr) ^
        "</code>which has type <code class=\"typeError\">"^ xml_escape(string_of_datatype fntype) ^ "</code>" ^ " (this tries to send a message of type " ^ string_of_datatype msgType ^ " to a process expecting message of type " ^ string_of_datatype mbType ^ ")"
      in
        format_exception_html(Type_error(pos, msg))
     
  | WrongArgumentTypeError(pos, fexpr, fntype, pexpr, paramtype, mb) ->
      let msg = "The expression(s) <pre class=\"typeError\">" ^ 
        mapstrcat "</pre><pre class=\"typeError\">" (indent 2 -<- xml_escape) pexpr ^ (get_mailbox_msg true mb) ^
        "</pre> have type(s) <code class=\"typeError\">" ^ 
        mapstrcat "</code><pre class=\"typeError\">" (indent 2 -<- xml_escape -<- string_of_datatype) paramtype ^
        "</code> and cannot be passed to function <pre class=\"typeError\">"^ xml_escape(fexpr) ^
        (* TBD: report the error in terms of argument types ? *)
        "</pre>which has type <code class=\"typeError\">"^ 
        xml_escape(string_of_datatype fntype) ^ "</code>"
      in
        format_exception_html(Type_error(pos, msg))

  | NonfuncAppliedTypeError(pos, fexpr, fntype, pexpr, paramtype, mb) ->
      let msg = "The expression <pre class=\"typeError\">"^ 
        xml_escape fexpr ^"</pre> which has type <code class=\"typeError\">"^ 
        string_of_datatype fntype ^
        "</code> cannot be applied to <pre class=\"typeError\">"^ 
        xml_escape (String.concat ", " pexpr) ^"</pre>, of types <code class=\"typeError\">" ^ 
        mapstrcat ", " string_of_datatype paramtype ^ "</code>" ^ (get_mailbox_msg true mb)
      in
        format_exception_html(Type_error(pos, msg))

  | MultiplyDefinedToplevelNames duplicates -> 
      let show_pos : Syntax.position -> string = fun ((pos : Lexing.position), _, _) ->
        Printf.sprintf "file <code>%s</code>, line %d" pos.Lexing.pos_fname pos.Lexing.pos_lnum
      in
        "<h1>Links Syntax Error</h1><p>Duplicate top-level bindings:</p><ul>" ^
          (StringMap.fold (fun name positions message -> message ^ "<li>" ^ 
                                     name ^ ":<ul>" ^
			             (mapstrcat "\n" 
                                        (fun l -> "<li>" ^ l ^ "</li>")
                                        (List.map show_pos (List.rev positions)))
                                     ^ "</ul></li>\n")
             duplicates "") ^ "</ul>"
          
  | Result.Runtime_error s -> "<h1>Links Runtime Error</h1> " ^ s
  | ASTSyntaxError ((pos,_,expr), s) -> 
      Printf.sprintf "<h1>Links Syntax Error</h1> Syntax error at <code>%s</code> line %d. %s\nIn expression: <code>%s</code>\n" 
        pos.pos_fname pos.pos_lnum s (xml_escape expr)
  | PatternDuplicateNameError((pos,_,expr), name, pattern) -> 
      Printf.sprintf
        "<h1>Links Syntax Error</h1> <p><code>%s</code> line %d:</p><p>Duplicate name <code>%s</code> in pattern\n<code>%s</code>.</p>\n<p>In expression: <code>%s</code></p>" 
        pos.pos_fname pos.pos_lnum name (xml_escape pattern) (xml_escape expr)
  | Failure msg -> "<h1>Links Fatal Error</h1>\n" ^ msg
  | NoMainExpr -> "<h1>Links Syntax Error</h1>\nNo \"main\" expression at end of file"
  | ManyMainExprs es -> "<h1>Links Syntax Error</h1>\nMore than one \"main\" expression at end of file : " ^ 
      mapstrcat "<br/>" Syntax.string_of_expression es
  | Result.UnrealizableContinuation ->
      "<h1>Links Error: Unrealizable continuation</h1> <div>Perhaps the code changed after the previous page was served?</div>"
  | exn -> "<h1>Links Error</h1>\n" ^ Printexc.to_string exn
      (* raise exn (* use for backtraces *) *)

let display ?(default=(fun e -> raise e)) ?(stream=stderr) (e) = 
  try 
    Lazy.force e
  with exc ->
    output_string stream (format_exception exc ^ "\n");
    flush stream;
    default exc

let display_fatal ?(stream=stderr) f a = 
  display ~default:(fun _ -> exit 1) ~stream:stream (lazy (f a))

let display_fatal_l ?(stream=stderr) e =
  display ~default:(fun _ -> exit 1) ~stream:stream e
