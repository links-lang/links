open Notfound

open Types
open Lexing
open Utility

type synerrspec = {filename : string; linespec : string; 
                   message : string; linetext : string;
                   marker : string}

exception UndefinedVariable of string
    
exception ASTSyntaxError = SourceCode.ASTSyntaxError

exception Type_error of (SourceCode.pos * string)
exception MultiplyDefinedToplevelNames of ((SourceCode.pos list) stringmap)
exception RichSyntaxError of synerrspec

exception WrongArgumentTypeError of (SourceCode.pos *
				       string * Types.datatype * 
                                       string list * Types.datatype list *
				       Types.datatype option)

exception NonfuncAppliedTypeError of (SourceCode.pos * string * Types.datatype *
					string list * Types.datatype list *
					Types.datatype option)

exception Runtime_error of string


let show_pos : SourceCode.pos -> string = 
  fun ((pos : Lexing.position), _, _) ->
    Printf.sprintf "%s:%d" pos.Lexing.pos_fname pos.Lexing.pos_lnum
               
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
  | Type_error (pos, s) ->
      let (pos, _, expr) = SourceCode.resolve_pos pos in
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
  | Runtime_error s -> "*** Runtime error: " ^ s
  | ASTSyntaxError (pos, s) -> 
      let (pos,_,expr) = SourceCode.resolve_pos pos in
        Printf.sprintf "%s:%d: Syntax error: %s\nIn expression: %s\n" 
          pos.pos_fname pos.pos_lnum s expr
  | Sugartypes.RedundantPatternMatch pos -> 
      let (pos,_,expr) = SourceCode.resolve_pos pos in
        Printf.sprintf "%s:%d: Redundant pattern match:\nIn expression: %s\n" 
          pos.pos_fname pos.pos_lnum expr
  | Sugartypes.PatternDuplicateNameError(pos, name) ->
      (* BUG: this can't be right (the pattern and the expression are the same!) *)
      let _,_,pattern = SourceCode.resolve_pos pos in
      let (pos,_,expr) = SourceCode.resolve_pos pos in
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
  | Sys.Break -> "Caught interrupt"
  | exn -> "*** Error: " ^ Printexc.to_string exn

let rec format_exception_html = function
  | RichSyntaxError s ->
      ("<h1>Links Syntax Error</h1>\n<p>Syntax error in <code>" ^ s.filename ^ "</code> line "
       ^ s.linespec ^ ":</p><p>"
       ^ s.message ^ "</p><pre>" ^ xml_escape s.linetext ^ "\n"
       ^ s.marker ^ "</pre>")
  | Getopt.Error s -> s
  | Type_error (pos, s) -> 
      let (pos,_,expr) = SourceCode.resolve_pos pos in
        Printf.sprintf ("<h1>Links Type Error</h1>\n<p>Type error at <code>%s</code>:%d:</p> <p>%s</p><p>In expression:</p>\n<pre>%s</pre>\n")
          pos.pos_fname pos.pos_lnum s (xml_escape expr)
  | WrongArgumentTypeError(pos, fexpr, fntype, pexpr, paramtype, mb) ->
      let msg = "The expression(s) <pre class=\"typeError\">" ^ 
        mapstrcat "</pre><pre class=\"typeError\">" (indent 2 -<- xml_escape) pexpr ^ (get_mailbox_msg true mb) ^
        "</pre> have type(s) <code class=\"typeError\">" ^ 
        mapstrcat "</code><pre class=\"typeError\">" (indent 2 -<- xml_escape -<- string_of_datatype) paramtype ^
        "</pre> and cannot be passed to function <pre class=\"typeError\">"^ xml_escape(fexpr) ^
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
      let show_pos : SourceCode.pos -> string = fun ((pos : Lexing.position), _, _) ->
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
          
  | Runtime_error s -> "<h1>Links Runtime Error</h1> " ^ s
  | ASTSyntaxError (pos, s) -> 
      let (pos,_,expr) = SourceCode.resolve_pos pos in
        Printf.sprintf "<h1>Links Syntax Error</h1> Syntax error at <code>%s</code> line %d. %s\nIn expression: <code>%s</code>\n" 
          pos.pos_fname pos.pos_lnum s (xml_escape expr)
  | Sugartypes.PatternDuplicateNameError(pos, name) ->
      (* BUG: this can't be right (the pattern and the expression are the same!) *)
      let _,_,pattern = SourceCode.resolve_pos pos in
      let (pos,_,expr) = SourceCode.resolve_pos pos in
        Printf.sprintf
          "<h1>Links Syntax Error</h1> <p><code>%s</code> line %d:</p><p>Duplicate name <code>%s</code> in pattern\n<code>%s</code>.</p>\n<p>In expression: <code>%s</code></p>" 
          pos.pos_fname pos.pos_lnum name (xml_escape pattern) (xml_escape expr)
  | Failure msg -> "<h1>Links Fatal Error</h1>\n" ^ msg
  | exn -> "<h1>Links Error</h1>\n" ^ Printexc.to_string exn
      (*raise exn (* use for backtraces *) *)

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
