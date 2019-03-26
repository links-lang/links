open Lexing
open Utility
open SourceCode


type synerrspec = {filename : string; linespec : string;
                   message : string; linetext : string;
                   marker : string}

exception UndefinedVariable of string

exception Type_error of (Position.t * string)
exception MultiplyDefinedMutualNames of ((Position.t list) stringmap)
exception InvalidMutualBinding of Position.t
exception RichSyntaxError of synerrspec
exception SugarError of (Position.t * string)
exception Runtime_error of string
exception UnboundTyCon of (Position.t * string)
exception InternalError of { filename: string; message: string }
exception TypeApplicationArityMismatch of
  { pos: Position.t; name: string; expected: int; provided: int}
exception TypeApplicationKindMismatch of
  { pos: Position.t; name: string; tyarg_number: int;
    expected: string; provided: string }

let show_pos : Position.t -> string =
  fun (pos) ->
    let pos = Position.start pos in
    Printf.sprintf "%s:%d" pos.Lexing.pos_fname pos.Lexing.pos_lnum

let prefix_lines prefix s =
  prefix ^ Str.global_replace (Str.regexp "\n") ("\n" ^ prefix) s

let format_exception =
  function
  | RichSyntaxError s ->
      ("*** Parse error: " ^ s.filename ^ ":"
       ^ s.linespec ^ "\n"
       ^ s.message ^ "\n" ^ prefix_lines "  " s.linetext ^ "\n"
       ^ "   " ^ s.marker)
  | SugarError (pos, s) ->
      let pos, expr = Position.resolve_start_expr pos in
        ("*** Syntactic sugar error: " ^ pos.pos_fname ^ ":"
         ^ string_of_int pos.pos_lnum ^ "\n"
         ^ prefix_lines "   " (s ^ "\nIn expression: " ^ expr ^ "\n"))
  | Getopt.Error s -> s
  | Type_error (pos, s) ->
      let pos, expr = Position.resolve_start_expr pos in
        Printf.sprintf "%s:%d: Type error: %s\nIn expression: %s.\n"
          pos.pos_fname pos.pos_lnum s expr
  | UnboundTyCon (pos, tycon) ->
      let pos, _ = Position.resolve_start_expr pos in
      Printf.sprintf "%s:%d: Unbound type constructor %s\n"
                    pos.pos_fname pos.pos_lnum tycon
  | Runtime_error s -> "*** Runtime error: " ^ s
  | Position.ASTSyntaxError (pos, s) ->
      let pos, expr = Position.resolve_start_expr pos in
        Printf.sprintf "%s:%d: Syntax error: %s\nIn expression: %s\n"
          pos.pos_fname pos.pos_lnum s expr
  | Sugartypes.RedundantPatternMatch pos ->
      let pos, expr = Position.resolve_start_expr pos in
        Printf.sprintf "%s:%d: Redundant pattern match:\nIn expression: %s\n"
          pos.pos_fname pos.pos_lnum expr
  | Sugartypes.PatternDuplicateNameError(pos, name) ->
      (* BUG: this can't be right (the pattern and the expression are the same!) *)
      let pos, expr = Position.resolve_start_expr pos in
      let pattern = expr in
        Printf.sprintf
          "%s:%d: Syntax Error: Duplicate name `%s' in pattern\n  %s\nIn expression: %s"
          pos.pos_fname pos.pos_lnum name (xml_escape pattern) (xml_escape expr)
  | Failure msg -> "*** Fatal error : " ^ msg
  | MultiplyDefinedMutualNames duplicates ->
      "*** Error: Duplicate mutually-defined bindings\n" ^
        StringMap.fold (fun name positions message ->
                          message^" "^name^":\n  "^
			    (mapstrcat "\n  " show_pos (List.rev positions)))
          duplicates ""
  | InvalidMutualBinding pos ->
      let pos, expr = Position.resolve_start_expr pos in
      Printf.sprintf
        "%s:%d: Mutual blocks can only contain `fun` and `typename` bindings, but the block contained: %s.\n"
        pos.pos_fname pos.pos_lnum expr
  | InternalError { filename; message } ->
      Printf.sprintf
        "*** Internal Error in %s (Please report as a bug): %s\n"
        filename message
  | TypeApplicationArityMismatch { pos; name; expected; provided } ->
      let pos, expr = Position.resolve_start_expr pos in
      Printf.sprintf ("%s:%d: Arity mismatch: Type %s expects %d type arguments, but %d arguments were provided. In: %s\n")
          pos.pos_fname pos.pos_lnum name expected provided expr
  | TypeApplicationKindMismatch { pos; name; tyarg_number; expected; provided } ->
      let pos, expr = Position.resolve_start_expr pos in
      Printf.sprintf "%s:%d: Kind mismatch: Type argument %d for type constructor %s has kind %s, but an argument of kind %s was expected. \nIn:\n%s\n"
          pos.pos_fname pos.pos_lnum tyarg_number name provided expected expr
  | Sys.Break -> "Caught interrupt"
  | exn -> "*** Error: " ^ Printexc.to_string exn

let format_exception_html = function
  | RichSyntaxError s ->
      ("<h1>Links syntax error</h1>\n<p>Syntax error in <code>" ^ s.filename ^ "</code> line "
       ^ s.linespec ^ ":</p><p>"
       ^ s.message ^ "</p><pre>" ^ xml_escape s.linetext ^ "\n"
       ^ s.marker ^ "</pre>")
  | SugarError (pos, s) ->
      let pos, expr = Position.resolve_start_expr pos in
        ("<h1>Links syntactic sugar error</h1>\n<p>Syntactic sugar error at <code>"
         ^ pos.pos_fname ^"</code>:"
         ^ string_of_int pos.pos_lnum ^ ":</p> <p>"
         ^ s ^ "</p><p>In expression:</p>\n<pre>"
         ^ xml_escape expr ^ "</pre>\n")
  | Getopt.Error s -> s
  | Type_error (pos, s) ->
      let pos, expr = Position.resolve_start_expr pos in
        Printf.sprintf ("<h1>Links type error</h1>\n<p>Type error at <code>%s</code>:%d:</p> <p>%s</p><p>In expression:</p>\n<pre>%s</pre>\n")
          pos.pos_fname pos.pos_lnum s (xml_escape expr)
  | UnboundTyCon (pos, tycon) ->
      let pos, _ = Position.resolve_start_expr pos in
        Printf.sprintf ("<h1>Links type error</h1>\n<p>Type error at <code>%s</code>:%d:</p> <p>Unbound type constructor:</p>\n<pre>%s</pre>\n")
          pos.pos_fname pos.pos_lnum tycon
  | MultiplyDefinedMutualNames duplicates ->
      let show_pos : Position.t -> string = fun pos ->
        let pos, _ = Position.resolve_start_expr pos in
        Printf.sprintf "file <code>%s</code>, line %d" pos.Lexing.pos_fname pos.Lexing.pos_lnum
      in
        "<h1>Links Syntax Error</h1><p>Duplicate mutual bindings:</p><ul>" ^
          (StringMap.fold (fun name positions message -> message ^ "<li>" ^
                                     name ^ ":<ul>" ^
			             (mapstrcat "\n"
                                        (fun l -> "<li>" ^ l ^ "</li>")
                                        (List.map show_pos (List.rev positions)))
                                     ^ "</ul></li>\n")
             duplicates "") ^ "</ul>"

  | InvalidMutualBinding pos ->
      let pos, expr = Position.resolve_start_expr pos in
      ("<h1>Links mutual binding error</h1>\n<p>"
         ^ "<code>" ^ expr ^ "</code> was contained in a mutual block, however "
         ^ "mutual blocks may only contain function and typename bindings."
         ^ "(at line " ^ string_of_int pos.pos_lnum ^ ")</p>\n")
  | Runtime_error s -> "<h1>Links Runtime Error</h1> " ^ s
  | Position.ASTSyntaxError (pos, s) ->
      let pos, expr = Position.resolve_start_expr pos in
        Printf.sprintf "<h1>Links Syntax Error</h1> Syntax error at <code>%s</code> line %d. %s\nIn expression: <code>%s</code>\n"
          pos.pos_fname pos.pos_lnum s (xml_escape expr)
  | Sugartypes.PatternDuplicateNameError(pos, name) ->
      (* BUG: this can't be right (the pattern and the expression are the same!) *)
      let pos, expr = Position.resolve_start_expr pos in
      let pattern = expr in
        Printf.sprintf
          "<h1>Links Syntax Error</h1> <p><code>%s</code> line %d:</p><p>Duplicate name <code>%s</code> in pattern\n<code>%s</code>.</p>\n<p>In expression: <code>%s</code></p>"
          pos.pos_fname pos.pos_lnum name (xml_escape pattern) (xml_escape expr)
  | Failure msg -> "<h1>Links Fatal Error</h1>\n" ^ msg
  | TypeApplicationArityMismatch { pos; name; expected; provided } ->
      let pos, expr = Position.resolve_start_expr pos in
        Printf.sprintf ("<h1>Links type error</h1>\n<p>Type error at <code>%s</code>:%d:</p> <p>In expression:</p>\n<pre>%s</pre>. Type constructor %s expects %d arguments, but %d were provided.\n")
          pos.pos_fname pos.pos_lnum (xml_escape expr) name expected provided
  | TypeApplicationKindMismatch { pos; name; tyarg_number; expected; provided } ->
      let pos, expr = Position.resolve_start_expr pos in
        Printf.sprintf "<h1>Links type error</h1>\n<p>Type error at <code>%s</code>:%d:</p> <p>In expression:</p>\n<pre>%s</pre>. Type argument %d for type constructor %s has kind %s, but an argument of kind %s was expected."
          pos.pos_fname pos.pos_lnum (xml_escape expr) tyarg_number name provided expected
  | InternalError { filename; message } ->
      Printf.sprintf "<h1>Links Internal Error in %s</h1>\n<p>%s</p>" filename message
  | exn -> "<h1>Links Error</h1>\n" ^ Printexc.to_string exn
      (* raise exn  (* use for backtraces *) *)

let display ?(default=(fun e -> raise e)) ?(stream=stderr) (e) =
  try
    Lazy.force e
  with exc ->
    (if Printexc.print_backtraces
     then Printexc.print_backtrace stderr);
    output_string stream (format_exception exc ^ "\n");
    flush stream;
    default exc

let internal_error ~filename ~message =
  InternalError { filename; message }


