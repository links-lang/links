open Lexing
open Utility
open SourceCode


type synerrspec = {filename : string; linespec : string;
                   message : string; linetext : string;
                   marker : string}

type sugar_error_stage =
  | DesugarFormlets
  | DesugarRegexes
  | CheckQuasiquotes
  | DesugarLAttributes
  | DesugarPages
  | CheckXML
  | DesugarInners

let string_of_stage = function
  | DesugarFormlets    -> "compiling formlets"
  | DesugarRegexes     -> "compiling regular expressions"
  | CheckQuasiquotes   -> "checking quasiquotes"
  | DesugarLAttributes -> "compiling attributes"
  | DesugarPages       -> "compiling page expressions"
  | CheckXML           -> "checking XML"
  | DesugarInners      -> "desugaring inner types"

exception RuntimeError of string
exception UndefinedVariable of string
exception InvalidMutualBinding of Position.t
exception Type_error of (Position.t * string)
exception IRTypeError of string
exception MultiplyDefinedMutualNames of ((Position.t list) stringmap)
exception RichSyntaxError of synerrspec
exception DesugaringError of
  { pos: Position.t; stage: sugar_error_stage; message: string }
exception UnboundTyCon of (Position.t * string)
exception InternalError of { filename: string; message: string }
exception TypeApplicationArityMismatch of
  { pos: Position.t; name: string; expected: int; provided: int}
exception TypeApplicationKindMismatch of
  { pos: Position.t; name: string; tyarg_number: int;
    expected: string; provided: string }
exception SettingsError of string
exception DynlinkError of string


let prefix_lines prefix s =
  prefix ^ Str.global_replace (Str.regexp "\n") ("\n" ^ prefix) s

let pos_prefix ?pos:(pos=None) line =
  let prefix =
    match pos with
      | Some pos -> Printf.sprintf "%s:%d:" pos.pos_fname pos.pos_lnum
      | None -> "***" in
  Printf.sprintf "%s:%s " prefix line

let format_exception =
  function
  | RichSyntaxError s ->
      pos_prefix ("Parse error: " ^ s.filename ^ ":"
       ^ s.linespec ^ "\n"
       ^ s.message ^ "\n" ^ prefix_lines "  " s.linetext ^ "\n"
       ^ "   " ^ s.marker)
  | DesugaringError { pos; stage; message } ->
      let pos, expr = Position.resolve_start_expr pos in
      pos_prefix ~pos:(Some pos)
        (Printf.sprintf "Error %s: %s\nIn expression: %s.\n"
           (string_of_stage stage) message expr)
  | Getopt.Error s -> s
  | Type_error (pos, s) ->
      let pos, expr = Position.resolve_start_expr pos in
      pos_prefix ~pos:(Some pos)
        (Printf.sprintf "Type error: %s\nIn expression: %s.\n" s expr)
  | IRTypeError msg -> pos_prefix (Printf.sprintf "IR Type Error: %s" msg)
  | UnboundTyCon (pos, tycon) ->
      let pos, _ = Position.resolve_start_expr pos in
      pos_prefix ~pos:(Some pos)
        (Printf.sprintf "Unbound type constructor %s\n" tycon)
  | RuntimeError s -> pos_prefix ("Runtime error: " ^ s)
  | Position.ASTSyntaxError (pos, s) ->
      let pos, expr = Position.resolve_start_expr pos in
      pos_prefix ~pos:(Some pos)
        (Printf.sprintf "Syntax error: %s\nIn expression: %s\n" s expr)
  | Failure msg -> pos_prefix ("Fatal error : " ^ msg)
  | MultiplyDefinedMutualNames duplicates ->
      pos_prefix
        ("Error: Duplicate mutually-defined bindings\n" ^
          StringMap.fold (fun name positions message ->
                            message^" "^name^":\n  "^
                  (mapstrcat "\n  " Position.show (List.rev positions)))
          duplicates "")
  | InvalidMutualBinding pos ->
      let pos, expr = Position.resolve_start_expr pos in
      pos_prefix ~pos:(Some pos)
        (Printf.sprintf
           "Mutual blocks can only contain `fun` and `typename` bindings, but the block contained: %s.\n" expr)
  | InternalError { filename; message } ->
      pos_prefix
      (Printf.sprintf
         "Internal Error in %s (Please report as a bug): %s\n"
         filename message)
  | TypeApplicationArityMismatch { pos; name; expected; provided } ->
      let pos, expr = Position.resolve_start_expr pos in
      pos_prefix ~pos:(Some pos)
        (Printf.sprintf ("Arity mismatch: Type %s expects %d type arguments, but %d arguments were provided. In: %s\n")
             name expected provided expr)
  | TypeApplicationKindMismatch { pos; name; tyarg_number; expected; provided } ->
      let pos, expr = Position.resolve_start_expr pos in
      pos_prefix ~pos:(Some pos)
        (Printf.sprintf "Kind mismatch: Type argument %d for type constructor %s has kind %s, but an argument of kind %s was expected. \nIn:\n%s\n"
             tyarg_number name provided expected expr)
  | SettingsError message ->
      pos_prefix (Printf.sprintf "Settings Error: %s" message)
  | Sys.Break -> "Caught interrupt"
  | exn -> pos_prefix ("Error: " ^ Printexc.to_string exn)

let format_exception_html e =
  Printf.sprintf "<h1>Links Error</h1><p>%s</p>\n" (format_exception e)

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

let desugaring_error ~pos ~stage ~message =
  DesugaringError { pos; stage; message }

let settings_error message = (SettingsError message)
let runtime_error message = (RuntimeError message)
let dynlink_error message = (DynlinkError message)
