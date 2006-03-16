(** Support for importing modules **)

(* Retrieve text from a URL *)
val grab_url : string -> string

(* Find all import directives within a file and return the
corresponding lists of value and type environments *)
val import_modules : Sl_syntax.expression list
                  -> (string * Sl_result.environment * Sl_kind.environment) list
