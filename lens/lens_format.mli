include module type of Format with type formatter = Format.formatter

type 'a fmt_fn = formatter -> 'a -> unit

(** Returns a formatter which prints a constant string value. *)
val pp_constant : string -> unit fmt_fn

(** Returns a formatter which prints a constant string value, ignoring the input value. *)
val pp_constant_poly : string -> 'a fmt_fn

(** Transforms the input using the function f and prints it using the supplied formatter. *)
val pp_map : f:('a -> 'b) -> 'b fmt_fn -> 'a fmt_fn

(** A formatter which prints a constant ", " *)
val pp_comma : unit fmt_fn

(** Format a list as comma separated values.

    Example: {[
      Format.asprintf "%a" (Format.pp_comma_list Format.pp_print_int) [5, 6, 7]
    ]}

    evaluates to: 5, 6, 7
 *)
val pp_comma_list : 'a fmt_fn -> 'a list fmt_fn

(** Format a list of string as comma separated values.

    Example: {[
      Format.asprintf "%a" Format.pp_comma_string_list ["hello", "world"]
    ]}

    evaluates to: "hello, world"
*)
val pp_comma_string_list : string list fmt_fn

(** Format a list as newline separated values. See [Format.pp_comma_list]. *)
val pp_newline_list : 'a fmt_fn -> 'a list fmt_fn

(** Pad a string so it has [length] characters.

    Example: {[
      Format.asprintf "%a - %s"
        (Format.pp_padded ~length:8 Format.pp_print_string) "hello" "world"
    ]}

    evaluates to: "hello    - world" *)
val pp_padded : length:int -> 'a fmt_fn -> 'a fmt_fn

(** Construct a [show] command of type ['a -> string] from a formatter. *)
val show_of_pp : 'a fmt_fn -> 'a -> string
