include module type of Format

type 'a fmt_fn = formatter -> 'a -> unit

val pp_comma : unit fmt_fn

val pp_comma_list : 'a fmt_fn -> 'a list fmt_fn

val pp_newline_list : 'a fmt_fn -> 'a list fmt_fn

val pp_padded_string : length:int -> string fmt_fn
