include Format

type 'a fmt_fn = formatter -> 'a -> unit

let pp_comma b () = fprintf b ", "

let pp_comma_list f b v = pp_print_list ~pp_sep:pp_comma f b v

let pp_comma_string_list b v = pp_comma_list pp_print_string b v

let pp_newline_list f b v = pp_print_list ~pp_sep:pp_print_newline f b v

let pp_padded ~length fmt f v =
  let str = Format.asprintf "%a" fmt v in
  let count = Pervasives.max 0 @@ length - String.length str in
  Format.fprintf f "%s%s" str @@ String.make count ' '
