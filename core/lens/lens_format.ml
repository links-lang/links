include Format

type 'a fmt_fn = formatter -> 'a -> unit

let pp_comma b () = fprintf b ", "

let pp_comma_list f b v = pp_print_list ~pp_sep:pp_comma f b v

let pp_newline_list f b v = pp_print_list ~pp_sep:pp_print_newline f b v

let pp_padded_string ~length f v =
  let count = Pervasives.max 0 @@ String.length v in
  Format.fprintf f "%s%s" v @@ String.make count ' '
