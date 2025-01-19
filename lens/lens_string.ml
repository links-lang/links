open Sexplib.Conv

module String = struct
  include String

  let pp = Format.pp_print_string

  let show s = s
end

include String

module Set = struct
  include Lens_set.Make (String)
end

module Map = struct
  include Lens_map.Make (String)

  let sexp_of_t conv v =
    let l = to_list (fun k v -> (k, v)) v in
    sexp_of_list (sexp_of_pair sexp_of_string conv) l

  let t_of_sexp conv v =
    let l = list_of_sexp (pair_of_sexp string_of_sexp conv) v in
    from_alist l
end
