module type OrderedShow = sig
  type t [@@deriving show]

  include Map.OrderedType with type t := t
end

module type S = Lens_map_intf.S

module Make (Ord : OrderedShow) = struct
  include Map.Make (Ord)

  let find_exn m ~key = find key m

  let find m ~key = find_opt key m

  let from_alist l = List.fold_right (fun (k, v) m -> add k v m) l empty

  let to_list f m = fold (fun k v l -> f k v :: l) m []

  let pp af formatter map =
    Format.pp_open_box formatter 0;
    Format.pp_print_string formatter "{";
    iter
      (fun key value ->
        Format.pp_open_box formatter 0;
        Ord.pp formatter key;
        Format.pp_print_string formatter " => ";
        af formatter value;
        Format.fprintf formatter ";@;";
        Format.pp_close_box formatter ())
      map;
    Format.pp_print_string formatter "}";
    Format.pp_close_box formatter ()

  let show af x = Format.asprintf "%a" (pp af) x
end
