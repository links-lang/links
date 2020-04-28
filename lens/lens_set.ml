module type OrderedShow = sig
  type t [@@deriving show]

  include Set.OrderedType with type t := t
end

module Seq = Lens_seq

module type S = Lens_set_intf.S

module Make (Ord : OrderedShow) = struct
  include Set.Make ((Ord : OrderedShow))

  let of_seq s = Seq.fold_left (fun set v -> add v set) empty s

  let to_seq s = elements s |> Lens_list.to_seq

  let union_all s = List.fold_right union s empty

  let pp formatter set =
    Format.pp_open_box formatter 0;
    Format.pp_print_string formatter "{";
    iter
      (fun elt ->
        Format.pp_open_box formatter 0;
        Ord.pp formatter elt;
        Format.fprintf formatter ";@;";
        Format.pp_close_box formatter ())
      set;
    Format.pp_print_string formatter "}";
    Format.pp_close_box formatter ()

  let show : t -> string = fun x -> Format.asprintf "%a" pp x
end
