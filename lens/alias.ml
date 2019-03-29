open Lens_utility

type t = string
  [@@deriving show]

module Map = struct
  include String.Map

  let find t ~key =
    find_opt key t
end

module Set = struct
  include String.Set

  let pp_pretty fmt cs =
    List.iter (fun c -> Format.fprintf fmt "%s " c) (elements cs)
end
