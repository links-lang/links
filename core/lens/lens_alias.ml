open Utility

type t = string

module Map = struct
  include StringMap

  let find t ~key =
    find_opt key t

  let from_alist l =
    List.to_seq l
    |> of_seq
end

module Set = struct
  include StringSet

  let pp_pretty fmt cs =
    List.iter (fun c -> Format.fprintf fmt "%s " c) (elements cs)
end
