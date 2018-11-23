open Utility

type t = string

module Set = struct
  include StringSet

  let pp_pretty fmt cs =
    List.iter (fun c -> Format.fprintf fmt "%s " c) (elements cs)
end
