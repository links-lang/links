open Lens_utility

type t = string [@@deriving show, eq]

module Map = struct
  include String.Map

  let find t ~key = find_opt key t
end

module Set = struct
  module Base = String.Set
  include Base

  let pp_pretty fmt cs =
    Format.fprintf fmt "%a"
      (Format.pp_print_list ~pp_sep:(Format.pp_constant " ")
         Format.pp_print_string)
      (elements cs)

  module List = struct
    type elt = Base.t

    type t = elt list

    exception Not_disjoint of Base.t

    let is_disjoint s =
      try
        List.fold_right
          (fun e acc ->
            let int = Base.inter e acc in
            if Base.is_empty int |> not then raise (Not_disjoint int) ;
            Base.union e acc)
          s Base.empty
        |> ignore ;
        Result.return ()
      with Not_disjoint t -> Result.error t
  end

  module Set = struct
    include Set.Make (String.Set)

    let is_disjoint s = elements s |> List.is_disjoint
  end
end
