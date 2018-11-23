open Types
open Utility

type t = Types.lens_col

let alias t = t.alias
let exists (cols : t list) (colalias : string) = List.exists (fun c -> alias c = colalias) cols
let present t = t.present
let typ t = t.typ

let hide t =
    { t with present = false }

let rename t ~alias =
    { t with alias }

let equal c1 c2 =
  (* Equality in this case can be the simple structural equality *)
  c1 = c2

module Compare = struct
  type t = Types.lens_col [@@deriving show]

  let compare t1 t2 = String.compare (alias t1) (alias t2)
end


module Set = struct
  include Set.Make (Compare)

  let dummy_alias alias = {
    alias;
    present = true;
    table = "";
    name = "";
    typ = `Not_typed
  }

  let alias_set t =
    to_seq t
    |> Seq.filter present
    |> Seq.map alias
    |> Lens_alias.Set.of_seq

  let mem_alias t ~alias = mem (dummy_alias alias) t

  let find_alias t ~alias = find (dummy_alias alias) t

  let find_alias_opt t ~alias = find_opt (dummy_alias alias) t
end

module Map = Utility.StringMap

module List = struct
  type t = Types.lens_col list

  let present t = List.filter present t

  let aliases t = List.map alias t

  let present_aliases t =
    present t |> aliases

  let find_alias t ~alias =
    List.find_opt (fun c -> c.alias = alias) t

  let mem_alias t ~alias =
    find_alias t ~alias |> OptionUtils.is_some

  let colset t = Set.of_list t

  let colmap t = List.map (fun t -> alias t, t) t |> Map.from_alist

  let record_type t =
    let map = present t
              |> List.fold_left (fun acc v -> StringMap.add v.alias (`Present v.typ) acc) StringMap.empty in
    `Record (map, Unionfind.fresh `Closed, false)
end
