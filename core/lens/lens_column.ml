open Types
open Lens_utility

type t = Types.lens_col

let name t = t.name
let alias t = t.alias
let present t = t.present
let typ t = t.typ

let table t = t.table

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

module List = struct
  type elt = t
  type t = elt list

  let present t = List.filter present t

  let aliases t = List.map alias t

  let present_aliases t =
    present t |> aliases

  let find_alias t ~alias =
    List.find_opt (fun c -> c.alias = alias) t

  let mem_alias t ~alias =
    find_alias t ~alias |> Option.is_some

  let colset t = Set.of_list t

  let colmap t = List.map (fun t -> alias t, t) t |> Lens_alias.Map.from_alist

  let record_type t =
    let map = present t
              |> List.fold_left (fun acc v -> String.Map.add v.alias (`Present v.typ) acc) String.Map.empty in
    `Record (map, Unionfind.fresh `Closed, false)
end
