open Lens_utility
module Type = Phrase_type

type t = {
  table : string;
  name : string;
  alias : string;
  typ : Type.t;
  present : bool;
}
[@@deriving show, sexp]

let make ~table ~name ~alias ~typ ~present =
  { table; name; alias; typ; present }

let name t = t.name

let alias t = t.alias

let present t = t.present

let typ t = t.typ

let set_typ ~typ t = { t with typ }

let table t = t.table

let hide t = { t with present = false }

let rename t ~alias = { t with alias }

let equal c1 c2 =
  (* Equality in this case can be the simple structural equality *)
  c1 = c2

let set_table t ~table = { t with table }

module Compare = struct
  type elt = t [@@deriving show]

  type t = elt [@@deriving show]

  let _ = show_elt

  let compare t1 t2 = String.compare (alias t1) (alias t2)
end

module Set = struct
  include Set.Make (Compare)

  let dummy_alias alias =
    { alias; present = true; table = ""; name = ""; typ = Phrase_type.Bool }

  let alias_set t =
    to_seq t |> Seq.filter present |> Seq.map alias |> Alias.Set.of_seq

  let mem_alias t ~alias = mem (dummy_alias alias) t

  let find_alias t ~alias = find (dummy_alias alias) t

  let find_alias_opt t ~alias = find_opt (dummy_alias alias) t
end

module List = struct
  type elt = t [@@deriving eq]

  type t = elt list [@@deriving eq]

  let present t = List.filter present t

  let aliases t = List.map ~f:alias t

  let present_aliases t = present t |> aliases

  let present_aliases_set t = present t |> aliases |> Alias.Set.of_list

  let find_alias t ~alias = List.find_opt (fun c -> c.alias = alias) t

  let mem_alias t ~alias = find_alias t ~alias |> Option.is_some

  let colset t = Set.of_list t

  let colmap t = List.map ~f:(fun t -> (alias t, t)) t |> Alias.Map.from_alist

  let record_type t =
    let map =
      present t
      |> List.fold_left
           (fun acc v -> String.Map.add v.alias v.typ acc)
           String.Map.empty
    in
    Type.Record map
end
