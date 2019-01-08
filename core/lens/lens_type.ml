type t = Types.typ

let sort (v : t) =
  match v with
  | `Lens (sort) -> sort
  | _e -> failwith "Type is not a lens (LensType.sort)."

let equal t1 t2 =
  match t1, t2 with
  | `Lens sort1, `Lens sort2 -> Lens_sort.equal sort1 sort2
  | _, _ -> false
