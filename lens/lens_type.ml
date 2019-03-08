type t = Lens of Lens_sort.t

let pp f _ = Format.fprintf f "Lens"

let show _ = "Lens"

let sort v =
  match v with
  | Lens sort -> sort

let equal t1 t2 =
  match t1, t2 with
  | Lens sort1, Lens sort2 -> Lens_sort.equal sort1 sort2
