type t = Types.typ

let sort (v : t) =
  match v with
  | `Lens (sort) -> sort
  | _e -> failwith "Type is not a lens (LensType.sort)."
