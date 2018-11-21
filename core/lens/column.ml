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
