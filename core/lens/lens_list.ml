open Utility

include List

let rec filter_map t ~f =
  match t with
  | [] -> []
  | x :: xs ->
    (match f x with
     | None -> filter_map xs ~f
     | Some y -> y :: filter_map xs ~f)

let rec unzip3 l =
  match l with
  | (v1, v2, v3) :: ys ->
    let l1, l2, l3 = unzip3 ys in
    v1 :: l1, v2 :: l2, v3 :: l3
  | [] -> [], [], []
