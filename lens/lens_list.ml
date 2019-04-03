include List
module Seq = Lens_seq

let rec mem t v ~equal =
  match t with
  | [] -> false
  | x :: xs -> if equal x v then true else mem xs v ~equal

let map t ~f = map f t

let map_if t ~b ~f =
  let f x = if b x then f x else x in
  map ~f t

let rec find t ~f =
  match t with
  | [] -> None
  | x :: xs -> if f x then Some x else find xs ~f

let find_exn t ~f = find t ~f |> fun v -> Lens_option.value_exn v

let for_all t ~f = for_all f t

let rec for_all_or_error t ~f ~error =
  match t with
  | [] -> Result.Ok ()
  | x :: xs ->
      if f x then for_all_or_error xs ~f ~error else Result.Error (error x)

let rec filter_opt t =
  match t with
  | [] -> []
  | Some x :: xs -> x :: filter_opt xs
  | None :: xs -> filter_opt xs

let rec filter_map t ~f =
  match t with
  | [] -> []
  | x :: xs -> (
    match f x with
    | None -> filter_map xs ~f
    | Some y -> y :: filter_map xs ~f )

let rec unzip3 l =
  match l with
  | (v1, v2, v3) :: ys ->
      let l1, l2, l3 = unzip3 ys in
      (v1 :: l1, v2 :: l2, v3 :: l3)
  | [] -> ([], [], [])

let rec take l ~n =
  match (l, n) with
  | [], _ | _, 0 -> []
  | x :: xs, n -> x :: take xs ~n:(n - 1)

let rec drop l ~n =
  match (l, n) with
  | l, 0 -> l
  | _ :: xs, n -> drop xs ~n:(n - 1)
  | [], _ -> []

let zip_exn = combine

let rec zip_nofail l1 l2 =
  match (l1, l2) with
  | x :: xs, y :: ys -> (x, y) :: zip_nofail xs ys
  | _, _ -> []

let rec to_seq l () =
  match l with
  | [] -> Seq.Nil
  | x :: xs -> Seq.Cons (x, to_seq xs)

let for_all2_exn s t ~f = for_all2 f s t
