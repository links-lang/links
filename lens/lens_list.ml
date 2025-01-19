include List
module Seq = Lens_seq

let rec mem t v ~equal =
  match t with
  | [] -> false
  | x :: xs -> if equal x v then true else mem xs v ~equal

let iter t ~f = iter f t

let mapi t ~f = mapi f t

let map t ~f = map f t

let rec map_result t ~f =
  match t with
  | [] -> Lens_result.return []
  | x :: xs -> (
      match f x with
      | Result.Ok x -> (
          match map_result xs ~f with
          | Result.Ok xs -> x :: xs |> Lens_result.return
          | Result.Error _ as err -> err)
      | Result.Error _ as err -> err)

let map_if t ~b ~f =
  let f x = if b x then f x else x in
  map ~f t

let findi t ~f =
  let rec fr t ind =
    match t with
    | [] -> None
    | x :: xs -> if f x then Some (ind, x) else fr xs (ind + 1)
  in
  fr t 0

let find t ~f = findi t ~f |> Lens_option.map ~f:snd

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
      | Some y -> y :: filter_map xs ~f)

let rec unzip3 l =
  match l with
  | (v1, v2, v3) :: ys ->
      let l1, l2, l3 = unzip3 ys in
      (v1 :: l1, v2 :: l2, v3 :: l3)
  | [] -> ([], [], [])

let rec take l ~n =
  match (l, n) with
  | [], _
   |_, 0 ->
      []
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

let groupBy (type a b) (module M : Lens_map.S with type key = a) ~(f : b -> a)
    (s : b list) : (a * b list) list =
  let open M in
  let grps =
    List.fold_right
      (fun v acc ->
        let key = f v in
        M.update key
          (function
            | None -> Some [ v ]
            | Some vs -> Some (v :: vs))
          acc)
      s empty
  in
  M.to_list (fun k v -> (k, v)) grps
