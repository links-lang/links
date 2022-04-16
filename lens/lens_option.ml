type 'a t = 'a option

let iter v ~f =
  match v with
  | Some v -> f v
  | None -> ()

let bind v ~f =
  match v with
  | None -> None
  | Some v -> f v

let map v ~f =
  let f x = Some (f x) in
  bind ~f v

let map2 v1 v2 ~f = bind ~f:(fun v1 -> map ~f:(fun v2 -> f v1 v2) v2) v1

let combine v1 v2 ~f =
  match v1 with
  | None -> v2
  | Some v1 -> (
      match v2 with
      | None -> Some v1
      | Some v2 -> Some (f v1 v2))

let value v ~default =
  match v with
  | None -> default
  | Some v -> v

let value_exn ?(exn = Failure "Tried to unpack option") v =
  match v with
  | None -> raise exn
  | Some v -> v

let return v = Some v

let is_some v =
  match v with
  | Some _ -> true
  | None -> false

let is_none v = is_some v |> not

let negate v ~value =
  match v with
  | None -> Some value
  | Some _ -> None

let equal f v1 v2 =
  match (v1, v2) with
  | None, None -> true
  | Some v1, Some v2 -> f v1 v2
  | _ -> false
