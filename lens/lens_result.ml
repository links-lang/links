include Result

let return v = Result.Ok v

let error v = Result.Error v

let bind v ~f =
  match v with
  | Result.Ok p -> f p
  | Result.Error _ as err -> err

let map v ~f =
  bind v ~f:(fun v -> f v |> return)

let of_option v ~error =
  match v with
  | Some v -> return v
  | None -> error v

let try_with f =
  try f () |> return
  with e -> error e

let map_error ~f r =
  match r with
  | Error e -> f e |> error
  | Ok r -> Ok r

module O = struct
  let (>>|) v f = map v ~f

  let (>>=) v f = bind v ~f
end
