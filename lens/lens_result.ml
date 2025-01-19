include Result

let return v = Result.Ok v

let error v = Result.Error v

let bind v ~f =
  match v with
  | Result.Ok p -> f p
  | Result.Error _ as err -> err

let map v ~f = bind v ~f:(fun v -> f v |> return)

let ok_exn v =
  match v with
  | Result.Ok v -> v
  | _ -> failwith "Unexpected error unpacking result."

let ok_internal v ~pp =
  match v with
  | Result.Ok v -> v
  | Result.Error err -> Lens_exception.Internal.raise pp err

let of_option v ~error =
  match v with
  | Some v -> return v
  | None -> error v

let try_with f =
  try f () |> return with
  | e -> error e

let map_error ~f r =
  match r with
  | Error e -> f e |> error
  | Ok r -> Ok r

let of_bool cond ~error = if cond then return () else Result.Error error

let unpack_error_exn t =
  match t with
  | Error e -> e
  | _ -> failwith "Unexpected error unpacking result to error."

let is_ok t =
  match t with
  | Ok _ -> true
  | _ -> false

let is_error t = is_ok t |> not

module O = struct
  let ( >>| ) v f = map v ~f

  let ( >>= ) v f = bind v ~f
end
