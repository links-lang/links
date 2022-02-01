type ('a, 'st) t = 'st -> 'a * 'st

let return v t = (v, t)

let apply f v t =
  let f, t = f t in
  let v, t = v t in
  (f v, t)

let map f v t =
  let v, t = v t in
  (f v, t)

let bind f v t =
  let v, t = v t in
  f v t

let product v1 v2 t =
  let v1, t = v1 t in
  let v2, t = v2 t in
  ((v1, v2), t)

let run_state ~init f = f init

let state_context f f_inv arr st =
  let res, st' = arr (f st) in
  (res, f_inv st st')

let map_state f st = ((), f st)

let get st = (st, st)

let put st _ = ((), st)

let push_context init arr =
  state_context (fun st -> (init, st)) (fun _ -> snd) arr

let pop_context arr = state_context fst (fun (_, st) st' -> (st', st)) arr

let skip_context arr = state_context snd (fun (st, _) st' -> (st, st')) arr

let rec for_loop ~f fr upto st =
  if fr <= upto then
    let (), st = f fr st in
    for_loop ~f (fr + 1) upto st
  else ((), st)

module Hack = struct
  let in_hack f st =
    let stm = ref st in
    let hack act =
      let res, st = act !stm in
      stm := st;
      res
    in
    let res = f hack in
    (res, !stm)
end

module List = struct
  let rec iter ~f l =
    match l with
    | [] -> return ()
    | x :: xs ->
        fun st ->
          let (), st = f x st in
          iter ~f xs st

  let rec fold ~init ~f l =
    match l with
    | [] -> return init
    | x :: xs ->
        fun st ->
          let init, st = f init x st in
          fold ~init ~f xs st

  let rec map ~f l =
    match l with
    | [] -> return []
    | x :: xs ->
        fun st ->
          let x, st = f x st in
          let xs, st = map ~f xs st in
          (x :: xs, st)

  let lift l = map ~f:(fun x -> x) l

  let rec filter ~f l =
    match l with
    | [] -> return []
    | x :: xs ->
        fun st ->
          let keep, st = f x st in
          let xs, st = filter ~f xs st in
          ((if keep then x :: xs else xs), st)

  module Result = struct
    let rec iter ~f l =
      match l with
      | [] -> return (Result.Ok ())
      | x :: xs -> (
          fun st ->
            let r, st = f x st in
            match r with
            | Result.Error _ -> (r, st)
            | Result.Ok () -> iter ~f xs st)
  end
end

module Option = struct
  let bind ~f opt =
    match opt with
    | None -> return None
    | Some s ->
        fun t ->
          let s, t = f s t in
          (s, t)

  let merge ~f opt1 opt2 =
    match (opt1, opt2) with
    | None, None -> return None
    | Some _, None -> return opt1
    | None, Some _ -> return opt2
    | Some s1, Some s2 ->
        fun st ->
          let v, st = f s1 s2 st in
          (v, st)
end

module Result = struct
  let bind ~f =
    let f res =
      match res with
      | Result.Error e -> return (Result.Error e)
      | Result.Ok v ->
          fun t ->
            let v, t = f v t in
            (v, t)
    in
    bind f

  let map ~f =
    let f x =
      let v = f x in
      Result.Ok v |> return
    in
    bind ~f

  let try_revert state t =
    let res, tnew = state t in
    if Result.is_ok res then (res, tnew) else (res, t)

  module Syntax = struct
    let ( let$+ ) x f = map ~f x

    let ( let$* ) x f = bind ~f x

    let return_ok v = return (Result.Ok v)
  end
end

module Syntax = struct
  let ( <*> ) fa xa = apply fa xa

  let ( let+ ) x f = map f x

  let ( let* ) x f = bind f x

  let ( and+ ) o1 o2 = product o1 o2

  let return v = return v
end
