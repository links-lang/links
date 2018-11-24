open Value

type t = Value.t

let get t ~key =
  Value.unbox_record t
  |> List.find (fun (k,_) -> k = key)
  |> (fun (_,v) -> v)
