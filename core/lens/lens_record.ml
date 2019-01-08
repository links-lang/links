type t = Value.t

let get t ~key =
  Value.unbox_record t
  |> List.find (fun (k,_) -> k = key)
  |> (fun (_,v) -> v)

let set t ~key ~value =
  Value.unbox_record t
  |> List.map (fun (k,v) -> if k = key then k,value else k,v)
  |> Value.box_record

let match_on t1 t2 ~on =
  List.for_all (fun key -> get t1 = get t2) on
