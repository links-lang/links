open Utility

module Ts = struct
  let empty = []

  let keys ts = List.map fst ts

  let incr_cols ts i = List.map (fun (offset, ti) -> (offset + i, ti)) ts

  let decr_cols ts i =  List.map (fun (offset, ti) -> (offset - i, ti)) ts

  let append = List.append

  let lookup = List.assoc

  let length = List.length

  (* remove all mappings whose refcols are not in keys *)
  let keep_cols = keep_keys
end

module Vs = struct
  let empty = []

  let key_columns vs : int list = List.map (fst -<- fst) vs

  let incr_cols vs i = List.map (fun ((col, tag), ti) -> ((col + i, tag), ti)) vs

  let decr_cols vs i = List.map (fun ((col, tag), ti) -> ((col - i, tag), ti)) vs

  let append = List.append

  let lookup = List.assoc

  let length = List.length

  let keep_cols vs cols = 
    List.filter (fun ((col, _), _) -> List.mem col cols) vs
end
