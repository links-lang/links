(* A few graph algorithms.  Pure interfaces, but impure
   implementations.  A graph is represented as node lists + adjacency
   list.  Some abstraction might be nice.
*)

open Sl_utility



(* Utility bits for hashtables and mutable lists *)
let push elem list = 
  list := elem :: !list;
  list

let pushnew elem list = 
  if not (List.mem elem !list) then
    push elem list
  else list

let find_def table elem def = 
  try Hashtbl.find table elem
  with Not_found -> def

let set table k v = 
  Hashtbl.replace table k v;
  v

let hashtbl_as_list tbl = 
  let list = ref [] in
    Hashtbl.iter (fun k v -> list := (k,v) :: !list) tbl;
    !list 

(* CLR 23.3 *)
let dfs nodes edges = 
  let nnodes = List.length nodes in
  let color = Hashtbl.create nnodes 
  and pi = Hashtbl.create nnodes 
  and d = Hashtbl.create nnodes 
  and f = Hashtbl.create nnodes in
    List.iter (fun u ->
		 Hashtbl.add color u `white;
		 Hashtbl.add pi u None;
              ) nodes;
    let time = ref 0 in
    let rec dfs_visit u = 
      Hashtbl.replace color u `grey;
      Hashtbl.replace d u (incr time; !time);
      List.iter (fun (_, v) -> if Hashtbl.find color v = `white 
                 then (Hashtbl.replace pi v (Some u);
                       dfs_visit v))
        (List.filter ((=) u -<- fst) edges);
      Hashtbl.replace color u `black ;
      Hashtbl.replace f u (incr time; !time)
    in
      List.iter (fun u ->
		   if Hashtbl.find color u = `white
		   then dfs_visit u) nodes;
      (f, d, pi)


(* CLR 23.4 *)
let topological_sort nodes edges = 
  let f, _, _ = dfs nodes edges in
    List.sort (fun (_,y1) (_,y2) -> - (compare y1 y2)) (hashtbl_as_list f)

(* CLR Ex 23.1-3 *)
let transpose_edges : ('a * 'b) list -> ('b * 'a) list = 
  fun list -> List.map (fun (x,y) -> (y,x)) list

(* CLR 23.5 *)
let strongly_connected_components (nodes : 'a list) (edges : ('a * 'a) list) = 
  let group nodes = (* Probably not the best way *)
    let table = Hashtbl.create (List.length nodes) in
      List.iter (function
                   | node, None -> (if not (Hashtbl.mem table node)
                                    then Hashtbl.add table node (ref [node]))
                   | node, Some partner -> 
                       ignore (pushnew node (set table node (find_def table partner (ref []))));
                       ignore (pushnew partner (set table partner (find_def table node (ref []))))) nodes;
      unduplicate (=) (List.map (snd ->- (!)) (hashtbl_as_list table)) in
  let f, _, _ = dfs nodes edges in 
  let _, _, p = (dfs
                 (let cmp_snd_desc (_,y1) (_,y2) = - compare y1 y2 in
                    (List.map fst (List.sort cmp_snd_desc (hashtbl_as_list f))))
                 (transpose_edges edges)) in
    group (hashtbl_as_list p)
