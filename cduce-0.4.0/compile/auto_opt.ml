open Ident
open Patterns.Compile

type node = ptr ref
and ptr = Disp of descr | Link of node
and descr = { disp: dispatcher; mutable minimized: bool }

let rec repr n = match !n with Disp _ -> n | Link n -> repr n
let descr n = match !n with Disp d -> d | _ -> assert false
let log = ref []
let link n1 n2 = log := (n1, !n1) :: !log; n1 := Link n2
let backtrack n0 =
  let rec aux = function
    | (n,d)::rest -> n := d; if (n != n0) then aux rest else log := rest
    | _ -> assert false
  in
  aux !log

let nodes = Hashtbl.create 64

let node_of d =
  try Hashtbl.find nodes (id d)
  with Not_found ->
    let n = ref (Disp { disp = d; minimized = false }) in
    Hashtbl.add nodes (id d) n;
    n

exception Not_equal

let rec check_equal_nodes n1 n2 =
  let n1 = repr n1 and n2 = repr n2 in
  if (n1 != n2) then (
    let d1 = descr n1 and d2 = descr n2 in
    if d1.minimized && d2.minimized then raise Not_equal;
    let n1,n2 = if d1.minimized then (n2,n1) else (n1,n2) in
    link n1 n2;
    try check_equal_actions (actions d1.disp) (actions d2.disp)
    with Not_equal -> backtrack n1; raise Not_equal
  )
and check_equal_actions a1 a2 = match a1,a2 with
  | AIgnore r1, AIgnore r2 -> check_equal_results r1 r2
  | AKind k1, AKind k2 -> 
      (check_equal_basics k1.basic k2.basic;
       check_equal_prods k1.prod k2.prod;
       check_equal_prods k1.xml k2.xml;
       match k1.record,k2.record with
	 | Some(RecLabel(l1,p1)), Some(RecLabel(l2,p2)) 
	     when Label.equal l1 l2 ->
	     check_equal_prods p1 p2
	 | Some(RecNolabel (a1,b1)), Some(RecNolabel (a2,b2)) -> 
	     check_equal_result_options a1 a2;
	     check_equal_result_options b1 b2;
	 | None, None -> ()
	 | _ -> raise Not_equal)
  | _ -> raise Not_equal
and check_equal_result_options a1 a2 = match a1,a2 with
  | Some r1, Some r2 -> check_equal_results r1 r2
  | None, None -> ()
  | _ -> raise Not_equal
and check_equal_prods a1 a2 = match a1,a2 with
  | Dispatch (d1,r1), Dispatch (d2,r2) when Array.length r1 = Array.length r2 ->
      check_equal_disps d1 d2;
      Array.iteri (fun i x -> check_equal_prod2 x r2.(i)) r1
  | TailCall d1, TailCall d2 -> check_equal_disps d1 d2
  | Ignore c1, Ignore c2 -> check_equal_prod2 c1 c2
  | Impossible, Impossible -> ()
  | _ -> raise Not_equal
and check_equal_prod2 a1 a2 = match a1,a2 with
  | Dispatch (d1,r1), Dispatch (d2,r2) when Array.length r1 = Array.length r2 ->
      check_equal_disps d1 d2;
      Array.iteri (fun i x -> check_equal_results x r2.(i)) r1
  | TailCall d1, TailCall d2 -> check_equal_disps d1 d2
  | Ignore c1, Ignore c2 -> check_equal_results c1 c2
  | Impossible, Impossible -> ()
  | _ -> raise Not_equal
and check_equal_disps d1 d2 =
  if d1 == d2 then ()
  else check_equal_nodes (node_of d1) (node_of d2)
and check_equal_results (c1,s1,p1) (c2,s2,p2) =
  if (c1 != c2) || (p1 != p2) || (Array.length s1 != Array.length s2) 
  then raise Not_equal;
  Array.iteri (fun i x -> check_equal_source x s2.(i)) s1
and check_equal_source s1 s2 = if (s1 != s2) then match s1,s2 with
  | Const c1, Const c2 when Types.Const.equal c1 c2 -> ()
  | Stack i1, Stack i2 when i1 == i2 -> ()
  | Recompose (i1,j1), Recompose (i2,j2) when i1 == i2 && j1 == j2 -> ()
  | _ -> raise Not_equal
and check_equal_basics a1 a2 =
  if (List.length a1 != List.length a2) then raise Not_equal;
  List.iter2 (fun (t1,r1) (t2,r2) -> 
		if not (Types.equiv t1 t2) then raise Not_equal;
		check_equal_results r1 r2) a1 a2

let equal_nodes n1 n2 =
  try check_equal_nodes n1 n2; true
  with Not_equal -> false

let minimized = ref []

let rec auto d =
  let n = node_of d in
  if not (List.exists (equal_nodes n) !minimized) then
    let n = repr n in
    (descr n).minimized <- true;
    minimized := n :: !minimized;
    iter_disp_actions auto (actions d)

let make_branches t brs =
  let d,r = make_branches t brs in
  auto d;
  d,r

let () =
  Stats.register Stats.Summary
    (fun ppf ->
       Format.fprintf ppf 
	 "Number of minimized states:%i@." 
	 (List.length !minimized));
