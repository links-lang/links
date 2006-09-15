(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

type 'a regexp = 
  | Empty
  | Epsilon
  | Seq of 'a regexp * 'a regexp
  | Alt of 'a regexp * 'a regexp
  | Star of 'a regexp
  | Plus of 'a regexp
  | Trans of 'a

module type TABLE = sig
  type key
  type 'a t
  val create: int -> 'a t
  val add: 'a t -> key -> 'a -> unit
  val find: 'a t -> key -> 'a
end

module type S = sig
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val hash: t -> int
end

module Decompile(H : TABLE)(S : S) = struct

(* Now attempt to simplify regexp. Does not work.... disabled *)
  module A = struct
  type atom =
    | AStar of trie
    | APlus of trie
    | ATrans of S.t
  and trie =
    | AEmpty
    | AEps
    | ABranch of atom list * trie * trie * bool * int * int
	(* Branching atom, left, right,
	   nullable,
	   hash,
	   uid *)


  type re = trie



  let empty = AEmpty
  let epsilon = AEps

  let nullable = function
    | AEmpty -> false
    | AEps -> true
    | ABranch (_,_,_,n,_,_) -> n

  let nullable_atom = function
    | AStar _ -> true
    | APlus t -> assert(not (nullable t)); false
    | ATrans _ -> false
  let nullable_atom_list = List.exists nullable_atom

(*
  let size = function
    | AEmpty -> 0
    | AEps -> 0
    | ABranch (_,_,_,_,_,_,sz) -> sz
*)

  let compare_trie t1 t2 = match t1,t2 with
    | AEmpty, AEmpty | AEps, AEps -> 0
    | AEmpty, _ -> -1 | _,AEmpty -> 1
    | AEps, _ -> -1 | _, AEps -> 1
    | ABranch (_,_,_,_,_,id1), ABranch (_,_,_,_,_,id2) -> id1 - id2

  let equal_atom a1 a2 = match a1,a2 with
    | AStar t1, AStar t2 | APlus t1, APlus t2 -> t1 == t2
    | ATrans t1, ATrans t2 -> S.equal t1 t2
    | _ -> false

  let rec equal_atom_list a1 a2 = match a1,a2 with
    | [],[] -> true
    | hd1::tl1,hd2::tl2 -> equal_atom hd1 hd2 && equal_atom_list tl1 tl2
    | _ -> false

  let compare_atom a1 a2 = match a1,a2 with
    | AStar t1, AStar t2 | APlus t1, APlus t2 -> compare_trie t1 t2
    | AStar _, _ -> -1 | _, AStar _ -> 1
    | APlus _, _ -> -1 | _, APlus _ -> 1
    | ATrans t1, ATrans t2 -> S.compare t1 t2


  let hash_trie = function
    | AEmpty -> 0
    | AEps -> 1
    | ABranch (_,_,_,_,h,_) -> h

  let hash_atom = function
    | AStar t -> 17 * (hash_trie t)
    | APlus t -> 1 + 17 * (hash_trie t)
    | ATrans t -> 2 + 17 * (S.hash t)

  let rec hash_atom_list = function
    | hd::tl -> hash_atom hd + 17 * (hash_atom_list tl)
    | [] -> 0

  module T = struct
    type t = atom list * trie * trie * int

    let equal (a,ay,an,_) (b,by,bn,_) =
      (equal_atom_list a b) && (ay == by) && (an == bn)
    let hash (a,ay,an,h) =
      h
  end

  module HT = Hashtbl.Make(T)

  let branches = HT.create 17
  let uid = ref 0

  let branch0 a ay an =
    let h = hash_atom_list a + 17 * (hash_trie ay) + 257 * (hash_trie an) in
    let b = (a,ay,an,h) in
    try HT.find branches b
    with Not_found ->
      let h = T.hash b in
      incr uid;
      let nullable = 
	nullable an || ((nullable ay) && (nullable_atom_list a)) in
      let x = ABranch (a,ay,an,nullable,h,!uid) in
      HT.add branches b x;
      x

  let branch a ay an =
(*    assert (List.length a > 0);
    match ay,an with
      | ABranch (b,by,bn,_,_,_), AEmpty -> branch0 (a @ b) by bn
      | AEmpty, AEmpty -> AEmpty
      | _ -> *) branch0 a ay an

  let rec opt = function
    | ABranch (a,ay,an,_,_,_) -> branch0 a ay (opt an)
    | AEmpty -> AEps
    | t -> t

  let rec factor accu ctx x y = match x,y with
    | hd1::tl1, hd2::tl2 when equal_atom hd1 hd2 -> 
	factor (hd1::accu) (hd1::ctx) tl1 tl2
    | _ -> List.rev accu, ctx,x,y
	
  let rec get_seq accu = function
    | ABranch (a,AEps,AEmpty,_,_,_) -> Some a
    | AEps -> Some []
    | _ -> None

  let get_seq = get_seq []

  let apply_factor f r =
    branch0 f r AEmpty

  let apply_ctx ctx r =
    List.fold_right (fun a r -> branch0 a r AEmpty) ctx r

  let star x r = match x with
    | AEmpty | AEps -> AEps
    | t -> branch0 [ AStar t ] r AEmpty

  let plus x =
    if nullable x then AStar x else APlus x

  (*  (AB)*A   ==>  A(BA)*
      BA(BA)*  ==> (BA)+ *)
  let rec create_plus ctx = function
    | AStar x :: follow ->
	(match get_seq x with
	   | Some s ->
	       let (accu,ctx,s,follow) = factor [] ctx s follow in
	       let s = s @ accu in
	       let rec aux accu = function
		 | ctx,[] -> 
		     create_plus
		       (plus (apply_factor accu AEps) :: ctx)
		       follow
		 | a::b,c::d when equal_atom a c -> aux (a::accu) (b,d)
		 | _ -> create_plus (AStar x :: ctx) follow
	       in
	       aux [] (ctx,s)
	   | None -> create_plus (AStar x :: ctx) follow)
    | x :: follow -> create_plus (x :: ctx) follow
    | [] -> List.rev ctx


  let rec size = function
    | AEps -> 1
    | AEmpty -> 0
    | ABranch (a,ay,an,_,_,_) ->
	if (ay == an) then 1 + (size ay)
	else 3 + (size ay) + (size an)

  let choose u v =
    if size u > size v then v else u
    

  let rec alt t1 t2 = match t1,t2 with
    | AEmpty,t | t,AEmpty -> t
    | AEps,t | t,AEps -> opt t
    | ABranch (_,_,_,_,_,id1), ABranch (_,_,_,_,_,id2) when id1 = id2 -> t1
    | ABranch (al,ay,an,_,_,_), ABranch (bl,by,bn,_,_,_) ->
(* 	br al ay (alt an t2) *)
	let (accu,_,al,bl) = factor [] [] al bl in
	match accu with
	  | [] ->
(*	      let u = br al ay (alt an t2)
	      and v = br bl by (alt bn t1) in
	      choose u v  *)
 	      branch al ay (alt an t2)
	  | _ ->
	      let t1 = br al ay AEps in
	      let t2 = br bl by AEps in
	      branch accu (alt t1 t2) (alt an bn)


  and br a ay an =
(*    match a with
      | [] -> alt ay an
      | l -> *) branch a ay an

  and seq t1 t2 = match t1,t2 with
    | AEmpty,_|_,AEmpty -> AEmpty
    | AEps,t | t,AEps -> t
    | ABranch (a,ay,an,_,_,_), t2 ->
(*	  (alt 
	     (branch a (seq ay t2) AEmpty)
	     (seq an t2) )
*)
	  (branch a (seq ay t2) (seq an t2))

  let rtrans t = branch  [ATrans t] AEps AEmpty
  let star = function
    | AEmpty | AEps -> AEps
    | t -> branch [AStar t] AEps AEmpty

  let rseq r1 r2 = match r1,r2 with
    | Epsilon, z | z, Epsilon -> z
    | Empty, _ | _, Empty -> Empty 
    | x,y -> Seq (x,y)
  let ralt r1 r2 = match r1,r2 with
    | Empty, z | z, Empty -> z 
    | x,y -> Alt (x,y)

  let rec minim = function
    | AEmpty -> AEmpty
    | AEps -> AEps
    | ABranch (a,ay,(ABranch (b,by,bn,_,_,_) as an),_,_,_) as br
      when ay != an ->
	choose (branch b (minim by) (branch a (minim ay) bn)) br
    | br -> br

  let rec minim_trie r =
    let r' = minim r in
    if (size r' < size r) then minim_trie r' else r

  let rec regexp r =
(*    let r = minim_trie r in *)
    match r with
      | AEmpty -> Empty
      | AEps -> Epsilon
      | ABranch (a,ay,an,_,_,_) when ay == an ->
(*   	  let a = create_plus [] a in *)
	  rseq (ralt (regexp_atom_list a) Epsilon) (regexp ay)
      |  ABranch (a,ay,an,_,_,_) ->
(*    	   let a = create_plus [] a in *)
	   ralt (rseq (regexp_atom_list a) (regexp ay)) (regexp an)

  and regexp_atom_list = function
    | hd::tl -> rseq (regexp_atom hd) (regexp_atom_list tl)
    | [] -> Epsilon
  and regexp_atom = function
    | AStar t -> Star (regexp t)
    | APlus t -> Plus (regexp t)
    | ATrans t -> Trans t

  let () = () and  (* Hack to avoid "let regexp ..." (ulex construction) *)
  regexp r =
    (* Need to clear hashtable because S.t objects might have different
       meaning across calls *)
    let re = regexp r in
    HT.clear branches;
    re

  end

  module B = struct
  type re =
  | RSeq of re list
  | RAlt of re list
  | RTrans of S.t
  | RStar of re
  | RPlus of re

  let rec compare s1 s2 = 
    if s1 == s2 then 0 
    else match (s1,s2) with
      | RSeq x, RSeq y | RAlt x, RAlt y -> compare_list x y
      | RSeq _, _ -> -1 | _, RSeq _ -> 1
      | RAlt _, _ -> -1 | _, RAlt _ -> 1
      | RTrans x, RTrans y -> S.compare x y
      | RTrans _, _ -> -1 | _, RTrans _ -> 1
      | RStar x, RStar y | RPlus x, RPlus y -> compare x y
      | RStar _, _ -> -1 | _, RStar _ -> 1
  and compare_list l1 l2 = match (l1,l2) with
    | x1::y1, x2::y2 -> 
	let c = compare x1 x2 in if c = 0 then compare_list y1 y2 else c
    | [], [] -> 0
    | [], _ -> -1 | _, [] -> 1

  let rec dump ppf = function
    | RSeq l -> Format.fprintf ppf "Seq(%a)" dump_list l
    | RAlt l -> Format.fprintf ppf "Alt(%a)" dump_list l
    | RStar r -> Format.fprintf ppf "Star(%a)" dump r
    | RPlus r -> Format.fprintf ppf "Plus(%a)" dump r
    | RTrans x -> Format.fprintf ppf "Trans"
  and dump_list ppf = function
    | [] -> ()
    | [h] ->  Format.fprintf ppf "%a" dump h
    | h::t ->  Format.fprintf ppf "%a,%a" dump h dump_list t

  let rec factor accu l1 l2 = match (l1,l2) with
    | (x1::y1,x2::y2) when compare x1 x2 = 0 -> factor (x1::accu) y1 y2 
    | (l1,l2) -> (accu,l1,l2)
   

  let rec regexp = function
    | RSeq l ->
	let rec aux = function 
	    | [h] -> regexp h 
	    | h::t -> Seq (regexp h,aux t) 
	    | [] -> Epsilon in
	aux l
    | RAlt l ->
	let rec aux = function 
	    | [h] -> regexp h 
	    | h::t -> Alt (regexp h,aux t) 
	    | [] -> Empty in
	aux l
    | RTrans x -> Trans x
    | RStar r -> Star (regexp r)
    | RPlus r -> Plus (regexp r)

  let epsilon = RSeq []
  let empty = RAlt []
  let rtrans t = RTrans t

  let rec nullable = function
    | RAlt l -> List.exists nullable l
    | RSeq l -> List.for_all nullable l
    | RPlus r -> nullable r
    | RStar _ -> true
    | RTrans _ -> false

  let has_epsilon =
    List.exists (function RSeq [] -> true | _ -> false)

  let remove_epsilon =
    List.filter (function RSeq [] -> false | _ -> true)

  let rec merge l1 l2 = match (l1,l2) with
    | x1::y1, x2::y2 ->
	let c = compare x1 x2 in
	if c = 0 then x1::(merge y1 y2)
	else if c < 0 then x1::(merge y1 l2)
	else x2::(merge l1 y2)
    | [], l | l,[] -> l

  let sort l =
    let rec initlist = function
      | [] -> []
      | e::rest -> [e] :: initlist rest in
    let rec merge2 = function
      | l1::l2::rest -> merge l1 l2 :: merge2 rest
      | x -> x in
    let rec mergeall = function
      | [] -> []
      | [l] -> l
      | llist -> mergeall (merge2 llist) in
    mergeall (initlist l)

  let rec sub l1 l2 =
    (compare l1 l2 = 0) ||
    match (l1,l2) with
      | RSeq [x], y -> sub x y
      | RPlus x, (RStar y | RPlus y) -> sub x y
      | RSeq (x::y), (RPlus z | RStar z) -> 
	  (sub x z) && (sub (RSeq y) (RStar z))
      | x, (RStar y | RPlus y) -> sub x y
      | _ -> false


  let rec absorb_epsilon = function
    | RPlus r :: l -> RStar r :: l
    | (r :: _) as l when nullable r -> l
    | r :: l -> r :: (absorb_epsilon l)
    | [] -> [ epsilon ]

  let rec simplify_alt accu = function
    | [] -> List.rev accu
    | x::rest -> 
	if (List.exists (sub x) accu) || (List.exists (sub x) rest)
	then simplify_alt accu rest
	else simplify_alt (x::accu) rest

  let alt s1 s2 =
    let s1 = match s1 with RAlt x -> x | x -> [x] in
    let s2 = match s2 with RAlt x -> x | x -> [x] in
    let l = merge s1 s2 in
    let l = 
      if has_epsilon l 
      then absorb_epsilon (remove_epsilon l)
      else l in
    let l = simplify_alt [] l in
    match l with
      | [x] -> x
      | l -> RAlt l

  let rec simplify_seq = function
    | RStar x :: ((RStar y | RPlus y) :: _ as rest) 
	when compare x y = 0 ->
	simplify_seq rest
    | RPlus x :: (RPlus y :: _ as rest) 
	when compare x y = 0 ->
	simplify_seq (x :: rest)
    | RPlus x :: (RStar y :: rest) when compare x y = 0 ->
	simplify_seq (RPlus y :: rest)
    | x :: rest -> x :: (simplify_seq rest)
    | [] -> []

  let rec seq s1 s2 =
    match (s1,s2) with
      | RAlt [], _ | _, RAlt [] -> epsilon
      | RSeq [], x | x, RSeq [] -> x
      | _ ->
	  let s1 = match s1 with RSeq x -> x | x -> [x] in
	  let s2 = match s2 with RSeq x -> x | x -> [x] in
	  find_plus [] (s1 @ s2)
  and find_plus before = function
    | [] -> 
	(match before with [h] -> h | l -> RSeq (simplify_seq (List.rev l)))
    | (RStar s)::after ->
	let star = match s with RSeq x -> x | x -> [x] in
	let (right,star',after') = factor [] star after in
	let (left,star'',before') = factor [] (List.rev star') before in
	(match star'' with
	   | [] ->
	       let s = find_plus [] (left @ (List.rev right)) in
	       find_plus ((RPlus s)::before') after'
	   | _  -> 
	       find_plus ((RStar s)::before) after)
    | x::after -> find_plus (x::before) after

  let star = function
    | RAlt [] | RSeq [] -> epsilon
    | RStar _ as s -> s
    | RPlus s -> RStar s
    | s -> RStar s
  end

  open B

  type slot = { 
    mutable weight : int;
    mutable outg : (slot * re) list;
    mutable inc  : (slot * re) list;
    mutable self : re;
    mutable ok   : bool
  }
  let alloc_slot () = 
    { weight = 0; outg = []; inc = []; self = empty; ok = false }

  let decompile trans n0 =
    let slot_table = H.create 121 in
    let slots = ref [] in
    let slot n =
      try H.find slot_table n
      with Not_found -> 
	let s = alloc_slot () in
	H.add slot_table n s;
	slots := s :: !slots;
	s in

    let add_trans s1 s2 t =
      if s1 == s2 
      then s1.self <- alt s1.self t
      else (s1.outg <- (s2,t) :: s1.outg; s2.inc <- (s1,t) :: s2.inc) in

    let final = alloc_slot () in
    let initial = alloc_slot () in

    let rec conv n =
      let s = slot n in
      if not s.ok then (
	s.ok <- true;
	let (tr,f) = trans n in
	if f then add_trans s final epsilon;
	List.iter (fun (l,dst) -> add_trans s (conv dst) (rtrans l)) tr;
      );
      s in

    let elim s =
      s.weight <- (-1);
      let loop = star s.self in
      List.iter 
	(fun (s1,t1) -> if s1.weight >= 0 then 
	   List.iter 
	     (fun (s2,t2) -> if s2.weight >= 0 then 
		add_trans s1 s2 (seq t1 (seq loop t2)))
	     s.outg
	) s.inc in

    add_trans initial (conv n0) epsilon;
    List.iter 
      (fun s -> s.weight <- List.length s.inc * List.length s.outg)
      !slots;
    let slots = 
      List.sort (fun s1 s2 -> Pervasives.compare s1.weight s2.weight) !slots in
    List.iter elim slots;
    let r = 
      List.fold_left 
	(fun accu (s,t) -> 
	   if s == final then alt accu t else accu)
	empty
	initial.outg in
    regexp r
end
