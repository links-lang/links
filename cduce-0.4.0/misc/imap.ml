(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

type 'a t = Obj.t array

let empty = Obj.magic ()
  (* [| |] does not ensure physical equality with ocamlopt (?) after deserialization.
     0 does not work either: the code generator considers registers set to empty as integer-only
     registers, which is not safe. *)

let get = Array.unsafe_get
let set = Array.unsafe_set

let get_len a : int = Obj.magic (get a 0)
let set_len a (i : int) = set a 0 (Obj.repr i)
let get_int a i : int = Obj.magic (get a i)
let set_int a (i : int) (x : int) = set a i (Obj.repr x)

let acreate n = Array.create n (Obj.repr n)

let elements (t : 'a t) : (int * 'a) list =
  if t == empty then [] else
  let rec aux accu i =
    if (i > 0) 
    then aux ((get_int t i, Obj.magic (get t (succ i)))::accu) (i - 2)
    else accu
  in
  aux [] (get_len t - 2)

let map_elements f t =
  if t == empty then [] else
  let rec aux accu i =
    if (i > 0) 
    then aux (f (get_int t i) (Obj.magic (get t (succ i)))::accu) (i - 2)
    else accu
  in
  aux [] (get_len t - 2)

let sort a = 
  Array.sort 
    (fun (i,_) (j,_) -> 
       assert (i != j); if (i:int) < j then (-1) else 1) a

let real_create a = 
  let n = Array.length a in
  let m = (n lsl 1) + 1 in
  let t = acreate m in
  for i = 1 to n do
    let j = i lsl 1 in
    let (idx,v) = a.(pred i) in
    set_int t (pred j) idx;
    set t j (Obj.repr v);
  done;
  t

let create a =
  if Array.length a = 0 then empty else (sort a; real_create a)

let create_default def a =
  sort a;
  let l = Array.to_list a in
  let rec aux i = function
    | [] ->
	if (i == max_int) then []
	else [(succ i, def)]
    | ((i1,_) as c)::rest ->
	if (succ i == i1) then c :: (aux i1 rest)
	else (succ i, def) :: c :: (aux i1 rest)
  in
  let l =  
    match l with
      | ((i1,_) as c)::rest ->
	  if (i1 == min_int) then c :: (aux i1 rest)
	  else (min_int,def) :: c :: (aux i1 rest)
      | [] -> [(min_int,def)] in
  let a = Array.of_list l in
  real_create a
  

let rec find_aux t (i : int) low high =
  if (low >= high) then low
  else
    let m = ((low + high) lsr 1) lor 1 in
    if i < get_int t m then find_aux t i low (m-2)
    else find_aux t i m high

let find (t : 'a t) i : 'a =
  if t == empty then raise Not_found;
  let j = find_aux t i 1 (get_len t - 2) in
  if (get_int t j == i) then Obj.magic (get t (succ j))
  else raise Not_found

let find_default t def i =
  if t == empty then def
  else
    let j = find_aux t i 1 (get_len t - 2) in
    if (get_int t j == i) then Obj.magic (get t (succ j))
    else def

let find_lower (t : 'a t) i : 'a =
  Obj.magic (get t (succ (find_aux t i 1 (get_len t - 2))))

let merge (t1 : 'a t) (t2 : 'a t) =
  if t1 == empty then t2 else if t2 == empty then t1
  else
    let n1 = get_len t1 and n2 = get_len t2 in
    let m = pred (n1 + n2) in
    let t = acreate m in
    let rec aux i i1 (l1:int) i2 l2 =
      if l1 == l2 then
	(set_int t i l1;
	 set t (succ i) (get t2 (pred i2));
	 let i = i + 2 in
	 if (i1 = n1) then (
	   let l = n2 - i2 in
	   Array.blit t2 i2 t i l;
	   i + l
	 ) else if (i2 = n2) then (
	   let l = n1 - i1 in
	   Array.blit t1 i1 t i l;
	   i + l
	 ) else 
	   let l1 = get_int t1 i1 and l2 = get_int t2 i2 in
	   let i1 = i1 + 2 and i2 = i2 + 2 in
	   aux i i1 l1 i2 l2)
      else if l1 < l2 then
	(set_int t i l1;
	 set t (succ i) (get t1 (pred i1));
	 let i = i + 2 in
	 if (i1 = n1) then (
	   let i2 = i2 - 2 in
	   let l = n2 - i2 in
	   Array.blit t2 i2 t i l;
	   i + l
	 ) else
	   let l1 = get_int t1 i1 in 
	   let i1 = i1 + 2 in
	   aux i i1 l1 i2 l2)
      else 
	(set_int t i l2;
	 set t (succ i) (get t2 (pred i2));
	 let i = i + 2 in
	 if (i2 = n2) then (
	   let i1 = i1 - 2 in
	   let l = n1 - i1 in
	   Array.blit t1 i1 t i l;
	   i + l
	 ) else
	   let l2 = get_int t2 i2 in 
	   let i2 = i2 + 2 in
	   aux i i1 l1 i2 l2)
    in
    set_len t (aux 1 3 (get_int t1 1) 3 (get_int t2 1));
    t

let cardinal t =
  if t == empty then 0
  else (pred (get_len t)) lsr 1

let map f t =
  if t == empty then empty
  else 
    let n = get_len t in
    let t' = acreate n in
    Array.blit t 0 t' 0 n;
    let rec aux i =
      if (i = 0) then t'
      else (set t' i (Obj.magic (f (Obj.magic (get t i)))); aux (i - 2))
    in
    aux (pred n)

let compare f t1 t2 =
  if (t1 == t2) then 0
  else if t1 == empty then (-1)
  else if t2 == empty then 1
  else
    let n1 = get_len t1 and n2 = get_len t2 in
    if (n1 < n2) then (-1) else if (n1 > n2) then 1
    else
      let rec aux i =
	if (i < 0) then 0
	else
	  let l1 = get t1 i and l2 = get t2 i in
	  if (l1 < l2) then (-1) else if (l1 > l2) then 1
	  else let x1 = Obj.magic (get t1 (succ i)) 
	       and x2 = Obj.magic (get t2 (succ i)) 
	  in let c = f x1 x2 in
	  if c != 0 then c else aux (i - 2)
      in
      aux (n1 - 2)

let hash f t =
  if t == empty then 1
  else
    let rec aux accu i =
      if (i < 0) then accu
      else aux (accu * 65537 
		+ 257 * (f (Obj.magic (get t (succ i))))
		+ (get_int t i)) (i - 2) in
    aux 1 (get_len t - 2)

let remove t i =
  if t == empty then t
  else
    let j = find_aux t i 1 (get_len t - 2) in
    if (get_int t j != i) then t
    else
      let n = get_len t - 2 in
      if (n = 1) then empty
      else
	let t' = acreate n in
	Array.blit t 1 t' 1 (j - 1);
	Array.blit t (j + 2) t' j (n - j);
	t'

let iter f t =
  if t == empty then ()
  else
    let rec aux i =
      if (i < 0) then ()
      else f (get_int t i) (Obj.magic (get t (succ i))) in
    aux (get_len t - 2)

(*
type 'a t = (int * 'a) list

let empty = []

let create a =
  Array.sort (fun (i,_) (j,_) -> assert (i != j); if i < j then (-1) else 1) a;
  Array.to_list a

let create_default def a =
  let l = create a in
  let rec aux i = function
    | [] ->
	if (i == max_int) then []
	else [(succ i, def)]
    | ((i1,_) as c)::rest ->
	if (succ i == i1) then c :: (aux i1 rest)
	else (succ i, def) :: c :: (aux i1 rest)
  in
  let l =  
    match l with
      | ((i1,_) as c)::rest ->
	  if (i1 == min_int) then c :: (aux i1 rest)
	  else (min_int,def) :: c :: (aux i1 rest)
      | [] -> [(min_int,def)] in
  l

let merge l1 l2 = assert false

let find l x = List.assoc x l

let rec find_lower l x = match l with
  | (_,v)::(y,_)::_ when x < y -> v
  | _::l -> find_lower l x
  | [] -> assert false

let find_default l def x = try find l x with Not_found -> def

let cardinal = List.length

let elements l = l

let map f l = List.map (fun (x,v) -> (x, f v)) l

let map_elements f l = List.map (fun (x,v) -> f x v) l

let compare _ _ = assert false
let hash _ = assert false
let remove _ _ = assert false
let iter _ _ = assert false
*)
