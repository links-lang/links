(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

let (<) : int -> int -> bool = (<)
let (>) : int -> int -> bool = (>)
let (=) : int -> int -> bool = (=)

module type S =
sig
  type elem
  include Custom.T

  val get: t -> (elem list * elem list) list
  val get': t -> (elem list * (elem list) list) list

  val empty : t
  val full  : t
  val cup   : t -> t -> t
  val cap   : t -> t -> t
  val diff  : t -> t -> t
  val atom  : elem -> t

  val iter: (elem-> unit) -> t -> unit

  val compute: empty:'b -> full:'b -> cup:('b -> 'b -> 'b) 
    -> cap:('b -> 'b -> 'b) -> diff:('b -> 'b -> 'b) ->
    atom:(elem -> 'b) -> t -> 'b

(*
  val print: string -> (Format.formatter -> elem -> unit) -> t ->
    (Format.formatter -> unit) list
*)

  val trivially_disjoint: t -> t -> bool
end

module type MAKE = functor (X : Custom.T) -> S with type elem = X.t

module Make(X : Custom.T) =
struct
  type elem = X.t
  type t =
    | True
    | False
    | Split of int * elem * t * t * t

  let rec equal a b =
    (a == b) ||
    match (a,b) with
      | Split (h1,x1, p1,i1,n1), Split (h2,x2, p2,i2,n2) ->
	  (h1 == h2) &&
	  (equal p1 p2) && (equal i1 i2) &&
	  (equal n1 n2) && (X.equal x1 x2)
      | _ -> false

(* Idea: add a mutable "unique" identifier and set it to
   the minimum of the two when egality ... *)


  let rec compare a b =
    if (a == b) then 0 
    else match (a,b) with
      | Split (h1,x1, p1,i1,n1), Split (h2,x2, p2,i2,n2) ->
	  if h1 < h2 then -1 else if h1 > h2 then 1 
	  else let c = X.compare x1 x2 in if c <> 0 then c
	  else let c = compare p1 p2 in if c <> 0 then c
	  else let c = compare i1 i2 in if c <> 0 then c 
	  else compare n1 n2
      | True,_  -> -1
      | _, True -> 1
      | False,_ -> -1
      | _,False -> 1


  let rec hash = function
    | True -> 1
    | False -> 0
    | Split(h, _,_,_,_) -> h

  let compute_hash x p i n = 
	(X.hash x) + 17 * (hash p) + 257 * (hash i) + 16637 * (hash n)


  let rec check = function
    | True | False -> ()
    | Split (h,x,p,i,n) ->
	assert (h = compute_hash x p i n);
	(match p with Split (_,y,_,_,_) -> assert (X.compare x y < 0) | _ -> ());
	(match i with Split (_,y,_,_,_) -> assert (X.compare x y < 0) | _ -> ());
	(match n with Split (_,y,_,_,_) -> assert (X.compare x y < 0) | _ -> ());
	X.check x; check p; check i; check n

  let atom x =
    let h = X.hash x + 17 in (* partial evaluation of compute_hash... *)
    Split (h, x,True,False,False)
 
  let neg_atom x =
    let h = X.hash x + 16637 in (* partial evaluation of compute_hash... *)
    Split (h, x,False,False,True)

  let rec iter f = function
    | Split (_, x, p,i,n) -> f x; iter f p; iter f i; iter f n
    | _ -> ()

  let rec dump ppf = function
    | True -> Format.fprintf ppf "+"
    | False -> Format.fprintf ppf "-"
    | Split (_,x, p,i,n) -> 
	Format.fprintf ppf "%i(@[%a,%a,%a@])" 
	(* X.dump x *) (X.hash x) dump p dump i dump n


  let rec print f ppf = function
    | True -> Format.fprintf ppf "Any"
    | False -> Format.fprintf ppf "Empty"
    | Split (_, x, p,i, n) ->
	let flag = ref false in
	let b () = if !flag then Format.fprintf ppf " | " else flag := true in
	(match p with 
	   | True -> b(); Format.fprintf ppf "%a" f x
	   | False -> ()
	   | _ -> b (); Format.fprintf ppf "%a & @[(%a)@]" f x (print f) p );
	(match i with 
	   | True -> assert false;
	   | False -> ()
	   | _ -> b(); print f ppf i);
	(match n with 
	   | True -> b (); Format.fprintf ppf "@[~%a@]" f x
	   | False -> ()
	   | _ -> b (); Format.fprintf ppf "@[~%a@] & @[(%a)@]" f x (print f) n)
	
  let print a f = function
    | True -> [ fun ppf -> Format.fprintf ppf "%s" a ]
    | False -> []
    | c -> [ fun ppf -> print f ppf c ]
	
	
  let rec get accu pos neg = function
    | True -> (pos,neg) :: accu
    | False -> accu
    | Split (_,x, p,i,n) ->
	(*OPT: can avoid creating this list cell when pos or neg =False *)
	let accu = get accu (x::pos) neg p in
	let accu = get accu pos (x::neg) n in
	let accu = get accu pos neg i in
	accu
	  
  let get x = get [] [] [] x

  let rec get' accu pos neg = function
    | True -> (pos,neg) :: accu
    | False -> accu
    | Split (_,x,p,i,n) ->
	let accu = get' accu (x::pos) neg p in
	let rec aux l = function
	  | Split (_,x,False,i,n') when equal n n' ->
	      aux (x :: l) i
	  | i ->
(*	      if (List.length l > 1) then (print_int (List.length l); flush stdout); *)
	      let accu = get' accu pos (l :: neg) n in
	      get' accu pos neg i
	in
	aux [x] i

  let get' x = get' [] [] [] x

		
  let compute ~empty ~full ~cup ~cap ~diff ~atom b =
    let rec aux = function
      | True -> full
      | False -> empty
      | Split(_,x, p,i,n) ->
	  let p = cap (atom x) (aux p)
	  and i = aux i
	  and n = diff (aux n) (atom x) in
	  cup (cup p i) n
    in
    aux b
      
(* Invariant: correct hash value *)

  let split0 x pos ign neg =
    Split (compute_hash x pos ign neg, x, pos, ign, neg)


    


  let empty = False
  let full = True

(* Invariants:
     Split (x, pos,ign,neg) ==>  (ign <> True), (pos <> neg)
*)

  let rec has_true = function
    | [] -> false
    | True :: _ -> true
    | _ :: l -> has_true l

  let rec has_same a = function
    | [] -> false
    | b :: l -> (equal a b) || (has_same a l)

  let rec split x p i n =
    if i == True then True 
    else if equal p n then p ++ i
    else let p = simplify p [i] and n = simplify n [i] in
    if equal p n then p ++ i
    else split0 x p i n

  and simplify a l =
    match a with
      | False -> False
      | True -> if has_true l then False else True
      | Split (_,x,p,i,n) ->
	  if (has_true l) || (has_same a l) then False
	  else s_aux2 a x p i n [] [] [] l
  and s_aux2 a x p i n ap ai an = function
    | [] -> 
	let p = simplify p ap 
	and n = simplify n an
	and i = simplify i ai in
	if equal p n then p ++ i else split0 x p i n
    | b :: l -> s_aux3 a x p i n ap ai an l b 
  and s_aux3 a x p i n ap ai an l = function
    | False -> s_aux2 a x p i n ap ai an l
    | True -> assert false
    | Split (_,x2,p2,i2,n2) as b ->
	if equal a b then False 
	else let c = X.compare x2 x in
	if c < 0 then s_aux3 a x p i n ap ai an l i2
	else if c > 0 then s_aux2 a x p i n (b :: ap) (b :: ai) (b :: an) l
	else s_aux2 a x p i n (p2 :: i2 :: ap) (i2 :: ai) (n2 :: i2 :: an) l

  and ( ++ ) a b = if a == b then a
  else match (a,b) with
    | True, _ | _, True -> True
    | False, a | a, False -> a
    | Split (_,x1, p1,i1,n1), Split (_,x2, p2,i2,n2) ->
	let c = X.compare x1 x2 in
	if c = 0 then split x1 (p1 ++ p2) (i1 ++ i2) (n1 ++ n2)
	else if c < 0 then split x1 p1 (i1 ++ b) n1
	else split x2 p2 (i2 ++ a) n2

(* seems better not to make ++ and this split mutually recursive;
   is the invariant still inforced ? *)

  let rec ( ** ) a b = if a == b then a else match (a,b) with
    | True, a | a, True -> a
    | False, _ | _, False -> False
    | Split (_,x1, p1,i1,n1), Split (_,x2, p2,i2,n2) ->
	let c = X.compare x1 x2 in
	if c = 0 then
	  split x1 
	    (p1 ** (p2 ++ i2) ++ (p2 ** i1))
	    (i1 ** i2)
	    (n1 ** (n2 ++ i2) ++ (n2 ** i1))  
(*	  if (p2 == True) && (n2 == False) 
	  then split x1 (p1 ++ i1) (i1 ** i2) (n1 ** i2)
	  else if (p2 == False) && (n2 == True)
	  then split x1 (p1 ** i2) (i1 ** i2) (n1 ++ i1)
	  else 
	    split x1 ((p1++i1) ** (p2 ++ i2)) False ((n1 ++ i1) ** (n2 ++ i2)) 
*)
	else if c < 0 then split x1 (p1 ** b) (i1 ** b) (n1 ** b)
	else split x2 (p2 ** a) (i2 ** a) (n2 ** a)

  let rec trivially_disjoint a b =
    if a == b then a == False
    else match (a,b) with
      | True, a | a, True -> a == False
      | False, _ | _, False -> true
      | Split (_,x1, p1,i1,n1), Split (_,x2, p2,i2,n2) ->
	  let c = X.compare x1 x2 in
	  if c = 0 then
(* try expanding -> p1 p2; p1 i2; i1 p2; i1 i2 ... *)
	    trivially_disjoint (p1 ++ i1) (p2 ++ i2) &&
	    trivially_disjoint (n1 ++ i1) (n2 ++ i2)
	  else if c < 0 then
	    trivially_disjoint p1 b &&
	    trivially_disjoint i1 b &&
	    trivially_disjoint n1 b
	  else
	    trivially_disjoint p2 a &&
	    trivially_disjoint i2 a &&
	    trivially_disjoint n2 a

  let rec neg = function
    | True -> False
    | False -> True
    | Split (_,x, p,i,False) -> split x False (neg (i ++ p)) (neg i)
    | Split (_,x, False,i,n) -> split x (neg i) (neg (i ++ n)) False 
    | Split (_,x, p,False,n) -> split x (neg p) (neg (p ++ n)) (neg n)  
(*    | Split (_,x, p, False, False) -> 
	split x False (neg p) True
    | Split (_,x, False, False, n) -> split x True (neg n) False *)
    | Split (_,x, p,i,n) -> split x (neg (i ++ p)) False (neg (i ++ n))
	      
  let rec ( // ) a b =  
(*    if equal a b then False  *)
    if a == b then False 
    else match (a,b) with
      | False,_ | _, True -> False
      | a, False -> a
      | True, b -> neg b
      | Split (_,x1, p1,i1,n1), Split (_,x2, p2,i2,n2) ->
	  let c = X.compare x1 x2 in
	  if c = 0 then
	    if (i2 == False) && (n2 == False) 
	    then split x1 (p1 // p2) (i1 // p2) (n1 ++ i1)
(*	    else if (i2 == False) && (p2 == False)
	    then split x1 (p1 ++ i1) (i1 // n2) (n1 // n2) *)
	    else 
	      split x1 ((p1++i1) // (p2 ++ i2)) False ((n1++i1) // (n2 ++ i2))
	  else if c < 0 then
	    split x1 (p1 // b) (i1 // b) (n1 // b) 
(*	    split x1 ((p1 ++ i1)// b) False ((n1 ++i1) // b)  *)
	  else
	    split x2 (a // (i2 ++ p2)) False (a // (i2 ++ n2))
	      

  let cup = ( ++ )
  let cap = ( ** )
  let diff = ( // )

(*
  let rec cap_atom x pos a = (* Assume that x does not appear in a *)
    match a with
      | False -> False
      | True -> if pos then atom x else neg_atom x
      | Split (_,y,p,i,n) ->
	  let c = X.compare x y in
	  assert (c <> 0);
	  if (c < 0) then 
	    if pos then split x a False False
	    else split x False False a
	  else split y (cap_atom x pos p) (cap_atom x pos i) (cap_atom x pos n)
*)

    
(*
  let not_triv = function
    | True | False -> false
    | _ -> true

  let diff x y =
    let d = diff x y in
    if (not_triv x) && (not_triv y) then
      Format.fprintf Format.std_formatter "X = %a@\nY = %a@\nX\\Z = %a@\n"
	dump x dump y dump d;  
    d

  let cap x y =
    let d = cap x y in
    if (not_triv x) && (not_triv y) then
      Format.fprintf Format.std_formatter "X = %a@\nY = %a@\nX**Z = %a@\n"
	dump x dump y dump d;  
    d

  let cup x y =
    let d = cup x y in
    if (not_triv x) && (not_triv y) then
      Format.fprintf Format.std_formatter "X = %a@\nY = %a@\nX++Z = %a@\n"
	dump x dump y dump d;  
    d
*)
end


(*
module type S' = sig
  include S
  type bdd = False | True | Br of elem * t * t
  val br: t -> bdd
end
module MakeBdd(X : Custom.T) =
struct
  type elem = X.t
  type t =
    | Zero
    | One
    | Branch of elem * t * t * int * t
  type node = t

  let neg = function
    | Zero -> One | One -> Zero
    | Branch (_,_,_,_,neg) -> neg

  let id = function
    | Zero -> 0
    | One -> 1
    | Branch (_,_,_,id,_) -> id

(* Internalization + detection of useless branching *)
  let max_id = ref 2 (* Must be >= 2 *)
  module W = Weak(*Myweak*).Make(
    struct
      type t = node
	  
      let hash = function
	| Zero | One -> assert false
	| Branch (v,yes,no,_,_) -> 
	    1 + 17*X.hash v + 257*(id yes) + 65537*(id no)

      let equal x y = (x == y) || match x,y with
	| Branch (v1,yes1,no1,id1,_), Branch (v2,yes2,no2,id2,_) ->
	    (id1 == 0 || id2 == 0) && X.equal v1 v2 && 
	      (yes1 == yes2) && (no1 == no2)
	| _ -> assert false
    end)
  let table = W.create 16383
  type branch = 
      { v : X.t; yes : node; no : node; mutable id : int; neg : branch }
  let mk v yes no =
    if yes == no then yes
    else
      let rec pos = Branch (v,yes,no,0,Branch (v,neg yes,neg no,0,pos)) in
      let x = W.merge table pos in
      let pos : branch = Obj.magic x in
      if (pos.id == 0) 
      then (let n = !max_id in
	    max_id := succ n;
	    pos.id <- n;
	    pos.neg.id <- (-n));
      x

  let atom v = mk v One Zero

  let dummy = Obj.magic (ref 0)
  let memo_size = 16383
  let memo_keys = Array.make memo_size (Obj.magic dummy)
  let memo_vals = Array.make memo_size (Obj.magic dummy)
  let memo_occ = Array.make memo_size 0

  let eg2 (x1,y1) (x2,y2) = x1 == x2 && y1 == y2
  let rec cup x1 x2 = if (x1 == x2) then x1 else match x1,x2 with
    | One, x | x, One -> One
    | Zero, x | x, Zero -> x
    | Branch (v1,yes1,no1,id1,neg1), Branch (v2,yes2,no2,id2,neg2) ->
	if (x1 == neg2) then One
	else
	  let k,h = 
	    if id1<id2 then (x1,x2),id1+65537*id2 else (x2,x1),id2+65537*id1 in
	  let h = (h land max_int) mod memo_size in
	  let i = memo_occ.(h) in
	  let k' = memo_keys.(h) in
	  if (k' != dummy) && (eg2 k k') 
	  then (memo_occ.(h) <- succ i; memo_vals.(h))
	  else 
	    let r = 
              let c = X.compare v1 v2 in
	      if (c = 0) then mk v1 (cup yes1 yes2) (cup no1 no2)
	      else if (c < 0) then mk v1 (cup yes1 x2) (cup no1 x2)
	      else mk v2 (cup yes2 x1) (cup no2 x1) in
	    if (i = 0) then (memo_keys.(h) <- k; memo_vals.(h) <- r;
			     memo_occ.(h) <- 1)
	    else memo_occ.(h) <- pred i;
	    r
  
  let rec dump ppf = function
    | One -> Format.fprintf ppf "+"
    | Zero -> Format.fprintf ppf "-"
    | Branch (x,p,n,id,_) -> 
	Format.fprintf ppf "%i:%a(@[%a,%a@])" 
	  id
	  X.dump x dump p dump n

(*
  let cup x y =
    let d = cup x y in
    Format.fprintf Format.std_formatter "X = %a@\nY = %a@\nX+Z = %a@\n"
      dump x dump y dump d;  
    d
*)
	  

  let cap x1 x2 = neg (cup (neg x1) (neg x2))
  let diff x1 x2 = neg (cup (neg x1) x2)


  let rec iter f = function
    | Branch (x,p,n,_,_) -> f x; iter f p; iter f n
    | _ -> ()



  let rec print f ppf = function
    | One -> Format.fprintf ppf "Any"
    | Zero -> Format.fprintf ppf "Empty"
    | Branch (x,p,n,_,_) ->
	let flag = ref false in
	let b () = if !flag then Format.fprintf ppf " | " else flag := true in
	(match p with 
	   | One -> b(); Format.fprintf ppf "%a" f x
	   | Zero -> ()
	   | _ -> b (); Format.fprintf ppf "%a & @[(%a)@]" f x (print f) p );
	(match n with 
	   | One -> b (); Format.fprintf ppf "@[~%a@]" f x
	   | Zero -> ()
	   | _ -> b (); Format.fprintf ppf "@[~%a@] & @[(%a)@]" f x (print f) n)
	
  let print a f = function
    | One -> [ fun ppf -> Format.fprintf ppf "%s" a ]
    | Zero -> []
    | c -> [ fun ppf -> print f ppf c ]
	
  let rec get accu pos neg = function
    | One -> (pos,neg) :: accu
    | Zero -> accu
    | Branch (x,p,n,_,_) ->
	(*OPT: can avoid creating this list cell when pos or neg =False *)
	let accu = get accu (x::pos) neg p in
	let accu = get accu pos (x::neg) n in
	accu
	  
  let get x = get [] [] [] x
		
  let compute ~empty ~full ~cup ~cap ~diff ~atom b =
    let rec aux = function
      | One -> full
      | Zero -> empty
      | Branch(x,p,n,_,_) ->
	  let p = cap (atom x) (aux p)
	  and n = diff (aux n) (atom x) in
	  cup p n
    in
    aux b
      
  let empty = Zero
  let full = One

  let rec serialize t = function
    | (Zero | One) as b -> 
	Serialize.Put.bool t true; Serialize.Put.bool t (b == One)
    | Branch (x,p,n,_,_) ->
	Serialize.Put.bool t false;
	X.serialize t x;
	serialize t p;
	serialize t n

  let rec deserialize t =
    if Serialize.Get.bool t then
      if Serialize.Get.bool t then One else Zero
    else
      let x = X.deserialize t in
      let p = deserialize t in
      let n = deserialize t in

      let x = atom x in
      cup (cap x p) (cap (neg x) n)

      (* mk x p n is not ok, because order of keys might have changed!
	 OPT TODO: detect when this is ok *)

  let trivially_disjoint x y = neg x == y  
  let compare x y = compare (id x) (id y)
  let equal x y = x == y
  let hash x = id x
  let check x = ()

  type bdd = False | True | Br of elem * t * t 
  let br = function
    | Zero -> False | One -> True | Branch (x,p,n,_,_) -> Br (x,p,n)
end


module Make2(X : Custom.T) = struct
  module XSet = Hashset.MakeSet(X)

  type 'a s = {
    pos : XSet.t;
    neg : XSet.t;
    sub : 'a;
(*    vars : XSet.t; *)
    hash : int;
  }

  let tset_equal = ref ()
  let tset_compare = ref ()

  module rec TSet : Hashset.SET with type elt = TSet.t s = 
    Hashset.MakeSet(
      struct
	type t = TSet.t s
	let compare t1 t2 = 
	  if (t1 == t2) then 0 
	  else let x = t1.hash and y = t2.hash in
	  if x < y then (-1) else if x > y then 1
	  else let c = XSet.compare t1.pos t2.pos in if c <> 0 then c
	  else let c = XSet.compare t1.neg t2.neg in if c <> 0 then c
	  else (Obj.magic !tset_compare) t1.sub t2.sub
	    
	let equal t1 t2 = 
	  (t1 == t2) ||
	    (t1.hash == t2.hash &&
	       XSet.equal t1.pos t2.pos &&
	       XSet.equal t1.neg t2.neg &&
	       (Obj.magic !tset_equal) t1.sub t2.sub)

	let hash t = t.hash
      end)
      
  let () = 
    tset_compare := Obj.magic TSet.compare;
    tset_equal := Obj.magic TSet.equal

  type elem = X.t
  type t = TSet.t s

  let compare t1 t2 = 
    if (t1 == t2) then 0 
    else let x = t1.hash and y = t2.hash in
    if x < y then (-1) else if x > y then 1
    else let c = XSet.compare t1.pos t2.pos in if c <> 0 then c
    else let c = XSet.compare t1.neg t2.neg in if c <> 0 then c
    else TSet.compare t1.sub t2.sub

  let equal t1 t2 = 
    (t1 == t2) ||
      (t1.hash == t2.hash &&
	 XSet.equal t1.pos t2.pos &&
	 XSet.equal t1.neg t2.neg &&
	 TSet.equal t1.sub t2.sub)

  let hash t = t.hash


  let rec print f ppf t =
    Format.fprintf ppf "(";
    let first = ref true in
    let sep () = if !first then first := false else Format.fprintf ppf "&" in
    XSet.iter (fun x -> sep (); f ppf x) t.pos;
    XSet.iter (fun x -> sep (); Format.fprintf ppf "~"; f ppf x) t.neg;
    TSet.iter (fun t -> sep (); Format.fprintf ppf "~"; print f ppf t) t.sub;
    Format.fprintf ppf ")"

  let make pos neg sub =
(*    let vars =
      TSet.fold (fun t accu -> XSet.union t.vars accu) sub (XSet.union pos neg)
    in *)
    { pos = pos;
      neg = neg;
      sub = sub;
(*       vars = vars; *)
      hash = XSet.hash pos + 17 * XSet.hash neg + 257 * TSet.hash sub }
	

  let any = make XSet.empty XSet.empty TSet.empty
  let empty = make XSet.empty XSet.empty (TSet.singleton any)

  let atom x = make (XSet.singleton x) XSet.empty TSet.empty

  let compl t =
    if t == any then empty else if t == empty then any 
    else
    match XSet.is_empty t.pos, XSet.is_empty t.neg, TSet.is_empty t.sub with
      | true,true,false ->
	  (match TSet.is_singleton t.sub with
	     | Some t -> t
	     | None -> make XSet.empty XSet.empty (TSet.singleton t))
      | false,true,true ->
	  (match XSet.is_singleton t.pos with
	     | Some _ -> make XSet.empty t.pos TSet.empty
	     | None -> make XSet.empty XSet.empty (TSet.singleton t))
      | true,false,true ->
	  (match XSet.is_singleton t.neg with
	     | Some _ -> make t.neg XSet.empty TSet.empty
	     | None -> make XSet.empty XSet.empty (TSet.singleton t))
      | _ -> make XSet.empty XSet.empty (TSet.singleton t)

  let triv_subset t1 t2 =
    XSet.subset t2.pos t1.pos &&
      XSet.subset t2.neg t1.neg &&
      TSet.subset t2.sub t1.sub

  let can_factor t1 t2 =
    not (XSet.disjoint t1.pos t2.pos &&
      XSet.disjoint t1.neg t2.neg &&
      TSet.disjoint t1.sub t2.sub)


  let cap t1 t2 =
    if (t1 == any) || (equal t1 t2) || (triv_subset t2 t1) then t2 
    else if (t2 == any) || (triv_subset t1 t2) then t1
    else if (t1 == empty || t2 == empty) 
      || not (XSet.disjoint t1.pos t2.neg)
      || not (XSet.disjoint t1.neg t2.pos)
      || TSet.mem t1 t2.sub
      || TSet.mem t2 t1.sub then empty 
    else
      if XSet.is_empty t1.pos && XSet.is_empty t2.pos &&
	XSet.is_empty t1.neg && XSet.is_empty t2.neg then
	  match TSet.is_singleton t1.sub,TSet.is_singleton t2.sub with
	    | Some n1, Some n2 when triv_subset n1 n2 -> t2
	    | Some n1, Some n2 when triv_subset n2 n1 -> t1
(*	    | Some n1, Some n2 when can_factor n1 n2 ->
		Format.fprintf Format.std_formatter "XXX@.";
		let pos = XSet.inter n1.pos n2.pos
		and neg = XSet.inter n1.neg n2.neg
		and sub = TSet.inter n1.sub n2.sub in
		let t1 = make (XSet.diff n1.pos pos) (XSet.diff n1.neg neg)
		  (TSet.diff n1.sub sub)
		and t2 = make (XSet.diff n2.pos pos) (XSet.diff n2.neg neg)
		  (TSet.diff n2.sub sub) in
		let t1t2 = TSet.add t2 (TSet.singleton t1) in
		make XSet.empty XSet.empty 
		  (TSet.singleton
		     (make pos neg (TSet.add (make XSet.empty XSet.empty 
						t1t2) sub))) *)
	    | _ ->
		make XSet.empty XSet.empty (TSet.union t1.sub t2.sub)
      else
	make 
	  (XSet.union t1.pos t2.pos) 
	  (XSet.union t1.neg t2.neg)
	  (TSet.union t1.sub t2.sub)

  let dump ppf t =
    print (fun ppf x -> Format.fprintf ppf "%i" (X.hash x)) ppf t

  let not_triv t = t != empty && t != any

  let cap t1 t2 =
    let ppf = Format.std_formatter in
    let t' = cap t1 t2 in
    if (not_triv t1) && (not_triv t2) then
      Format.fprintf ppf "%a &&& %a === %a@." dump t1 dump t2 dump t';
    t'

  let cup t1 t2 = compl (cap (compl t1) (compl t2))
  let diff t1 t2 = cap t1 (compl t2)


  let rec simplify pos neg t =
    if not (XSet.disjoint t.pos neg) 
      || not (XSet.disjoint t.neg pos) then empty
    else
      let npos = XSet.diff t.pos pos
      and nneg = XSet.diff t.neg neg in
      let n = 
	if XSet.is_empty npos && XSet.is_empty nneg then any
	else make npos nneg TSet.empty in
      let pos' = XSet.union pos npos
      and neg' = XSet.union neg nneg in
      try 
	TSet.fold 
	  (fun t accu ->
	     let t = simplify pos' neg' t in
	     if t == any then raise Exit;
	     if t == empty then accu else diff accu t)
	  t.sub n
      with Exit -> empty
(*  and simplify pos neg t =
    let ppf = Format.std_formatter in
    let t' = simplify' pos neg t in
    Format.fprintf ppf "Simplify %a ==> %a@." dump t dump t';
    t' *)

  let dnf f (t : t) =
    let rec loop pos neg pos0 = function
      | [] -> f (XSet.elements pos) neg
      | t::tl when not (XSet.disjoint t.neg pos) -> loop pos neg pos0 tl
      | t::tl ->
	  let pos0 = 
	    XSet.fold 
	      (fun x pos0' -> 
		 loop (XSet.add x pos) neg pos0' tl;
		 XSet.add x pos0'
	      ) (XSet.diff t.neg pos0) pos0 in

	  let p = XSet.diff t.pos pos in
	  if not (XSet.is_empty p) then
	    loop pos (XSet.elements p :: neg) pos0 tl; 
	  TSet.iter (fun s ->loop 
		       (XSet.union s.pos pos)
		       (XSet.fold (fun x accu -> [x]::accu) s.neg neg)
		       pos0
		       (TSet.fold (fun t tl -> t::tl) s.sub tl)) t.sub
    in
    loop t.pos (List.map (fun x -> [x]) (XSet.elements t.neg))  XSet.empty
      (TSet.elements t.sub)  

  let trivially_disjoint t1 t2 =
    TSet.mem t1 t2.sub || TSet.mem t2 t1.sub ||
      not (XSet.disjoint t1.pos t2.neg) ||
      not (XSet.disjoint t1.neg t2.pos)

  let iter f t =
    let rec aux t =
      XSet.iter f t.pos;
      XSet.iter f t.neg;
      TSet.iter aux t.sub in
    aux t

  let full = any

  let get t =
    let l = ref [] in
    let rec aux pos neg = function
      | [] -> l := (pos,neg) :: !l
      | x::tl -> List.iter (fun a -> aux pos (a::neg) tl) x
    in
    dnf (fun pos neg -> aux pos [] neg) t;
    !l
	   

  let rec serialize s t =
    Serialize.Put.list X.serialize s (XSet.elements t.pos);
    Serialize.Put.list X.serialize s (XSet.elements t.neg);
    Serialize.Put.list serialize s (TSet.elements t.sub)

  let rec deserialize s =
    let pos = XSet.of_list (Serialize.Get.list X.deserialize s) in
    let neg = XSet.of_list (Serialize.Get.list X.deserialize s) in
    let sub = TSet.of_list (Serialize.Get.list deserialize s) in
    let t = make pos neg sub in
    if equal t empty then empty
    else if equal t any then any
    else t

  let check t = ()

  let compute ~empty ~full ~cup ~cap ~diff ~atom t =
    let rec aux t =
      let accu = XSet.fold (fun x accu -> cap (atom x) accu) t.pos empty in
      let accu = XSet.fold (fun x accu -> diff accu (atom x)) t.neg accu in
      let accu = TSet.fold (fun t accu -> diff accu (aux t)) t.sub accu in
      accu
    in
    aux t


(*
  let cap t1 t2 =
    let ppf = Format.std_formatter in
    let t' = cap t1 t2 in
    if (not_triv t1) && (not_triv t2) then
      Format.fprintf ppf "%a &&& %a === %a@." dump t1 dump t2 dump t';
    let t'' = simplify XSet.empty XSet.empty t' in
    if not (equal t' t'') then
      Format.fprintf ppf "SIMPLIF %a@." dump t'';
    t''

  let cup t1 t2 =
    let ppf = Format.std_formatter in
    let t' = cup t1 t2 in
    if (not_triv t1) && (not_triv t2) then
      Format.fprintf ppf "%a ||| %a === %a@." dump t1 dump t2 dump t';
    let t'' = simplify XSet.empty XSet.empty t' in
    if not (equal t' t'') then
      Format.fprintf ppf "SIMPLIF %a@." dump t'';
    t''

  let diff t1 t2 =
    let ppf = Format.std_formatter in
    let t' = diff t1 t2 in
    if (not_triv t1) && (not_triv t2) then
      Format.fprintf ppf "%a --- %a === %a@." dump t1 dump t2 dump t';
    let t'' = simplify XSet.empty XSet.empty t' in
    if not (equal t' t'') then
      Format.fprintf ppf "SIMPLIF %a@." dump t'';
    t''
*)
end

module type S'' = sig
  include S
  val dnf: (elem list -> (elem list) list -> unit) -> t -> unit
end
*)
