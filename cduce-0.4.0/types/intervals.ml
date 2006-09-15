(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Big_int

module V = struct

(* Hack to compute hash value for bigints *)
let hash_nat x = Nat.nth_digit_nat x 0
let hash_bigint (sign,nat) = sign * 17 + hash_nat nat

type t = big_int
let print ppf i = Format.fprintf ppf "%s" (string_of_big_int i)
let dump = print
let compare = compare_big_int
let hash x = hash_bigint (Obj.magic x)
let equal = eq_big_int

let check i = ()

let from_int i = big_int_of_int i
let from_bigint i = i
  

let mk = big_int_of_string
let to_string = string_of_big_int
let get_int = int_of_big_int
let get_bigint i = i
let is_int = is_int_big_int
let add = add_big_int
let mult = mult_big_int
let sub = sub_big_int
let div = div_big_int
let modulo  = mod_big_int
let succ = succ_big_int
let pred = pred_big_int
let negat t = sub_big_int zero_big_int t

let lt = lt_big_int
let gt = gt_big_int

let zero = big_int_of_int 0
let one = big_int_of_int 1
let minus_one = big_int_of_int (-1)
let is_zero = equal zero

let from_int32 i = mk (Int32.to_string i)
let from_int64 i = mk (Int64.to_string i)
let to_int32 i = Int32.of_string (to_string i)
let to_int64 i = Int64.of_string (to_string i)
end

type interval = 
  | Bounded of big_int * big_int
  | Left of big_int
  | Right of big_int
  | Any

type t = interval list

let dump ppf _ = Format.fprintf ppf "<Intervals.t>"

let rec check = function
  | []
  | [ Any ] | [ Right _ ] | [ Left _ ] | [ Bounded _ ] -> ()
  | (Left i | Bounded (_,i)) :: ((Bounded (j,_) | (Right j)) :: _ as rem) ->
      assert (V.compare i j < 0);
      check rem
  | _ -> assert false

let rec compare l1 l2 =
  match (l1,l2) with
    | [], [] -> 0
    | [],_ -> -1
    | _,[] -> 1
    | Bounded (a1,b1) :: l1, Bounded (a2,b2) :: l2 ->
	let c = V.compare a1 a2 in if c <> 0 then c 
	else let c = V.compare b1 b2 in if c <> 0 then c
	else compare l1 l2
    | Bounded (_,_) :: _, _ -> -1
    | _, Bounded (_,_) :: _ -> 1

    | Left a1 :: l1, Left a2 :: l2 ->
	let c = V.compare a1 a2 in if c <> 0 then c 
	else compare l1 l2
    | Left _ :: _, _ -> -1
    | _, Left _ :: _ -> 1

    | Right a1 :: l1, Right a2 :: l2 ->
	let c = V.compare a1 a2 in if c <> 0 then c 
	else compare l1 l2
    | Right _ :: _, _ -> -1
    | _, Right _ :: _ -> 1

    | Any :: _, Any :: _ -> 0


let rec equal l1 l2 =
  (l1 == l2) ||
  match (l1,l2) with
    | (Bounded (a1,b1) :: l1, Bounded (a2,b2) :: l2) ->
	(eq_big_int a1 a2) &&
	(eq_big_int b1 b2) &&
	(equal l1 l2)
    | (Left b1 :: l1, Left b2 :: l2) ->
	(eq_big_int b1 b2) &&
	(equal l1 l2)
    | (Right a1 :: l1, Right a2 :: l2) ->
	(eq_big_int a1 a2) &&
	(equal l1 l2)
    | (Any :: _, Any :: _) -> true
    | ([], []) -> true
    | _ -> false

let rec hash accu = function
  | Bounded (a,b) :: rem  ->
      hash (1 + 2 * (V.hash a) + 3 * (V.hash b) + 17 * accu) rem
  | Left b :: rem ->
      hash (3 * V.hash b + 17 * accu) rem
  | Right a :: _ ->
      2 * (V.hash a) + 17 * accu
  | Any :: _ -> 17 * accu + 1234
  | [] -> accu + 3

let hash = hash 0

let empty = []
let any = [Any]

let bounded a b =
  if le_big_int a b then [Bounded (a,b)] else empty

let left a = [Left a]
let right a = [Right a]
let atom a = bounded a a


let rec iadd_left l b = match l with
  | [] -> [Left b]
  | (Bounded (a1,_) | Right a1) :: _
      when (lt_big_int b (pred_big_int a1)) -> 
      Left b :: l
  | Bounded (_,b1) :: l' -> 
      iadd_left l' (max_big_int b b1)
  | Left b1 :: _ when le_big_int b b1-> l
  | Left _ :: l' ->
      iadd_left l' b
  | _ -> any

let rec iadd_bounded l a b = match l with
  | [] -> 
      [Bounded (a,b)]
  | (Bounded (a1,_) | Right a1) :: _
      when (lt_big_int b (pred_big_int a1)) -> 
      Bounded (a,b) :: l
  | ((Bounded (_,b1) | Left b1) as i') :: l' 
      when (lt_big_int (succ_big_int b1) a) -> 
      i'::(iadd_bounded l' a b)
  | Bounded (a1,b1) :: l' -> 
      iadd_bounded l' (min_big_int a a1) (max_big_int b b1)
  | Left b1 :: l' ->
      iadd_left l' (max_big_int b b1)
  | Right a1 :: _ -> [Right (min_big_int a a1)]
  | Any :: _ -> any

let rec iadd_right l a = match l with
  | [] -> [Right a]
  | ((Bounded (_,b1) | Left b1) as i') :: l' 
      when (lt_big_int (succ_big_int b1) a) -> 
      i'::(iadd_right l' a)
  | (Bounded (a1,_) | Right a1) :: _ -> 
      [Right (min_big_int a a1)]
  | _ -> any

let iadd l = function
  | Bounded (a,b) -> iadd_bounded l a b
  | Left b -> iadd_left l b
  | Right a -> iadd_right l a
  | Any -> any

let rec neg' start l = match l with
  | [] -> [Right start]
  | Bounded (a,b) :: l' -> 
      Bounded (start, pred_big_int a) :: (neg' (succ_big_int b) l')
  | Right a :: l' ->
      [Bounded (start, pred_big_int a)]
  | _ -> assert false

let neg = function
  | Any :: _ -> []
  | [] -> any
  | Left b :: l -> neg' (succ_big_int b) l
  | Right a :: _ -> [Left (pred_big_int a)]
  | Bounded (a,b) :: l -> Left (pred_big_int a) :: neg' (succ_big_int b) l

let cup i1 i2 =
  List.fold_left iadd i1 i2

let cap i1 i2 =
  neg (cup (neg i1) (neg i2))

let diff i1 i2 =
  neg (cup (neg i1) i2)

let is_empty = function [] -> true | _ -> false

let rec disjoint a b =
  match (a,b) with
    | [],_ | _,[] -> true
    | Any::_,_ | _,Any::_ -> false
    | Left _::_, Left _::_ -> false
    | Right _::_, Right _::_ -> false
    | Left x :: a, Bounded (y,_) :: _ -> (lt_big_int x y) && (disjoint a b)
    | Bounded (y,_) :: _, Left x :: b -> (lt_big_int x y) && (disjoint a b)
    | Left x :: _, Right y :: _ -> lt_big_int x y
    | Right y :: _, Left x :: _ -> lt_big_int x y
    | Right y :: _, Bounded (_,x) :: _ -> lt_big_int x y
    | Bounded (_,x) :: _, Right y :: _ -> lt_big_int x y
    | Bounded (xa,ya) :: a', Bounded (xb,yb) :: b' ->
	let c = compare_big_int xa xb in
	if c = 0 then false
	else
	  if c < 0 then (lt_big_int ya xb) && (disjoint a' b)
	  else (lt_big_int yb xa) && (disjoint a b')


(* TODO: can optimize this to stop running through the list earlier *)
let contains n =
  List.exists (function
		 | Any -> true
		 | Left b -> le_big_int n b
		 | Right a -> le_big_int a n
		 | Bounded (a,b) -> (le_big_int a n) && (le_big_int n b)
	      )

let sample = function
  | (Left x | Right x | Bounded (x,_)) :: _ -> x
  | Any :: _ -> zero_big_int
  | [] -> raise Not_found
  
let single = function
  | [ Bounded (x,y) ] when eq_big_int x y -> x
  | [] -> raise Not_found
  | _ -> raise Exit

let print =
  List.map 
    (fun x ppf -> match x with
       | Any ->
	   Format.fprintf ppf "Int"
       | Left b -> 
	   Format.fprintf ppf "*--%s" 
	     (string_of_big_int b)
       | Right a -> 
	   Format.fprintf ppf "%s--*" 
	     (string_of_big_int a)
       | Bounded (a,b) when eq_big_int a b -> 
	   Format.fprintf ppf "%s" 
	     (string_of_big_int a)
       | Bounded (a,b) ->
	   Format.fprintf ppf "%s--%s" 
	     (string_of_big_int a) 
	     (string_of_big_int b)
    )


let ( + ) = add_big_int
let ( * ) = mult_big_int


let add_inter i1 i2 = 
  match (i1,i2) with
    | Bounded (a1,b1), Bounded (a2,b2) -> Bounded (a1+a2, b1+b2)
    | Bounded (_,b1), Left b2 
    | Left b1, Bounded (_,b2)
    | Left b1, Left b2 -> Left (b1+b2)
    | Bounded (a1,_), Right a2 
    | Right a1, Bounded (a2,_)
    | Right a1, Right a2 -> Right (a1+a2)
    | _ -> Any


(* Optimize this ... *)
let add l1 l2 =
  List.fold_left 
    (fun accu i1 ->
       List.fold_left
	 (fun accu i2 -> iadd accu (add_inter i1 i2))
	 accu l2
	 
    ) empty l1

let negat = 
  List.rev_map 
    (function
       | Bounded (i,j) -> Bounded (minus_big_int j, minus_big_int i)
       | Left i -> Right (minus_big_int i)
       | Right j -> Left (minus_big_int j)
       | Any -> Any
    )

let sub l1 l2 =
  add l1 (negat l2)

type i = PlusInf | MinusInf | Int of V.t

let ( * ) x y = 
  match (x,y) with
    | PlusInf,PlusInf | MinusInf,MinusInf -> PlusInf
    | PlusInf,MinusInf | MinusInf,PlusInf -> MinusInf
    | Int x, Int y -> Int (x * y)
    | i,(Int x as ix) | (Int x as ix), i ->
	(match i, sign_big_int x with
	   | PlusInf,1 | MinusInf,-1 -> PlusInf
	   | PlusInf,-1 | MinusInf,1 -> MinusInf
	   | _ -> ix)

let min a b =
  match (a,b) with
    | MinusInf,_ | _,PlusInf -> a
    | PlusInf,_ | _,MinusInf -> b
    | Int x, Int y -> if le_big_int x y then a else b

let max a b =
  match (a,b) with
    | MinusInf,_ | _,PlusInf -> b
    | PlusInf,_ | _,MinusInf -> a
    | Int x, Int y -> if le_big_int x y then b else a

let max4 a b c d = max a (max b (max c d))
let min4 a b c d = min a (min b (min c d))

let ival = function
  | Bounded (a,b) -> (Int a,Int b)
  | Left a -> (MinusInf,Int a)
  | Right a -> (Int a,PlusInf)
  | Any -> (MinusInf,PlusInf)

let vali = function
  | (Int a, Int b) -> Bounded (a,b)
  | (MinusInf, Int a) -> Left a
  | (Int a, PlusInf) -> Right a
  | (MinusInf, PlusInf) -> Any
  | _ -> assert false

let mul_inter i1 i2 =
  let (a1,b1) = ival i1 and (a2,b2) = ival i2 in
  let a = a1 * a2 and b = b1 * b2 and c = a1 * b2 and d = a2 * b1 in
  vali (min4 a b c d, max4 a b c d)

let mul l1 l2 =
  List.fold_left 
    (fun accu i1 ->
       List.fold_left
	 (fun accu i2 -> iadd accu (mul_inter i1 i2))
	 accu l2
	 
    ) empty l1


let div i1 i2 = any
let modulo i1 i2 = any

let dmp s i =
  let ppf = Format.std_formatter in
  Format.fprintf ppf "%s = [ " s;
  List.iter (fun x -> x ppf; Format.fprintf ppf " ") (print i);  
  Format.fprintf ppf "] "

(*
let diff i1 i2 =
  let ppf = Format.std_formatter in
  Format.fprintf ppf "Intervals.diff:";
  dump "i1" i1;
  dump "i2" i2; 
  dump "i1-i2" (diff i1 i2); 
  Format.fprintf ppf "@\n";
  diff i1 i2
*)
(*
let cap i1 i2 =
  let ppf = Format.std_formatter in
  Format.fprintf ppf "Intervals.cap:";
  dmp "i1" i1;
  dmp "i2" i2; 
  dmp "i1*i2" (cap i1 i2); 
  Format.fprintf ppf "@.";
  cap i1 i2

*)

let int32 = bounded (V.from_int32 Int32.min_int) (V.from_int32 Int32.max_int)
let int64 = bounded (V.from_int64 Int64.min_int) (V.from_int64 Int64.max_int)
