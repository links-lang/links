module V = struct

  include Custom.Int
  
let print ppf i = Format.fprintf ppf "%i" i
let from_int i = i

let mk = int_of_string
let to_string = string_of_int
let get_int i = i
let is_int _ = true
let add = (+)
let mult = ( * )
let sub = (-)
let div = (/)
let modulo  = (mod)
let succ = succ
let pred = pred

let lt (x:int) y = x < y
let gt (x:int) y = x > y

let zero = 0
let one = 1
let minus_one = (-1)
let is_zero = (=) 0
end

module I = Custom.Pair(Custom.Int)(Custom.Int)
module IL = Custom.List(I)
include IL

let empty = []
let any = [min_int,max_int]

let bounded (a:int) b = if (a <= b) then [a,b] else []
let left a = [min_int,a]
let right a = [a,max_int]
let atom a = [a,a]

let rec iadd l ((a,b) as i) = match l with
  | [] -> [a,b]
  | (a1,_) :: _ when (b < pred a1) && (a1 > min_int)  -> 
      (a,b)::l
  | ((_,b1) as i') :: l' when (succ b1 < a) && (b1 < max_int) -> 
      i'::(iadd l' i)
  | (a1,b1) :: l' -> iadd l' (min a a1, max b b1)

let rec neg' start l = match l with
  | [] -> [start,max_int]
  | [a,b] when b = max_int -> if (a > start) then [start, pred a] else []
  | (a,b) :: l' -> if (a > start) then (start, pred a) :: (neg' (succ b) l')
    else neg' (succ b) l'

let neg l = neg' min_int l

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
    | (xa,ya) :: a', (xb,yb) :: b' ->
	let c = V.compare xa xb in
	if c = 0 then false
	else
	  if c < 0 then (ya < xb) && (disjoint a' b)
	  else (yb < xa) && (disjoint a b')


(* TODO: can optimize this to stop running through the list earlier *)
let contains n = List.exists (fun (a,b) -> (a <= n) && (n <= b))

let sample = function (x,_) :: _ -> x | [] -> raise Not_found
  
let single = function
  | [ (x,y) ] when (x:int) = y -> x
  | [] -> raise Not_found
  | _ -> raise Exit

let print =
  List.map 
    (fun (a,b) ppf ->
       if (a=b) then Format.fprintf ppf "%i" a
       else match (a=min_int,b=max_int) with
	 | true,true -> Format.fprintf ppf "Int"
	 | false,false -> Format.fprintf ppf "%i--%i" a b
	 | true,false -> Format.fprintf ppf "*--%i" b
	 | false,true -> Format.fprintf ppf "%i--*" a
    )

let may_add x y =
  (x = 0) || (y = 0) || (
    if (x > 0) && (y > 0) then x + y > y
    else if (x < 0) && (y < 0) then x + y < y
    else true
  )

let add l1 l2 =
  List.fold_left 
    (fun accu (a,b) ->
       List.fold_left
	 (fun accu (c,d) ->
	    if (may_add a c) && (may_add b d) then iadd accu (a+c,b+d)
	    else any)
	 accu l2)
    [] l1

let negat l = any
let sub l1 l2 = any
let mul l1 l2 = any

