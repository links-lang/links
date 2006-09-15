(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

module V = struct
  include Custom.Int

let max_char = 0x10FFFF

let check i = assert(i >= 0); assert(i <= max_char)

let mk_int c = 
  if (c < 0) || (c > max_char) then 
    failwith "Chars.mk_int: code point out of bound";
  c

let mk_char c =
  Char.code c
    
let to_int c = c

let to_char c =
  if (c > 255) then failwith "Chars.to_char: code-point > 255";
  Char.chr c

let print_in_string ppf = function
  | 10 -> Format.fprintf ppf "\\n"
  | 13 -> Format.fprintf ppf "\\r"
  | 9  -> Format.fprintf ppf "\\t"
  | 39 -> Format.fprintf ppf "\\'"
  | 34 -> Format.fprintf ppf "\\\""
  | c ->
      if (c < 32) || ((c >= 128) && (c < 192)) || (c > 255)
      then Format.fprintf ppf "\\%i;" c
      else Format.fprintf ppf "%c" (Char.chr c)

let print ppf c =
  Format.fprintf ppf "'%a'" print_in_string c
end
open V
      
include Custom.List(Custom.Pair(V)(V))

let rec check = function
  | [] -> ()
  | (a,b)::((c,d)::_ as tl) -> assert (a <= b); assert (b < c - 1); check tl
  | [(a,b)] -> assert (a <= b)  


let from_int c = 
  if (c < 0) || (c > V.max_char) then 
    failwith "Chars.from_int: code point out of bound";
  c

let to_int c = c

let empty = []
let any = [0,V.max_char]

let char_class a b = if a<=b then [a,b] else empty

let atom a = [a,a]

let rec add l ((a,b) as i) = match l with
  | [] -> 
      [i]
  | ((a1,_) :: _) as l when (b < a1 - 1) -> 
      i::l
  | ((a1,b1) as i' :: l') when (a > b1 + 1) -> 
      i'::(add l' i)
  | (a1,b1) :: l' -> 
      add l' (min a a1, max b b1)


let rec neg' start l = match l with
  | [] -> [start,max_char]
  | [ (a,b) ] when b = max_char -> [start,a-1]
  | (a,b) :: l' -> (start, a-1) :: (neg' (b+1) l')

let neg = function
  | (0,b) :: l -> if b = max_char then []  else neg' (b+1) l
  | l -> neg' 0 l

let cup i1 i2 = List.fold_left add i1 i2

let mk_classes c =
  List.fold_left (fun accu (i,j) -> cup accu (char_class i j)) empty c

(* TODO: optimize this ? *)
let cap i1 i2 = neg (cup (neg i1) (neg i2))

let diff i1 i2 = neg (cup (neg i1) i2)

let is_empty i = i = []

let rec disjoint (a : t) b =
  match (a,b) with
    | [],_ | _,[] -> true
    | (xa,ya)::a', (xb,yb)::b' ->
	if xa = xb then false
	else
	  if xa < xb then (ya < xb) && (disjoint a' b)
	  else (yb < xa) && (disjoint a b')

let contains n = List.exists (fun (a,b) -> (n>=a) && (n<=b))

let sample = function
  | (i,j) :: _ -> i
  | _ -> raise Not_found

let single = function
  | [ (i,j) ] when i = j -> i
  | [] -> raise Not_found
  | _ -> raise Exit

let is_char = function
  | [(i,j) ] when i = j -> Some i
  | _ -> None

let print =
  List.map 
    (fun (a,b) ->
       if a = b 
       then fun ppf -> 
	 V.print ppf a
       else fun ppf -> 
	 if a = 0 && b = max_char then Format.fprintf ppf "Char" else
	 Format.fprintf ppf "%a--%a" V.print a V.print b
    )

type 'a map = (int * 'a) list
(* Optimize lookup:
   - decision tree
   - merge adjacent segment with same result
*)

let mk_map l =
  let m = 
    List.fold_left 
      (fun accu (i,x) -> 
	 List.fold_left (fun accu (a,b) -> (b,x)::accu) accu i) [] l in
  let m = 
    List.sort 
      (fun (b1,x1) (b2,x2) -> 
	 if (b1 : int) < b2 then -1 else if b1 = b2 then 0 else 1)
      m in
  m

let rec get_map c = function
  | [_,x] -> x
  | (b,x)::rem -> if (c : int) <= b then x else get_map c rem
  | [] -> assert false

