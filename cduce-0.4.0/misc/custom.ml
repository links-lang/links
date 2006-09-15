(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

module type T = sig
  type t

  (* Debugging *)
  val dump: Format.formatter -> t -> unit
  val check: t -> unit (* Check internal invariants *)

  (* Data structures *)
  val equal: t -> t -> bool
  val hash: t -> int
  val compare:t -> t -> int
end

module Dummy = struct
  let dump ppf _ = failwith "dump not implemented"
  let check _ = failwith "check not implemented"
  let equal t1 t2 = failwith "equal not implemented"
  let hash t = failwith "hash not implemented"
  let compare t1 t2 = failwith "compare not implemented"
end

let dump_list ?(sep="; ") f ppf l =
  Format.pp_print_string ppf "[ ";
  (match l with 
     | [] -> ()
     | [hd] -> f ppf hd
     | hd::tl -> 
	 f ppf hd; 
	 List.iter (fun x -> Format.pp_print_string ppf sep; f ppf x) tl
  );
  Format.pp_print_string ppf " ]"

let dump_array ?(sep="; ") f ppf a = dump_list ~sep f ppf (Array.to_list a)

module String : T with type t = string = struct
  type t = string
  let dump = Format.pp_print_string
  let check s = ()

  let rec compare_string_aux s1 s2 l =
    if (l == 0) then 0 
    else
      let l = pred l in
      let c1 = Char.code (String.unsafe_get s1 l)
      and c2 = Char.code (String.unsafe_get s2 l) in
      if c1 != c2 then c2 - c1 else compare_string_aux s1 s2 l

  let compare s1 s2 =
    let l1 = String.length s1 and l2 = String.length s2 in
    if l1 != l2 then l2 - l1 else compare_string_aux s1 s2 l1


  let equal x y = compare x y = 0

  (* From btype.ml *)
  let hash s =
    let accu = ref 0 in
    for i = 0 to String.length s - 1 do
      accu := 223 * !accu + Char.code s.[i]
    done;
    (* reduce to 31 bits *)
    accu := !accu land (1 lsl 31 - 1);
    (* make it signed for 64 bits architectures *)
    if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu
end

module Int : T with type t = int = struct
  type t = int
  let dump = Format.pp_print_int
  let check s = ()
  let equal : t -> t -> bool = (=)
  let compare : t -> t -> int = Pervasives.compare
  let hash x = x
end

module Bool : T with type t = bool = struct
  type t = bool
  let dump = Format.pp_print_bool
  let check s = ()
  let equal : t -> t -> bool = (=)
  let compare : t -> t -> int = Pervasives.compare
  let hash x = if x then 1 else 0
end

module Array(X : T) = struct
  module Elem = X
  type t = X.t array
  let dump = dump_array X.dump
  let check a = Array.iter X.check a

  let rec compare_elems a1 a2 i l =
    if (i = l) then 0
    else
      let c = X.compare a1.(i) a2.(i) in
      if c <> 0 then c else compare_elems a1 a2 (succ i) l

  let compare a1 a2 =
    let l1 = Array.length a1 and l2 = Array.length a2 in
    let c = Pervasives.compare l1 l2 in if c <> 0 then c
    else compare_elems a1 a2 0 l1

  let equal a1 a2 = compare a1 a2 == 0

  let hash a =
    let h = ref (Array.length a) in
    Array.iter (fun x -> h := 17 * !h + X.hash x) a;
    !h
end

module List(X : T) = struct
  module Elem = X
  type t = X.t list
  let dump = dump_list X.dump
  let check l = List.iter X.check l

  let rec equal l1 l2 =
    (l1 == l2) ||
    match (l1,l2) with
      | x1::l1, x2::l2 -> (X.equal x1 x2) && (equal l1 l2)
      | _ -> false

  let rec hash accu = function
    | [] -> 1 + accu
    | x::l -> hash (17 * accu + X.hash x) l

  let hash l = hash 1 l

  let rec compare l1 l2 =
    if l1 == l2 then 0 
    else match (l1,l2) with
      | x1::l1, x2::l2 -> 
	  let c = X.compare x1 x2 in if c <> 0 then c 
	  else compare l1 l2
      | [],_ -> -1
      | _ -> 1
end


module Pair(X : T)(Y : T) = struct
  module Fst = X
  module Snd = Y

  type t = X.t * Y.t
  let dump ppf (x,y) = Format.fprintf ppf "(%a,%a)" X.dump x Y.dump y
  let check (x,y) = X.check x; Y.check y
  let compare (x1,y1) (x2,y2) =
    let c = X.compare x1 x2 in if c <> 0 then c
    else Y.compare y1 y2
  let equal (x1,y1) (x2,y2) = (X.equal x1 x2) && (Y.equal y1 y2)
  let hash (x,y) = X.hash x + 65599 * Y.hash y
end

type ('a,'b) choice = Left of 'a | Right of 'b

module Sum(X : T)(Y : T) = struct
  type t = (X.t,Y.t) choice
  let equal t1 t2 =
    match t1,t2 with
      | Left t1, Left t2 -> X.equal t1 t2
      | Right t1, Right t2 -> Y.equal t1 t2
      | _ -> false
  let hash = function
    | Left t1 -> 1 + 3 * X.hash t1
    | Right t2 -> 2 + 3 * Y.hash t2
  let compare t1 t2 =
    match t1,t2 with
      | Left t1, Left t2 -> X.compare t1 t2 
      | Right t1, Right t2 -> Y.compare t1 t2
      | Left _, Right _ -> -1
      | Right _, Left _ -> 1
  let check t = ()
  let dump ppf = function
    | Left t -> Format.fprintf ppf "L%a" X.dump t
    | Right t -> Format.fprintf ppf "R%a" Y.dump t
end

