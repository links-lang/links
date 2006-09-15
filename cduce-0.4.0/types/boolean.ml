(* Optimization ideas:
    -    (A|B) & (C|D) = A | (B & (C|D))    if A <= C
*)

module type S =
sig
  include Custom.T
  type elem

  external get: t -> (elem list * elem list) list = "%identity"

  val empty : t
  val full  : t
  val cup   : t -> t -> t
  val cap   : t -> t -> t
  val diff  : t -> t -> t
  val atom  : elem -> t

  val map : (elem -> elem) -> t -> t
  val iter: (elem -> unit) -> t -> unit
  val compute: empty:'d -> full:'c -> cup:('d -> 'c -> 'd) 
    -> cap:('c -> 'b -> 'c) -> diff:('c -> 'b -> 'c) ->
    atom:(elem -> 'b) -> t -> 'd
  val compute_bool: (elem -> t) -> t -> t
    
  val print: string -> (Format.formatter -> elem -> unit) -> t ->
    (Format.formatter -> unit) list
    
  val trivially_disjoint : t -> t -> bool    
end

module Make(X : Custom.T) = struct
  type elem = X.t
  module SList = SortedList.Make(X)
  include SortedList.Make(Custom.Pair(SList)(SList))




let empty = [ ]
let full  = [ [],[] ]

let atom x = [ [x],[] ]

let may_remove (p1,n1) (p2,n2) =
(*  false *)
  (SList.subset p2 p1) && (SList.subset n2 n1)

(* in some cases, it is faster to avoid may_remove...
   investigate this... *)


let cup t s = 
  if t == s then t
  else match (t,s) with
    | [],s -> s
    | t,[] -> t
    | [ [], [] ], _ | _, [ [], [] ] -> full
    | _ ->
	let s=
	  filter 
	    (fun (p,n) -> not (exists (may_remove (p,n)) t)) s in
	let t=
	  filter 
	    (fun (p,n) -> not (exists (may_remove (p,n)) s)) t in 
	cup s t

(*
let clean accu t =
  let rec aux accu = function
    | (p,n) :: rem ->
	if (List.exists (may_remove (p,n)) accu)
	  || (List.exists (may_remove (p,n)) rem)
	then aux accu rem
	else aux ((p,n)::accu) rem 
    | [] -> accu
  in
  from_list (aux accu t)
*)

    

let rec fold2_aux f a x = function
  | [] -> x
  | h :: t -> fold2_aux f a (f x a h) t

let rec fold2 f x l1 l2 =
  match l1 with
    | [] -> x
    | h :: t -> fold2 f (fold2_aux f h x l2) t l2

let rec should_add x = function
  | [] -> true
  | y::rem -> if may_remove x y then false else should_add x rem

let rec clean_add accu x = function
  | [] -> accu
  | y::rem -> 
      if may_remove y x then clean_add accu x rem 
      else clean_add (y::accu) x rem

let cap s t =
  if s == t then s
  else if s == full then t
  else if t == full then s
  else if (s == empty) || (t == empty) then empty
  else
    let (lines1,common,lines2) = split s t in
    let rec aux lines (p1,n1) (p2,n2) =
        if (SList.disjoint p1 n2) && (SList.disjoint p2 n1)
        then 
	  let x = (SList.cup p1 p2, SList.cup n1 n2) in
	  if should_add x lines then clean_add [x] x lines else lines
	else lines
    in
    from_list 
      (fold2 aux (get common) (get lines1) (get lines2))

let diff c1 c2 =
  if (c2 == full) || (c1 == c2) then empty
  else if (c1 == empty) || (c2 == empty) then c1
  else
    let c1 = diff c1 c2 in
    let line (p,n) =
      let acc = SList.fold (fun acc a -> ([], [a]) :: acc) [] p in
      let acc = SList.fold (fun acc a -> ([a], []) :: acc) acc n in
      from_list acc
    in
    fold (fun c1 l -> cap c1 (line l)) c1 c2


let rec map f t =
  let lines =
    List.fold_left
      (fun lines (p,n) ->
	 let p = SList.map f p and n = SList.map f n in
	 if (SList.disjoint p n) then (p,n) :: lines else lines)
      []
      t
  in
  from_list lines

let iter f t =
  iter (fun (p,n) -> SList.iter f p; SList.iter f n) t

let compute ~empty ~full ~cup ~cap ~diff ~atom t =
  let line (p,n) =
    List.fold_left (fun accu x -> diff accu (atom x)) (
      List.fold_left (fun accu x -> cap accu (atom x)) full p
    ) n in
  List.fold_left (fun accu l -> cup accu (line l)) empty t
  
let compute_bool f = 
  compute ~empty ~full ~cup ~cap ~diff ~atom:f


let print any f =
  List.map 
    (function 
	 (p1::p,n) -> 
	   (fun ppf ->
	      Format.fprintf ppf "@[%a" f p1;
	      List.iter (fun x -> Format.fprintf ppf " &@ %a" f x) p;
	      List.iter (fun x -> Format.fprintf ppf " \\@ %a" f x) n;
	      Format.fprintf ppf "@]";
	   )
       | ([],[]) -> 
	   (fun ppf -> Format.fprintf ppf "%s" any)
       | ([],[n]) ->
	   (fun ppf -> Format.fprintf ppf "@[%s \\ %a@]" any f n)
       | ([],n1::n) ->
	   (fun ppf -> 
              Format.fprintf ppf "@[%s" any;
	      List.iter (fun x -> Format.fprintf ppf " \\@ %a" f x) n;
	      Format.fprintf ppf "@]";
	   )
    )

let trivially_disjoint a b = cap a b = []

  external get: t -> (elem list * elem list) list = "%identity"

end

