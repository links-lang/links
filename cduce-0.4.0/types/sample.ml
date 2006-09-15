(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Ident

type t = Types.t

let rec try_seq f = function
  | [] -> raise Not_found
  | hd::tl -> try f hd with Not_found -> try_seq f tl

module D = Set.Make(Types)
  
let absent = Types.cons (Types.Record.or_absent Types.empty)

let rec get memo t =
  if D.mem t memo then raise Not_found;
  let memo = D.add t memo in
  let cons t = Types.cons (get memo t) in
  let pair (t1,t2) = Types.times (cons t1) (cons t2) in
  let xml (t1,t2) = Types.xml (cons t1) (cons t2) in
  let fields = function
    | (true,_) -> assert false (* absent *)
    | (false,t) -> cons t in
  let record (r,some,none) = 
    let r = LabelMap.filter (fun l (o,t) -> not o) r in
    Types.record_fields (not none, LabelMap.map fields r) in
  let typ u = 
    let u = Types.cap t u in 
    if Types.is_empty u then raise Not_found else u in
  try try_seq typ [ Types.Int.any; Types.Atom.any; Types.Char.any ] with Not_found ->
  try try_seq pair (Types.Product.get t) with Not_found ->
  try try_seq xml (Types.Product.get ~kind:`XML t) with Not_found ->
  try 
    let r = Types.Record.get t in
    let r = List.sort (fun (_,_,n1) (_,_,n2) -> -(compare n1 n2)) r in
    try_seq record r with Not_found ->
  try Types.Arrow.sample t with Not_found -> raise Not_found
(* t *)
(*  raise Not_found *)

     
let get = get D.empty

let print = Types.Print.print


let try_single r f x =
  try
    let v = f x in
    match !r with
      | None -> r := Some v
      | Some v' -> if (Types.Const.compare v v' !=0) then raise Exit
  with Not_found -> ()

let rec single memo t =
  if D.mem t memo then raise Exit;
  let memo = D.add t memo in
  let pair (t1,t2) = Types.Pair (single memo t1, single memo t2) in
  let xml (t1,t2) = Types.Xml (single memo t1, single memo t2) in
  let int t = Types.Integer (Intervals.single (Types.Int.get t)) in
  let atom t = Types.Atom (Atoms.single (Types.Atom.get t)) in
  let char t = Types.Char (Chars.single (Types.Char.get t)) in
  let fields = function
    | (true,_) -> assert false
    | (false,t) -> single memo t in
  let record = function
    | (r,false,true) -> 
	let r = 
	  LabelMap.filter 
	    (fun l (o,t) -> 
	       if o then if (Types.non_empty t) then raise Exit else false
	       else true) r in
	Types.Record (LabelMap.map fields r)
    | _ -> raise Exit in
  let r = ref None in
  try_single r int t;
  try_single r char t;
  try_single r atom t;
  List.iter (try_single r pair) (Types.Product.get t);
  List.iter (try_single r xml) (Types.Product.get ~kind:`XML t);
  List.iter (try_single r record) (Types.Record.get t);
  (try ignore (Types.Arrow.sample t); raise Exit with Not_found -> ());
  match !r with
    | None -> raise Not_found
    | Some c -> c

let single = single D.empty

let single_opt t =
  try Some (single t)
  with Not_found | Exit -> None
