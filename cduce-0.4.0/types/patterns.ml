(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

exception Error of string
open Ident

let get_opt = function Some x -> x | None -> assert false

(*
To be sure not to use generic comparison ...
*)
let (=) : int -> int -> bool = (==)
let (<) : int -> int -> bool = (<)
let (<=) : int -> int -> bool = (<=)
let (<>) : int -> int -> bool = (<>)
let compare = 1


(* Syntactic algebra *)
(* Constraint: any node except Constr has fv<>[] ... *)
type d =
  | Constr of Types.t
  | Cup of descr * descr
  | Cap of descr * descr
  | Times of node * node
  | Xml of node * node
  | Record of label * node
  | Capture of id
  | Constant of id * Types.const
  | Dummy
and node = {
  id : int;
  mutable descr : descr;
  accept : Types.Node.t;
  fv : fv
} and descr = Types.t * fv * d



let id x = x.id
let descr x = x.descr
let fv x = x.fv
let accept x = Types.internalize x.accept

let printed = ref []
let to_print = ref []
let rec print ppf (a,_,d) = 
  match d with
    | Constr t -> Types.Print.print ppf t
    | Cup (p1,p2) -> Format.fprintf ppf "(%a | %a)" print p1 print p2
    | Cap (p1,p2) -> Format.fprintf ppf "(%a & %a)" print p1 print p2
    | Times (n1,n2) -> 
	Format.fprintf ppf "(P%i,P%i)" n1.id n2.id;
	to_print := n1 :: n2 :: !to_print
    | Xml (n1,n2) -> 
	Format.fprintf ppf "XML(P%i,P%i)" n1.id n2.id;
	to_print := n1 :: n2 :: !to_print
    | Record (l,n) -> 
	Format.fprintf ppf "{ %a =  P%i }" Label.print_attr l n.id;
	to_print := n :: !to_print
    | Capture x ->
	Format.fprintf ppf "%a" Ident.print x
    | Constant (x,c) ->
	Format.fprintf ppf "(%a := %a)" Ident.print x
	  Types.Print.print_const c
    | Dummy ->
	Format.fprintf ppf "*DUMMY*"

let dump_print ppf =
  while !to_print != [] do
    let p = List.hd !to_print in
    to_print := List.tl !to_print;
    if not (List.mem p.id !printed) then
      ( printed := p.id :: !printed;
	Format.fprintf ppf "P%i:=%a\n" p.id print (descr p)
      )
  done

let print ppf d =
  Format.fprintf ppf "%a@\n" print d;
  dump_print ppf

let print_node ppf n =
  Format.fprintf ppf "P%i" n.id;
  to_print := n :: !to_print;
  dump_print ppf


let counter = ref 0

let dummy = (Types.empty,IdSet.empty,Dummy)
let make fv =
  incr counter;
  { id = !counter; descr = dummy; accept = Types.make (); fv = fv }

let define x ((accept,fv,_) as d) =
  (* assert (x.fv = fv); *)
  Types.define x.accept accept;
  x.descr <- d

let cons fv d =
  let q = make fv in
  define q d;
  q

let constr x = (x,IdSet.empty,Constr x)
let cup ((acc1,fv1,_) as x1) ((acc2,fv2,_) as x2) = 
  if not (IdSet.equal fv1 fv2) then (
    let x = match IdSet.pick (IdSet.diff fv1 fv2) with
      | Some x -> x
      | None -> get_opt (IdSet.pick (IdSet.diff fv2 fv1))
    in
    raise 
      (Error 
	 ("The capture variable " ^ (Ident.to_string x) ^ 
	  " should appear on both side of this | pattern"))
  );
  (Types.cup acc1 acc2, IdSet.cup fv1 fv2, Cup (x1,x2))
let cap ((acc1,fv1,_) as x1) ((acc2,fv2,_) as x2) = 
  if not (IdSet.disjoint fv1 fv2) then (
    let x = get_opt (IdSet.pick (IdSet.cap fv1 fv2)) in
    raise 
      (Error 
	 ("The capture variable " ^ (Ident.to_string x) ^ 
	    " cannot appear on both side of this & pattern")));
  (Types.cap acc1 acc2, IdSet.cup fv1 fv2, Cap (x1,x2))
let times x y =
  (Types.times x.accept y.accept, IdSet.cup x.fv y.fv, Times (x,y))
let xml x y =
  (Types.xml x.accept y.accept, IdSet.cup x.fv y.fv, Xml (x,y))
let record l x = 
  (Types.record l x.accept, x.fv, Record (l,x))
let capture x = (Types.any, IdSet.singleton x, Capture x)
let constant x c = (Types.any, IdSet.singleton x, Constant (x,c))


let print_node = ref (fun _ _ -> assert false)

module Node = struct
  type t = node
  let compare n1 n2 = n1.id - n2.id
  let equal n1 n2 = n1.id == n2.id
  let hash n = n.id

  let check n = ()
  let dump ppf x = !print_node ppf x
end

(* Pretty-print *)

module Pat = struct
  type t = descr
  let rec compare (_,_,d1) (_,_,d2) = if d1 == d2 then 0 else
    match (d1,d2) with
      | Constr t1, Constr t2 -> Types.compare t1 t2
      | Constr _, _ -> -1 | _, Constr _ -> 1

      | Cup (x1,y1), Cup (x2,y2) | Cap (x1,y1), Cap (x2,y2) ->
	  let c = compare x1 x2 in if c <> 0 then c 
	  else compare y1 y2
      | Cup _, _ -> -1 | _, Cup _ -> 1
      | Cap _, _ -> -1 | _, Cap _ -> 1

      | Times (x1,y1), Times (x2,y2) | Xml (x1,y1), Xml (x2,y2) ->
	  let c = Node.compare x1 x2 in if c <> 0 then c
	  else Node.compare y1 y2
      | Times _, _ -> -1 | _, Times _ -> 1
      | Xml _, _ -> -1 | _, Xml _ -> 1

      | Record (x1,y1), Record (x2,y2) ->
	  let c = Label.compare x1 x2 in if c <> 0 then c
	  else Node.compare y1 y2
      | Record _, _ -> -1 | _, Record _ -> 1

      | Capture x1, Capture x2 ->
	  Id.compare x1 x2
      | Capture _, _ -> -1 | _, Capture _ -> 1

      | Constant (x1,y1), Constant (x2,y2) ->
	  let c = Id.compare x1 x2 in if c <> 0 then c
	  else Types.Const.compare y1 y2
      | Constant _, _ -> -1 | _, Constant _ -> 1

      | Dummy, Dummy -> assert false

  let equal p1 p2 = compare p1 p2 == 0

  let rec hash (_,_,d) = match d with
    | Constr t -> 1 + 17 * (Types.hash t)
    | Cup (p1,p2) -> 2 + 17 * (hash p1) + 257 * (hash p2)
    | Cap (p1,p2) -> 3 + 17 * (hash p1) + 257 * (hash p2)
    | Times (q1,q2) -> 4 + 17 * q1.id + 257 * q2.id
    | Xml (q1,q2) -> 5 + 17 * q1.id + 257 * q2.id
    | Record (l,q) -> 6 + 17 * (Label.hash l) + 257 * q.id
    | Capture x -> 7 + (Id.hash x)
    | Constant (x,c) -> 8 + 17 * (Id.hash x) + 257 * (Types.Const.hash c)
    | Dummy -> assert false

  let check _ = assert false
  let dump _ = assert false
end

module Print = struct
  module M = Map.Make(Pat)
  module S = Set.Make(Pat)

  let names = ref M.empty
  let printed = ref S.empty
  let toprint = Queue.create ()
  let id = ref 0

  let rec mark seen ((_,_,d) as p) =
    if (M.mem p !names) then ()
    else if (S.mem p seen) then
      (incr id;
       names := M.add p !id !names;
       Queue.add p toprint)
    else 
      let seen = S.add p seen in
      match d with
	| Cup (p1,p2) | Cap (p1,p2) -> mark seen p1; mark seen p2
	| Times (q1,q2) | Xml (q1,q2) -> mark seen q1.descr; mark seen q2.descr
	| Record (_,q) -> mark seen q.descr
	| _ -> ()

  let rec print ppf p =
    try 
      let i = M.find p !names in
      Format.fprintf ppf "P%i" i
    with Not_found ->
      real_print ppf p
  and real_print ppf (_,_,d) =  match d with
    | Constr t ->
	Types.Print.print ppf t
    | Cup (p1,p2) ->
	Format.fprintf ppf "(%a | %a)" print p1 print p2
    | Cap (p1,p2) ->
	Format.fprintf ppf "(%a & %a)" print p1 print p2
    | Times (q1,q2) ->
	Format.fprintf ppf "(%a,%a)" print q1.descr print q2.descr
    | Xml (q1,{ descr = (_,_,Times(q2,q3)) }) ->
	Format.fprintf ppf "<(%a) (%a)>(%a)" print q1.descr print q2.descr print q3.descr
    | Xml _ -> assert false
    | Record (l,q) ->
	Format.fprintf ppf "{%a=%a}" Label.print_attr l print q.descr
    | Capture x ->
	Format.fprintf ppf "%a" Ident.print x
    | Constant (x,c) ->
	Format.fprintf ppf "(%a:=%a)" Ident.print x Types.Print.print_const c
    | Dummy -> assert false
      
  let print ppf p =
    mark S.empty p;
    print ppf p;
    let first = ref true in
    (try while true do
       let p = Queue.pop toprint in
       if not (S.mem p !printed) then 
	 ( printed := S.add p !printed;
	   Format.fprintf ppf " %s@ @[%a=%a@]"
	     (if !first then (first := false; "where") else "and")
	     print p
	     real_print p
	);
     done with Queue.Empty -> ());
    id := 0;
    names := M.empty;
    printed := S.empty


  let print_xs ppf xs =
    Format.fprintf ppf "{";
    let rec aux = function
      | [] -> ()
      | [x] -> Ident.print ppf x
      | x::q -> Ident.print ppf x; Format.fprintf ppf ","; aux q
    in
    aux xs;
    Format.fprintf ppf "}"
end

let () = print_node := (fun ppf n -> Print.print ppf (descr n))


(* Static semantics *)

let cup_res v1 v2 = Types.Positive.cup [v1;v2]
let empty_res fv = IdMap.constant (Types.Positive.ty Types.empty) fv
let times_res v1 v2 = Types.Positive.times v1 v2

(* Try with a hash-table *)
module MemoFilter = Map.Make 
  (struct 
     type t = Types.t * node 
     let compare (t1,n1) (t2,n2) = 
       if n1.id < n2.id then -1 else if n1.id > n2.id then 1 else
       Types.compare t1 t2
   end)

let memo_filter = ref MemoFilter.empty

let rec filter_descr t (_,fv,d) : Types.Positive.v id_map =
(* TODO: avoid is_empty t when t is not changing (Cap) *)
  if Types.is_empty t 
  then empty_res fv
  else
    match d with
      | Constr _ -> IdMap.empty
      | Cup ((a,_,_) as d1,d2) ->
	  IdMap.merge cup_res
	    (filter_descr (Types.cap t a) d1)
	    (filter_descr (Types.diff t a) d2)
      | Cap (d1,d2) ->
	  IdMap.merge cup_res (filter_descr t d1) (filter_descr t d2)
      | Times (p1,p2) -> filter_prod fv p1 p2 t
      | Xml (p1,p2) -> filter_prod ~kind:`XML fv p1 p2 t
      | Record (l,p) ->
	  filter_node (Types.Record.project t l) p
      | Capture c ->
	  IdMap.singleton c (Types.Positive.ty t)
      | Constant (c, cst) ->
	  IdMap.singleton c (Types.Positive.ty (Types.constant cst))
      | Dummy -> assert false

and filter_prod ?kind fv p1 p2 t =
  List.fold_left 
    (fun accu (d1,d2) ->
       let term = 
	 IdMap.merge times_res (filter_node d1 p1) (filter_node d2 p2)
       in
       IdMap.merge cup_res accu term
    )
    (empty_res fv)
    (Types.Product.normal ?kind t)


and filter_node t p : Types.Positive.v id_map =
  try MemoFilter.find (t,p) !memo_filter
  with Not_found ->
    let (_,fv,_) = descr p in
    let res = IdMap.map_from_slist (fun _ -> Types.Positive.forward ()) fv in
    memo_filter := MemoFilter.add (t,p) res !memo_filter;
    let r = filter_descr t (descr p) in
    IdMap.collide Types.Positive.define res r;
    r

let filter t p =
  let r = filter_node t p in
  memo_filter :=  MemoFilter.empty;
  IdMap.map Types.Positive.solve r
let filter_descr t p =
  let r = filter_descr t p in
  memo_filter :=  MemoFilter.empty;
  IdMap.get (IdMap.map Types.Positive.solve r)


(* Factorization of capture variables and constant patterns *)

module Factorize = struct
  module NodeTypeSet = Set.Make(Custom.Pair(Node)(Types))

  let pi1 ~kind t = Types.Product.pi1 (Types.Product.get ~kind t)
  let pi2 ~kind t = Types.Product.pi2 (Types.Product.get ~kind t)

(* Note: this is incomplete because of non-atomic constant patterns:
# debug approx (_,(x:=(1,2))) (1,2);;
[DEBUG:approx]
x=(1,2)
*)
  let rec approx_var seen (a,fv,d) t xs =
(*    assert (Types.subtype t a); 
      assert (IdSet.subset xs fv); *)
    if (IdSet.is_empty xs) || (Types.is_empty t) then xs
    else match d with
      | Cup ((a1,_,_) as p1,p2) ->
	  approx_var seen p2 (Types.diff t a1)
	    (approx_var seen p1 (Types.cap t a1) xs) 
      | Cap ((_,fv1,_) as p1,((_,fv2,_) as p2)) ->
	  IdSet.cup
	    (approx_var seen p1 t (IdSet.cap fv1 xs))
	    (approx_var seen p2 t (IdSet.cap fv2 xs))
      | Capture _ ->
	  xs
      | Constant (_,c) -> 
	  if (Types.subtype t (Types.constant c)) then xs else IdSet.empty
      | Times (q1,q2) ->
	  let xs = IdSet.cap xs (IdSet.cap q1.fv q2.fv) in
	  IdSet.cap
	    (approx_var_node seen q1 (pi1 ~kind:`Normal t) xs)
	    (approx_var_node seen q2 (pi2 ~kind:`Normal t) xs)
      | Xml (q1,q2) ->
	  let xs = IdSet.cap xs (IdSet.cap q1.fv q2.fv) in
	  IdSet.cap
	    (approx_var_node seen q1 (pi1 ~kind:`XML t) xs)
	    (approx_var_node seen q2 (pi2 ~kind:`XML t) xs)
      | Record _ -> IdSet.empty
      | _ -> assert false
	  
  and approx_var_node seen q t xs =
    if (NodeTypeSet.mem (q,t) seen) 
    then xs
    else approx_var (NodeTypeSet.add (q,t) seen) q.descr t xs
      

(* Obviously not complete ! *)      
  let rec approx_nil seen (a,fv,d) t xs =
    assert (Types.subtype t a); 
    assert (IdSet.subset xs fv);
    if (IdSet.is_empty xs) || (Types.is_empty t) then xs
    else match d with
      | Cup ((a1,_,_) as p1,p2) ->
	  approx_nil seen p2 (Types.diff t a1)
	    (approx_nil seen p1 (Types.cap t a1) xs) 
      | Cap ((_,fv1,_) as p1,((_,fv2,_) as p2)) ->
	  IdSet.cup
	    (approx_nil seen p1 t (IdSet.cap fv1 xs))
	    (approx_nil seen p2 t (IdSet.cap fv2 xs))
      | Constant (_,c) when Types.Const.equal c Sequence.nil_cst -> xs
      | Times (q1,q2) ->
	  let xs = IdSet.cap q2.fv (IdSet.diff xs q1.fv) in
	  approx_nil_node seen q2 (pi2 ~kind:`Normal t) xs
      | _ -> IdSet.empty
	  
  and approx_nil_node seen q t xs =
    if (NodeTypeSet.mem (q,t) seen) 
    then xs
    else approx_nil (NodeTypeSet.add (q,t) seen) q.descr t xs

(*
  let cst ((a,_,_) as p) t xs =
    if IdSet.is_empty xs then IdMap.empty
    else
      let rec aux accu (x,t) =
	if (IdSet.mem xs x) then
	  match Sample.single_opt (Types.descr t) with
	    | Some c -> (x,c)::accu
	    | None -> accu
	else accu in
      let t = Types.cap t a in
      IdMap.from_list_disj (List.fold_left aux [] (filter_descr t p))
*)
	
  let var ((a,_,_) as p) t = 
    approx_var NodeTypeSet.empty p (Types.cap t a)

  let nil ((a,_,_) as p) t = 
    approx_nil NodeTypeSet.empty p (Types.cap t a)
end




(* Normal forms for patterns and compilation *)

let min (a:int) (b:int) = if a < b then a else b

let any_basic = Types.Record.or_absent Types.non_constructed

let rec first_label (acc,fv,d) =
  if Types.is_empty acc 
  then Label.dummy
  else match d with
    | Constr t -> Types.Record.first_label t
    | Cap (p,q) -> Label.min (first_label p) (first_label q)
    | Cup ((acc1,_,_) as p,q) -> Label.min (first_label p) (first_label q)
    | Record (l,p) -> l
    | _ -> Label.dummy

module Normal = struct

  type source = SCatch | SConst of Types.const 
  type result = source id_map

  let compare_source s1 s2 =
    if s1 == s2 then 0 
    else match (s1,s2) with
      | SCatch, _ -> -1 | _, SCatch -> 1
      | SConst c1, SConst c2 -> Types.Const.compare c1 c2

(*
  let hash_source = function
    | SCatch -> 1
    | SConst c -> Types.Const.hash c
*)
    
  let compare_result r1 r2 =
    IdMap.compare compare_source r1 r2

  module ResultMap = Map.Make(struct
				type t = result
				let compare = compare_result
			      end)

  module NodeSet = SortedList.Make(Node)

  module Nnf = struct
    include Custom.Dummy

    type t = NodeSet.t * Types.t * IdSet.t (* pl,t;   t <= \accept{pl} *)
	
    let check (pl,t,xs) =
      List.iter (fun p -> assert(Types.subtype t (Types.descr p.accept)))
	(NodeSet.get pl)
    let print ppf (pl,t,xs) =
      Format.fprintf ppf "@[(pl=%a;t=%a)@]" NodeSet.dump pl Types.Print.print t
    let compare (l1,t1,xs1) (l2,t2,xs2) =
      let c = NodeSet.compare l1 l2 in if c <> 0 then c
      else let c = Types.compare t1 t2 in if c <> 0 then c
      else IdSet.compare xs1 xs2
    let hash (l,t,xs) = 
      (NodeSet.hash l) + 17 * (Types.hash t) + 257 * (IdSet.hash xs)
    let equal x y = compare x y == 0


    let first_label (pl,t,xs) = 
      List.fold_left
	(fun l p -> Label.min l (first_label (descr p)))
	(Types.Record.first_label t)
	pl


  end

  module NProd = struct
    type t = result * Nnf.t * Nnf.t

    let compare (r1,x1,y1) (r2,x2,y2) =
      let c = compare_result r1 r2 in if c <> 0 then c
      else let c = Nnf.compare x1 x2 in if c <> 0 then c
      else Nnf.compare y1 y2
  end

  module NLineProd = Set.Make(NProd)

  type record =
    | RecNolabel of result option * result option
    | RecLabel of label * NLineProd.t
  type t = {
    nprod  : NLineProd.t;
    nxml   : NLineProd.t;
    nrecord: record
  }

  let fus = IdMap.union_disj

  let nempty lab = 
    { nprod = NLineProd.empty; 
      nxml = NLineProd.empty;
      nrecord = (match lab with 
		   | Some l -> RecLabel (l,NLineProd.empty)
		   | None -> RecNolabel (None,None))
    }
  let dummy = nempty None


  let ncup nf1 nf2 = 
    { nprod   = NLineProd.union nf1.nprod nf2.nprod;
      nxml    = NLineProd.union nf1.nxml nf2.nxml;
      nrecord = (match (nf1.nrecord,nf2.nrecord) with
		   | RecLabel (l1,r1), RecLabel (l2,r2) -> 
		       (* assert (l1 = l2); *) 
		       RecLabel (l1, NLineProd.union r1 r2)
		   | RecNolabel (x1,y1), RecNolabel (x2,y2) -> 
		       RecNolabel((if x1 == None then x2 else x1),
				(if y1 == None then y2 else y1))
		   | _ -> assert false)
    }

  let double_fold_prod f l1 l2 =
    NLineProd.fold
      (fun x1 accu -> NLineProd.fold (fun x2 accu -> f accu x1 x2) l2 accu)
      l1 NLineProd.empty

  let ncap nf1 nf2 =
    let prod accu (res1,(pl1,t1,xs1),(ql1,s1,ys1)) (res2,(pl2,t2,xs2),(ql2,s2,ys2)) =
      let t = Types.cap t1 t2 in
      if Types.is_empty t then accu else
	let s = Types.cap s1 s2  in
	if Types.is_empty s then accu else
	  NLineProd.add (fus res1 res2, 
	   (NodeSet.cup pl1 pl2, t, IdSet.cup xs1 xs2),
	   (NodeSet.cup ql1 ql2, s, IdSet.cup ys1 ys2)) 
	  accu
    in
    let record r1 r2 = match r1,r2 with
      | RecLabel (l1,r1), RecLabel (l2,r2) ->
	  (* assert (l1 = l2); *)
	  RecLabel(l1, double_fold_prod prod r1 r2)
      | RecNolabel (x1,y1), RecNolabel (x2,y2) ->
	  let x = match x1,x2 with 
	    | Some res1, Some res2 -> Some (fus res1 res2) 
	    | _ -> None
	  and y = match y1,y2 with
	    | Some res1, Some res2 -> Some (fus res1 res2)
	    | _ -> None in
	  RecNolabel (x,y)
      | _ -> assert false
    in
    { nprod = double_fold_prod prod nf1.nprod nf2.nprod;
      nxml = double_fold_prod prod nf1.nxml nf2.nxml;
      nrecord = record nf1.nrecord nf2.nrecord;
    }

  let nnode p xs = NodeSet.singleton p, Types.descr p.accept, xs
  let nc t = NodeSet.empty, t, IdSet.empty
  let ncany = nc Types.any
  let ncany_abs = nc Types.Record.any_or_absent

  let empty_res = IdMap.empty

  let single_prod src p q = NLineProd.singleton (src, p,q)

  let ntimes lab acc p q xs = 
    let xsp = IdSet.cap xs p.fv and xsq = IdSet.cap xs q.fv in
    { (nempty lab) with 
	nprod = single_prod empty_res (nnode p xsp) (nnode q xsq)
    }

  let nxml lab acc p q xs = 
    let xsp = IdSet.cap xs p.fv and xsq = IdSet.cap xs q.fv in
    { (nempty lab) with 
	nxml =  single_prod empty_res (nnode p xsp) (nnode q xsq)
    }
    
  let nrecord (lab : Label.t option) acc (l : Label.t) p xs =
    let label = get_opt lab in
(*    Format.fprintf
      Format.std_formatter "label=%a l=%a@."
      Label.print label Label.print l; *)
    assert (Label.compare label l <= 0);
    let lft,rgt =
      if Label.equal l label
      then nnode p xs, ncany
      else ncany_abs, nnode (cons p.fv (record l p)) xs
    in
    { (nempty lab) with
	nrecord = RecLabel(label, single_prod empty_res lft rgt) }

  let nconstr lab t =
    let aux l =
      List.fold_left (fun accu (t1,t2) -> 
			NLineProd.add (empty_res, nc t1,nc t2) accu)
	NLineProd.empty l in
    let record = match lab with
      | None ->
	  let (x,y) = Types.Record.empty_cases t in
	  RecNolabel ((if x then Some empty_res else None), 
		      (if y then Some empty_res else None))
      | Some l ->
	  RecLabel (l,aux (Types.Record.split_normal t l)) in
    {  nprod = aux (Types.Product.normal t);
       nxml  = aux (Types.Product.normal ~kind:`XML t);
       nrecord = record
    }

  let nany lab res =
    { nprod  = single_prod res ncany ncany;
      nxml   = single_prod res ncany ncany;
      nrecord = match lab with
	| None -> RecNolabel (Some res, Some res)
	| Some lab -> RecLabel (lab, single_prod res ncany_abs ncany)
    }

  let nconstant lab x c = nany lab (IdMap.singleton x (SConst c))
  let ncapture lab x = nany lab (IdMap.singleton x SCatch)

  let rec nnormal lab (acc,fv,d) xs =
    let xs = IdSet.cap xs fv in
    if Types.is_empty acc then nempty lab
    else if IdSet.is_empty xs then nconstr lab acc
    else match d with
      | Constr t -> assert false
      | Cap (p,q) -> ncap (nnormal lab p xs) (nnormal lab q xs)
      | Cup ((acc1,_,_) as p,q) -> 
	  ncup 
	    (nnormal lab p xs) 
	    (ncap (nnormal lab q xs) (nconstr lab (Types.neg acc1)))
      | Times (p,q) -> ntimes lab acc p q xs
      | Xml (p,q) -> nxml lab acc p q xs
      | Capture x -> ncapture lab x
      | Constant (x,c) -> nconstant lab x c
      | Record (l,p) -> nrecord lab acc l p xs
      | Dummy -> assert false

(*TODO: when an operand of Cap has its first_label > lab,
  directly shift it*)


  let facto f t xs pl =
    List.fold_left 
      (fun vs p -> IdSet.cup vs (f (descr p) t (IdSet.cap (fv p) xs)))
      IdSet.empty
      pl

  let factorize t0 (pl,t,xs) =
    let t0 = if Types.subtype t t0 then t else Types.cap t t0 in
    let vs_var = facto Factorize.var t0 xs pl in
    let xs = IdSet.diff xs vs_var in
    let vs_nil = facto Factorize.nil t0 xs pl in
    let xs = IdSet.diff xs vs_nil in
    (vs_var,vs_nil,(pl,t,xs))

  let normal l t pl xs =
    List.fold_left 
      (fun a p -> ncap a (nnormal l (descr p) xs)) (nconstr l t) pl

  let nnf lab t0 (pl,t,xs) = 
(*    assert (not (Types.disjoint t t0)); *)
    let t = if Types.subtype t t0 then t else Types.cap t t0 in
    normal lab t (NodeSet.get pl) xs

  let basic_tests f (pl,t,xs) =
    let rec aux more s accu t res = function
	(* Invariant: t and s disjoint, t not empty *)
      | [] -> 
	  let accu =
	    try 
	      let t' = ResultMap.find res accu in
	      ResultMap.add res (Types.cup t t') accu
	    with Not_found -> ResultMap.add res t accu in
	  cont (Types.cup t s) accu more
      | (tp,xp,d) :: r -> 
	  if (IdSet.disjoint xp xs) 
	  then aux_check more s accu (Types.cap t tp) res r
	  else match d with
	    | Cup (p1,p2) -> aux ((t,res,p2::r)::more) s accu t res (p1::r)
	    | Cap (p1,p2) -> aux more s accu t res (p1 :: p2 :: r)
	    | Capture x -> aux more s accu t (IdMap.add x SCatch res) r
	    | Constant (x,c) -> 
		aux more s accu t (IdMap.add x (SConst c) res) r
	    | _ -> cont s accu more
    and aux_check more s accu t res pl =
      if Types.is_empty t then cont s accu more else aux more s accu t res pl
    and cont s accu = function
      | [] -> ResultMap.iter f accu
      | (t,res,pl)::tl -> aux_check tl s accu (Types.diff t s) res pl
    in
    aux_check [] Types.empty ResultMap.empty (Types.cap t any_basic) 
      IdMap.empty (List.map descr pl)

(*
  let prod_tests (pl,t,xs) =
    let rec aux accu q1 q2 res = function
      | [] -> (res,q1,q2) :: accu
      | (tp,xp,d) :: r -> 
	  if (IdSet.disjoint xp xs) 
	  then aux_check accu q1 q2 res tp r
	  else match d with
	    | Cup (p1,p2) -> aux (aux accu q1 q2 res (p2::r)) q1 q2 res (p1::r)
	    | Cap (p1,p2) -> aux accu q1 q2 res (p1 :: p2 :: r)
	    | Capture x -> aux accu q1 q2 (IdMap.add x SCatch res) r
	    | Constant (x,c) -> aux accu q1 q2 (IdMap.add x (SConst c) res) r
	    | Times (p1,p2) -> 
		let (pl1,t1,xs1) = q1 and (pl2,t2,xs2) = q2 in
		let t1 = Types.cap t1 (Types.descr (accept p1)) in
		if Types.is_empty t1 then accu
		else let t2 = Types.cap t2 (Types.descr (accept p2)) in
		if Types.is_empty t2 then accu
		else
		  let q1 =
		    let xs1' = IdSet.cap (fv p1) xs in
		    if IdSet.is_empty xs1' then (pl1,t1,xs1)
		    else (NodeSet.add p1 pl1, t1, IdSet.cup xs1 xs1')
		  and q2 = 
		    let xs2' = IdSet.cap (fv p2) xs in
		    if IdSet.is_empty xs2' then (pl2,t2,xs2)
		    else (NodeSet.add p2 pl2, t2, IdSet.cup xs2 xs2')
		  in
		  aux accu q1 q2 res r
	    | _ -> accu
    and aux_check accu q1 q2 res t r =
      List.fold_left
	(fun accu (t1',t2') ->
	   let (pl1,t1,xs1) = q1 and (pl2,t2,xs2) = q2 in
	   let t1 = Types.cap t1 t1' in
	   if Types.is_empty t1 then accu
	   else let t2 = Types.cap t2 t2' in
	   if Types.is_empty t2 then accu
	   else aux accu (pl1,t1,xs1) (pl2,t2,xs2) res r)
	accu (Types.Product.clean_normal (Types.Product.normal t))
    in
    aux_check [] ncany ncany IdMap.empty t (List.map descr pl)
*)
end


module Compile = struct
  open Auto_pat
      
  type return_code = 
      Types.t * int *   (* accepted type, arity *)
      int id_map option array

  type interface =
    [ `Result of int
    | `Switch of interface * interface
    | `None ]

  type dispatcher = {
    id : int;
    t  : Types.t;
    pl : Normal.Nnf.t array;
    label : label option;
    interface : interface;
    codes : return_code array;
    state : state;
  }

  let dispatcher_of_state = Hashtbl.create 1024

  let equal_array f a1 a2 =
    let rec aux i = (i < 0) || ((f a1.(i) a2.(i)) && (aux (i - 1))) in
    let l1 = Array.length a1 and l2 = Array.length a2 in
    (l1 == l2) && (aux (l1 - 1))

  let array_for_all f a =
    let rec aux f a i = (i < 0) || (f a.(i) && (aux f a (pred i))) in
    aux f a (Array.length a - 1)

  let array_for_all_i f a =
    let rec aux f a i = (i < 0) || (f i a.(i) && (aux f a (pred i))) in
    aux f a (Array.length a - 1)

  let equal_source s1 s2 =
    (s1 == s2) || match (s1,s2) with
      | Const x, Const y -> Types.Const.equal x y 
      | Stack x, Stack y -> x == y
      | Recompose (x1,x2), Recompose (y1,y2) -> (x1 == y1) && (x2 == y2)
      | _ -> false

  let equal_result (r1,s1,l1) (r2,s2,l2) =
    (r1 == r2) && (equal_array equal_source s1 s2) && (l1 == l2)

  let equal_result_dispatch d1 d2 = (d1 == d2) || match (d1,d2) with
    | Dispatch (d1,a1), Dispatch (d2,a2) -> 
	(d1 == d2) && (equal_array equal_result a1 a2)
    | TailCall d1, TailCall d2 -> d1 == d2
    | Ignore a1, Ignore a2 -> equal_result a1 a2
    | _ -> false

  let immediate_res basic prod xml record =
    let res : result option ref = ref None in
    let chk = function Catch | Const _ -> true | _ -> false in
    let f ((_,ret,_) as r) =
      match !res with
	| Some r0 when equal_result r r0 -> ()
	| None when array_for_all chk ret -> res := Some r
	| _ -> raise Exit in
    (match basic with [_,r] -> f r | [] -> () | _ -> raise Exit);
    (match prod with Ignore (Ignore r) -> f r |Impossible ->()| _->raise Exit);
    (match xml with Ignore (Ignore r) -> f r |Impossible ->()| _->raise Exit);
    (match record with
      | None -> ()
      | Some (RecLabel (_,Ignore (Ignore r))) -> f r
      | Some (RecNolabel (Some r1, Some r2)) -> f r1; f r2
      | _ -> raise Exit);
    match !res with Some r -> r | None -> raise Exit
	  
  let split_kind basic prod xml record = {
    basic = basic;
    atoms = Atoms.mk_map (List.map (fun (t,r) -> Types.Atom.get t, r) basic);
    chars = Chars.mk_map (List.map (fun (t,r) -> Types.Char.get t, r) basic);
    prod = prod; 
    xml = xml; 
    record = record
  }

  let combine_kind basic prod xml record =
    try AIgnore (immediate_res basic prod xml record)
    with Exit -> AKind (split_kind basic prod xml record)
      
  let combine f (disp,act) =
    if Array.length act == 0 then Impossible
    else
      if (array_for_all (fun (_,ar,_) -> ar == 0) disp.codes) 
	 && (array_for_all ( f act.(0) ) act) then
	   Ignore act.(0)
      else
	Dispatch (disp.state, act)

  let detect_tail_call f = function
    | Dispatch (disp,branches) when array_for_all_i f branches -> TailCall disp
    | x -> x

  let detect_right_tail_call =
    detect_tail_call
      (fun i (code,ret,_) ->
	 (i == code) && 
	   let ar = Array.length ret in
	   (array_for_all_i 
	      (fun pos -> 
		 function Stack j when pos + j == ar -> true | _ -> false)
	      ret
	   )
      )

  let detect_left_tail_call =
    detect_tail_call
      (fun i -> 
	 function 
	   | Ignore (code,ret,_) when (i == code) ->
	       let ar = Array.length ret in
	       array_for_all_i 
		 (fun pos -> 
		    function Stack j when pos + j == ar -> true | _ -> false)
		 ret
	   | _ -> false
      )
   
  let cur_id = ref 0
		 
  module NfMap = Map.Make(Normal.Nnf)
  module NfSet = Set.Make(Normal.Nnf)

  module DispMap = Map.Make(Custom.Pair(Types)(Custom.Array(Normal.Nnf)))

    (* Try with a hash-table ! *)
    
  let dispatchers = ref DispMap.empty

  let rec print_iface ppf = function
    | `Result i -> Format.fprintf ppf "Result(%i)" i
    | `Switch (yes,no) -> Format.fprintf ppf "Switch(%a,%a)"
	print_iface yes print_iface no
    | `None -> Format.fprintf ppf "None"
   
  let dump_disp disp =
    let ppf = Format.std_formatter in
    Format.fprintf ppf "Dispatcher t=%a@." Types.Print.print disp.t;
    Array.iter (fun p ->
		 Format.fprintf ppf "  pat %a@." Normal.Nnf.print p;
	      ) disp.pl
   
  let first_lab t reqs =
    let aux l req = Label.min l (Normal.Nnf.first_label req) in
    let lab = 
      Array.fold_left aux (Types.Record.first_label t) reqs in
    if lab == Label.dummy then None else Some lab

  let dummy_actions = AIgnore ((-1),[||],(-1))

  let compute_actions = ref (fun _ -> assert false)

  let dispatcher t pl : dispatcher =
    try DispMap.find (t,pl) !dispatchers
    with Not_found ->
      let lab = first_lab t pl in
      let nb = ref 0 in
      let codes = ref [] in
      let rec aux t arity i accu = 
	if i == Array.length pl 
	then (incr nb; let r = Array.of_list (List.rev accu) in 
	      codes := (t,arity,r)::!codes; `Result (!nb - 1))
	else
	  let (_,tp,v) = pl.(i) in
	  let a1 = Types.cap t tp in
	  if Types.is_empty a1 then
	    `Switch (`None,aux t arity (i+1) (None::accu))
	  else
	    let a2 = Types.diff t tp in
	    let accu' = Some (IdMap.num arity v) :: accu in
	    if Types.is_empty a2 then
	      `Switch (aux t (arity + (IdSet.length v)) (i+1) accu',`None)
	    else
	      `Switch (aux a1 (arity + (IdSet.length v)) (i+1) accu',
		       aux a2 arity (i+1) (None::accu))

(* Unopt version:
	    `Switch 
	      (
	       aux (Types.cap t tp) (arity + (IdSet.length v)) (i+1) accu',
	       aux (Types.diff t tp) arity (i+1) accu
	      )
*)

      in
      let iface = if Types.is_empty t then `None else aux t 0 0 [] in
      let codes = Array.of_list (List.rev !codes) in
      let state = { 
	uid = !cur_id;
	arity = Array.map (fun (_,ar,_) -> ar) codes;
	actions = dummy_actions;
	fail_code = (-1);
	expected_type = "";
      } in
      let disp = { 
	id = !cur_id; 
	t = t; label = lab; pl = pl;
	interface = iface; codes = codes; state = state } in
      incr cur_id;
      Hashtbl.add dispatcher_of_state state.uid disp;
      dispatchers := DispMap.add (t,pl) disp !dispatchers;
      !compute_actions disp;
      disp
    
  let find_code d a =
    let rec aux i = function
      | `Result code -> code
      | `None -> 
	  Format.fprintf Format.std_formatter
	    "IFACE=%a@." print_iface d.interface; 
	  for i = 0 to Array.length a - 1 do
	    Format.fprintf Format.std_formatter
	      "a.(i)=%b@." (a.(i) != None)
	  done;
	  assert false
      | `Switch (yes,_) when a.(i) != None -> aux (i + 1) yes
      | `Switch (_,no) -> aux (i + 1) no in
    aux 0 d.interface

  let create_result pl =
    let aux x accu = match x with Some b -> b @ accu | None -> accu in
    Array.of_list (Array.fold_right aux pl [])

  let return disp pl f ar =
    let aux = function x::_ -> Some (f x) | [] -> None in
    let final = Array.map aux pl in
    (find_code disp final, create_result final, ar)

  let conv_source = function
    | Normal.SCatch -> Catch
    | Normal.SConst c -> Const c
    
  let return_basic disp selected =
    let aux_final res = IdMap.map_to_list conv_source res in
    return disp selected aux_final 0

(*  let print_idset ppf s =
    let s = String.concat "," (List.map (fun x -> Ident.to_string x) s) in
    Format.fprintf ppf "{%s}" s
  let print_idmap ppf s =
    print_idset ppf (IdMap.domain s) *)

  let merge_res_prod ofs1 ofs2 (lvars,lnils,lres) (rvars,rnils,rres) extra =
    let lres =
      IdMap.union_disj
	(IdMap.map (fun i -> Stack (ofs1 + ofs2 - i)) lres)
	(IdMap.union_disj 
	   (IdMap.constant Left lvars) (IdMap.constant Nil lnils)) in
    let rres =
      IdMap.union_disj
	(IdMap.map (fun i -> Stack (ofs2 - i)) rres)
	(IdMap.union_disj 
	   (IdMap.constant Right rvars) (IdMap.constant Nil rnils)) in
    let sub = 
      IdMap.merge
	(fun l r ->
	   match l,r with
	     | Left,Right -> Catch
	     | _ -> 
		 let l = 
		   match l with Left -> (-1) | Nil -> (-2) 
		     | Stack i -> i | _ -> assert false in
		 let r = 
		   match r with Right -> (-1) | Nil -> (-2) 
		     | Stack i -> i | _ -> assert false in
		 Recompose (l,r)) lres rres in
    IdMap.map_to_list (fun x -> x)
      (IdMap.union_disj sub (IdMap.map conv_source extra))
    

  module TypeList = SortedList.Make(Types)

  let dispatch_basic disp : (Types.t * result) list =
    let tests =
      let accu = ref [] in
      let aux i res t = accu := (t, [i,res]) :: !accu in
      Array.iteri (fun i p -> Normal.basic_tests (aux i) p) disp.pl;
      TypeList.Map.get (TypeList.Map.from_list (@) !accu) in

    let t = Types.cap any_basic disp.t in
    let accu = ref [] in
    let rec aux (success : (int * Normal.result) list) t l = 
      if Types.non_empty t 
      then match l with
	| [] ->
	    let selected = Array.create (Array.length disp.pl) [] in
	    let add (i,res) = selected.(i) <- res :: selected.(i) in
	    List.iter add success;
	    accu := (t, return_basic disp selected) :: !accu
	| (ty,i) :: rem -> 
	    aux (i @ success) (Types.cap t ty) rem; 
	    aux success (Types.diff t ty) rem
    in
    aux [] t tests;
    !accu

  let get_tests facto pl f t d post =
    let pl = Array.map (List.map f) pl in

    (* Collect all subrequests *)
    let aux reqs (req,_) = NfSet.add req reqs in
    let reqs = Array.fold_left (List.fold_left aux) NfSet.empty pl in
    let reqs = Array.of_list (NfSet.elements reqs) in

    (* Map subrequest -> idx in reqs *)
    let idx = ref NfMap.empty in
    Array.iteri (fun i req -> idx := NfMap.add req i !idx) reqs;
    let idx = !idx in

    (* Build dispatcher *)
    let reqs_facto =
      if facto then Array.map (Normal.factorize t) reqs
      else Array.map (fun r -> [],[],r) reqs in
    let reqs = Array.map (fun (_,_,req) -> req) reqs_facto in

    let disp = dispatcher t reqs in
    
    (* Build continuation *)
    let result (t,ar,m) =
      let get (req,info) a =
	let i = NfMap.find req idx in
	let (var,nil,_) = reqs_facto.(i) in
	match m.(i) with Some res -> ((var,nil,res),info)::a | _ -> a in
      let pl = Array.map (fun l -> List.fold_right get l []) pl in
      d t ar pl
    in
    let res = Array.map result disp.codes in
    post (disp,res)

  let make_branches t brs =
    let t0 = ref t in
    let aux (p,e) = 
      let xs = fv p in
      let tp = Types.descr (accept p) in
      let nnf = (Normal.NodeSet.singleton p, Types.cap !t0 tp, xs) in
      t0 := Types.diff !t0 tp;
      [(nnf, (xs, e))] in
    let res _ _ pl =
      let aux r = function 
	| [(([],[],res), (xs,e))] -> assert (r == Fail); 
	    let i = ref 0 in
	    List.iter (fun x -> assert (IdMap.assoc x res = !i); incr i) xs;
	    Match (List.length xs, e)
	| [] -> r | _ -> assert false in
      Array.fold_left aux Fail pl in
    let pl = Array.map aux (Array.of_list brs) in
    get_tests false pl (fun x -> x) t res (fun (disp,rhs) -> disp.state,rhs)


  let rec dispatch_prod0 disp t pl =
    get_tests true pl
      (fun (res,p,q) -> p, (res,q))
      (Types.Product.pi1 t)
      (dispatch_prod1 disp t)
      (fun x -> detect_left_tail_call (combine equal_result_dispatch x))
  and dispatch_prod1 disp t t1 ar1 pl =
    get_tests true pl
      (fun (ret1, (res,q)) -> q, (ret1,res) ) 
      (Types.Product.pi2_restricted t1 t)
      (dispatch_prod2 disp ar1)
      (fun x -> detect_right_tail_call (combine equal_result x))
  and dispatch_prod2 disp ar1 t2 ar2 pl =
    let aux_final (ret2, (ret1, res)) = merge_res_prod ar1 ar2 ret1 ret2 res in
    return disp pl aux_final (ar1 + ar2)

  let dispatch_prod disp pl =
    let t = Types.Product.get disp.t in
    dispatch_prod0 disp t 
      (Array.map (fun p -> Normal.NLineProd.elements p.Normal.nprod) pl) 
(*    dispatch_prod0 disp t (Array.map Normal.prod_tests disp.pl)  *)

  let dispatch_xml disp pl = 
    let t = Types.Product.get ~kind:`XML disp.t in
    dispatch_prod0 disp t 
      (Array.map (fun p -> Normal.NLineProd.elements p.Normal.nxml) pl)

  let dispatch_record disp pl : record option =
    let t = disp.t in
    if not (Types.Record.has_record t) then None 
    else
      match disp.label with
	| None -> 
	    let (some,none) = Types.Record.empty_cases t in
	    let some =
	      if some then 
		let pl = Array.map (fun p -> match p.Normal.nrecord with
				      | Normal.RecNolabel (Some x,_) -> [x]
				      | Normal.RecNolabel (None,_) -> []
				      | _ -> assert false) pl in
		Some (return disp pl (IdMap.map_to_list conv_source) 0)
	      else None
	    in
	    let none =
	      if none then 
		let pl = Array.map (fun p -> match p.Normal.nrecord with
				      | Normal.RecNolabel (_,Some x) -> [x]
				      | Normal.RecNolabel (_,None) -> []
				      | _ -> assert false) pl in
		Some (return disp pl (IdMap.map_to_list conv_source) 0)
	      else None
	    in	      
	    Some (RecNolabel (some,none))
	| Some lab ->
	    let t = Types.Record.split t lab in
	    let pl = Array.map (fun p -> match p.Normal.nrecord with
				  | Normal.RecLabel (_,l) -> 
				      Normal.NLineProd.elements l
				  | _ -> assert false) pl in
	    Some (RecLabel (lab,dispatch_prod0 disp t pl))

(*      
  let iter_disp_disp f g = function
    | Dispatch (d,a) -> f d; Array.iter g a
    | TailCall d -> f d
    | Ignore a -> g a
    | Impossible -> ()

  let iter_disp_prod f = iter_disp_disp f (iter_disp_disp f (fun _ -> ()))

  let rec iter_disp_actions f = function
    | AIgnore _ -> ()
    | AKind k ->
	iter_disp_prod f k.prod;
	iter_disp_prod f k.xml;
	(match k.record with Some (RecLabel (_,p)) -> iter_disp_prod f p 
	   | _ -> ())
*)
    
  let () =
    compute_actions := 
      (fun disp ->
	 let pl = Array.map (Normal.nnf disp.label disp.t) disp.pl in
	 let a = combine_kind
	   (dispatch_basic disp)
	   (dispatch_prod disp pl)
	   (dispatch_xml disp pl)
	   (dispatch_record disp pl)
	 in
	 disp.state.actions <- a)


  let find_array pred a = 
    let res = ref (-1) in 
    for i = 0 to Array.length a - 1 do
      if pred a.(i) then (assert (!res = (-1)); res := i)
    done;
    !res
      
  let new_fail_res fail =
    find_array (function (code,_,_) when code = fail -> true | _ -> false)
  let new_fail_disp fail =
    find_array (function Ignore (code,_,_) when code = fail -> true | _ -> false)

  let rec prepare_checker fail state =
    if (state.fail_code >= 0) then (
      assert(state.fail_code == fail);
    ) else (
      state.fail_code <- fail;
      let expect = ref Types.empty in
      Array.iteri
	(fun i (t,_,_) -> 
	   if i != fail then expect := Types.cup t !expect)
	(Hashtbl.find dispatcher_of_state state.uid).codes;
      state.expected_type <- Types.Print.to_string !expect;
      prepare_checker_actions fail state.actions
    )
  and prepare_checker_actions fail = function
    | AIgnore _ -> ()
    | AKind k ->
	prepare_checker_prod fail k.prod;
	prepare_checker_prod fail k.xml;
	match k.record with
	  | Some (RecLabel (_,d)) -> prepare_checker_prod fail d
	  | _ -> ()
  and prepare_checker_prod fail = function
    | Dispatch (state,cont) ->
	let f = new_fail_disp fail cont in
	if (f >= 0) then prepare_checker f state;
	Array.iter (prepare_checker_prod2 fail) cont
    | TailCall state -> prepare_checker fail state
    | Ignore d2 -> prepare_checker_prod2 fail d2
    | Impossible -> ()
  and prepare_checker_prod2 fail = function
    | Dispatch (state,cont) ->
	let f = new_fail_res fail cont in
	assert(f >= 0);
	prepare_checker f state
    | TailCall state -> prepare_checker fail state
    | Ignore _ -> ()
    | Impossible -> ()



  let make_checker t0 t =
    let p = make IdSet.empty in
    define p (constr t);
    let (d,rhs) = make_branches t0 [ (p,()) ] in
    let code = ref (-1) in
    Array.iteri 
      (fun (i : int) (rhs : unit rhs) -> 
	 match rhs with
	   | Fail -> assert (!code < 0); code := i | _ -> ()) rhs;
    if (!code >= 0) then prepare_checker !code d;
    d
end
