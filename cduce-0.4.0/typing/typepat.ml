(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Ident

type err = string -> exn
let deferr s = raise (Patterns.Error s)

  type node = {
    mutable desc: desc;
    mutable smallhash: int;  (* Local hash *)
    mutable rechash: int;    (* Global (recursive) hash *)
    mutable sid: int;        (* Sequential id used to compute rechash *)
    mutable t: Types.t option;
    mutable tnode: Types.Node.t option;
    mutable p: Patterns.descr option;
    mutable pnode: Patterns.node option;
    mutable fv: fv option
  } 
  and desc =
    | ILink of node
    | IType of Types.descr * int
    | IOr of node * node * err
    | IAnd of node * node * err
    | IDiff of node * node * err
    | ITimes of node * node
    | IXml of node * node
    | IArrow of node * node
    | IOptional of node * err
    | IRecord of bool * (node * node option) label_map * err
    | ICapture of id
    | IConstant of id * Types.const
    | IConcat of node * node * err
    | IMerge of node * node * err


  let concats = ref []

  let rec node_temp = { 
    desc = ILink node_temp;
    smallhash = 0; rechash = 0; sid = 0;
    t = None; tnode = None; p = None; pnode = None;
    fv = None
  }

  let mk d = { node_temp with desc = d }
  let mk_delayed () = { node_temp with desc = ILink node_temp }

  let mk_type t = mk (IType (t, Types.hash t))
  let mk_or ?(err=deferr) n1 n2 = mk (IOr (n1,n2,err))
  let mk_and ?(err=deferr) n1 n2 = mk (IAnd (n1,n2,err))
  let mk_diff ?(err=deferr) n1 n2 = mk (IDiff (n1,n2,err))
  let mk_prod n1 n2 = mk (ITimes (n1,n2))
  let mk_xml n1 n2 = mk (IXml (n1,n2))
  let mk_arrow n1 n2 = mk (IArrow (n1,n2))
  let mk_optional ?(err=deferr) n = mk (IOptional (n,err))
  let mk_record ?(err=deferr) n1 n2 = mk (IRecord (n1,n2,err))
  let mk_capture n = mk (ICapture n)
  let mk_constant n1 n2 = mk (IConstant (n1,n2))
  let mk_concat ?(err=deferr) n1 n2 =
    let n = mk (IConcat (n1,n2,err)) in concats := n :: !concats; n
  let mk_merge ?(err=deferr) n1 n2 = 
    let n = mk (IMerge (n1,n2,err)) in concats := n :: !concats; n

  let iempty = mk_type Types.empty

  let mk_or ?err p1 p2 =
    if p1.desc == iempty.desc then p2 
    else if p2.desc == iempty.desc then p1 
    else mk_or ?err p1 p2

  let mk_and ?err p1 p2 =
    if (p1.desc == iempty.desc) || (p2.desc == iempty.desc) then iempty 
    else mk_and ?err p1 p2
			
(* Recursive hash-consing *)

  let hash_field f = function
    | (p, Some e) -> 1 + 17 * f p + 257 * f e
    | (p, None) -> 2 + 17 * f p

  let rec hash f n = match n.desc with
    | ILink n -> hash f n
    | IType (t,h) -> 1 + 17 * h
    | IOr (p1,p2,_) -> 2 + 17 * f p1 + 257 * f p2
    | IAnd (p1,p2,_) -> 3 + 17 * f p1 + 257 * f p2
    | IDiff (p1,p2,_) -> 4 + 17 * f p1 + 257 * f p2
    | ITimes (p1,p2) -> 5 + 17 * f p1 + 257 * f p2
    | IXml (p1,p2) -> 6 + 17 * f p1 + 257 * f p2
    | IArrow (p1,p2) -> 7 + 17 * f p1 + 257 * f p2
    | IOptional (p,_) -> 8 + 17 * f p
    | IRecord (o,r,_)->9+(if o then 17 else 0)+
	257*(LabelMap.hash (hash_field f) r)
    | ICapture x -> 10 + 17 * (Id.hash x)
    | IConstant (x,c) -> 11 + 17 * (Id.hash x) + 257*(Types.Const.hash c)
    | IConcat _ | IMerge _ -> assert false

  let hash0 = hash (fun n -> 1)
  let hash1 = hash hash0
  let hash2 = hash hash1
  let hash3 = hash hash2

  let smallhash n =
    if n.smallhash !=0 then n.smallhash
    else (
      let h = hash2 n in 
      n.smallhash <- h; h
    )

  let rec repr = function
    | { desc = ILink n } as m -> let z = repr n in m.desc <- ILink z; z
    | n -> n

  let back = ref []

  let rec prot_repr = function
    | { desc = ILink n } -> repr n
    | n -> n

  let link x y = match x,y with
    | { t = None } as x, y 
    | y, ({ t = None } as x) -> back := (x,x.desc) :: !back; x.desc <- ILink y
    | _ -> assert false

  exception Unify

  let rec unify x y =
    if x == y then ()
    else let x = prot_repr x and y = prot_repr y in if x == y then ()
    else if (smallhash x != smallhash y) then raise Unify 
    else if (x.t != None) && (y.t != None) then raise Unify
      (* x and y have been internalized; if they were equivalent,
	 they would be equal *)
    else match x.desc,y.desc with
      | IType (tx,_), IType (ty,_) when Types.equal tx ty -> link x y
      | IOr (x1,x2,_), IOr (y1,y2,_)
      | IAnd (x1,x2,_), IAnd (y1,y2,_)
      | IDiff (x1,x2,_), IDiff (y1,y2,_)
      | ITimes (x1,x2), ITimes (y1,y2)
      | IXml (x1,x2), IXml (y1,y2)
      | IArrow (x1,x2), IArrow (y1,y2) -> link x y; unify x1 y1; unify x2 y2
      | IOptional (x1,_), IOptional (y1,_) -> link x y; unify x1 y1
      | IRecord (xo,xr,_), IRecord (yo,yr,_) when xo == yo ->
	  link x y; LabelMap.may_collide unify_field Unify xr yr
      | ICapture xv, ICapture yv when Id.equal xv yv -> ()
      | IConstant (xv,xc), IConstant (yv,yc) when
	  Id.equal xv yv && Types.Const.equal xc yc -> ()
      | _ -> raise Unify
  and unify_field f1 f2 = match f1,f2 with
    | (p1, Some e1), (p2, Some e2) -> unify p1 p2; unify e1 e2
    | (p1, None), (p2, None) -> unify p1 p2
    | _ -> raise Unify


  let may_unify x y =
    try unify x y; back := []; true
    with Unify ->
      List.iter (fun (x,xd) -> x.desc <- xd) !back; back := []; false

  module SmallHash = Hashtbl.Make(
    struct 
      type t = node
      let equal = may_unify
      let hash = smallhash
    end
  )

  let iter_field f = function
    | (x, Some y) -> f x; f y
    | (x, None) -> f x
  let iter f = function
    | IOr (x,y,_) | IAnd (x,y,_) | IDiff (x,y,_)
    | ITimes (x,y) | IXml (x,y) | IArrow (x,y) -> f x; f y
    | IOptional (x,_) -> f x
    | IRecord (_,r,_) -> LabelMap.iter (iter_field f) r
    | _ -> ()

  let minimize (mem,add) =
    let rec aux n =
      let n = repr n in
      if mem n then () else (
	let n = repr n in add n (); 
	if n.t == None then iter aux n.desc
      )
    in aux

  let to_clear = ref []
  let sid = ref 0
  let rec rechash n =
    let n = repr n in
    if (n.sid != 0) then 17 * n.sid
    else (incr sid; n.sid <- !sid; to_clear := n :: !to_clear; hash rechash n)

  let clear () =
    sid := 0; List.iter (fun x -> x.sid <- 0) !to_clear;
    to_clear := []

  let rechash n =
    let n = repr n in
    if (n.rechash != 0) then n.rechash 
    else (let h = rechash n in clear (); n.rechash <- h; h)

  module RecHash = Hashtbl.Make(
    struct
      type t = node
      let equal = may_unify
      let hash = smallhash
    end
  )


(** Two-phases recursive hash-consing **)
(*
  let gtable = RecHash.create 17577

  let internalize n =
    let local = SmallHash.create 17 in
    minimize (SmallHash.mem local, SmallHash.add local) n; 
    minimize (RecHash.mem gtable, RecHash.add gtable) n;
    ()
*)

(** Single-phase hash-consing **)
  let gtable = SmallHash.create 17

  let internalize n =
    minimize (SmallHash.mem gtable, SmallHash.add gtable) n



(*  let internalize n = () *)

(* Compute free variables *)

  let fv n =
    let fv = ref IdSet.empty in
    let rec aux n =
      let n = repr n in
      if (n.sid = 0) then (
	n.sid <- 1;
	to_clear := n :: !to_clear; 
	match n.fv, n.desc with
	  | Some x, _ -> fv := IdSet.cup !fv x
	  | None, (ICapture x | IConstant (x,_)) -> fv := IdSet.add x !fv
	  | None, d -> iter aux d
      )
    in
    assert(!to_clear == []);
    match n.fv with
      | Some x -> x
      | None -> aux n; clear (); n.fv <- Some !fv; !fv

(* optimized version to check closedness *)

  let no_fv = Some IdSet.empty
  exception FoundFv of id
  let peek_fv n =
    let err x = raise (FoundFv x) in
    let rec aux n =
      let n = repr n in
      if (n.sid = 0) then (
	n.sid <- 1;
	to_clear := n :: !to_clear; 
	match n.fv, n.desc with
	  | Some x, _ when IdSet.is_empty x -> ()
	  | Some x, _ -> err (IdSet.choose x)
	  | None, (ICapture x | IConstant (x,_)) -> err x;
	  | None, d -> iter aux d
      )
    in
    assert(!to_clear == []);
    try
      match n.fv with
	| Some x when IdSet.is_empty x -> ()
	| Some x -> err (IdSet.choose x)
	| None -> aux n; 
	    List.iter (fun n -> n.sid <- 0; n.fv <- no_fv) !to_clear;
	    to_clear := []
    with exn -> clear (); raise exn


  let has_no_fv n =
    try peek_fv n; true
    with FoundFv _ -> false

  let peek_fv n =
    try peek_fv n; None
    with FoundFv x -> Some x


(* From the intermediate representation to the internal one *)


  let rec typ n =
    let n = repr n in
    match n.t with
      | Some t -> t
      | None -> let t = compute_typ n.desc in n.t <- Some t; t
  and compute_typ = function
    | IType (t,_) -> t
    | IOr (s1,s2,_) -> Types.cup (typ s1) (typ s2)
    | IAnd (s1,s2,_) ->  Types.cap (typ s1) (typ s2)
    | IDiff (s1,s2,_) -> Types.diff (typ s1) (typ s2)
    | ITimes (s1,s2) -> Types.times (typ_node s1) (typ_node s2)
    | IXml (s1,s2) -> Types.xml (typ_node s1) (typ_node s2)
    | IArrow (s1,s2) -> Types.arrow (typ_node s1) (typ_node s2)
    | IOptional (s,_) -> Types.Record.or_absent (typ s)
    | IRecord (o,r,err) ->  
	Types.record_fields (o, LabelMap.map (compute_typ_field err) r)
    | ILink _ -> assert false
    | ICapture _ | IConstant (_,_) -> assert false
    | IConcat _ | IMerge _ -> assert false
  and compute_typ_field err = function
    | (s, None) -> typ_node s
    | (s, Some _) -> 
	raise (err "Or-else clauses are not allowed in types")

  and typ_node n =
    let n = repr n in
    match n.tnode with
      | Some t -> t
      | None ->
	  let x = Types.make () in
	  n.tnode <- Some x;
	  Types.define x (typ n);
	  x
      
  let rec pat n =
    let n = repr n in
    if has_no_fv n
    then Patterns.constr (typ n)
    else match n.p with
      | Some p -> p
      | None -> let p = compute_pat n.desc in n.p <- Some p; p

  and compute_pat = function
    | IOr (s1,s2,err) -> 
	(try Patterns.cup (pat s1) (pat s2)
	 with Patterns.Error s -> raise (err s))
    | IAnd (s1,s2,err) ->
	(try Patterns.cap (pat s1) (pat s2)
	 with Patterns.Error s -> raise (err s))
    | IDiff (s1,s2,_) when IdSet.is_empty (fv s2) ->
	let s2 = Types.neg (typ s2) in
	Patterns.cap (pat s1) (Patterns.constr s2)
    | IDiff (_,_,err) ->
	raise (err "Differences are not allowed in patterns")
    | ITimes (s1,s2) -> Patterns.times (pat_node s1) (pat_node s2)
    | IXml (s1,s2) -> Patterns.xml (pat_node s1) (pat_node s2)
    | IOptional (_,err) -> 
	raise (err "Optional fields are not allowed in record patterns")
    | IRecord (o,r,err) ->
	let pats = ref [] in
	let aux l = function
	  | (s,None) ->
	      if IdSet.is_empty (fv s) then typ_node s
	      else
		( pats := Patterns.record l (pat_node s) :: !pats;
		  Types.any_node )
	  | (s,Some e) ->
	      if IdSet.is_empty (fv s) then
		raise (err "Or-else clauses are not allowed in types")
	      else
		( pats := Patterns.cup 
		    (Patterns.record l (pat_node s))
		    (pat e) :: !pats;
		  Types.Record.any_or_absent_node )
	in
	let constr = Types.record_fields (o,LabelMap.mapi aux r) in
	List.fold_left Patterns.cap (Patterns.constr constr) !pats
	  (* TODO: can avoid constr when o=true, and all fields have fv *)
    | ICapture x -> Patterns.capture x
    | IConstant (x,c) -> Patterns.constant x c
    | IArrow _ ->
	raise (Patterns.Error "Arrows are not allowed in patterns")
    | IType _ | ILink _ | IConcat _ | IMerge _ -> assert false
      
  and pat_node n =
    let n = repr n in
    match n.pnode with
      | Some p -> p
      | None ->
	  let x = Patterns.make (fv n) in
	  try
	    n.pnode <- Some x;
	    Patterns.define x (pat n);
	    x
	  with exn -> n.pnode <- None; raise exn


  type regexp =
    | PElem of node
    | PGuard of node
    | PSeq of regexp list
    | PAlt of regexp list
    | PStar of regexp
    | PWeakStar of regexp

  let rec nullable = function
    | PElem _ -> false
    | PSeq rl -> List.for_all nullable rl
    | PAlt rl -> List.exists nullable rl
    | PStar _ | PWeakStar _ | PGuard _ -> true

  let eps = PSeq []
  let emp = PAlt []
  let star x = PStar x
  let elem x = PElem x

  let seq r1 r2 =
    let r1 = match r1 with PSeq l -> l | x -> [ x ] in
    let r2 = match r2 with PSeq l -> l | x -> [ x ] in
    match r1 @ r2 with
      | [ x ] -> x
      | l -> PSeq l

  let alt r1 r2 =
    let r1 = match r1 with PAlt l -> l | x -> [ x ] in
    let r2 = match r2 with PAlt l -> l | x -> [ x ] in
    match r1 @ r2 with
      | [ x ] -> x
      | l -> PAlt l

  let rec merge_alt = function
    | PElem p::PElem q::l -> merge_alt (PElem (mk_or p q) :: l)
    | r::l -> r::(merge_alt l)
    | [] -> []

(* Works only for types, not patterns, because
   [ (x&Int|_) R' ] is possible *)
  let rec simplify_regexp = function
    | PSeq l -> PSeq (List.map simplify_regexp l)
    | PAlt l -> PAlt (merge_alt (List.map simplify_regexp l))
    | PStar r | PWeakStar r -> PStar (simplify_regexp r)
    | x -> x

  let rec print_regexp ppf = function
    | PElem _ -> Format.fprintf ppf "Elem"
    | PGuard _ -> Format.fprintf ppf "Guard"
    | PSeq l -> Format.fprintf ppf "Seq(%a)" print_regexp_list l
    | PAlt l -> Format.fprintf ppf "Alt(%a)" print_regexp_list l
    | PStar r -> Format.fprintf ppf "Star(%a)" print_regexp r
    | PWeakStar r -> Format.fprintf ppf "WStar(%a)" print_regexp r
  and print_regexp_list ppf l =
    List.iter (fun x -> Format.fprintf ppf "%a;" print_regexp x) l

  let rec remove_regexp r q = 
    match r with
    | PElem p ->
	mk_prod p q
    | PGuard p ->
	mk_and p q
    | PSeq l ->
	List.fold_right (fun r a -> remove_regexp r a) l q
    | PAlt rl ->
	List.fold_left (fun a r -> mk_or a (remove_regexp r q)) iempty rl
    | PStar r ->
	let x = mk_delayed () in
	let res = mk_or x q in
	x.desc <- ILink (remove_regexp_nullable r res iempty);
	res
    | PWeakStar r ->
	let x = mk_delayed () in
	let res = mk_or q x in
	x.desc <- ILink (remove_regexp_nullable r res iempty);
	res

  and remove_regexp_nullable r q_nonempty q_empty =
    if nullable r then remove_regexp2 r q_nonempty q_empty
    else remove_regexp r q_nonempty

  and remove_regexp2 r q_nonempty q_empty =
    (* Assume r is nullable *)
    if q_nonempty == q_empty then remove_regexp r q_nonempty
    else match r with
      | PSeq [] ->
          q_empty
      | PElem p ->
	  assert false
      | PGuard p ->
	  mk_and p q_empty
      | PSeq (r::rl) ->
          remove_regexp2 r
            (remove_regexp (PSeq rl) q_nonempty)
            (remove_regexp2 (PSeq rl) q_nonempty q_empty)
      | PAlt rl ->
	  List.fold_left 
	    (fun a r -> mk_or a (remove_regexp_nullable r q_nonempty q_empty))
	    iempty rl
      | PStar r ->
 	  let x = mk_delayed () in
          x.desc <- ILink (remove_regexp_nullable r (mk_or x q_nonempty) iempty);
          mk_or x q_empty
      | PWeakStar r ->
 	  let x = mk_delayed () in
          x.desc <- ILink (remove_regexp_nullable r (mk_or q_nonempty x) iempty);
          mk_or q_empty x


  let pcdata = star (PElem (mk_type (Types.char Chars.any)))
  let mix_regexp regexp =
    let rec aux = function
      | PSeq [] -> eps
      | PElem re -> PElem re
      | PGuard re -> assert false
      | PSeq (r::rl) -> seq (aux r) (seq pcdata (aux (PSeq rl)))
      | PAlt rl -> PAlt (List.map aux rl)
      | PStar re -> star (seq pcdata (aux re))
      | PWeakStar re -> assert false
    in
    seq pcdata (seq (aux regexp) pcdata)

  let cst_nil = Types.Atom Sequence.nil_atom
  let capture_all vars p = 
    IdSet.fold (fun p x -> mk_and p (mk_capture x)) p vars
  let termin b vars p = 
    if b then p 
    else IdSet.fold 
      (fun p x -> seq p (PGuard (mk_constant x cst_nil))) p vars


  type re =
    | Epsilon | Empty | Elem of node | Guard of node 
    | Seq of re * re
    | Alt of re * re
    | Star of re
    | WeakStar of re
    | SeqCapture of id * re

  let mk_empty = Empty
  let mk_epsilon = Epsilon
  let mk_elem n = Elem n
  let mk_guard n = Guard n
  let mk_seq n1 n2 = Seq (n1,n2)
  let mk_alt n1 n2 = Alt (n1,n2)
  let mk_star n = Star n
  let mk_weakstar n = WeakStar n
  let mk_seqcapt x n = SeqCapture (x,n)

  let rec prepare_regexp vars b rvars f = function
      (* - vars: seq variables to be propagated top-down and added
	 to each captured element
	 - b: below a star ?
	 - rvars: seq variables that appear on the right of the regexp
	 - f: tail position
	 
	 returns the set of seq variable of the regexp minus rvars
	 (they have already been terminated if not below a star)
      *)
    | Epsilon -> 
	PSeq [], IdSet.empty
    | Empty ->
	PAlt [], IdSet.empty
    | Elem p -> 
	PElem (capture_all vars p), IdSet.empty
    | Guard p ->
	PGuard p, IdSet.empty
    | Seq (p1,p2) -> 
	let (p2,v2) = prepare_regexp vars b rvars f p2 in
	let (p1,v1) = prepare_regexp vars b (IdSet.cup rvars v2) false p1 in
	seq p1 p2, IdSet.cup v1 v2
    | Alt (p1,p2) -> 
	let (p1,v1) = prepare_regexp vars b rvars f p1
	and (p2,v2) = prepare_regexp vars b rvars f p2 in
	alt (termin b (IdSet.diff v2 v1) p1) (termin b (IdSet.diff v1 v2) p2),
	IdSet.cup v1 v2
    | Star p -> 
	let (p,v) = prepare_regexp vars true rvars false p in
	termin b v (PStar p), v
    | WeakStar p -> 
	let (p,v) = prepare_regexp vars true rvars false p in
	termin b v (PWeakStar p), v
    | SeqCapture (x,p) -> 
	let vars = if f then vars else IdSet.add x vars in
	let after = IdSet.mem rvars x in
	let rvars = IdSet.add x rvars in
	let (p,v) = prepare_regexp vars b rvars false p in
	(if f 
	 then seq (PGuard (mk (ICapture x))) p 
	 else termin (after || b) (IdSet.singleton x) p), 
	(if after then v else IdSet.add x v)

  let rexp r = 
    let r,_ = prepare_regexp IdSet.empty false IdSet.empty true r in
    remove_regexp r (mk_type Sequence.nil_type)

  let rexp_simplify ~mix r =
    let r,_ = prepare_regexp IdSet.empty false IdSet.empty true r in
    let r = if mix then mix_regexp r else r in
    let r = simplify_regexp r in
    remove_regexp r (mk_type Sequence.nil_type)

  let check_wf p =
    let rec aux q = if p == q then raise Exit; aux2 q.desc
    and aux2 = function
      | IOr (q1,q2,_) | IAnd (q1,q2,_) | IDiff (q1,q2,_) -> aux q1; aux q2
      | ILink q -> aux q
      | _ -> ()
    in
    try aux2 p.desc; true
    with Exit -> false


    
  module H = Hashtbl.Make(Types)

  let rec elim_concat n =
    match n.desc with
      | IConcat (a,b,err) ->
	  if (n.sid > 0) 
	  then 	raise (err "Ill-formed concatenation loop");
	  n.sid <- 1;
	  n.desc <- ILink (elim_conc a b err)
      | IMerge (a,b,err) ->
	  if (n.sid > 0) 
	  then raise (err "Ill-formed merge loop");
	  n.sid <- 1;
	  n.desc <- ILink (elim_merge a b err)
      | _ -> ()
  and elim_merge a b err =
    let get_rec t =
      let t = Types.Record.get t in
      List.map (fun (l,o,_) ->
		  o, 
		  LabelMap.map 
		    (fun (opt,x) ->
		       let x = mk_type x in 
		       (if opt then mk_optional x else x),
		       None)
		    l) t in
    let merge (o1,l1) (o2,l2) =
      mk_record  (o1||o2) (LabelMap.merge (fun _ x -> x) l1 l2) in
    (* Problem: repr can loop with ill-formed recursion.
       type t = s + t where s = s | s;; *)

    let a = repr a  and b = repr b in
    elim_concat a; elim_concat b;
    let a = repr a  and b = repr b in
    match a.desc, b.desc with
      | IType (t1,_), IType (t2,_) -> 
	  if not (Types.subtype t1 Types.Record.any) then
	    raise 
	      (err
		 "Left argument of record concatenation is not a record type");
	  if not (Types.subtype t2 Types.Record.any) then
	    raise 
	      (err
		 "Right argument of record concatenation is not a record type");
	  mk_type (Types.Record.merge t1 t2)
      | IOr (a1,a2,_), _ -> mk_or (elim_merge a1 b err) (elim_merge a2 b err)
      | _, IOr (b1,b2,_) -> mk_or (elim_merge a b1 err) (elim_merge a b2 err)
      | IRecord (o1,l1,_), IRecord (o2,l2,_) -> merge (o1,l1) (o2,l2)
      | IType (t1,_), IRecord (o2,l2,_) ->
	  if not (Types.subtype t1 Types.Record.any) then
	    raise 
	      (err
		 "Left argument of record concatenation is not a record type");
	  List.fold_left (fun accu (o1,l1) -> 
			    mk_or accu (merge (o1,l1) (o2,l2)))
	    iempty (get_rec t1)
      | IRecord (o1,l1,_), IType (t2,_) ->
	  if not (Types.subtype t2 Types.Record.any) then
	    raise 
	      (err
		 "Right argument of record concatenation is not a record type");
	  List.fold_left (fun accu (o2,l2) -> 
			    mk_or accu (merge (o1,l1) (o2,l2)))
	    iempty (get_rec t2)
      | _ -> raise (err "Cannot compute record concatenation")
  and elim_conc n q err =
    let mem = ref [] in
    let rec aux n =
      try List.assq n !mem
      with Not_found ->
	let r = mk_delayed () in
	mem := (n,r) :: !mem;
	let rec aux2 n =
	  match n.desc with
	    | ILink n' -> aux2 n'
	    | IOr (a,b,_) -> mk_or (aux a) (aux b)
	    | ITimes (a,b) -> mk_prod a (aux b)
	    | IConcat (a,b,_) -> elim_concat n; aux2 n
	    | IType (t,_) -> elim_concat_type t q err
	    | _ -> raise (err "Cannot compute concatenation")
	in
	r.desc <- ILink (aux2 n);
	r
    in
    aux n
  and elim_concat_type t q err =
    if not (Types.subtype t Sequence.any) then
      raise (err "Left argument of concatenation is not a sequence type");
    let mem = H.create 17 in
    let rec aux t =
      try H.find mem t 
      with Not_found ->
	let n = mk_delayed () in
	H.add mem t n;
	let d = 
	  List.fold_left
	    (fun accu (t1,t2) -> mk_or accu (mk_prod (mk_type t1) (aux t2)))
	    (if Types.Atom.has_atom t Sequence.nil_atom then q else iempty)
	    (Types.Product.get t) in
	n.desc <- d.desc;
	n
    in
    aux t
    
   
    
  let elim_concats () =
    try
      List.iter elim_concat !concats;
      List.iter (fun n -> n.sid <- 0) !concats;
      concats := []
    with exn ->
      List.iter (fun n -> n.sid <- 0) !concats;
      concats := [];
      raise exn

  let link a b = a.desc <- ILink b


  let get_ct c =
    let c = repr c in
    match c.desc with
      | ITimes (k,content) ->
	  (match (repr k).desc  with
	     | IType (t,_) -> (t,content)
	     | _ -> assert false)
      | _ -> assert false
