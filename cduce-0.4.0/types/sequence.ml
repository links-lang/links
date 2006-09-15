(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

type error = 
  | CopyTag of Types.t * Types.t 
  | CopyAttr of Types.t * Types.t
  | UnderTag of Types.t * exn
exception Error of error


let nil_atom = Atoms.V.mk_ascii "nil"
let nil_type = Types.atom (Atoms.atom nil_atom)
let nil_node = Types.cons nil_type
let nil_cst  = Types.Atom nil_atom

let decompose t =
  (Types.Atom.has_atom t nil_atom,
   Types.Product.get t)

module V = Types.Positive
module H = Map.Make(Types)
module H2 = Map.Make(Custom.Pair(Types)(Types))

let mapping f t queue =
  let memo = ref H.empty in
  let rec aux t =
    try H.find t !memo
    with Not_found ->
      let v = V.forward () in
      memo := H.add t v !memo;
      let (has_nil,rect) = decompose t in
      let l = List.map (fun (t1,t2) -> f t1 (aux t2)) rect in
      let l = if has_nil then queue :: l else l in
      V.define v (V.cup l);
      v
  in
  aux t
  

let aux_concat = mapping (fun t v -> V.times (V.ty t) v)
let aux_flatten t = mapping aux_concat t (V.ty nil_type)
let aux_map f t = mapping (fun t v -> V.times (V.ty (f t)) v) t (V.ty nil_type)

let solve x = Types.descr (V.solve x)

let concat t1 t2 = solve (aux_concat t1 (V.ty t2))
let flatten t = solve (aux_flatten t)
let map f t = solve (aux_map f t)


let map_mono t =
  let ts = ref [] in
  let vs = ref [] in
  let r =
    mapping (fun t v ->
	       let v' = V.forward () in
	       ts := t :: !ts; vs := v' :: !vs;
	       V.times v' v) t (V.ty nil_type) in
  !ts, (fun fts ->
	  List.iter2 (fun t v -> V.define v (V.ty t)) fts !vs;
	  solve r)


let recurs f =
  let n = Types.make () in
  Types.define n (f n);
  Types.internalize n

let star_node t = recurs (fun n -> Types.cup nil_type (Types.times t n ))

let any_node = star_node (Types.cons Types.any)
let any = Types.descr any_node
let seqseq = Types.descr (star_node any_node)

let star t = Types.descr (star_node (Types.cons t))
let plus t = let t = Types.cons t in Types.times t (star_node t)
let string = star (Types.Char.any)  

let option t =
  Types.cup (Types.times t nil_node) nil_type

let repet min max t =
  let t = Types.cons t in
  let tail = match max with
    | None -> star_node t
    | Some m -> 
	let rec aux accu l = function
	  | 0 -> Types.cons accu
	  | m -> 
	      let l = Types.times t (Types.cons l) in
	      aux (Types.cup accu l) l (pred m)
	in 
	aux Types.empty nil_type m
  in
  let rec aux = function
    | 0 -> tail
    | n -> Types.cons (Types.times t (aux (pred n)))
  in
  Types.descr (aux min)

(* Mmmmh, it may be faster to add pi1(rect) and iterate over pi2(rect) ... ? *)
let approx t =
  let memo = ref H.empty in
  let res = ref Types.empty in
  let rec aux t =
    try H.find t !memo
    with Not_found ->
      memo := H.add t () !memo;
      let rect = Types.Product.get t in
      List.iter (fun (t1,t2) -> res := Types.cup t1 !res; aux t2) rect;
  in
  aux t;
  !res


let precise = ref true
  
(* Note: the precision of this implementation depends on the constraint...
   This is bad and should be fixed. *)
let map_tree cstr f seq =
  let memo = ref H2.empty in
  let rec aux cstr t =
    let x = (cstr,t) in
    try H2.find x !memo
    with Not_found ->
      let v = V.forward () in
      memo := H2.add x v !memo;
      let v' = mapping (descr_aux cstr) t (V.ty nil_type) in
      V.define v v';
      v
  and descr_aux cstr t v =
    let cstr0 = 
      if !precise then Types.Product.normal ~kind:`XML cstr
      else [] in
    let cstr_tag = 
      if !precise then Types.Product.pi1 cstr0
      else Types.any in

    let (result,residual) = f (star cstr) t in
    let f2 cstr_attr cstr1 (attr,child) = 
      let cstr_sub = 
	if !precise then (
	  if not (Types.subtype attr cstr_attr) then
	    raise (Error (CopyAttr (attr,cstr_attr)));
	  approx (Types.Product.constraint_on_2 cstr1 attr))
	else Types.any in
      V.times (V.ty attr) (aux cstr_sub child) in
    let f1 (tag,x) =
      let cstr1 =
	if !precise then (
	  if not (Types.subtype tag cstr_tag) then
	    raise (Error (CopyTag (tag,cstr_tag)));
	  Types.Product.normal 
	    (Types.Product.constraint_on_2 cstr0 tag))
	else [] in
      let cstr_attr = 
	if !precise then Types.Product.pi1 cstr1
	else Types.any in
      try
	let x = V.cup (List.map (f2 cstr_attr cstr1) (Types.Product.get x)) in
	V.xml (V.ty tag) x
      with exn ->
	raise (Error (UnderTag (tag,exn)))
    in
    let iter = List.map f1 (Types.Product.get ~kind:`XML residual) in
    let resid = Types.Product.other ~kind:`XML residual in
    let iter = if Types.is_empty resid then iter else V.ty resid :: iter in
    let result = aux_concat result v in
    if iter = [] then result else
      V.cup [V.times (V.cup iter) v; result ]
  in
  let cstr = if !precise then approx cstr else Types.any in
  Types.descr (V.solve (aux cstr seq))

let map_tree_mono domain seq =
  let inp = ref Types.empty in
  let ts = ref [] in
  let vs = ref [] in

  let memo = ref H.empty in
  let rec aux t =
    try H.find t !memo
    with Not_found ->
      let v = V.forward () in
      memo := H.add t v !memo;
      let v' = mapping descr_aux t (V.ty nil_type) in
      V.define v v';
      v
  and descr_aux t v =
    inp := Types.cup !inp t;
    let residual = Types.diff t domain in

    let f2 (attr,child) = V.times (V.ty attr) (aux child) in
    let f1 (tag,x) =
      let x = V.cup (List.map f2 (Types.Product.get x)) in
      V.xml (V.ty tag) x in
    let iter = List.map f1 (Types.Product.get ~kind:`XML residual) in
    let resid = Types.Product.other ~kind:`XML residual in
    let iter = if Types.is_empty resid then iter else V.ty resid :: iter in

    let result = V.forward () in
    ts := (Types.cap domain t) :: !ts; vs := (result,v) :: !vs;
    if iter = [] then result else
    V.cup [V.times (V.cup iter) v; result ]

  in
  let r = aux seq in
  !inp, !ts, (fun fts ->
	  List.iter2 (fun t (result,v) -> V.define result (aux_concat t v))
	    fts !vs;
	  solve r)

(* TODO: avoid flushing the memo between calls to mapping inside map_tree *)

let seq_of_list l =
  let times' t acc = Types.times (Types.cons t) (Types.cons acc) in
  List.fold_right times' l nil_type


let char_latin1 = Types.char (Chars.mk_classes [ (0,255) ])
let string_latin1 = star char_latin1


let any_xtransformable =
  Types.descr
    (recurs 
       (fun n -> 
	  let elt = 
	    Types.xml 
	      Types.any_node
	      (Types.cons (Types.times Types.any_node n))
	  in
	  let non_elt = Types.neg (Types.xml Types.any_node Types.any_node)
	  in
	  let t = Types.cons (Types.cup elt non_elt) in
	  Types.cup nil_type (Types.times t n)))

