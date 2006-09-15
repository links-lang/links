(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Ident
open Encodings

type t =
  | Pair of t * t
  | Xml of t * t * t
  | XmlNs of t * t * t * Ns.table
  | Record of t Imap.t
  | Atom of Atoms.V.t
  | Integer of Intervals.V.t
  | Char of Chars.V.t
  | Abstraction of (Types.descr * Types.descr) list option * (t -> t)
  | Abstract of Types.Abstract.V.t
  | String_latin1 of int * int * string * t
  | String_utf8 of Utf8.uindex * Utf8.uindex * Utf8.t * t
  | Concat of t * t
  | Absent

(* 
  The only representation of the empty sequence is nil.
  In particular, in String_latin1 and String_utf8, the string cannot be empty.
*)

let dump_forward = ref (fun _ _ -> assert false)

exception CDuceExn of t

let nil = Atom Sequence.nil_atom
let string_latin1 s = 
  if String.length s = 0 then nil
  else String_latin1 (0,String.length s, s, nil)
let string_utf8 s = 
  if String.length (Utf8.get_str s) = 0 then nil
  else String_utf8 (Utf8.start_index s,Utf8.end_index s, s, nil)
let substring_utf8 i j s q =
  if Utf8.equal_index i j then q 
  else String_utf8 (i,j,s,q)
let vtrue = Atom (Atoms.V.mk_ascii "true")
let vfalse = Atom (Atoms.V.mk_ascii "false")
let vbool x = if x then vtrue else vfalse

let vrecord l =
  let l = List.map (fun (lab,v) -> Upool.int lab, v) l in
  Record (Imap.create (Array.of_list l))

let get_fields = function
  | Record map -> Obj.magic (Imap.elements map)
  | _ -> raise (Invalid_argument "Value.get_fields")

let rec sequence = function
  | [] -> nil
  | h::t -> Pair (h, sequence t)

let rec sequence_rev accu = function
  | [] -> accu
  | h::t -> sequence_rev (Pair (h,accu)) t

let sequence_rev l = sequence_rev nil l

let sequence_of_array a =
  let rec aux accu i =
    if (i = 0) then accu
    else let i = pred i in aux (Pair (a.(i), accu)) i in
  aux nil (Array.length a)

let tuple_of_array a =
  let rec aux accu i =
    if (i = 0) then accu
    else let i = pred i in aux (Pair (a.(i), accu)) i in
  let n = Array.length a in
  aux a.(n) (pred n)

let concat v1 v2 = 
  match (v1,v2) with
    | (Atom _, v) | (v, Atom _) -> v
    | (v1,v2) -> Concat (v1,v2)

let append v1 v2 =
  concat v1 (Pair (v2,nil))

let raise' v = raise (CDuceExn v)
let failwith' s = raise' (string_latin1 s)


let rec const = function
  | Types.Integer i -> Integer i
  | Types.Atom a -> Atom a
  | Types.Char c -> Char c
  | Types.Pair (x,y) -> Pair (const x, const y)
  | Types.Xml (x, Types.Pair (y, z)) -> Xml (const x, const y, const z)
  | Types.Xml (_,_) -> assert false
  | Types.Record x -> 
      let x = LabelMap.mapi_to_list (fun l c -> (Upool.int l,const c)) x in
      Record (Imap.create (Array.of_list x))
  | Types.String (i,j,s,c) -> String_utf8 (i,j,s, const c)

let rec inv_const = function
  | Pair (x, y) -> Types.Pair (inv_const x, inv_const y)
  | Xml (x, y, z) | XmlNs (x,y,z,_) ->
      Types.Pair (inv_const x, Types.Pair (inv_const y, inv_const z))
  | Record x -> 
      let x = Imap.elements x in
      let x = List.map (fun (l,c) -> (Label.from_int l,inv_const c)) x in
      Types.Record (LabelMap.from_list_disj x)
  | Atom a -> Types.Atom a
  | Integer i -> Types.Integer i
  | Char c -> Types.Char c
  | String_latin1 (_, _, s, v) ->
      let s = Utf8.mk s in
      Types.String (Utf8.start_index s, Utf8.end_index s, s, inv_const v)
  | String_utf8 (i, j, s, v) -> Types.String (i, j, s, inv_const v)
  | Concat (x, y) as v ->
      let rec children = function
        | Concat (x, y) -> children x @ children y
        | x -> [x]
      in
      inv_const (sequence (children v))
  | _ -> failwith "inv_const"

let normalize_string_latin1 i j s q = 
  if i = j then q else
    Pair (Char (Chars.V.mk_char (String.unsafe_get s i)), String_latin1 (succ i,j,s,q))

let normalize_string_utf8 i j s q = 
  if Utf8.equal_index i j then q 
  else
    let (c,i) = Utf8.next s i in
    Pair (Char (Chars.V.mk_int c), String_utf8 (i,j,s,q))



(***** The dirty things **********)

type pair = { dummy : t; mutable pair_tl : t }
type str  = { dummy1 : t; dummy2 : t; dummy3 : t; mutable str_tl : t }

(* Could optimize this function by changing the order of the fields
   in String_latin1, String_utf8 *)

let set_cdr cell tl =
  match cell with
    | Pair (_,_) -> (Obj.magic cell).pair_tl <- tl
    | String_latin1 (_,_,_,_) 
    | String_utf8(_,_,_,_)-> (Obj.magic cell).str_tl <- tl
    | _ -> assert false

let rec append_cdr cell tl =
  match tl with
    | Concat (x,y) ->
	append_cdr (append_cdr cell x) y
    | Pair (x,tl) -> 
	let cell' = Pair (x,Absent) in
	set_cdr cell cell';
	append_cdr cell' tl
    | String_latin1 (s,i,j,tl) ->
	let cell' = String_latin1 (s,i,j,Absent) in
	set_cdr cell cell';
	append_cdr cell' tl
    | String_utf8 (s,i,j,tl) ->
	let cell' = String_utf8 (s,i,j,Absent) in
	set_cdr cell cell';
	append_cdr cell' tl
    | _ -> cell


let rec flatten = function
  | Pair (x,y) -> concat x (flatten y)
  | Concat (x,y) -> concat (flatten x) (flatten y)
  | q -> q

let eval_lazy_concat v =
  let accu = Obj.magic (Pair (nil,Absent)) in
  let rec aux accu = function
    | Concat (x,y) -> aux (append_cdr accu x) y
    | v -> set_cdr accu v
  in
  aux accu v;
  let nv = match snd accu with 
    | Pair (_,_) as nv -> nv
    | String_latin1 (i,j,s,q) -> normalize_string_latin1 i j s q
    | String_utf8 (i,j,s,q) -> normalize_string_utf8 i j s q
    | _ -> assert false in
  let v = Obj.repr v in
  let nv = Obj.repr nv in
  Obj.set_tag v (Obj.tag nv);
  Obj.set_field v 0 (Obj.field nv 0);
  Obj.set_field v 1 (Obj.field nv 1)
	  
  

(******************************)

let normalize = function
  | String_latin1 (i,j,s,q) -> normalize_string_latin1 i j s q
  | String_utf8 (i,j,s,q) -> normalize_string_utf8 i j s q
  | Concat (_,_) as v -> eval_lazy_concat v; v
  | v -> v




let buf = Buffer.create 100

let rec add_buf_utf8_to_latin1 src i j =
  if Utf8.equal_index i j  then ()
  else
    let (c,i) = Utf8.next src i in
    if (c > 255) then failwith' "get_string_latin1";
    Buffer.add_char buf (Char.chr c);
    add_buf_utf8_to_latin1 src i j

let rec add_buf_latin1_to_utf8 src i j =
  for k = i to j - 1 do 
    Utf8.store buf (Char.code src.[k]) 
  done

let get_string_latin1 e =
  let rec aux = function
    | Pair (Char x,y) -> Buffer.add_char buf (Chars.V.to_char x); aux y
    | String_latin1 (i,j,src,y) -> Buffer.add_substring buf src i (j - i); aux y
    | String_utf8 (i,j,src,y) -> add_buf_utf8_to_latin1 src i j; aux y
    | Concat (_,_) as v -> eval_lazy_concat v; aux v
    | _ -> () in
  Buffer.clear buf;
  aux e;
  let s = Buffer.contents buf in
  Buffer.clear buf;
  s

let get_string_utf8 e =
  let rec aux = function
    | Pair (Char x,y) -> Utf8.store buf (Chars.V.to_int x); aux y
    | String_latin1 (i,j,src,y) -> add_buf_latin1_to_utf8 src i j; aux y
    | String_utf8 (i,j,src,y) -> Utf8.copy buf src i j; aux y
    | Concat (_,_) as v -> eval_lazy_concat v; aux v
    | q -> q in
  let q = aux e in
  let s = Buffer.contents buf in
  Buffer.clear buf;
  (Utf8.mk s, q)

let get_int = function
  | Integer i when Intervals.V.is_int i -> Intervals.V.get_int i
  | _ -> raise (Invalid_argument "Value.get_int")

let get_integer = function
  | Integer i -> i
  | _ -> assert false

let rec is_seq = function
  | Pair (_, y) when is_seq y -> true
  | Atom a when a = Sequence.nil_atom -> true
  | String_latin1 (_,_,_,y) | String_utf8 (_,_,_,y) when is_seq y  -> true
  | Concat (_,_) as v -> eval_lazy_concat v; is_seq v
  | _ -> false

let rec is_str = function
  | Pair (Char _, y) -> is_str y
  | Atom a when a = Sequence.nil_atom -> true
  | String_latin1 (_,_,_,q) | String_utf8(_,_,_,q) -> is_str q
  | Concat (_,_) as v -> eval_lazy_concat v; is_str v
  | _ -> false

let rec print ppf v =
  if is_str v then 
    (Format.fprintf ppf "\"";
     print_quoted_str ppf v;
     Format.fprintf ppf "\"")
  else if is_seq v then Format.fprintf ppf "[ @[<hv>%a@]]" print_seq v
  else  match v with
    | Pair (x,y) -> Format.fprintf ppf "(%a,%a)" print x print y
    | Xml (x,y,z) | XmlNs (x,y,z,_) -> print_xml ppf x y z
    | Record l -> Format.fprintf ppf "{%a }" print_record (Imap.elements l)
    | Atom a ->	Atoms.V.print_quote ppf a
    | Integer i -> Intervals.V.print ppf i
    | Char c ->	Chars.V.print ppf c
    | Abstraction _ -> Format.fprintf ppf "<fun>"
    | String_latin1 (i,j,s,q) -> 
	Format.fprintf ppf "<string_latin1:%i-%i,%S,%a>" i j s print q
    | String_utf8 (i,j,s,q) -> 
	Format.fprintf ppf "<string_utf8:%i-%i,%S,%a>" 
	(Utf8.get_idx i) (Utf8.get_idx j) (Utf8.get_str s) print q
    | Concat (x,y) ->
	Format.fprintf ppf "<concat:%a;%a>" print x print y
    | Abstract ("float",o) ->
	Format.fprintf ppf "%f" (Obj.magic o : float)
    | Abstract (s,_) ->
	Format.fprintf ppf "<abstract=%s>" s
    | Absent -> 	
	Format.fprintf ppf "<[absent]>"
and print_quoted_str ppf = function
  | Pair (Char c, q) -> 
      Chars.V.print_in_string ppf c; 
      print_quoted_str ppf q
  | String_latin1 (i,j,s, q) ->
      for k = i to j - 1 do
	Chars.V.print_in_string ppf (Chars.V.mk_char s.[k])
      done;
      print_quoted_str ppf q
  | String_utf8 (i,j,s, q) ->
(*      Format.fprintf ppf "UTF8:{"; *)
      let rec aux i =
	if Utf8.equal_index i j then q
	else 
	  let (c,i) =Utf8.next s i in
	  Chars.V.print_in_string ppf (Chars.V.mk_int c);
	  aux i
      in
      let q = aux i in
(*      Format.fprintf ppf "}"; *)
      print_quoted_str ppf q
  | q -> q
and print_seq ppf = function
  | (Pair(Char _, _)|String_latin1 (_,_,_,_)|String_utf8 (_,_,_,_)) as s ->
      Format.fprintf ppf "'";
      let q = print_quoted_str ppf s in
      Format.fprintf ppf "'@ ";
      print_seq ppf q
  | Pair (x,y) -> 
      Format.fprintf ppf "@[%a@]@ " print x;
      print_seq ppf y
  | _ -> ()

and print_xml ppf tag attr content =
  if is_seq content then
    Format.fprintf ppf "@[<hv2><%a%a>[@ %a@]]"
      print_tag tag
      print_attr attr
      print_seq content
  else
    Format.fprintf ppf "@[<hv2><%a%a>@ %a@]"
      print_tag tag
      print_attr attr
      print content
and print_tag ppf = function
  | Atom tag -> Atoms.V.print ppf tag
  | tag -> Format.fprintf ppf "(%a)" print tag
and print_attr ppf = function
  | Record attr -> print_record ppf (Imap.elements attr)
  | attr -> Format.fprintf ppf "(%a)" print attr

and print_record ppf = function
  | [] -> ()
  | f :: rem -> Format.fprintf ppf " %a" print_field f; print_record ppf rem

and print_field ppf (l,v) = 
  Format.fprintf ppf "%a=%a" Label.print_attr (Label.from_int l) print v  

let dump_xml ppf v =
  let rec aux ppf = function
    | Pair (x, y) ->
        Format.fprintf ppf "@[<hv1>";
        Format.fprintf ppf "<pair>@,%a@,%a@,</pair>@]" aux x aux y
    | Xml (x, y, z) | XmlNs (x,y,z,_) ->
        Format.fprintf ppf "@[<hv1>";
        Format.fprintf ppf "<xml>@,%a@,%a@,%a@,</xml>@]" aux x aux y aux z
    | Record x ->
        Format.fprintf ppf "@[<hv1>";
        Format.fprintf ppf "<record>@,%a@,</record>@]"
          (fun ppf x -> print_record ppf (Imap.elements x)) x
    | Atom a ->
        Format.fprintf ppf "@[<hv1>";
        Format.fprintf ppf "<atom>@,%a@,</atom>@]"
          (fun ppf x -> Atoms.V.print ppf x) a
    | Integer i ->
        Format.fprintf ppf "@[<hv1>";
        Format.fprintf ppf "<integer>@,%a@,</integer>@]"
          (fun ppf x -> Intervals.V.print ppf x) i
    | Char c ->
        Format.fprintf ppf "@[<hv1>";
        Format.fprintf ppf "<char>@,%a@,</char>@]"
          (fun ppf x -> Chars.V.print ppf x) c
    | Abstraction _ ->
        Format.fprintf ppf "@[<hv1>";
        Format.fprintf ppf "<abstraction />@]"
    | Abstract (s,_) ->
	Format.fprintf ppf "<abstract>%s</abstract>" s
    | String_latin1 (_, _, s, v) ->
        Format.fprintf ppf "@[<hv1>";
        Format.fprintf ppf "<string_latin1>@,%s@,</string_latin1>@," s;
        Format.fprintf ppf "@[<hv1>";
        Format.fprintf ppf "<follow>@,%a@,</follow>@]</string_latin1>@]" aux v
    | String_utf8 (_, _, s, v) ->
        Format.fprintf ppf "@[<hv1>";
        Format.fprintf ppf "<string_utf8>@,%s@,</string_utf8>@,"
          (Utf8.get_str s);
        Format.fprintf ppf "@[<hv1>";
        Format.fprintf ppf "<follow>@,%a@,</follow>@]</string_utf8>@]" aux v
    | Concat (x, y) ->
        Format.fprintf ppf "@[<hv1>";
        Format.fprintf ppf "<concat>@,%a@,%a@,</concat>@]" aux x aux y
    | Absent ->
        Format.fprintf ppf "@[<hv1>";
        Format.fprintf ppf "<absent />@]"
  in
  Format.fprintf ppf "@[<hv1>";
  Format.fprintf ppf "<value>@,%a@,</value>@]" aux v

let rec compare x y =
  if (x == y) then 0
  else
    match (x,y) with
      | Pair (x1,x2), Pair (y1,y2) ->
	  let c = compare x1 y1 in if c <> 0 then c 
	  else compare x2 y2	    
      | (Xml (x1,x2,x3) | XmlNs (x1,x2,x3,_)), 
	  (Xml (y1,y2,y3) | XmlNs(y1,y2,y3,_)) ->
	  let c = compare x1 y1 in if c <> 0 then c 
	  else let c = compare x2 y2 in if c <> 0 then c 
	  else compare x3 y3
      | Record rx, Record ry -> Imap.compare compare rx ry
      | Atom x, Atom y -> Atoms.V.compare x y
      | Integer x, Integer y -> Intervals.V.compare x y
      | Char x, Char y -> Chars.V.compare x y
      | Abstraction (_,_), _
      | _, Abstraction (_,_) ->
	  raise (CDuceExn (string_latin1 "comparing functional values"))
      | Abstract (s1,v1), Abstract (s2,v2) ->
	  let c = Types.Abstract.T.compare s1 s2 in if c <> 0 then c 
	  else Pervasives.compare (Obj.magic v1 : float) (Obj.magic v2 : float)
	    (* raise (CDuceExn (string_latin1 "comparing abstract values")) *)
      | Absent,_ | _,Absent ->
	  Format.fprintf Format.std_formatter
	    "ERR: Compare %a %a@." print x print y;
	  assert false
      | Concat (_,_) as x, y -> eval_lazy_concat x; compare x y
      | x, (Concat (_,_) as y) -> eval_lazy_concat y; compare x y
      | String_latin1 (ix,jx,sx,qx), String_latin1 (iy,jy,sy,qy) ->
	  if (sx == sy) && (ix = iy) && (jx = jy) then compare qx qy
	  else
	    (* Note: we would like to compare first jx-ix and jy-iy,
	       but this is not compatible with the equivalence of values *)
	    let rec aux ix iy =
	      if (ix = jx) then
		if (iy = jy) then compare qx qy
		else compare qx (normalize_string_latin1 iy jy sy qy)
	      else
		if (iy = jy) then compare (normalize_string_latin1 ix jx sx qx) qy
		else
		  let c1 = String.unsafe_get sx ix
		  and c2 = String.unsafe_get sy iy in
		  if c1 < c2 then -1 else
		    if c1 > c2 then 1 else aux (ix + 1) (iy + 1)
	    in
	    aux ix iy
      | String_utf8 (ix,jx,sx,qx), String_utf8 (iy,jy,sy,qy) ->
	  if (sx == sy) && (Utf8.equal_index ix iy) && (Utf8.equal_index jx jy) then compare qx qy
	  else
	    let rec aux ix iy =
	      if (Utf8.equal_index ix jx) then
		if (Utf8.equal_index iy jy) then compare qx qy
		else compare qx (normalize_string_utf8 iy jy sy qy)
	      else
		if (Utf8.equal_index iy jy) then compare (normalize_string_utf8 ix jx sx qx) qy
		else
		  let (c1,ix) = Utf8.next sx ix in
		  let (c2,iy) = Utf8.next sy iy in
		  if c1 < c2 then -1 else
		    if c1 > c2 then 1 else aux ix iy
	    in
	    aux ix iy
      | String_latin1 (i,j,s,q), _ -> compare (normalize_string_latin1 i j s q) y
      | _, String_latin1 (i,j,s,q) -> compare x (normalize_string_latin1 i j s q)
      | String_utf8 (i,j,s,q), _   -> compare (normalize_string_utf8 i j s q) y
      | _, String_utf8 (i,j,s,q)   -> compare x (normalize_string_utf8 i j s q)

      | Pair (_,_), _ -> -1 | _, Pair(_,_) -> 1
      | (Xml (_,_,_) | XmlNs (_,_,_,_)),_ -> -1 
      | _, (Xml(_,_,_) | XmlNs(_,_,_,_)) -> 1
      | Record _,_ -> -1 | _, Record _ -> 1
      | Atom _,_ -> -1 | _, Atom _ -> 1
      | Integer _,_ -> -1 | _, Integer _ -> 1
      | Abstract _, _ -> -1 | _, Abstract _ -> 1

let rec hash = function
  | Pair (x1,x2) ->
      1 + hash x1 * 257 + hash x2 * 17 
  | (Xml (x1,x2,x3) | XmlNs (x1,x2,x3,_)) ->
      2 + hash x1 * 65537 + hash x2 * 257 + hash x3 * 17
  | Record rx -> 
      3 + 17 * Imap.hash hash rx
  | Atom x -> 
      4 + 17 * Atoms.V.hash x
  | Integer x -> 
      5 + 17 * Intervals.V.hash x
  | Char x -> 
      6 + 17 * Chars.V.hash x
  | Abstraction _ -> 7
  | Abstract _ -> 8
  | Absent -> assert false
  | Concat _ as x -> eval_lazy_concat x; hash x
  | String_latin1 (i,j,s,q) -> hash (normalize_string_latin1 i j s q)
  | String_utf8 (i,j,s,q) ->hash (normalize_string_utf8 i j s q)

let iter_xml pcdata_callback other_callback =
  let rec aux = function
    | v when compare v nil = 0 -> ()
    | Pair (Char c, tl) ->
        pcdata_callback (U.mk_char (Chars.V.to_int c));
        aux tl
    | String_latin1 (i,j,s,tl) ->
        pcdata_callback (U.mk_latin1 (String.sub s i j));
        aux tl
    | String_utf8 (i,j,s,tl) ->
        pcdata_callback (U.mk (U.get_substr s i j));
        aux tl
    | Pair (hd, tl) ->
        other_callback hd;
        aux tl
    | Concat (_,_) as v -> eval_lazy_concat v; aux v
    | v -> raise (Invalid_argument "Value.iter_xml")
  in
  function
    | Xml (_,_,cont) | XmlNs (_,_,cont,_) -> aux cont
    | _ -> raise (Invalid_argument "Value.iter_xml")

(*
let map_xml map_pcdata map_other =
  let patch_string_utf8 cont = function
    | String_utf8 (i, j, u, v) when compare v nil = 0 ->
        String_utf8 (i, j, u, cont)
    | _ -> assert false
  in
  let rec aux v =
    match v with
    | Pair (Char _, _) | String_latin1 _ | String_utf8 _ ->
        let (u, rest) = get_string_utf8 v in
        patch_string_utf8 (aux rest) (string_utf8 (map_pcdata u))
    | Pair (hd, tl) -> Pair (map_other hd, aux tl)
    | Concat (_,_) as v -> eval_lazy_concat v; aux v
    | v when compare v nil = 0 -> v
    | v -> raise (Invalid_argument "Value.map_xml")
  in
  function
    | Xml (tag,attrs,cont) -> Xml (tag, attrs, aux cont)
    | _ -> raise (Invalid_argument "Value.map_xml")
*)

let tagged_tuple tag vl =
  let ct = sequence vl in
  let at = Record Imap.empty in
  let tag = Atom (Atoms.V.mk_ascii tag) in
  Xml (tag, at, ct)

  (** set of values *)

type tmp = t
module OrderedValue =
  struct
    type t = tmp
    let compare = compare
  end
module ValueSet = Set.Make(OrderedValue)

let ( |<| ) x y = compare x y < 0
let ( |>| ) x y = compare x y > 0
let ( |<=| ) x y = let c = compare x y in c < 0 || c = 0
let ( |>=| ) x y = let c = compare x y in c > 0 || c = 0
let ( |=| ) x y = compare x y = 0
let equal = ( |=| )
let ( |<>| ) x y = compare x y <> 0

(*
let rec concat l1 l2 = match l1 with
  | Pair (x,y) -> Pair (x, concat y l2)
  | String_latin1 (s,i,j,q) -> String_latin1 (s,i,j, concat q l2)
  | String_utf8 (s,i,j,q) -> String_utf8 (s,i,j, concat q l2)
  | q -> l2

let rec flatten = function
  | Pair (x,y) -> concat x (flatten y)
  | q -> q

*)


let () = dump_forward := dump_xml

let get_pair v =
  match normalize v with
    | Pair (x,y) -> (x,y)
    | _ -> assert false

(* TODO: tail-rec version of get_sequence *)

let rec get_sequence v =
  match normalize v with
    | Pair (x,y) -> x :: (get_sequence y)
    | _ -> []

let rec get_sequence_rev accu v =
  match normalize v with
    | Pair (x,y) -> get_sequence_rev (x::accu) y
    | _ -> accu

let get_sequence_rev v = get_sequence_rev [] v

let rec fold_sequence f accu v =
  match normalize v with
    | Pair (x,y) -> fold_sequence f (f accu x) y
    | _ -> accu

let atom_ascii s =
  Atom (Atoms.V.mk_ascii s)

let get_variant = function
  | Atom a -> Atoms.V.get_ascii a, None
  | v -> match normalize v with
      | Pair (Atom a,x) -> Atoms.V.get_ascii a, Some x
      | _ -> assert false

let label_ascii s =
  Label.mk_ascii s

let record (l : (label * t) list) =
  Record (Imap.create (Array.of_list (Obj.magic l)))

let record_ascii l =
  record (List.map (fun (l,v) -> (label_ascii l, v)) l)


let get_field v l =
  match v with
    | Record fields -> Imap.find fields (Upool.int l)
    | _ -> raise Not_found

let get_field_ascii v l = get_field v (label_ascii l)

let abstract a v =
  Abstract (a,Obj.repr v)

let get_abstract = function
  | Abstract (_,v) -> Obj.magic v
  | _ -> assert false
  

let get_label = Upool.int (label_ascii "get")
let set_label = Upool.int (label_ascii "set")
let mk_rf ~get ~set =
  Imap.create [| get_label, get; set_label, set |]

let mk_ref t v =
  let r = ref v in
  let get = Abstraction (Some [Sequence.nil_type, t], fun _ -> !r)
  and set = Abstraction (Some [t, Sequence.nil_type], fun x -> r := x; nil) in
  Record (mk_rf ~get ~set)


let mk_ext_ref t get set =
  let get = Abstraction (
    (match t with Some t -> Some [Sequence.nil_type, t] | None -> None),
    fun _ -> get ())
  and set = Abstraction (
    (match t with Some t -> Some [t, Sequence.nil_type] | None -> None), 
    fun v -> set v; nil) in
  Record (mk_rf ~get ~set)
  

let ocaml2cduce_int i =
  Integer (Intervals.V.from_int i)

let cduce2ocaml_int = function
  | Integer i -> Intervals.V.get_int i
  | _ -> assert false

let ocaml2cduce_int32 i =
  Integer (Intervals.V.from_int32 i)

let cduce2ocaml_int32 = function
  | Integer i -> Intervals.V.to_int32 i
  | _ -> assert false

let ocaml2cduce_int64 i =
  Integer (Intervals.V.from_int64 i)

let cduce2ocaml_int64 = function
  | Integer i -> Intervals.V.to_int64 i
  | _ -> assert false

let ocaml2cduce_string s = string_latin1 (String.copy s)

let cduce2ocaml_string = get_string_latin1 (* Result is already fresh *)

let ocaml2cduce_string_utf8 s = string_utf8 (U.mk (String.copy (U.get_str s)))

let cduce2ocaml_string_utf8 s = fst (get_string_utf8 s) (* Result is already fresh *)

let ocaml2cduce_char c =
  Char (Chars.V.mk_char c)

let ocaml2cduce_wchar c =
  Char (Chars.V.mk_int c)

let cduce2ocaml_char = function
  | Char c -> Chars.V.to_char c 
  | _ -> assert false

let ocaml2cduce_bigint i =
  Integer (Intervals.V.from_bigint i)

let cduce2ocaml_bigint = function
  | Integer i -> Intervals.V.get_bigint i
  | _ -> assert false

let ocaml2cduce_atom q = Atom q
let cduce2ocaml_atom = function Atom a -> a | _ -> assert false

let print_utf8 v =
  print_string (U.get_str v);
  flush stdout


let float n =
  Abstract ("float", Obj.repr n)

let cduce2ocaml_option f v =
  match normalize v with
    | Pair (x,y) -> Some (f x)
    | _ -> None


let ocaml2cduce_option f = function
  | Some x -> Pair (f x, nil)
  | None -> nil



let add v1 v2 = match (v1,v2) with
  | (Integer x, Integer y) -> Integer (Intervals.V.add x y)
  | (Record r1, Record r2) -> Record (Imap.merge r1 r2)
  | _ -> assert false

let merge v1 v2 = match (v1,v2) with
  | (Record r1, Record r2) -> Record (Imap.merge r1 r2)
  | _ -> assert false

let sub v1 v2 = match (v1,v2) with
  | (Integer x, Integer y) -> Integer (Intervals.V.sub x y)
  | _ -> assert false

let mul v1 v2 = match (v1,v2) with
  | (Integer x, Integer y) -> Integer (Intervals.V.mult x y)
  | _ -> assert false

let div v1 v2 = match (v1,v2) with
  | (Integer x, Integer y) -> Integer (Intervals.V.div x y)
  | _ -> assert false

let modulo v1 v2 = match (v1,v2) with
  | (Integer x, Integer y) -> Integer (Intervals.V.modulo x y)
  | _ -> assert false


let pair v1 v2 = Pair (v1,v2)
let xml v1 v2 v3 = Xml (v1,v2,v3)

let mk_record labels fields =
  let l = ref [] in
  assert (Array.length labels == Array.length fields);
  for i = 0 to Array.length labels - 1 do
    l := (labels.(i),fields.(i)) :: !l;
  done;
  record !l
  

(* TODO: optimize cases
     - (f x = [])
     - all chars copied or deleted *)

let rec transform_aux f accu = function
  | Pair (x,y) -> let accu = concat accu (f x) in transform_aux f accu y
  | Atom _ -> accu
  | v -> transform_aux f accu (normalize v)

let transform f v = transform_aux f nil v


let rec xtransform_aux f accu = function
  | Pair (x,y) -> 
      let accu = match f x with
	| Absent ->
	    let x = match x with
	      | Xml (tag, attr, child) -> 
		  let child = xtransform_aux f nil child in
		  Xml (tag, attr, child)
	      | XmlNs (tag, attr, child, ns) ->
		  let child = xtransform_aux f nil child in
		  XmlNs (tag, attr, child, ns)
	      | x -> x in
	    concat accu (Pair (x,nil))
	| x -> concat accu x
      in 
      xtransform_aux f accu y
  | Atom _ -> accu
  | v -> xtransform_aux f accu (normalize v)

let xtransform f v = xtransform_aux f nil v

let remove_field l = function
  | Record r -> Record (Imap.remove r (Upool.int l))
  | _ -> assert false

let rec ocaml2cduce_list f = function
  | [] -> nil
  | hd::tl -> Pair (f hd, ocaml2cduce_list f tl)

let rec cduce2ocaml_list f v =
  match normalize v with
    | Pair (x,y) -> f x :: (cduce2ocaml_list f y)
    | _ -> []

let ocaml2cduce_array f x = ocaml2cduce_list f (Array.to_list x)
let cduce2ocaml_array f x = Array.of_list (cduce2ocaml_list f x)

let no_attr = Record Imap.empty

let ocaml2cduce_constr tag va = 
  Xml (tag, no_attr, sequence_of_array va)

let rec cduce2ocaml_constr m = function
  | Atom q ->
      Obj.repr (Atoms.get_map q m)
  | Xml (Atom q,_,f) | XmlNs (Atom q,_,f,_) ->
	let tag = Atoms.get_map q m in
	let x = Obj.repr (Array.of_list (get_sequence f)) in
	Obj.set_tag x tag;
	x
  | _ -> assert false

let rec cduce2ocaml_variant m = function
  | Atom q ->
      Obj.repr (Atoms.get_map q m)
  | Xml (Atom q,_,f) | XmlNs (Atom q,_,f,_) ->
	let tag = Atoms.get_map q m in
	let (x,_) = get_pair f in
	Obj.repr (tag,x)
  | _ -> assert false

      
let ocaml2cduce_fun farg fres f =
  Abstraction (None, fun x -> fres (f (farg x)))

let cduce2ocaml_fun farg fres = function
  | Abstraction (_, f) -> (fun x -> fres (f (farg x)))
  | _ -> assert false

let apply f arg = match f with
  | Abstraction (_,f) -> f arg
  | _ -> assert false


type pools = Ns.Uri.value array * Ns.Label.value array

let extract_all () =
  Ns.Uri.extract (),
  Ns.Label.extract ()
 
let intract_all (uri,label) =
  Ns.Uri.intract uri;
  Ns.Label.intract label
