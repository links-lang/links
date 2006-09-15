(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(* Running dispatchers *)

(* Possible simple optimizations:
     - in make_result_prod, see if buffer can be simply overwritten
       (precompute this ...)
     - optimize for Xml elements (don't build the Pair (attr,content))
*)

(*
let (<) : int -> int -> bool = (<);;
*)

open Value
open Ident
open Auto_pat
open Encodings


let buffer = ref (Array.create 127 Absent)
let cursor = ref 0

let blit a1 ofs1 a2 ofs2 len =
  for i = 0 to len - 1 do
    Array.unsafe_set a2 (ofs2 + i) (Array.unsafe_get a1 (ofs1 + i))
  done
(* important to do this in the increasing order ... *)


let ensure_room n =
  let l = Array.length !buffer in
  if !cursor + n > l then
    let buffer' = Array.create (l * 2 + n) Absent in
    blit !buffer 0 buffer' 0 !cursor;
    buffer := buffer'

let push v =
  ensure_room 1;
  !buffer.(!cursor) <- v;
  incr cursor


let make_result_prod v1 v2 v (code,r,pop) = 
  let n = Array.length r in
  if n > 0 then (
  ensure_room n;
  let buf = !buffer in
  let c = !cursor in
  for a = 0 to n - 1 do
    let x = match Array.unsafe_get r a with
      | Catch -> v
      | Const c -> const c
      | Nil -> nil
      | Left -> v1
      | Right -> v2
      | Stack i -> buf.(c - i)
      | Recompose (i,j) -> 
	  Pair (
	    (match i with (-1) -> v1 | (-2) -> nil | _ -> buf.(c - i)),
	    (match j with (-1) -> v2 | (-2) -> nil | _ -> buf.(c - j))
	  )
    in
    buf.(c + a) <- x
  done;
  if pop != 0 then blit buf c buf (c - pop) n);
  cursor := !cursor - pop + n;  (* clean space for GC ? *)
  code

let make_result_basic v (code,r,_) = 
  let n = Array.length r in
  if n > 0 then (
  ensure_room n;
  let buf = !buffer in
  for a = 0 to n - 1 do
    let x = match Array.unsafe_get r a with
      | Catch -> v
      | Const c -> const c 
      | _ -> assert false
    in
    buf.(!cursor) <- x;
    incr cursor
  done);
  code


let make_result_char ch (code,r,_) = 
  let n = Array.length r in
  if n > 0 then (
  ensure_room n;
  let buf = !buffer in
  for a = 0 to n - 1 do
    let x = match Array.unsafe_get r a with
      | Catch -> Char ch
      | Const c -> const c
      | _ -> assert false
    in
    buf.(!cursor + a) <- x
  done;
  cursor := !cursor + n);
  code

let tail_string_latin1 i j s q =
  if i + 1 == j then q else String_latin1 (i + 1,j,s,q)

let make_result_string_latin1 i j s q (code,r,pop) = 
  let n = Array.length r in
  if n > 0 then (
  ensure_room n;
  let c = !cursor in
  let buf = !buffer in
  for a = 0 to n - 1 do
    let x = match Array.unsafe_get r a with
      | Catch -> String_latin1 (i,j,s,q)
      | Const c -> const c
      | Nil -> nil
      | Left -> Char (Chars.V.mk_char s.[i])
      | Right -> tail_string_latin1 i j s q
      | Stack n -> buf.(c - n)
      | Recompose (n,m) -> 
	  Pair (
	    (match n with 
	       | (-1) -> Char (Chars.V.mk_char s.[i]) 
	       | (-2) -> nil 
	       | _ -> buf.(c - n)),
	    (match m with 
	       | (-1) -> tail_string_latin1 i j s q 
	       | (-2) -> nil 
	       | _ -> buf.(c - m))
	  )
    in
    buf.(c + a) <- x
  done;
  if pop != 0 then blit buf c buf (c - pop) n);
  cursor := !cursor - pop + n;
  code

let tail_string_utf8 i j s q =
  let i = Utf8.advance s i in
  if Utf8.equal_index i j then q else String_utf8 (i,j,s,q)

let make_result_string_utf8 i j s q (code,r,pop) = 
  let n = Array.length r in
  if n > 0 then (
  ensure_room n;
  let c = !cursor in
  let buf = !buffer in
  for a = 0 to n - 1 do
    let x = match Array.unsafe_get r a with
      | Catch -> String_utf8 (i,j,s,q)
      | Const c -> const c
      | Nil -> nil
      | Left -> Char (Chars.V.mk_int (Utf8.get s i))
      | Right -> tail_string_utf8 i j s q 
      | Stack n -> buf.(c - n)
      | Recompose (n,m) -> 
	  Pair (
	    (match n with 
	       | (-1) -> Char (Chars.V.mk_int (Utf8.get s i)) 
	       | (-2) -> nil 
	       | _ -> buf.(c - n)),
	    (match m with 
	       | (-1) -> tail_string_utf8 i j s q 
	       | (-2) -> nil 
	       | _ -> buf.(c - m))
	  )
    in
    buf.(c + a) <- x
  done;
  if pop != 0 then blit buf c buf (c - pop) n;
  );
  cursor := !cursor - pop + n;
  code

let rec run_disp_basic v f =  function
  | [(_,r)] -> make_result_basic v r
  | (t,r)::rem -> if f t then make_result_basic v r else run_disp_basic v f rem
  | _ ->  
      Format.fprintf Format.std_formatter "ERR: %a@." Value.print v;
      assert false

let rec run_dispatcher d v = 
(*  Format.fprintf Format.std_formatter "Matching (%a) with:@." Value.print v;
  Patterns.Compile.print_dispatcher Format.std_formatter d;  *)

  match d.actions with
    | AIgnore r -> make_result_basic v r
    | AKind k -> run_disp_kind k v

and run_disp_kind actions v =
  match v with
  | Pair (v1,v2) -> run_disp_prod v v1 v2 actions.prod
  | Xml (v1,v2,v3) 
  | XmlNs (v1,v2,v3,_) -> run_disp_prod v v1 (Pair (v2,v3)) actions.xml
  | Record r -> run_disp_record 0 v r actions.record
  | String_latin1 (i,j,s,q) -> 
(*      run_disp_kind actions (Value.normalize v)  *)
       run_disp_string_latin1 i j s q actions 
  | String_utf8 (i,j,s,q) -> 
(*      run_disp_kind actions (Value.normalize v)  *)
 	run_disp_string_utf8 i j s q actions  
  | Atom q -> make_result_basic v (Atoms.get_map q actions.atoms) 
  | Char c -> make_result_basic v (Chars.get_map c actions.chars) 
  | Integer i ->
      run_disp_basic v (fun t -> Types.Int.has_int t i) actions.basic
  | Abstraction (None,_) ->
      run_disp_basic v (fun t -> failwith "Run-time inspection of external abstraction")
        actions.basic
  | Abstraction (Some iface,_) ->
      run_disp_basic v (fun t -> Types.Arrow.check_iface iface t) 
        actions.basic
  | Abstract (abs,_) -> 
      run_disp_basic v (fun t -> Types.Abstract.contains abs (Types.get_abstract t))
	actions.basic
  | Absent ->
      run_disp_basic v (fun t -> Types.Record.has_absent t) actions.basic
  | Concat (_,_) as v -> run_disp_kind actions (Value.normalize v)

and run_disp_prod v v1 v2 = function
  | Impossible -> assert false
  | TailCall d1 -> run_dispatcher d1 v1
  | Ignore d2 -> run_disp_prod2 v1 v v2 d2
  | Dispatch (d1,b1) ->
      let code1 = run_dispatcher d1 v1 in
      run_disp_prod2 v1 v v2 b1.(code1)

and run_disp_prod2 v1 v v2 = function
  | Impossible -> assert false
  | Ignore r -> make_result_prod v1 v2 v r
  | TailCall d2 -> run_dispatcher d2 v2
  | Dispatch (d2,b2) ->
      let code2 = run_dispatcher d2 v2 in
      make_result_prod v1 v2 v b2.(code2)

and run_disp_record n v fields = function
  | None -> assert false
  | Some (RecLabel (l,d)) ->
      (* TODO: get rid of this exception... *)
      (try run_disp_record1 v (succ n) (Imap.find fields (Upool.int l)) fields d
       with Not_found -> run_disp_record1 v n Absent fields d)
  | Some (RecNolabel (some,none)) ->
      let r = if (n < Imap.cardinal fields) then some else none in
      match r with
	| Some r -> make_result_basic v r
	| None -> assert false
      
and run_disp_record1 v n v1 rem = function
  | Impossible -> assert false
  | TailCall d1 -> run_dispatcher d1 v1
  | Ignore d2 ->  run_disp_record2 v n v1 rem d2
  | Dispatch (d1,b1) ->
      let code1 = run_dispatcher d1 v1 in
      run_disp_record2 v n v1 rem b1.(code1)

and run_disp_record2 v n v1 rem = function
  | Impossible -> assert false
  | Ignore r -> make_result_prod v1 Absent v r
  | TailCall d2 -> run_disp_record_loop v n rem d2
  | Dispatch (d2,b2) ->
      let code2 = run_disp_record_loop v n rem d2 in
      make_result_prod v1 Absent v b2.(code2)

and run_disp_record_loop v n rem d =
  match d.actions with
    | AIgnore r -> make_result_basic v r
    | AKind k -> run_disp_record n v rem k.record
  

and run_disp_string_latin1 i j s q actions = 
  if i == j then run_disp_kind actions q 
  else match actions.prod with
    | Impossible -> assert false
    | TailCall d1 -> run_disp_string_latin1_char d1 (Chars.V.mk_char s.[i])
    | Ignore d2 -> run_disp_string_latin1_2 i j s q d2
    | Dispatch (d1,b1) ->
	let code1 = run_disp_string_latin1_char d1 (Chars.V.mk_char s.[i]) in
	run_disp_string_latin1_2 i j s q b1.(code1)
and run_disp_string_latin1_char d ch =
  match d.actions with
    | AIgnore r -> make_result_char ch r
    | AKind k -> make_result_char ch (Chars.get_map ch k.chars) 
and run_disp_string_latin1_2 i j s q = function
  | Impossible -> assert false
  | Ignore r -> 
      make_result_string_latin1 i j s q r
  | TailCall d2 -> run_disp_string_latin1_loop i j s q d2
  | Dispatch (d2,b2) ->
      let code2 = run_disp_string_latin1_loop i j s q d2 in
      make_result_string_latin1 i j s q b2.(code2)
and run_disp_string_latin1_loop i j s q d =
  let i = succ i in
  if i == j then run_dispatcher d q else
  match d.actions with
    | AIgnore r -> make_result_basic (Value.String_latin1 (i,j,s,q)) r
    | AKind k -> run_disp_string_latin1 i j s q k

and run_disp_string_utf8 i j s q actions = 
  if Utf8.equal_index i j then run_disp_kind actions q
  else
  match actions.prod with
    | Impossible -> assert false
    | TailCall d1 -> run_disp_string_utf8_char d1 (Chars.V.mk_int (Utf8.get s i))
    | Ignore d2 -> run_disp_string_utf8_2 i j s q d2
    | Dispatch (d1,b1) ->
	let code1 = run_disp_string_utf8_char d1 (Chars.V.mk_int (Utf8.get s i)) in
	run_disp_string_utf8_2 i j s q b1.(code1)
and run_disp_string_utf8_char d ch =
  match d.actions with
    | AIgnore r -> make_result_char ch r
    | AKind k -> make_result_char ch (Chars.get_map ch k.chars) 
and run_disp_string_utf8_2 i j s q = function
  | Impossible -> assert false
  | Ignore r -> 
      make_result_string_utf8 i j s q r
  | TailCall d2 -> run_disp_string_utf8_loop i j s q d2
  | Dispatch (d2,b2) ->
      let code2 = run_disp_string_utf8_loop i j s q d2 in
      make_result_string_utf8 i j s q b2.(code2)
and run_disp_string_utf8_loop i j s q d =
  let i = Utf8.advance s i in
  if Utf8.equal_index i j then run_dispatcher d q else
  match d.actions with
    | AIgnore r -> make_result_basic (Value.String_utf8 (i,j,s,q)) r
    | AKind k -> run_disp_string_utf8 i j s q k

let run_dispatcher d v =
  let code = run_dispatcher d v in
  cursor := 0;
  (code,!buffer) 
