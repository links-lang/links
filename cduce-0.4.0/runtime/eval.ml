(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Value
open Run_dispatch
open Ident
open Lambda

let ns_table = ref Ns.empty_table

let ops = Hashtbl.create 13
let register_op = Hashtbl.add ops
let eval_op = Hashtbl.find ops

(* To write tail-recursive map-like iteration *)

let make_accu () = Value.Pair(nil,Absent)
let get_accu a = snd (Obj.magic a)
let map f v = let acc0 = make_accu () in set_cdr (f acc0 v) nil; get_accu acc0


let rec ensure a i = 
  let n = Array.length !a in 
  if i >= n then (
    let b = Array.create (max (n*2) i) Value.Absent in
    Array.blit !a 0 b 0 n;
    a := b;
    ensure a i
  )

let set a i x =
  ensure a i;
  !a.(i) <- x


(* For the toplevel *)
let globs = ref (Array.create 64 Value.Absent)
let nglobs = ref 0


let get_globals = ref (fun cu -> assert false)
let get_external = ref (fun cu pos -> assert false)
let set_external = ref (fun cu pos -> assert false)
let get_builtin = ref (fun _ -> assert false)
let run_schema_validator = ref (fun _ _ -> assert false)

let eval_var env locals = function
  | Env i -> env.(i)
  | Local slot -> locals.(slot)
  | Dummy -> Value.Absent
  | Global i -> !globs.(i)
  | Ext (cu,pos) as x ->
      if pos < 0 then (Obj.magic cu : Value.t) else
	let v = (!get_globals cu).(pos) in
	let x = Obj.repr x in
	Obj.set_field x 0 (Obj.repr v);
	Obj.set_field x 1 (Obj.repr (-1));
	v
  | External (cu,pos) as x ->
      if pos < 0 then (Obj.magic cu : Value.t) else
	let v = !get_external cu pos in
	let x = Obj.repr x in
	Obj.set_field x 0 (Obj.repr v);
	Obj.set_field x 1 (Obj.repr (-1));
	v
  | Builtin s ->
      !get_builtin s

let tag_op_resolved = Obj.tag (Obj.repr (OpResolved ((fun _ -> assert false), [])))
let tag_const = Obj.tag (Obj.repr (Const (Obj.magic 0)))

let rec eval env locals = function
  | Var ((Global _ | Ext _ | External _ | Builtin _) as x)  as e ->
      let v = eval_var env locals x in
      Obj.set_field (Obj.repr e) 0 (Obj.repr v);
      Obj.set_tag (Obj.repr e) tag_const;
      v
  | Var x -> eval_var env locals x
  | Apply (e1,e2) -> 
      let v1 = eval env locals e1 in
      let v2 = eval env locals e2 in
      eval_apply v1 v2
  | Abstraction (slots,iface,body,lsize) -> 
      eval_abstraction env locals slots iface body lsize
  | Const c -> c
  | Pair (e1,e2) -> 
      let v1 = eval env locals e1 in
      let v2 = eval env locals e2 in
      Value.Pair (v1,v2)
  | Xml (e1,e2,e3) -> 
      let v1 = eval env locals e1 in
      let v2 = eval env locals e2 in
      let v3 = eval env locals e3 in
      Value.Xml (v1,v2,v3)
  | XmlNs (e1,e2,e3,ns) -> 
      let v1 = eval env locals e1 in
      let v2 = eval env locals e2 in
      let v3 = eval env locals e3 in
      Value.XmlNs (v1,v2,v3,ns)
  | Record r -> 
      Value.Record (Imap.map (eval env locals) r)
  | String (i,j,s,q) -> Value.substring_utf8 i j s (eval env locals q)
  | Match (e,brs) -> eval_branches env locals brs (eval env locals e)
  | Map (arg,brs) -> eval_map env locals brs (eval env locals arg)
  | Xtrans (arg,brs) -> eval_xtrans env locals brs (eval env locals arg)
  | Try (arg,brs) -> eval_try env locals arg brs
  | Transform (arg,brs) -> eval_transform env locals brs (eval env locals arg) 
  | Dot (e, l) -> eval_dot l (eval env locals e)
  | RemoveField (e, l) -> eval_remove_field l (eval env locals e)
  | Validate (e, v) -> eval_validate env locals e v
  | Ref (e,t) -> eval_ref env locals e t
  | Op (op,args) as e -> 
      let eval_fun = eval_op op in
      Obj.set_field (Obj.repr e) 0 (Obj.repr eval_fun);
      Obj.set_tag (Obj.repr e) tag_op_resolved;
      eval_fun (List.map (eval env locals) args)
  | OpResolved (f,args) ->
      f (List.map (eval env locals) args)
  | NsTable (ns,e) -> ns_table := ns; eval env locals e
  | Check (e,d) -> eval_check env locals e d

and eval_check env locals e d =
  Explain.do_check d (eval env locals e)

and eval_abstraction env locals slots iface body lsize =
  let local_env = Array.map (eval_var env locals) slots in
  let f arg =
    eval_branches local_env (Array.create lsize Value.Absent) body arg
  in
  let a = Value.Abstraction (Some iface,f)  in
  local_env.(0) <- a;
  a

and eval_apply f arg = match f with
  | Value.Abstraction (_,f) -> f arg
  | _  -> assert false

and eval_branches env locals brs arg =
  let (code, bindings) = Run_dispatch.run_dispatcher brs.brs_disp arg in
  match brs.brs_rhs.(code) with 
    | Auto_pat.Match (n,e) ->
	Array.blit bindings 0 locals brs.brs_stack_pos n;
	eval env locals e 
    | Auto_pat.Fail -> Value.Absent

and eval_ref env locals e t =
  Value.mk_ref (Types.descr t) (eval env locals e)

and eval_validate env locals e s = 
  try Schema_validator.run s (eval env locals e)
  with Schema_common.XSI_validation_error msg ->
    failwith' ("Schema validation failure: " ^ msg)

and eval_try env locals arg brs =
  try eval env locals arg
  with (CDuceExn v) as exn ->
    match eval_branches env locals brs v with
      | Value.Absent -> raise exn
      | x -> x

and eval_map env locals brs v =
  map (eval_map_aux env locals brs) v

and eval_map_aux env locals brs acc = function
  | Value.Pair (x,y) -> 
      let x = eval_branches env locals brs x in
      let acc' = Value.Pair (x, Absent) in
      set_cdr acc acc';
      eval_map_aux env locals brs acc' y
  | Value.String_latin1 (_,_,_,_) | Value.String_utf8 (_,_,_,_) as v -> 
      eval_map_aux env locals brs acc (normalize v)
  | Value.Concat (x,y) ->
      let acc = eval_map_aux env locals brs acc x in
      eval_map_aux env locals brs acc y
  | _ -> acc

and eval_transform env locals brs v =
  map (eval_transform_aux env locals brs) v

and eval_transform_aux env locals brs acc = function
  | Value.Pair (x,y) -> 
      (match eval_branches env locals brs x with 
	 | Value.Absent -> eval_transform_aux env locals brs acc y
	 | x -> eval_transform_aux env locals brs (append_cdr acc x) y)
  | Value.String_latin1 (_,_,_,q) | Value.String_utf8 (_,_,_,q) as v -> 
      if not brs.brs_accept_chars
      then eval_transform_aux env locals brs acc q
      else eval_transform_aux env locals brs acc (normalize v)
  | Value.Concat (x,y) ->
      let acc = eval_transform_aux env locals brs acc x in
      eval_transform_aux env locals brs acc y
  | _ -> acc

and eval_xtrans env locals brs v =
  map (eval_xtrans_aux env locals brs) v

and eval_xtrans_aux env locals brs acc = function
  | Value.String_utf8 (s,i,j,q) as v ->
      if not brs.brs_accept_chars
      then 
	let acc' = Value.String_utf8 (s,i,j, Absent) in
	set_cdr acc acc';
	eval_xtrans_aux env locals brs acc' q
      else eval_xtrans_aux env locals brs acc (normalize v)
  | Value.String_latin1 (s,i,j,q) as v ->
      if not brs.brs_accept_chars
      then 
	let acc' = Value.String_latin1 (s,i,j, Absent) in
	set_cdr acc acc';
	eval_xtrans_aux env locals brs acc' q
      else eval_xtrans_aux env locals brs acc (normalize v)
  | Value.Concat (x,y) ->
      let acc = eval_xtrans_aux env locals brs acc x in
      eval_xtrans_aux env locals brs acc y
  | Value.Pair (x,y) -> 
      let acc = 
	match eval_branches env locals brs x with
	  | Value.Absent -> 
	      let x = match x with
		| Value.Xml (tag, attr, child) -> 
		    let child = eval_xtrans env locals brs child in
		    Value.Xml (tag, attr, child)
		| Value.XmlNs (tag, attr, child, ns) ->
		    let child = eval_xtrans env locals brs child in
		    Value.XmlNs (tag, attr, child, ns)
		| x -> x in
	      let acc' = Value.Pair (x, Absent) in
	      set_cdr acc acc';
	      acc'
	  | x -> append_cdr acc x
      in
      eval_xtrans_aux env locals brs acc y
  | _ -> acc

and eval_dot l = function
  | Value.Record r 
  | Value.Xml (_,Value.Record r,_)
  | Value.XmlNs (_,Value.Record r,_,_) -> Imap.find_lower r (Upool.int l)
  | v -> assert false

and eval_remove_field l = function
  | Value.Record r -> Value.Record (Imap.remove r (Upool.int l))
  | _ -> assert false


let expr e lsize = eval [||] (Array.create lsize Value.Absent) e

(* Evaluation in the toplevel *)

let eval_toplevel = function
  | Eval (e,lsize) -> ignore (expr e lsize)
  | LetDecls (e,lsize,disp,n) ->
      let v = expr e lsize in
      let (_, bindings) = Run_dispatch.run_dispatcher disp v in
      ensure globs (!nglobs + n);
      Array.blit bindings 0 !globs !nglobs n;
      nglobs := !nglobs + n
  | LetDecl (e,lsize) ->
      let v = expr e lsize in
      set globs !nglobs v;
      incr nglobs

let eval_toplevel items = 
  let n = !nglobs in
  try List.iter eval_toplevel items
  with exn -> nglobs := n; raise exn

let eval_var v = 
  eval_var [||] [||] v

(* Evaluation of a compiled unit *)

let eval_unit globs nglobs = function
  | Eval (e,lsize) -> ignore (expr e lsize)
  | LetDecls (e,lsize,disp,n) ->
      let v = expr e lsize in
      let (_, bindings) = Run_dispatch.run_dispatcher disp v in
      Array.blit bindings 0 globs !nglobs n;
      nglobs := !nglobs + n
  | LetDecl (e,lsize) ->
      let v = expr e lsize in
      globs.(!nglobs) <- v;
      incr nglobs

let eval_unit globs items = 
  let nglobs = ref 0 in
  List.iter (eval_unit globs nglobs) items;
  assert (!nglobs = Array.length globs)

