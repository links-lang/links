(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Value
open Ident
open Auto_pat
open Encodings


type t = (Value.t * string) list

let rec print ppf = function
  | [] -> ()
  | (v, t) :: l ->
      print ppf l;
      Format.fprintf ppf 
	"Value @[%a@] does not match type @[%s@]@."
	Value.print v
	t

let print_to_string f x =
  let b = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer b in
  f ppf x;
  Format.pp_print_flush ppf ();
  Buffer.contents b

let to_string e =
  print_to_string print e

exception Failed of t

let make_result pt d (code,_,_) =
  if d.fail_code == code then raise (Failed pt);
  code

let rec run_disp_basic pt d f =  function
  | [(_,r)] -> make_result pt d r
  | (t,r)::rem -> 
      if f t then make_result pt d r 
      else run_disp_basic pt d f rem
  | _ -> assert false

let rec run_dispatcher pt d v = 
  match d.actions with
    | AIgnore r -> make_result pt d r
    | AKind k -> run_disp_kind pt d k v

and run_disp pt d v =
  run_dispatcher ((v, d.expected_type)::pt) d v

and run_disp_kind pt d actions = function
  | Pair (v1,v2) -> run_disp_prod pt d v1 v2 actions.prod
  | Xml (v1,v2,v3) | XmlNs (v1,v2,v3,_) -> run_disp_prod pt d v1 (Pair(v2,v3)) actions.xml
  | Record r -> run_disp_record pt d 0 r actions.record
  | Atom q -> make_result pt d (Atoms.get_map q actions.atoms) 
  | Char c -> make_result pt d (Chars.get_map c actions.chars) 
  | Integer i ->
      run_disp_basic pt d (fun t -> Types.Int.has_int t i) actions.basic
  | Abstraction (None,_) ->
      run_disp_basic pt d
	(fun t -> failwith "Run-time inspection of external abstraction")
        actions.basic
  | Abstraction (Some iface,_) ->
      run_disp_basic pt d (fun t -> Types.Arrow.check_iface iface t) 
        actions.basic
  | Absent ->
      run_disp_basic pt d (fun t -> Types.Record.has_absent t) actions.basic
  | v ->
      run_disp_kind pt d actions (normalize v)


and run_disp_prod pt d v1 v2 = function
  | Impossible -> assert false
  | TailCall d1 -> run_disp pt d1 v1
  | Ignore d2 -> run_disp_prod2 pt d v2 d2
  | Dispatch (d1,b1) ->
      let code1 = run_disp pt d1 v1 in
      run_disp_prod2 pt d v2 b1.(code1)

and run_disp_prod2 pt d v2 = function
  | Impossible -> assert false
  | Ignore r -> make_result pt d r
  | TailCall d2 -> run_disp pt d2 v2
  | Dispatch (d2,b2) ->
      let code2 = run_disp pt d2 v2 in
      make_result pt d b2.(code2)

and run_disp_record pt d n fields = function
  | None -> assert false
  | Some (RecLabel (l,r)) ->
      (* TODO: get rid of this exception... *)
      (try run_disp_record1 pt d l (succ n) 
	 (Imap.find fields (Upool.int l)) fields r
       with Not_found -> 
	 run_disp_record1 pt d l n Absent fields r)
  | Some (RecNolabel (some,none)) ->
      let r = if (n < Imap.cardinal fields) then some else none in
      match r with
	| Some r -> make_result pt d r
	| None -> assert false
      
and run_disp_record1 pt d l n v1 rem = function
  | Impossible -> assert false
  | TailCall d1 -> run_disp pt d1 v1
  | Ignore d2 ->  run_disp_record2 pt d n rem d2
  | Dispatch (d1,b1) ->
      let code1 = run_disp pt d1 v1 in
      run_disp_record2 pt d n rem b1.(code1)

and run_disp_record2 pt d n rem = function
  | Impossible -> assert false
  | Ignore r -> make_result pt d r
  | TailCall d2 -> run_disp_record_loop pt n rem d2
  | Dispatch (d2,b2) ->
      let code2 = run_disp_record_loop pt n rem d2 in
      make_result pt d b2.(code2)

and run_disp_record_loop pt n rem d =
  match d.actions with
    | AIgnore r -> make_result pt d r
    | AKind k -> run_disp_record pt d n rem k.record

let is_xml = function ((Xml _ | XmlNs _),_) -> true | _ -> false

let rec simplify = function
  | (Absent, _) :: l -> simplify l
  | x :: l -> (try [ x; List.find is_xml l ] with Not_found -> [ x ])
  | [] -> assert false

let check d v =
  if (d.fail_code < 0) then ()
  else
    let (code,_) = Run_dispatch.run_dispatcher d v in
    if code == d.fail_code then (ignore (run_disp [] d v); assert false)

let explain d v =
  try  check d v; None
  with Failed p -> Some p

let do_check d v =
  try check d v; v
  with Failed p ->
    let p = simplify p in
    let s = print_to_string print p in
    raise (CDuceExn (string_latin1 s))

let check_failure d v =
  try check d v; v
  with Failed p ->
    let p = simplify p in
    let s = print_to_string print p in
    failwith s

