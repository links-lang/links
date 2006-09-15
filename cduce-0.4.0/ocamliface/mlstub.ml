(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

#load "q_MLast.cmo";;

(* TODO:
   - optimizations: generate labels and atoms only once.
   - translate record to open record on positive occurence
*)


open Mltypes
open Ident

module IntMap = 
  Map.Make(struct type t = int let compare : t -> t -> int = compare end)

module IntHash =
  Hashtbl.Make(struct type t = int let hash i = i let equal i j = i == j end)

(* Compute CDuce type *)

let vars = ref [||]

let memo_typ = IntHash.create 13

let atom lab = Types.atom (Atoms.atom (Atoms.V.mk_ascii lab))
let label lab = Label.mk (Ns.empty, U.mk lab)
let bigcup f l = List.fold_left (fun accu x -> Types.cup accu (f x)) Types.empty l

let rec typ t =
  try IntHash.find memo_typ t.uid
  with Not_found ->
(*    print_int t.uid; print_char ' '; flush stdout; *)
    let node = Types.make () in
    IntHash.add memo_typ t.uid node;
    Types.define node (typ_descr t.def);
    node

and typ_descr = function
  | Link t -> typ_descr t.def
  | Arrow (_,t,s) -> Types.arrow (typ t) (typ s)
  | Tuple tl -> Types.tuple (List.map typ tl)
  | PVariant l -> bigcup pvariant l
  | Variant (_,l,_) -> bigcup variant l
  | Record (_,l,_) ->
      let l = List.map (fun (lab,t) -> label lab, typ t) l in
      Types.record_fields (false,(LabelMap.from_list_disj l))
  | Abstract "int" -> Builtin_defs.caml_int
  | Abstract "char" -> Builtin_defs.char_latin1
  | Abstract "string" -> Builtin_defs.string_latin1
  | Abstract s -> Types.abstract (Types.Abstract.atom s)
  | Builtin ("list", [t])
  | Builtin ("array", [t]) -> Types.descr (Sequence.star_node (typ t))
  | Builtin ("Pervasives.ref", [t]) -> Builtin_defs.ref_type (typ t)
  | Builtin ("Big_int.big_int", []) -> Builtin_defs.int
  | Builtin ("Cduce_lib.Value.t", []) -> Types.any
  | Builtin ("Cduce_lib.Encodings.Utf8.t", []) -> Builtin_defs.string
  | Builtin ("Cduce_lib.Atoms.V.t", []) -> Builtin_defs.atom
  | Builtin ("unit", []) -> Sequence.nil_type
  | Builtin ("option", [t]) -> Sequence.option (typ t)
  | Var i -> Types.descr (!vars).(i)
  | _ -> assert false
	   
and pvariant = function
  | (lab, None) -> atom lab
  | (lab, Some t) -> Types.times (Types.cons (atom lab)) (typ t)

and variant = function
  | (lab, []) -> atom lab
  | (lab, c) -> Types.tuple (Types.cons (atom lab) :: List.map typ c)


(* Syntactic tools *)

let var_counter = ref 0
let mk_var _ =
  incr var_counter;
  Printf.sprintf "x%i" !var_counter

let mk_vars = List.map mk_var

let _loc = (Lexing.dummy_pos,Lexing.dummy_pos)

let let_in p e body =
  <:expr< let $list:[ p, e ]$ in $body$ >>

let atom_ascii lab =
  <:expr< Value.atom_ascii $str: String.escaped lab$ >>

let label_ascii lab =
  <:expr< Value.label_ascii $str: String.escaped lab$ >>

let pair e1 e2 = <:expr< Value.Pair ($e1$,$e2$) >>

let pmatch e l = 
  let l = List.map (fun (p,e) -> p,None,e) l in
  <:expr< match $e$ with [ $list:l$ ] >>

let rec matches ine oute = function
  | [v1;v2] ->
      let_in <:patt<($lid:v1$,$lid:v2$)>> <:expr< Value.get_pair $ine$ >> oute
  | v::vl ->
      let r = mk_var () in
      let oute = matches <:expr< $lid:r$ >> oute vl in
      let_in <:patt<($lid:v$,$lid:r$)>> <:expr< Value.get_pair $ine$ >> oute
  | [] -> assert false

let list_lit el =
  List.fold_right (fun a e -> <:expr< [$a$ :: $e$] >>) el <:expr< [] >>

let protect e f =
  match e with
    | <:expr< $lid:x$ >> -> f e
    | e ->
	let x = mk_var () in
	let r = f <:expr< $lid:x$ >> in
	<:expr< let $lid:x$ = $e$ in $r$ >> 

(* Registered types *)

let gen_types = ref true
(* currently always off *)


module HashTypes = Hashtbl.Make(Types)
let registered_types = HashTypes.create 13
let nb_registered_types = ref 0

let register_type t =
  assert(!gen_types);
  let n =
    try HashTypes.find registered_types t
    with Not_found ->
      let i = !nb_registered_types in
      HashTypes.add registered_types t i;
      incr nb_registered_types;
      i 
  in
  <:expr< types.($int:string_of_int n$) >>

let get_registered_types () =
  let a = Array.make !nb_registered_types Types.empty in
  HashTypes.iter (fun t i -> a.(i) <- t) registered_types;
  a

(* OCaml -> CDuce conversions *)


let to_cd_gen = ref []

let to_cd_fun_name t = 
  Printf.sprintf "to_cd_%i" t.uid

let to_cd_fun t =
  to_cd_gen := t :: !to_cd_gen;
  to_cd_fun_name t

let to_ml_gen = ref []

let to_ml_fun_name t =
  Printf.sprintf "to_ml_%i" t.uid

let to_ml_fun t =
  to_ml_gen := t :: !to_ml_gen;
  to_ml_fun_name t

let rec tuple = function
  | [v] -> v
  | v::l -> <:expr< Value.Pair ($v$, $tuple l$) >> 
  | [] -> assert false

let pat_tuple vars = 
  let pl = List.map (fun id -> <:patt< $lid:id$ >>) vars in
  <:patt< ($list:pl$) >>


let call_lab f l x =
  if l = "" then <:expr< $f$ $x$ >>
  else
    if l.[0] = '?' then 
      let l = String.sub l 1 (String.length l - 1) in
      <:expr< $f$ (? $l$ : $x$) >>
    else 
      <:expr< $f$ (~ $l$ : $x$) >>

let abstr_lab l x res =
  if l = "" then <:expr< fun $lid:x$ -> $res$ >>
  else
    if l.[0] = '?' then 
      let l = String.sub l 1 (String.length l - 1) in
      <:expr< fun ? $l$ : ( $lid:x$ ) -> $res$ >>
    else
      <:expr< fun ~ $l$ : $lid:x$ -> $res$ >>



let rec to_cd e t =
(*  Format.fprintf Format.err_formatter "to_cd %a [uid=%i; recurs=%i]@."
    Mltypes.print t t.uid t.recurs;  *)
  if t.recurs > 0 then <:expr< $lid:to_cd_fun t$ $e$ >>
  else to_cd_descr e t.def

and to_cd_descr e = function
  | Link t -> to_cd e t
  | Arrow (l,t,s) -> 
      (* let y = <...> in Value.Abstraction ([t,s], fun x -> s(y ~l:(t(x))) *)
      protect e 
      (fun y ->
	 let x = mk_var () in
	 let arg = to_ml <:expr< $lid:x$ >> t in
	 let res = to_cd (call_lab y l arg) s in
	 let abs = <:expr< fun $lid:x$ -> $res$ >> in
	 let iface =
	   if !gen_types then
	     let tt = register_type (Types.descr (typ t)) in
	     let ss = register_type (Types.descr (typ s)) in
	     <:expr< Some [($tt$,$ss$)] >>
	   else <:expr< None >> in
	 <:expr< Value.Abstraction ($iface$,$abs$) >>
      )
  | Tuple tl -> 
      (* let (x1,...,xn) = ... in Value.Pair (t1(x1), Value.Pair(...,tn(xn))) *)
      let vars = mk_vars tl in
      let_in (pat_tuple vars) e (tuple (tuple_to_cd tl vars))
  | PVariant l ->
      (* match <...> with 
	 | `A -> Value.atom_ascii "A" 
	 | `B x -> Value.Pair (Value.atom_ascii "B",t(x))
      *)
      let cases = 
	List.map
	  (function 
	     | (lab,None) -> <:patt< `$lid:lab$ >>, atom_ascii lab
	     | (lab,Some t) -> 
		 <:patt< `$lid:lab$ x >>, 
		 pair (atom_ascii lab) (to_cd <:expr< x >> t)
	  ) l in
      pmatch e cases
  | Variant (p,l,_) ->
      (* match <...> with 
	 | P.A -> Value.atom_ascii "A" 
	 | P.B (x1,x2,..) -> Value.Pair (Value.atom_ascii "B",...,Value.Pair(tn(x)))
      *)
      let cases = 
	List.map
	  (function 
	     | (lab,[]) ->
		 let pat = match lab with (* Stupid Camlp4 *)
		    | "true" -> <:patt< True >>
		    | "false" -> <:patt< False >>
		    | lab -> <:patt< $lid:p^lab$ >>
		 in
		 pat, atom_ascii lab
	     | (lab,tl) -> 
		 let vars = mk_vars tl in
		 <:patt< $lid:p^lab$ $pat_tuple vars$ >>,
		 tuple (atom_ascii lab :: tuple_to_cd tl vars)
	  ) l in
      pmatch e cases
  | Record (p,l,_) ->
      (* let x = <...> in Value.record [ l1,t1(x.P.l1); ...; ln,x.P.ln ] *)
      protect e
      (fun x ->
	 let l = 
	   List.map
	     (fun (lab,t) ->
		let e = to_cd <:expr<$x$.$lid:p^lab$>> t in
		<:expr< ($label_ascii lab$, $e$) >>)
	     l
	 in
	 <:expr< Value.record $list_lit l$ >>)

  | Abstract "int" -> <:expr< Value.ocaml2cduce_int $e$ >>
  | Abstract "char" -> <:expr< Value.ocaml2cduce_char $e$ >>
  | Abstract "string" -> <:expr< Value.ocaml2cduce_string $e$ >>
  | Abstract s -> <:expr< Value.abstract $str:String.escaped s$ $e$ >>
  | Builtin ("list",[t]) ->
      (* Value.sequence_rev (List.rev_map fun_t <...>) *)
      <:expr< Value.sequence_rev (List.rev_map $lid:to_cd_fun t$ $e$) >>
  | Builtin ("array",[t]) ->
      <:expr< Value.sequence_rev (List.rev_map $lid:to_cd_fun t$ (Array.to_list $e$)) >>
  | Builtin ("Pervasives.ref",[t]) ->
      (* let x = <...> in 
         Value.mk_ext_ref t (fun () -> t(!x)) (fun y -> x := t'(y)) *)
      protect e 
      (fun e ->
	 let y = mk_var () in
	 let tt = if !gen_types then 
	   let t = register_type (Types.descr (typ t)) in
	   <:expr< Some $t$ >> 
	 else
	   <:expr< None >> in
	 let get_x = <:expr< $e$.val >> in
	 let get = <:expr< fun () -> $to_cd get_x t$ >> in
	 let tr_y = to_ml <:expr< $lid:y$ >> t in
	 let set = <:expr< fun $lid:y$ -> $e$.val := $tr_y$ >> in
	 <:expr< Value.mk_ext_ref $tt$ $get$ $set$ >>
      )
  | Builtin ("Big_int.big_int", []) -> 
      <:expr< Value.ocaml2cduce_bigint $e$ >>
  | Builtin ("Cduce_lib.Value.t", []) -> e
  | Builtin ("Cduce_lib.Encodings.Utf8.t", []) -> 
      <:expr< Value.ocaml2cduce_string_utf8 $e$ >>
  | Builtin ("Cduce_lib.Atoms.V.t", []) ->
      <:expr< Value.ocaml2cduce_atom $e$ >>
  | Builtin ("unit", []) -> <:expr< do { $e$; Value.nil } >>
  | Var _ -> e
  | Builtin ("option", [t]) ->
      <:expr< Value.ocaml2cduce_option $lid:to_cd_fun t$ $e$ >>

  | _ -> assert false

and tuple_to_cd tl vars = List.map2 (fun t id -> to_cd <:expr< $lid:id$ >> t) tl vars

(* CDuce -> OCaml conversions *)



and to_ml e t =
(*  Format.fprintf Format.err_formatter "to_ml %a@."
    Mltypes.print t;  *)
  if t.recurs > 0 then <:expr< $lid:to_ml_fun t$ $e$ >>
  else to_ml_descr e t.def

and to_ml_descr e = function
  | Link t -> to_ml e t
  | Arrow (l,t,s) -> 
      (* let y = <...> in fun ~l:x -> s(Eval.eval_apply y (t(x))) *)
      protect e 
      (fun y ->
	 let x = mk_var () in
	 let arg = to_cd <:expr< $lid:x$ >> t in
	 let res = to_ml <:expr< Eval.eval_apply $y$ $arg$ >> s in
	 abstr_lab l x res
      )

  | Tuple tl -> 
      (* let (x1,r) = Value.get_pair <...> in
         let (x2,r) = Value.get_pair r in
         ...
         let (xn-1,xn) = Value.get_pair r in
	 (t1(x1),...,tn(xn)) *)

      let vars = mk_vars tl in
      let el = tuple_to_ml tl vars in
      matches e <:expr< ($list:el$) >> vars
  | PVariant l ->
      (* match Value.get_variant <...> with 
	 | "A",None -> `A 
	 | "B",Some x -> `B (t(x))
	 | _ -> assert false
      *)
      let cases = 
	List.map 
	  (function 
	     | (lab,None) -> 
		 <:patt< ($str: String.escaped lab$, None) >>,
		 <:expr< `$lid:lab$ >>
	     | (lab,Some t) ->
		 let x = mk_var () in
		 let ex = <:expr< $lid:x$ >> in
		 <:patt< ($str: String.escaped lab$, Some $lid:x$) >>,
		 <:expr< `$lid:lab$ $to_ml ex t$ >>
	  ) l in
      let cases = cases @ [ <:patt< _ >>, <:expr< assert False >> ] in
      pmatch <:expr< Value.get_variant $e$ >> cases
  | Variant (_,l,false) ->
      failwith "Private Sum type"
  | Variant (p,l,true) ->
      (* match Value.get_variant <...> with 
	 | "A",None -> P.A 
	 | "B",Some x -> let (x1,r) = x in ... 
      *)
      let cases = 
	List.map 
	  (function 
	     | (lab,[]) -> 
		 <:patt< ($str: String.escaped lab$, None) >>,
		 (match lab with (* Stupid Camlp4 *)
		    | "true" -> <:expr< True >>
		    | "false" -> <:expr< False >>
		    | lab -> <:expr< $lid:p^lab$ >>)
	     | (lab,[t]) ->
		 let x = mk_var () in
		 let ex = <:expr< $lid:x$ >> in
		 <:patt< ($str: String.escaped lab$, Some $lid:x$) >>,
		 <:expr< $lid:p^lab$ $to_ml ex t$ >>
	     | (lab,tl) ->
		 let vars = mk_vars tl in
		 let el = tuple_to_ml tl vars in
		 let x = mk_var () in
		 <:patt< ($str: String.escaped lab$, Some $lid:x$) >>,
		 matches <:expr< $lid:x$ >> 
		         <:expr< $lid:p^lab$ ($list:el$) >> vars
	  ) l in
      let cases = cases @ [ <:patt< _ >>, <:expr< assert False >> ] in
      pmatch <:expr< Value.get_variant $e$ >> cases
  | Record (_,l,false) ->
      failwith "Private Record type"
  | Record (p,l,true) ->
      (* let x = <...> in
	 { P.l1 = t1(Value.get_field x "l1"); ... } *)
      protect e 
      (fun x ->
	 let l = 
	   List.map
	     (fun (lab,t) ->
		(<:patt< $lid:p^lab$>>,
		 to_ml 
		 <:expr< Value.get_field $x$ $label_ascii lab$ >> t)) l in
	 <:expr< {$list:l$} >>)

  | Abstract "int" -> <:expr< Value.cduce2ocaml_int $e$ >>
  | Abstract "char" -> <:expr< Value.cduce2ocaml_char $e$ >>
  | Abstract "string" -> <:expr< Value.cduce2ocaml_string $e$ >>
  | Abstract s -> <:expr< Value.get_abstract $e$ >>
  | Builtin ("list",[t]) ->
      (* List.rev_map fun_t (Value.get_sequence_rev <...> *)
      <:expr< List.rev_map $lid:to_ml_fun t$ (Value.get_sequence_rev $e$) >>
  | Builtin ("array",[t]) ->
      (* List.rev_map fun_t (Value.get_sequence_rev <...> *)
      <:expr< Array.of_list (List.rev_map $lid:to_ml_fun t$ (Value.get_sequence_rev $e$)) >>
  | Builtin ("Pervasives.ref",[t]) ->
      (* ref t(Eval.eval_apply (Value.get_field <...> "get") Value.nil)  *)
      let e = <:expr< Value.get_field $e$ $label_ascii "get"$ >> in
      let e = <:expr< Eval.eval_apply $e$ Value.nil >> in
      <:expr< Pervasives.ref $to_ml e t$ >>
  | Builtin ("Big_int.big_int", []) -> 
      <:expr< Value.cduce2ocaml_bigint $e$ >>
  | Builtin ("Cduce_lib.Value.t", []) -> e
  | Builtin ("Cduce_lib.Encodings.Utf8.t", []) -> 
      <:expr< Value.cduce2ocaml_string_utf8 $e$ >>
  | Builtin ("Cduce_lib.Atoms.V.t", []) ->
      <:expr< Value.cduce2ocaml_atom $e$ >>
  | Builtin ("unit", []) -> <:expr< ignore $e$ >>
  | Builtin ("option", [t]) ->
      <:expr< Value.cduce2ocaml_option $lid:to_ml_fun t$ $e$ >>
  | Var _ -> e
  | _ -> assert false

and tuple_to_ml tl vars = List.map2 (fun t id -> to_ml <:expr< $lid:id$ >> t) tl vars


let to_ml_done = IntHash.create 13
let to_cd_done = IntHash.create 13

let global_transl () = 
  let defs = ref [] in
  let rec aux hd tl gen don fun_name to_descr =
    gen := tl;
    if not (IntHash.mem don hd.uid) then (
      IntHash.add don hd.uid ();
      let p = <:patt< $lid:fun_name hd$ >> in
      let e = <:expr< fun x -> $to_descr <:expr< x >> hd.def$ >> in
      defs := (p,e) :: !defs
    );
    loop ()
  and loop () = match !to_cd_gen,!to_ml_gen with
    | hd::tl,_ -> aux hd tl to_cd_gen to_cd_done to_cd_fun_name to_cd_descr
    | _,hd::tl -> aux hd tl to_ml_gen to_ml_done to_ml_fun_name to_ml_descr
    | [],[] -> ()
  in
  loop ();
  !defs

(* Check type constraints and generate stub code *)

let err_ppf = Format.err_formatter

let exts = ref []

let check_value ty_env c_env (s,caml_t,t) =
  (* Find the type for the value in the CDuce module *)
  let id = (Ns.empty, U.mk s) in
  let vt = 
    try Typer.find_value id ty_env
    with Not_found ->
      Format.fprintf err_ppf
      "The interface exports a value %s which is not available in the module@." s;
      exit 1
  in
  (* Compute expected CDuce type *)
  let et = Types.descr (typ t) in

  (* Check subtyping *)
  if not (Types.subtype vt et) then
    (
      Format.fprintf
       err_ppf
       "The type for the value %s is invalid@\n\
        Expected Caml type:@[%a@]@\n\
        Expected CDuce type:@[%a@]@\n\
        Inferred type:@[%a@]@."
       s
       print_ocaml caml_t
       Types.Print.print et
       Types.Print.print vt;
      exit 1
    );
   
  (* Generate stub code *)
  let x = mk_var () in
  let slot = Compile.find_slot id c_env in
  let e = to_ml <:expr< slots.($int:string_of_int slot$) >> t in
  <:patt< $uid:s$ >>, <:expr< C.$uid:x$ >>, (<:patt< $uid:x$ >>, e)

let stub ty_env c_env exts values mk prolog =
  gen_types := false;
  let items = List.map (check_value ty_env c_env) values in

  let exts = List.rev_map (fun (s,t) -> to_cd <:expr< $lid:s$ >> t) exts in
  let g = global_transl () in

  let types = get_registered_types () in
  let raw = mk types in

  let items_def = List.map (fun (_,_,d) -> d) items in
  let items_expr = List.map (fun (_,e,_) -> e)  items in
  let items_pat = List.map (fun (p,_,_) -> p) items in

  let m = 
    [ <:str_item< open Cduce_lib >>;
      <:str_item< Config.init_all () >>;
      <:str_item< 
	value (types,set_externals,slots,run) = 
	Librarian.ocaml_stub $str:String.escaped raw$ >> ] @
      (if g = [] then [] else [ <:str_item< value rec $list:g$ >> ]) @    
      [ <:str_item< set_externals [|$list:exts$|] >>;
	<:str_item< run () >> ] @
      (if items = [] then [] else [ <:str_item< value $list:items_def$ >> ]) in

  let items_expr = 
    match items_expr with 
      | [] -> <:expr< () >> 
      | l -> <:expr< ($list:l$) >> in

  let str_items = 
    [ <:str_item< 
      value ($list:items_pat$) = 
	let module C = struct 
	  $list:m$ 
	end in $items_expr$ >>, (Lexing.dummy_pos, Lexing.dummy_pos) ] in

  print_endline prolog;
  !Pcaml.print_implem str_items
(*  let exe = Filename.concat (Filename.dirname Sys.argv.(0)) "cdo2ml" in
  let oc = Unix.open_process_out exe in
  Marshal.to_channel oc str_items [];
  flush oc;
  ignore (Unix.close_process_out oc) *)


let stub_ml name ty_env c_env exts mk =
  try
    let name = String.capitalize name in
    let exts = match (Obj.magic exts : (string * Mltypes.t) list option) with
      | None -> []
      | Some exts -> List.iter (fun (_,t) -> Mltypes.reg_uid t) exts; exts in
    (* First, read the description of ML types for externals.
       Don't forget to call reg_uid to avoid uid clashes...
       Do that before reading the cmi. *)
    let (prolog, values) = 
      try Mltypes.read_cmi name
      with Not_found ->  ("",[]) in
    stub ty_env c_env exts values mk prolog
  with Mltypes.Error s -> raise (Location.Generic s)


let register b s args = 
  try
    let (t,n) = Mltypes.find_value s in
    let m = List.length args in
    if n <> m then
      Location.raise_generic
	(Printf.sprintf 
	   "Wrong arity for external symbol %s (real arity = %i; given = %i)" s n m);
    let i = if b then
      let i = List.length !exts in
      exts := (s, t) :: !exts;
      i
    else
      0 in
    
    vars := Array.of_list args;
    let cdt = Types.descr (typ t) in
    vars := [| |];
    i,cdt
  with Not_found -> 
    Location.raise_generic
      (Printf.sprintf "Cannot resolve ocaml external %s" s)

(* Generation of wrappers *)

let wrapper values =
  gen_types := false;
  let exts = List.rev_map 
    (fun (s,t) ->
       let v = to_cd <:expr< $lid:s$ >> t in
       <:str_item< 
	 Librarian.register_static_external $str:String.escaped s$ $v$ >>)
    values in
  let g = global_transl () in

  let m = if g = [] then exts else <:str_item< value rec $list:g$ >>::exts in
  let m = [ <:str_item< open Cduce_lib >>;
	    <:str_item< Config.init_all () >>] @ m in

  <:str_item< declare $list:m$ end >>


let gen_wrapper vals =
  try
    let values = List.fold_left
      (fun accu s ->
	 try (s,fst (Mltypes.find_value s)) :: accu
	 with Not_found ->
	   let vals = 
	     try Mltypes.load_module s
	     with Not_found ->
	       failwith ("Cannot resolve " ^ s)
	   in
	   vals @ accu
      ) [] vals in

    wrapper values
  with Mltypes.Error s -> raise (Location.Generic s)

let make_wrapper fn = 
  let ic = open_in fn in
  let v = ref [] in
  (try while true do 
     let s = input_line ic in
     if s <> "" then
       match s.[0] with
	 | 'A'..'Z' -> v := s :: !v 
	 | '#' -> ()
	 | _ -> failwith "Error in primitive file: names must start with a capitalized letter"
   done 
   with End_of_file -> ());
  let s = gen_wrapper !v in
  !Pcaml.print_implem [ s,_loc ];
  print_endline "let () = Librarian.obj_path := [";
  List.iter (fun s -> Printf.printf "  %S;\n" s) !Librarian.obj_path;
  print_endline " ];;";
  print_endline "let () = Run.main ();;"


(* Dynamic coercions *)


(*
let to_cd_dyn = function
  | Link t -> to_cd_dyn e t
  | Arrow (l,t,s) ->
      let tt = Types.descr (typ t) in
      let ss = Types.descr (typ s) in
      let tf = to_ml_dyn t in
      let sf = to_cd_dyn t in
      (fun (f : Obj.repr) ->
	 let f = (Obj.magic f : Obj.repr -> Obj.repr) in
	 Value.Abstraction ([tt,ss],fun x -> sf (f (tf x))))
  | Tuple tl -> 
      let fs = List.map to_cd_dyn tl in
      (fun (x : Obj.repr) ->
	 let x = (Obj.magic x : Obj.repr array) in
	 let rec aux i = function
	   | [] -> assert false
	   | [f] -> f x.(i)
	   | f::tl -> Value.Pair (f x.(i), aux (succ i) tl) in
	 aux 0 fs)
*)


let register () =
  Typer.has_ocaml_unit :=
    (fun cu -> Mltypes.has_cmi (U.get_str cu));
  Librarian.stub_ml := stub_ml;
  Externals.register := register;
  Externals.ext_info := (fun () -> Obj.magic !exts);
  Librarian.make_wrapper := make_wrapper

let () =
  Config.register 
    "ocaml" 
    "OCaml interface" 
    register
