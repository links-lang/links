(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

#load "pa_extend.cmo";;

open Location
open Ast
open Ident
open Printf

(*
let ()  = Grammar.error_verbose := true
*)

let tloc (i,j) = (i.Lexing.pos_cnum,j.Lexing.pos_cnum)
let nopos = (Lexing.dummy_pos, Lexing.dummy_pos)


let mk loc x = Location.mk (tloc loc) x

exception Error of string
let error (i,j) s = Location.raise_loc i j (Error s)
let error loc s = error (tloc loc) s

let gram    = Grammar.gcreate Ulexer.lex

let id_dummy = U.mk "$$$"

let ident s =
  let b = Buffer.create (String.length s) in
  let rec aux i =
    if (i = String.length s) then Buffer.contents b
    else match s.[i] with
      | '\\' -> assert (s.[i+1] = '.'); Buffer.add_char b '.'; aux (i+2)
      | c -> Buffer.add_char b c; aux (i+1)
  in
  aux 0

let label s = U.mk (ident s)
let ident s = U.mk (ident s)

let prog    = Grammar.Entry.create gram "prog"
let top_phrases   = Grammar.Entry.create gram "toplevel phrases"
let expr    = Grammar.Entry.create gram "expression"
let pat     = Grammar.Entry.create gram "type/pattern expression"
let regexp  = Grammar.Entry.create gram "type/pattern regexp"
let keyword = Grammar.Entry.create gram "keyword"
		
let lop pos = loc_of_pos (tloc pos)
let exp pos e = LocatedExpr (lop pos,e)

let rec multi_prod loc = function
  | [ x ] -> x
  | x :: l -> mk loc (Prod (x, multi_prod loc l))
  | [] -> assert false
      
let rec tuple = function
  | [ x ] -> x
  | x :: l -> Pair (x, tuple l)
  | [] -> assert false

let tuple_queue = 
  List.fold_right (fun x q -> Pair (x, q))


let char = mknoloc (Internal (Types.char Chars.any))
let string_regexp = Star (Elem char)
	       
let seq_of_string s =
  let s = Encodings.Utf8.mk s in
  let rec aux i j =
    if Encodings.Utf8.equal_index i j then []
    else let (c,i) = Encodings.Utf8.next s i in c :: (aux i j)
  in
  aux (Encodings.Utf8.start_index s) (Encodings.Utf8.end_index s)


let parse_char loc s =
  match seq_of_string s with
    | [ c ] -> c
    | _ -> error loc "Character litteral must have length 1"

let include_stack = ref []

let protect_exn f g =
  try let x = f () in g (); x
  with e -> g (); raise e

let localize_exn f = 
  try f ()
  with
  | Stdpp.Exc_located (_, (Location _ as e)) -> raise e
(*  | Stdpp.Exc_located ((i,j), e) -> raise_loc i j e *)
  | Stdpp.Exc_located ((i,j), e) -> raise_loc i.Lexing.pos_cnum j.Lexing.pos_cnum e

let is_fun_decl =
   Grammar.Entry.of_parser gram "[is_fun_decl]"
     (fun strm ->
       match Stream.npeek 3 strm with
	 | [ ("", "fun"); ("IDENT", _); ("", "(") ]
	 | [ ("IDENT", _) ; ("", "(") ; _ ] -> ()
	 | _ -> raise Stream.Failure
     )

let is_capture =
   Grammar.Entry.of_parser gram "[is_capture]"
     (fun strm ->
       match Stream.npeek 2 strm with
	 | [ ("IDENT", _) ; ("", "::") ; _ ] -> ()
	 | _ -> raise Stream.Failure
     )


let if_then_else cond e1 e2 = Match (cond, [pat_true,e1; pat_false,e2])
 
let logical_and e1 e2 = if_then_else e1 e2 cst_false
let logical_or e1 e2 = if_then_else e1 cst_true e2
let logical_not e = if_then_else e cst_false cst_true

let apply_op2_noloc op e1 e2 = Apply (Apply (Var (ident op), e1), e2)
let apply_op2 loc op e1 e2 = exp loc (apply_op2_noloc op e1 e2)
 
let set_ref e1 e2 = Apply (Dot (e1, U.mk "set"), e2)
let get_ref e = Apply (Dot (e, U.mk "get"), cst_nil)
let let_in e1 p e2 =  Match (e1, [p,e2])
let seq e1 e2 = let_in e1 pat_nil e2
let concat e1 e2 = apply_op2_noloc "@" e1 e2

EXTEND
  GLOBAL: top_phrases prog expr pat regexp keyword;

  top_phrases: [
    [ l = LIST0 phrase; ";;" -> List.flatten l  ]
  ];

  prog: [
    [ l = LIST0 [ p = phrase ; OPT ";;" -> p ]; EOI -> List.flatten l ]
  ];

  phrase: [
    [ (f,p,e) = let_binding -> 
	if f then [ mk _loc (FunDecl e) ] else
	  [ mk _loc (LetDecl (p,e)) ]
    | (_,p,e1) = let_binding; "in"; e2 = expr LEVEL "top"->
        [ mk _loc (EvalStatement (exp _loc (let_in e1 p e2))) ]
    | "type"; x = located_ident; "="; t = pat -> [ mk _loc (TypeDecl (x,t)) ]
    | "using"; name = IDENT; "="; cu = [ IDENT | STRING2 ] ->
	[ mk _loc (Using (U.mk name, U.mk cu)) ]
    | "open"; ids = LIST1 [ IDENT | keyword ] SEP "." -> 	  
	let ids = List.map (fun x -> ident x) ids in
	[ mk _loc (Open ids) ]
    | "schema"; name = IDENT; "="; uri = STRING2 ->
	protect_op "schema";
        [ mk _loc (SchemaDecl (U.mk name, uri)) ]
    | n = namespace_binding ->
	let d = match n with
	    | `Prefix (name,ns) ->  Namespace (name, ns)
	    | `Keep b -> KeepNs b in
	[ mk _loc d ]
    | n = namespace_binding; "in"; e2 = expr LEVEL "top" ->
	let e = 
	  match n with
	    | `Prefix (name,ns) -> NamespaceIn (name, ns, e2)
	    | `Keep b -> KeepNsIn (b,e2)
	in
	[ mk _loc (EvalStatement (exp _loc e)) ]
    | "debug"; d = debug_directive -> [ mk _loc (Directive (`Debug d)) ]
    | "#"; IDENT "verbose" -> [ mk _loc (Directive `Verbose) ]
    | "#"; IDENT "silent" -> [ mk _loc (Directive `Silent) ]
    | "#"; IDENT "utf8" -> Ulexer.enc := Ulexing.Utf8; [ ]
    | "#"; IDENT "latin1" -> Ulexer.enc := Ulexing.Latin1; [ ]
    | "#"; IDENT "ascii" -> Ulexer.enc := Ulexing.Ascii; [ ]
    | "#"; IDENT "quit" -> [ mk _loc (Directive `Quit) ]
    | "#"; IDENT "env" -> [ mk _loc (Directive `Env) ]
    | "#"; IDENT "print_type"; t = pat ->
        [ mk _loc (Directive (`Print_type t)) ]
    | "#"; IDENT "dump_value"; e = expr -> [ mk _loc (Directive (`Dump e)) ]
    | "#"; IDENT "reinit_ns" -> [ mk _loc (Directive `Reinit_ns) ]
    | "#"; IDENT "help" -> [ mk _loc (Directive `Help) ]
    | "#"; IDENT "builtins" -> [ mk _loc (Directive `Builtins) ]
    | "include"; s = STRING2 ->
	let s = 
	  if Filename.is_relative s 
	  then Filename.concat (Location.current_dir ()) s
	  else s in
	protect_op "File inclusion";
	(* avoid looping; should issue an error ? *)
	(* it is possible to have looping with x/../x/../x/.. ....
	   Need to canonicalize filename *)
	if List.mem s !include_stack then [] 
	else (
	  include_stack := s :: !include_stack;
	  Location.push_source (`File s);
	  let saved_enc = !Ulexer.enc in
	  Ulexer.enc := Ulexing.Latin1;
	  protect_exn
	    (fun () ->
	       let chan = open_in s in
	       protect_exn
		 (fun () ->
		    let input = Stream.of_channel chan in
		    localize_exn (fun () -> Grammar.Entry.parse prog input))
		 (fun () -> close_in chan))
	    (fun () ->
	       Ulexer.enc := saved_enc;
	       Location.pop_source ();
	       include_stack := List.tl !include_stack)
	)
    ] | 
    [ e = expr -> [ mk _loc (EvalStatement e) ]
    ]
  ];

  debug_directive: [
    [ IDENT "filter"; t = pat; p = pat -> `Filter(t,p)
    | IDENT "accept"; p = pat -> `Accept p
    | IDENT "compile"; t = pat; p = LIST1 pat -> `Compile (t,p)
    | IDENT "sample"; t = pat -> `Sample t
    | IDENT "subtype"; t1 = pat; t2 = pat -> `Subtype (t1,t2)
    | IDENT "single"; t = pat -> `Single t
    ]
  ];

  keyword: [
    [ a = 
	[ "map" | "match" | "with" | "try" | "xtransform"
	| "if" | "then"	| "else"
	| "transform" | "fun" | "in"
	| "let" | "type" | "debug" | "include"
        | "and" | "or" | "validate" | "schema" | "namespace" | "ref" | "alias"
	| "not" | "as" | "where" | "select" | "from"
	]
	-> a
    ]
  ];

  expr: [
    "top" RIGHTA
    [ "match"; e = SELF; "with"; b = branches -> 
	exp _loc (Match (e,b))
    | "try"; e = SELF; "with"; b = branches -> 
	exp _loc (Try (e,b))
    | "map"; e = SELF; "with"; b = branches -> 
	exp _loc (Map (e,b))
    | "xtransform"; e = SELF; "with"; b = branches -> 
	exp _loc (Xtrans (e,b))
    | "if"; e = SELF; "then"; e1 = SELF; "else"; e2 = SELF ->
	exp _loc (if_then_else e e1 e2)
    | "transform"; e = SELF; "with"; b = branches -> 
	exp _loc (Transform (e,b))
    | "validate"; e = SELF; "with"; (schema, typ) = schema_ref ->
        exp _loc (Validate (e, [schema;typ]))
    | "select"; e = SELF; "from"; 
      l = LIST1 [ x = pat ; "in"; e = expr -> (x,e)] SEP "," ;
      cond = [ "where"; c = LIST1 [ expr ] SEP "and" -> c 
	     | -> [] ] -> exp _loc (SelectFW (e,l,cond)) 
    | "fun"; (f,a,b) = fun_decl ->
	exp _loc (Abstraction { fun_name = f; fun_iface = a; fun_body = b })
    | (_,p,e1) = let_binding; "in"; e2 = expr LEVEL "top"->
        exp _loc (let_in e1 p e2)
    | n = namespace_binding; "in"; e2 = expr LEVEL "top" ->
	(match n with
	   | `Prefix (name,ns) -> exp _loc (NamespaceIn (name, ns, e2))
	   | `Keep f -> exp _loc (KeepNsIn (f,e2)))
    | e = expr; ":"; p = pat ->
	exp _loc (Forget (e,p))
    | e = expr; ":"; "?"; p = pat ->
	exp _loc (Check (e,p))
    | e1 = expr; ";"; e2 = expr ->
	exp _loc (seq e1 e2) 
    | "ref"; p = pat; e = expr ->
	exp _loc (Ref (e,p))
    | "not"; e = expr -> exp _loc (logical_not e)
    ]
    |
    [ e1 = expr; ":="; e2 = expr -> exp _loc (set_ref e1 e2)
    ]
    | 
    [ e1 = expr; op = ["=" | "<=" | "<<" | ">>" | ">=" ]; e2 = expr -> 
	let op = match op with
	  | "<<" -> "<"
	  | ">>" -> ">"
	  | s -> s in
	apply_op2 _loc op e1 e2
    ]

    | 
    [ e1 = expr; op = ["+" | "-" | "@" ]; e2 = expr -> apply_op2 _loc op e1 e2
    | e1 = expr; ["||" | "or"]; e2 = expr -> exp _loc (logical_or e1 e2)
    | e = expr; "\\"; l = [IDENT | keyword ] -> 
	exp _loc (RemoveField (e, label l)) 
    ]
    |
    [ e1 = expr; op = ["*"]; e2 = expr -> apply_op2 _loc op e1 e2
    | e1 = expr; "&&"; e2 = expr -> exp _loc (logical_and e1 e2)
    | e = expr; op = "/"; p = pat LEVEL "simple" ->
	(* transform e with <(Atom)>[($$$::t|_)*] -> [$$$] *)
	let tag = mk _loc (Internal (Types.atom (Atoms.any))) in
	let att = mk _loc (Internal Types.Record.any) in
	let any = mk _loc (Internal Types.any) in
	let re = Star(Alt(SeqCapture(noloc,id_dummy,Elem p), Elem any)) in
	let ct = mk _loc (Regexp re) in
        let p = mk _loc (XmlT (tag, multi_prod _loc [att;ct])) in
	exp _loc (Transform (e,[p, Var id_dummy]))
    | e = expr; "/@"; a = [IDENT|keyword] ->
	(* transform e with <(Atom) {a=$$$}>_ -> [$$$] *)
        let tag = mk _loc (Internal (Types.atom Atoms.any)) in
        let any = mk _loc (Internal Types.any) in
        let att = mk _loc (Record
			    (true, [(label a,
				     (mk _loc (PatVar [id_dummy]),
				      None))])) in
        let p = mk _loc (XmlT (tag, multi_prod _loc [att;any])) in
        let t = (p, Pair (Var id_dummy,cst_nil)) in
        exp _loc (Transform (e,[t]))
     | e = expr; "//" ; p = pat ->
	 (*
	   let $stack=ref [p*] [] in
	   let _ = xtransform e with $$$ & p -> $stack := !$stack @ $$$ in
	   !stack;; 
	 *)
	 let stk = U.mk "$stack" in
	 let assign = 
	   set_ref 
	     (Var stk)
	     (concat (get_ref (Var stk)) (Pair (Var id_dummy,cst_nil))) in
	 let capt = mk _loc (And (mk _loc (PatVar [U.mk "$$$"]),p)) in
	 let xt = Xtrans (e,[capt,assign]) in
	 let rf = Ref (cst_nil, mk _loc (Regexp (Star(Elem p)))) in
	 let body = 
	   let_in rf (mk _loc (PatVar [stk]))
	     (let_in xt (mk _loc (Internal Types.any)) (get_ref (Var stk)))
	 in
	 exp _loc body
    ]
    | [ 
      e1 = SELF; IDENT "div"; e2 = expr -> apply_op2 _loc "/" e1 e2
    | e1 = SELF; IDENT "mod"; e2 = expr -> apply_op2 _loc "mod" e1 e2
    | e1 = SELF; e2 = expr -> exp _loc (Apply (e1,e2))
    ]

    | "no_appl" 

    [ e = expr;  "."; l = [IDENT | keyword ]; 
      tyargs = [ "with"; "{"; tyargs = LIST0 pat; "}" -> Some tyargs
      | -> None ] ->
	let e = Dot (e,label l) in
	match tyargs with None -> exp _loc e | Some tyargs -> 
	  exp _loc (TyArgs (e,tyargs))
    ]  
    | [ 
      "("; l = LIST1 expr SEP ","; ")" -> exp _loc (tuple l)
    | "[";  l = LIST0 seq_elem; e = OPT [ ";"; e = expr -> e ]; 
      loc_end = ["]" -> _loc] ->
	let e = match e with Some e -> e | None -> cst_nil in
        let e = exp loc_end e in
        let (_,loc_end) = loc_end in
	let l = List.fold_right 
		  (fun x q ->
		     match x with
		       | `String (loc,i,j,s) -> exp loc (String (i,j,s,q))
		       | `Elems ((loc,_),x) -> exp (loc,loc_end) (Pair(x,q))
		       | `Explode x -> concat x q
		  ) l e
	in
	exp _loc l
    | "<"; t = [ "("; e = expr; ")" -> e
	       | a = tag -> exp _loc a
	       ];
	a = expr_attrib_spec; ">"; c = expr ->
	  exp _loc (Xml (t, Pair (a,c)))
    | "{"; r = expr_record_spec; "}" -> r
    | s = STRING2 ->
	let s = U.mk s in
	exp _loc (String (U.start_index s, U.end_index s, s, cst_nil))
    | a = IDENT -> exp _loc (Var (ident a))
    | "!"; e = expr -> exp _loc (get_ref e)
    | i = INT -> exp _loc (Integer (Intervals.V.mk i))
    | "`"; a = tag -> a
    | c = char -> exp _loc (Char c)
    ]

  ];

  tag: [ [ a = [ IDENT | keyword ] -> exp _loc (Atom (ident a)) ] ];

  tag_type: [
    [ "_" ->  mk _loc (Internal (Types.atom Atoms.any))
    | a = [ IDENT | keyword ] -> mk _loc (Cst (Atom (ident a)))
    | t = ANY_IN_NS -> mk _loc (NsT (ident t)) 
    ]
  ];

  seq_elem: [
    [ x = STRING1 -> 
	let s = U.mk x in
	`String (_loc, U.start_index s, U.end_index s, s)
    | e = expr LEVEL "no_appl" -> `Elems (_loc,e)
    | "!"; e = expr LEVEL "no_appl" -> `Explode e
    ]
  ];
	
  namespace_binding: [
    [ "namespace"; r = [
	[ name = 
	    [ name = [ IDENT | keyword ]; "=" -> ident name
	    | -> U.mk "" ];
	  ns = ns_expr -> `Prefix (name,ns)
	| IDENT "on" -> `Keep true
	| IDENT "off" -> `Keep false ]
      ] -> r ]
  ];

  ns_expr: [
    [ uri = STRING2 -> `Uri (Ns.Uri.mk (ident uri))
    | ids = LIST1 [ IDENT | keyword ] SEP "." -> 	  
	let ids = List.map (fun x -> ident x) ids in
	`Path ids ]
  ];

  
  let_binding: [
    [ "let"; is_fun_decl; OPT "fun"; (f,a,b) = fun_decl ->
	let f = match f with Some x -> x | None -> assert false in
	let p = mk _loc (PatVar [snd f]) in
	let abst = { fun_name = Some f; fun_iface = a; fun_body = b } in
        let e = exp _loc (Abstraction abst) in
        (true,p,e)
    | "let"; p = pat; "="; e = expr -> (false,p,e)
    | "let"; p = pat; ":"; t = pat; "="; e = expr -> (false,p, Forget (e,t))
    | "let"; p = pat; ":"; "?"; t = pat; "="; e = expr -> 
	(false,p, Check (e,t))
    ] 
  ];

 fun_decl_after_lparen: [
(* need an hack to do this, because both productions would
   match   [ OPT IDENT; "("; pat ] .... *)
   [ p1 = pat LEVEL "no_arrow";
     res = [ "->"; p2 = pat;
	     a = [ ";"; a = LIST0 arrow SEP ";" -> a | -> [] ];
	     ")"; b = branches -> `Classic (p2,a,b)
	   | ":"; targ1 = pat;
	     args = LIST0 [ ","; arg = pat; ":"; targ = pat -> (arg,targ) ]; 
	     ")";
	     others = LIST0 
			[ "(";
			  args = 
			    LIST1 
			      [ arg = pat; ":"; targ = pat -> (arg,targ) ]
			      SEP ",";
			  ")" -> args ];
	     ":"; tres = pat ; 
	     "="; body = expr ->
	       `Compact (targ1,args,others,tres,body)
	   ] ->
       match res with
	 | `Classic (p2,a,b) -> (p1,p2)::a,b
	 | `Compact (targ1,args,others,tres,body) ->
	     let mkfun args =
	       multi_prod nopos (List.map snd args),
	       multi_prod nopos (List.map fst args)
	     in
	     let (tres,body) =
	       List.fold_right
		 (fun args (tres,body) ->
		    let (targ,arg) = mkfun args in
		    let e = Abstraction 
			      { fun_name = None; fun_iface = [targ,tres]; 
				fun_body = [arg,body] } in
		    let t = mknoloc (Arrow (targ,tres)) in
		    (t,e)
		 )
		 others (tres,body) in
	     let (targ,arg) = mkfun ((p1,targ1) :: args) in
	     [(targ,tres)],[(arg,body)]
	      ] ];


 fun_decl: [
   [ f = OPT located_ident; "("; (a,b) = fun_decl_after_lparen ->
       (f,a,b)
   ]
 ];

 arrow: [
    [ t1 = pat LEVEL "no_arrow"; "->"; t2 = pat -> (t1,t2)]
  ];

  branches: [
    [ OPT "|"; l = LIST1 branch SEP "|" -> l ]
  ];

  branch: [
    [ p = pat LEVEL "no_arrow"; "->"; e = expr -> (p,e) ]
  ];

	  
  regexp: [ 
    [ x = regexp; "|"; y = regexp -> 
	match (x,y) with
	  | Elem x, Elem y -> Elem (mk _loc (Or (x,y)))
	  | _ -> Alt (x,y) 
    ]
  | [ x = regexp; y = regexp -> Seq (x,y) ]
  | [ x = regexp; "&"; y = regexp ->
	match (x,y) with
	  | Elem x, Elem y -> Elem (mk _loc (And (x,y)))
	  | _ -> error _loc "Conjunction not allowed in regular expression"
    ]
  | [ a = IDENT; "::"; x = regexp -> SeqCapture (lop _loc,ident a,x) ] 
  | [ x = regexp; "*" -> Star x
    | x = regexp; "*?" -> WeakStar x
    | x = regexp; "+" -> Seq (x, Star x)
    | x = regexp; "+?" -> Seq (x, WeakStar x)
    | x = regexp; "?" ->  Alt (x, Epsilon)
    | x = regexp; "??" -> Alt (Epsilon, x) 
    | x = regexp; "**"; i = INT ->
	let rec aux i accu =
	  if (i = 0) then accu
	  else aux (pred i) (Seq (x, accu))
	in
	let i = 
	  try 
	    let i = int_of_string i in
	    if (i > 1024) then raise Exit else i
	      (* We cannot handle type that huge... *)
	  with Failure _ | Exit -> error _loc "Repetition number too large"
	in
	if (i <= 0) then 
	  error _loc "Repetition number must be a positive integer";
	aux i Epsilon
    ]
  | [ "("; x = LIST1 regexp SEP ","; ")" ->
	(match x with
	  | [ x ] -> x
	  | _ ->
	      let x = 
		List.map 
		  (function
		     | Elem x -> x
		     | _ -> error _loc 
			 "Mixing regular expressions and products")
		  x in
	      Elem (multi_prod _loc x))
    | "("; a = IDENT; ":="; c = expr; ")" -> 
	Elem (mk _loc (Constant ((ident a,c))))
    | "/"; p = pat LEVEL "simple" -> Guard p
    | IDENT "PCDATA" -> string_regexp
    | i = STRING1; "--"; j = STRING1 ->
	let i = Chars.V.mk_int (parse_char _loc i)
	and j = Chars.V.mk_int (parse_char _loc j) in
        Elem (mk _loc (Internal (Types.char (Chars.char_class i j))))
    |  s = STRING1 ->
	List.fold_right
	  (fun c accu -> 
	     let c = Chars.V.mk_int c in
	     let c = Chars.atom c in
	     Seq (Elem (mknoloc (Internal (Types.char c))), accu))
	  (seq_of_string s)
	  Epsilon ]
    | [ e = pat LEVEL "simple" -> Elem e
    ]
  ];

  schema_ref: [
    [ schema = IDENT; "."; typ = [ IDENT | keyword ] -> (U.mk schema, ident typ)
    ]
  ];

  located_ident: [ [ a = [IDENT|keyword] -> (lop _loc,ident a) ] ];

  pat: [ 
      [ x = pat; "where"; 
        b = LIST1 [ (la,a) = located_ident; "="; y = pat -> 
		      (la,a,y) ] SEP "and"
            -> mk _loc (Recurs (x,b)) ]
    | RIGHTA [ x = pat; "->"; y = pat -> mk _loc (Arrow (x,y))
             | x = pat; "@"; y = pat -> mk _loc (Concat (x,y))
             | x = pat; "+"; y = pat -> mk _loc (Merge (x,y)) ]
    | "no_arrow" [ x = pat; "|"; y = pat -> mk _loc (Or (x,y)) ] 
    | "simple" [ x = pat; "&"; y = pat -> mk _loc (And (x,y)) 
	       | x = pat; "\\"; y = pat -> mk _loc (Diff (x,y)) ]
    | 
      [ "{"; r = record_spec; "}" -> r
      | "ref"; p = pat ->
	  let get_fun = mk _loc (Arrow (pat_nil, p)) 
	  and set_fun = mk _loc (Arrow (p, pat_nil))in
	  let fields = 
	    [ label "get", (get_fun, None); label "set", (set_fun, None) ] in
	  mk _loc (Record (false, fields))
      | "_" -> mk _loc (Internal Types.any)
      | "("; a = IDENT; ":="; c = expr; ")" -> 
	  mk _loc (Constant (ident a,c))
      | "!"; a = IDENT ->
	  mk _loc (Internal (Types.abstract (Types.Abstract.atom a)))
      | ids = LIST1 [ IDENT | keyword ] SEP "." ->
	  let ids = List.map (fun x -> ident x) ids in
	  mk _loc (PatVar ids)
      | i = INT ; "--"; j = INT -> 
          let i =  Intervals.V.mk i 
	  and j =  Intervals.V.mk j in
          mk _loc (Internal (Types.interval (Intervals.bounded i j)))
      | i = INT -> 
          let i =  Intervals.V.mk i  in
          mk _loc (Internal (Types.interval (Intervals.atom i)))
      | "*"; "--"; j = INT ->
	  let j =  Intervals.V.mk j in
          mk _loc (Internal (Types.interval (Intervals.left j)))
      | i = INT; "--"; "*" ->
	  let i = Intervals.V.mk i in
          mk _loc (Internal (Types.interval (Intervals.right i)))
      | i = char ->
          mk _loc (Internal (Types.char (Chars.char_class i i)))
      | i = char ; "--"; j = char ->
          mk _loc (Internal (Types.char (Chars.char_class i j)))
      | "`"; c = tag_type -> c
      | "("; l = LIST1 pat SEP ","; ")" -> multi_prod _loc l
      | "["; r = [ r = regexp -> r | -> Epsilon ];
             q = [ ";"; q = pat -> Some q
                 | -> None ]; 
             "]" -> 
	       let r = match q with
		 | Some q -> 	
		     let any = mk _loc (Internal (Types.any)) in
		     Seq(r,Seq(Guard q, Star (Elem any)))
		 | None -> r
	       in
	       mk _loc (Regexp r)
      | "<"; t =
            [ x = tag_type -> x
            | "("; t = pat; ")" -> t ];
	a = attrib_spec; ">"; c = pat ->
          mk _loc (XmlT (t, multi_prod _loc [a;c]))
      | s = STRING2 ->
	  let s = 
	    List.map 
	      (fun c -> 
		 mknoloc (Internal
			     (Types.char
				(Chars.atom
				   (Chars.V.mk_int c))))) 
	      (seq_of_string s) in
	  let s = s @ [mknoloc (Internal (Sequence.nil_type))] in
	  multi_prod _loc s
      ]
    
  ];

  or_else : [ [ OPT [ "else"; y = pat -> y ]  ] ];

  opt_field_pat: [ [ OPT [ "=";
                  o = [ "?" -> true | -> false]; 
                  x = pat;  y = or_else -> (o,x,y) ] ] ];

  record_spec:
    [ [ r = LIST0 [ l = [IDENT | keyword ]; f = opt_field_pat; OPT ";" ->
		      let (o,x,y) =
			match f with
			  | None -> (false, mknoloc (PatVar [ident l]), None)
			  | Some z -> z
		      in
		      let x = if o then mk _loc (Optional x) else x in
		      (label l, (x,y))
                  ]; op = [ ".." -> true | -> false ] ->
	  mk _loc (Record (op,r))
      ] ];
  
  char:
    [ 
      [ c = STRING1 -> Chars.V.mk_int (parse_char _loc c) ]
    ];
     

  attrib_spec: [
    [ r = record_spec -> r
    | "("; t = pat; ")" -> t 
    ] ];

  opt_field_expr: [ [ OPT [ "="; x = expr LEVEL "no_appl" -> x ] ] ];

  expr_record_spec:
    [ [ r = LIST0
	      [ l = [IDENT | keyword ]; 
		x = opt_field_expr; OPT ";" ->
		  let x = match x with Some x -> x | None ->  Var (ident l) in
		  (label l,x) ] 
	  ->
	  exp _loc (RecordLitt r)
      ] ];

  expr_attrib_spec: [
    [ e = expr_record_spec -> e
    | "("; e = expr; ")" -> e 
    ] ];
END

module Hook = struct
  let expr = expr
  let pat = pat
  let keyword = keyword
end

let pat = Grammar.Entry.parse pat
and expr = Grammar.Entry.parse expr
and prog = Grammar.Entry.parse prog
and top_phrases = Grammar.Entry.parse top_phrases

let sync () = 
  match !Ulexer.lexbuf with
    | None -> ()
    | Some lb ->
	let rec aux () =
	  match !Ulexer.last_tok with
	    | ("",";;") | ("EOI","") -> ()
	    | _ -> 
		Ulexer.last_tok := fst (Ulexer.token lb); 
		aux ()
	in
	aux ()

let sync () =
  try sync ()
  with Ulexing.Error -> ()
