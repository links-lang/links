open Parser.Hook
open Location
open Ast
open Ident
(********************************)
(* For options in driver/run.ml *)

let nooptim = ref false 

(********************************)


let exp pos e = LocatedExpr (loc_of_pos pos,e)


let ou (loc,e1,e2)= exp loc (Parser.logical_or e1 e2)
let et (loc,e1,e2)= exp loc (Parser.logical_and e1 e2)
let non (loc,e) = exp loc (Parser.logical_not e)


type 'a  boolFormula =
    True
  | False
  | Varb of 'a
  | Not of 'a boolFormula
  | Ou  of 'a boolFormula * 'a boolFormula
  | Et  of 'a boolFormula * 'a boolFormula
  | Im  of 'a boolFormula * 'a boolFormula
  | OuN of 'a boolFormula list
  | EtN of 'a boolFormula list

let rec ast_of_bool(c,loc)=
	match (c,loc) with
	|(True,loc) -> cst_true
	|(False,loc) -> cst_false
	|(Varb(e),loc) -> e
	|(Not(Varb(e)),loc) -> non(loc,e)
	|(OuN([varb]),loc) -> ast_of_bool(varb,loc)
	|(OuN(l1::l2),loc) -> ou(loc,ast_of_bool(l1,loc),ast_of_bool(OuN(l2),loc))
	|(EtN([l]),loc) -> ast_of_bool(l,loc)  
	|(EtN(l1::l2),loc) -> et(loc,ast_of_bool(l1,loc),ast_of_bool(EtN(l2),loc)) 
	 (*pour le cas non optimise, ... *)
	|(Et(e1,e2),loc) -> et(loc,ast_of_bool(e1,loc),ast_of_bool(e2,loc))
	|(Ou(e1,e2),loc) -> ou(loc,ast_of_bool(e1,loc),ast_of_bool(e2,loc))
	| (_,_) -> assert false


let rec string_of_ppat p =
	let rec string_of_regexp rg =
	match rg with
	|Elem(e) -> string_of_ppat e
	|Guard(e) -> "/" ^ (string_of_ppat e)
	|SeqCapture(_,id,rg) -> U.get_str id ^"::"^ string_of_regexp rg
	|Seq(r1,r2) -> string_of_regexp r1 ^ " "^string_of_regexp r2
	|Alt(r1,r2) -> " ("^string_of_regexp r1 ^"|"^ string_of_regexp r2^")"
	|Star r1 -> string_of_regexp r1^"* "
	|WeakStar r1 -> string_of_regexp r1^"+ "
	|Epsilon -> ""
       in match p with 
       {loc = loc ; descr = descr } 
           -> ( match  descr with
             |PatVar(_,id) -> U.get_str ( id) 
	     |Cst(Atom a) -> U.get_str a
	     |Internal(descr) -> 
	         if descr=Builtin_defs.true_type then "`true" 
	         else if descr=Builtin_defs.false_type then "`false"
		 else if Types.is_empty descr then "Empty"
                 else if Types.atom ( Atoms.any)=descr then "_" 
		 else if Types.Record.any=descr then "_" 
		 else if Types.any=descr then "_" 
                 else  "" 
		 		 
	     |Or(p1,p2) -> " ("^string_of_ppat p1 ^"|"^ string_of_ppat p2^")"
	     |And(p1,p2) -> string_of_ppat p1 ^"&"^ string_of_ppat p2
	     |Diff(p1,p2) -> string_of_ppat p1 ^"-"^ string_of_ppat p2
	     |Prod(p1,p2) -> string_of_ppat p1 ^"> "^ string_of_ppat p2
	     |XmlT(p1,p2) -> "<"^string_of_ppat p1 ^ string_of_ppat p2
	     |Arrow(p1,p2) -> string_of_ppat p1 ^"->"^ string_of_ppat p2
	     |Optional(p1) -> string_of_ppat p1^"?"
(* a changer |Record(b,lm) -> let rec listing l=(match l with 
mais pas prioritaire          [] -> ""
			     |(s,ppat)::r -> " "^(U.get_str
                               ((LabelPool.value s)))^"="^string_of_ppat(ppat)^listing r
			     )in listing (lm)  *)	      
	     |Constant(i,t) -> U.get_str i
	     |Regexp(rg) -> "["^string_of_regexp rg ^ "]"
	     | _ ->"?"
	     )

let rec var_of_ppat x = 
    let rec var_of_rg rg =
	match rg with
	|Elem(e) -> []
	|Guard(e) -> []
	|SeqCapture(_,id,rg) -> [ident (Ns.empty,id)] @ var_of_rg rg
	|Seq(r1,r2) -> var_of_rg r1 @ var_of_rg r2
	|Alt(r1,r2) -> var_of_rg r1 @ var_of_rg r2
	|Star r1 -> var_of_rg r1
	|WeakStar r1 -> var_of_rg r1
	|Epsilon -> []

       in match x with 
       {loc = loc ; descr = descr } 
           -> ( match  descr with
             |PatVar(None,id) -> [ident (Ns.empty,id)]
	     |PatVar(Some _,_) -> []
	     |Internal(descr) ->  []
	     |Or(p1,p2) -> var_of_ppat p1 @ var_of_ppat p2
	     |And(p1,p2) -> var_of_ppat p1 @ var_of_ppat p2
	     |Diff(p1,p2) -> var_of_ppat p1 @ var_of_ppat p2
	     |Prod(p1,p2) -> var_of_ppat p1 @ var_of_ppat p2
	     |XmlT(p1,p2) -> var_of_ppat p1 @ var_of_ppat p2
	     |Arrow(p1,p2) -> var_of_ppat p1 @ var_of_ppat p2
	     |Optional(p1) -> var_of_ppat p1
             |Record(b,lm) -> 
		let rec aux accu (_,(ppat,e)) = 
		  let accu = var_of_ppat ppat @ accu in
		  match e with
		    | None -> accu
		    | Some ppat -> var_of_ppat ppat @ accu
		in 
		List.fold_left aux [] lm
	     |Constant(i,t) -> [ident (Ns.empty,i)]
	     |Regexp(rg) -> var_of_rg rg
	     |_ ->[]
	     )

let rec string_of_pexpr x =
   let rec string_of_branches l =
      match l with
        [] -> "\n"
       |(p,e)::r -> "\n| "^string_of_ppat p ^" -> " ^ string_of_pexpr(e) ^ string_of_branches (r)  

   in match x with   
       | Integer i -> string_of_int(Intervals.V.get_int(i)) 
       | Atom a  -> "`" ^ U.get_str ( a)
       | Var( s) ->  U.get_str (s) 
       | Xml(e1,e2) -> " <" ^ string_of_pexpr e1 ^ ">" ^ string_of_pexpr e2 
       | Pair(e1,e2) -> "(" ^ string_of_pexpr e1 ^ "," ^ string_of_pexpr e2 ^ ")"
       | RecordLitt e  -> "att"
       | Char c  -> "\"" ^ Char.escaped (Chars.V.to_char c) ^ "\""
       | LocatedExpr(_,x) -> string_of_pexpr x
       | Apply(e1,e2) -> string_of_pexpr e1 ^ string_of_pexpr e2
       | Transform(e,b) -> " (transform "^string_of_pexpr e ^ " with"
                          ^string_of_branches(b) ^ ")"
       | Match(e,b) -> " (match "^string_of_pexpr e ^ " with"
                       ^string_of_branches(b) ^ ")"
       | Abstraction(_) -> "abstr "
       | Forget(_)->"frgt"
       | Map(e,b) -> "(map" ^string_of_pexpr e ^ " with"
                      ^string_of_branches(b) ^ ")"
       | Xtrans(e,b) -> "(xtransform" ^string_of_pexpr e ^ " with"
                      ^string_of_branches(b) ^ ")"
       | Dot(e1,_,_) -> "Dot(" ^ string_of_pexpr e1 ^",lbl)"
       | RemoveField (e,l) -> "RF(" ^ string_of_pexpr e ^",lbl)"	      
       | Const (Types.Atom a) -> (match (Atoms.V.value a) with
				 (_,utf) -> "`"^U.get_str utf )
       | Const(t) -> " Cst "
       | _ -> "!"

let rec var_of_pexpr x =
match x with
        LocatedExpr(_,x) -> var_of_pexpr x
       |Var(s) -> [ident (Ns.empty,s)]
       |Pair(e1,e2) -> var_of_pexpr e1 @ var_of_pexpr e2
       |Apply(e1,e2) ->  var_of_pexpr e1 @ var_of_pexpr e2
       |Transform(e,_) -> var_of_pexpr e
       |Match(e,_) -> var_of_pexpr e 
       |Map(e,_) -> var_of_pexpr e 
       |Xtrans(e,_) -> var_of_pexpr e 
       | _ -> []

let rec aff_var l =
	match l with
	[] -> ""
	| s::r -> Ident.to_string s ^" "^ aff_var r

(*************************************************
**************************************************
**************************************************)

(*
  [affBoolFormula] : boolFormula -> ('a -> string) -> string
  affiche une formule booleene sur 'a
*)


let rec affBoolFormula bf =
  let rec listAff l s =
    match l with
        [] -> ""
      | [a] -> affBoolFormula a
      | t::q ->
          (affBoolFormula t)
          ^s
          ^(listAff q s)
  in
  match bf with
      True -> "True"
    | False -> "False"
    | Varb(x) -> string_of_pexpr x 
    | Not(x) -> "Non("^(affBoolFormula x )^")"
    | Ou(x,y) -> "("
	^(affBoolFormula x )^" Ou "^(affBoolFormula y )^")"
    | Et(x,y) -> "("
	^(affBoolFormula x )^" Et "^(affBoolFormula y )^")"
    | Im(x,y) -> "("
	^(affBoolFormula x )^" => "^(affBoolFormula y )^")"
    | OuN(l) -> "{"^(listAff l " Ou ")^"}"
    | EtN(l) -> "["^(listAff l " Et ")^"]"
      

(****************************************
 * passage en forme normale conjonctive *
 ****************************************)

(*
  [supprimeImp] : boolFormula -> boolFormula
  \\supprime les implications 
*)
let rec supprimeImp bf =
  match bf with
      Im(bf1,bf2) ->
        Ou(Not(supprimeImp bf1),supprimeImp bf2)
    | Ou(bf1,bf2) ->
        let sbf1 = supprimeImp bf1 in
        let sbf2 = supprimeImp bf2 in
          Ou(sbf1,sbf2)
    | Et(bf1,bf2) ->
        let sbf1 = supprimeImp bf1 in
        let sbf2 = supprimeImp bf2 in
          Et(sbf1,sbf2)
    | Not(bf1) ->
        let sbf1 = supprimeImp bf1 in
          Not(sbf1)
    | _ -> bf

(*
  [notDown] : 'a boolFormula -> 'a boolFormula
*)
let rec notDown bf =
    match bf with
        Not(bf1) -> (
          match bf1 with
              Not(nbf1) -> notDown nbf1
            | True -> False
            | False -> True
            | Varb(a) -> Not(Varb(a))
            | Ou(nbf1,nbf2) ->
                Et(notDown (Not(nbf1)), notDown (Not(nbf2)))
            | Et(nbf1,nbf2) ->
                Ou(notDown (Not(nbf1)), notDown (Not(nbf2)))
            | _ ->
                bf
        )
      | Ou(bf1,bf2) ->
          Ou(notDown(bf1), notDown(bf2))
      | Et(bf1,bf2) ->
          Et(notDown(bf1), notDown(bf2))
      | _ -> bf

(*
  [downEt] : 'a boolFormula -> 'a boolFormula
*)
let rec downOu bf =
  match bf with
      Et(bf1,bf2) ->
        Et(downOu bf1, downOu bf2)
    | Ou(Et(bf1,bf2),bf3) ->
        Et(downOu (Ou(bf1,bf3)), downOu (Ou(bf2,bf3)))
    | Ou(bf3,Et(bf1,bf2)) ->
        Et(downOu (Ou(bf1,bf3)), downOu (Ou(bf2,bf3)))
    | Ou(bf1,bf2) ->
        let dbf1 = downOu bf1 in
        let dbf2 = downOu bf2 in
          (match (dbf1,dbf2) with
              (Et(bf3,bf4),bf5) ->
                Et(downOu (Ou(bf3,bf5)), downOu (Ou(bf4,bf5)))
            | (bf5,Et(bf3,bf4)) ->
                Et(downOu (Ou(bf3,bf5)), downOu (Ou(bf4,bf5)))
            | _ ->
                Ou(dbf1,dbf2))
    | _ -> bf

(*
  [etNaire] : 'a boolFormula -> 'a boolFormula
*)
let rec ouN bf =
  let rec aplatOu ebf =
    match ebf with
        True      -> [True]
      | False     -> [False]
      | Varb(a)    -> [Varb(a)]
      | Not(e)    -> [Not(e)]
      | Ou(e1,e2) -> (aplatOu e1)@(aplatOu e2)
      | _         -> assert false
  in
    match bf with
        Et(bf1,bf2) ->
          Et(ouN bf1, ouN bf2)
      | Ou(bf1,bf2) ->
          OuN ((aplatOu bf1)@(aplatOu bf2))
      | _ -> bf


(*
  [ouNaire] : 'a boolFormula -> 'a boolFormula
*)
let rec etN bf =
  let rec aplatEt ebf =
    match ebf with
      | Et(e1,e2) -> (aplatEt e1)@(aplatEt e2)
      | OuN(e) -> [OuN(e)]	
      | _ -> [OuN[ebf]]
  in
    match bf with
        Et(bf1,bf2) ->
          EtN ((aplatEt bf1)@(aplatEt bf2))
      |OuN(bf) -> EtN[OuN(bf)] 	  
      | _ -> EtN[OuN[bf]]


let rec purge e =
      match e with
       LocatedExpr(_,e) -> purge e
       | e ->e

(* code pas beau du tout, a reprendre clairement, c est pour evaluer que a="2"
est equivalent a "2"=a il reste a faire plusieurs petites choses encore...*)
let rec egal_expr(e1,e2) = e1 == e2
  (* On desactive cette optimisation pour le moment... *)
(*
 let egal_expr2(e1,e2)= string_of_pexpr(e1)=string_of_pexpr(e2) || egal_expr(e1,e2)
 in match (purge(e1),purge(e2)) with
  (Var(e1),Var(e2)) -> e1=e2
  |(Op(op1,[e1;e2]),Op(op2,[e3;e4])) 
     -> op1=op2 && ((egal_expr2(e1,e3) && (egal_expr2(e2,e4)))
                    || (op1="=" && (egal_expr2(e1,e4) && egal_expr2(e2,e3))))
       
  |_ -> false
*)

let rec appartient(t,l) =
	match (t,l) with
	(_,[]) -> false
        | (Varb(t),Varb(p)::r) -> 
		 if egal_expr(t,p) then true else appartient(Varb(t),r)
        | (Not(Varb(t)),Not(Varb(p))::r) -> 
		 if egal_expr(t,p) then true else appartient(Not(Varb(t)),r)
	| (t,p::r) ->  appartient(t,r)	

let rec retire_occurs l =
	match l with 
	 [] -> []
	 | t::q ->
             if (appartient(t,q)) then retire_occurs q
                                  else t::retire_occurs q

let rec is_false l =
        match l with
	 [] -> false
	 |Not(Varb(t))::q -> if appartient(Varb(t),q) then true else is_false(q)
	 | t::q-> is_false q

let rec contient_true l =
	match l with
	[] -> false
	|True::r -> true
	|p::r -> contient_true r  

let rec retire_false l =
	match l with
	[] -> []
	| False::r -> retire_false r
	| p:: r -> p::retire_false r

let rec simplifieOuN f=	  
   let rec simplifieOuN2 f =
      match f with
      OuN(l) -> let l =(retire_occurs l) 
                in if is_false l || is_false (List.rev l) then [True] 
	                       else l
      | _ -> assert false 	
   in let f=simplifieOuN2 f
      in let f = retire_false(f)
	 in if contient_true f then OuN[True]
	                       else OuN(f)  
   
let rec contient(l1,l2)=
  match(l1,l2)with
  (p1::r1,l2) -> (appartient(p1,l2) & contient(r1,l2)) 
  |([],l) -> true	   

let rec simplifieEtN loun =
   let rec aux(oun,l) =
   match(oun,l) with
   (OuN(o1),OuN(o2)::r) -> if contient(o1,o2) then aux(oun,r) 
                                             else OuN(o2)::aux(OuN(o1),r)
   |(_,[]) -> []
   | (_,b) -> b
   in match loun with
      (oun::r) -> oun::simplifieEtN(aux(oun,r))
      | [] -> []

let rec simplifie f =
  match f with
    EtN (loun) -> let rec aux loun = match loun with
                 [] -> []
		|OuN([True])::r -> aux r   
	        |oun::r -> simplifieOuN(oun)::aux(r)
               in 
	          let rec isFalse loun = match loun with
		  [] -> false
		  | OuN([False])::r -> true
		  |p::r -> isFalse r
	       in if isFalse loun then EtN[OuN[False]]
	                          else let s=aux(simplifieEtN(loun))
                                       in if s=[] then EtN[OuN[True]]
				                     else EtN(s) 
  | x -> EtN[OuN[x]]

let fnc bf = simplifie(etN(ouN(downOu(notDown(supprimeImp bf)))))	
(*************************************************
**************************************************
**************************************************)

let rec retirer_redondances l =
	let rec app(e,l)=
	    match (e,l) with
	    (e,[]) -> false
	    |(e,p::r) -> if e=p then true else app(e,r)
	
	in match l with
	[] -> []
	|p::r -> if app(p,r) then retirer_redondances r
	                     else p::retirer_redondances r 

let rec dans(lv,env)=
	let rec dans2(p,env)=
	    match (p,env)with 
	    (_,[])->false
	    |(p,h::t) -> (p=h) || dans2(p,t)  
	in match (lv,env) with
	([],env) -> true
	|(p::r,env) -> dans2(p,env)&&dans(r,env)

(* renvoie les clauses associees aux variables et la conjontions moins ces clauses*)
let rec c2(env,l)=
	match (env,l) with
	(env,[]) -> ([],[([True],[])]) 
	|(env,(lf,lv)::r)-> if dans(lv,env)
	             then (lf::fst(c2(env,r)),snd(c2(env,r)))
                     else (fst(c2(env,r)),(lf,lv)::snd(c2(env,r)))	


(*l:(Ast.pexpr boolFormula list * Ident.id list) list  *)
let rec place(t,l) =
	match (t,l) with
	([],_) -> []
	|([(i,p,x,env,c)],l2) ->  let l2 = match l2 with 
					|l->let rec reformate l =
					       match l with 
					       [] -> []
					       | (lf,lv)::r -> OuN(lf)::reformate(r)
					       in simplifie(EtN(reformate l))
				in [(i,p,x,env,l2)]  
        |((i,p,x,env,c)::r,l) -> let (c2,l2) = c2(env,l)
				 in let c2 = match c2 with 
					|l->let rec reformate l =
					       match l with 
					       [] -> []
					       | p::r -> OuN(p)::reformate(r)
					       in simplifie(EtN(reformate l))
				in(i,p,x,env,c2)::place(r,l2)

let rec sortir_clauses f =
    let rec aux f =
         let rec sortir_variables l =
      	     match l with
	        [] -> []
	        |Varb(p)::r -> var_of_pexpr p @ sortir_variables r
	        |Not(Varb(p))::r ->  var_of_pexpr p @ sortir_variables r
		|p::r-> sortir_variables r
         in match f with
             OuN(l) -> (l,sortir_variables(l))
	     |_ ->assert false
    in match f with 
      EtN([]) -> []
    | EtN(p::r) -> aux(p)::sortir_clauses(EtN(r))
    | _-> assert false 

let rec print_clauses l =
	match l with
	[] -> "\n"
	|(b,v)::r -> affBoolFormula(OuN(b)) ^":"^ aff_var(v) ^"-"^ print_clauses r
	 
let rec pp l =
	match l with
	| [] -> ()
	| (i,p,x,env,c)::l -> print_string "T";
	                  print_int i; 
			  print_string ": e= ";
	                  print_string (aff_var(var_of_pexpr x)) ;
			  print_string " p= ";
			  print_string (aff_var(var_of_ppat p));
			  print_string " c= ";
			  print_string (affBoolFormula c);
			  print_string " E=";
			  print_string (aff_var env);
			  print_string"\n";
			  pp l



(* t  est le tableau correspondant i,p,x,env,c *)
let select2(loc,e,t) = 
	let rec ifthenelse(c,e,loc)=
	   match c with
	   EtN[OuN[True]] -> e
	   |True -> e
	   |c -> exp loc (Parser.if_then_else (ast_of_bool(c,loc)) e cst_nil)
	in let rec saux loc e t = 
	(match t with	
	     [] -> exp loc (Pair(cst_nil,cst_nil))
	    |[(n,pn,xn,envn,cn)] -> 
	       let condi= ifthenelse(cn,exp loc e ,loc)
	       in exp loc (Transform(xn,[(pn,condi)]))
	       
	    |(i,pi,xi,envi,ci)::r-> 
	       let condi=ifthenelse(ci,(saux loc e r),loc)
	       in exp loc (Transform(xi,[(pi,condi)]))
	       
	)
	in saux loc e t 

let selectOpt(loc,e,l,condi) =
  let rec tableau (l,env,i) = 
    match l with
      | [] -> []
      | (p,x)::r -> 
	  let env2 = 
	    retirer_redondances (env @ var_of_pexpr x @ var_of_ppat p) in
	  (i,p,x,env2,True) :: (tableau (r,env2,i+1))
  in
  let tableau = tableau (l,[],1) in
(*  pp tableau; *)
  let t = place (tableau, sortir_clauses (fnc condi)) in
(*  pp t; *)
  let s = select2 (loc,e,t) in
(*  print_string(string_of_pexpr(s)); *)
  s

let select(loc,e,l) = 
	let rec saux loc l = 
	(match l with	
	     [] -> exp loc (Pair(cst_nil,cst_nil))
            |[(xn,en)] -> exp loc (Transform(en,[(xn,e)]))
	    |(xi,ei)::r-> exp loc (Transform(ei,[(xi,
	    (saux loc r))]))
	)
	in let s=saux loc l
	 in (*print_string(string_of_pexpr(s));*)s
