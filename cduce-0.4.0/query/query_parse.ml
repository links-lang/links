open Location
open Ast
open Ident
open Printf

open Parser.Hook
open Query
#load "pa_extend.cmo";;


let tloc (i,j) = (i.Lexing.pos_cnum,j.Lexing.pos_cnum)
let mk loc x = Location.mk (tloc loc) x

let exp pos e = LocatedExpr (loc_of_pos (tloc pos),e)

let cst_nil =  Const Sequence.nil_cst
let parse_ident = U.mk

let id_dummy = U.mk "$$$"

let label = parse_ident

let rec multi_prod loc = function
  | [ x ] -> x
  | x :: l -> mk loc (Prod (x, multi_prod loc l))
  | [] -> assert false

let if_then_else cond e1 e2 = Match (cond, [pat_true,e1; pat_false,e2])

let op2 op e1 e2 = Apply (Apply (Var (U.mk op), e1), e2)

EXTEND 
  GLOBAL: expr pat keyword;


  expr: [
    "top" RIGHTA[
     "select"; e = expr;
     "from";l = LIST1 [ x= pat ; "in"; e = expr -> (x,e)] SEP "," ;
       z=OPT[ "where"  ; w = cond -> w]  -> 
	let (condi,fin) =
	  match z with
              Some w -> 
                (w, exp loc 
		   (Parser.if_then_else (Query.ast_of_bool(w,tloc loc)) 
		      (Pair (e,cst_nil)) 
		      cst_nil))
            | None -> (True, exp loc (Pair(e,cst_nil)))
	in 
	if !Query.nooptim 
        then Query.select(tloc loc,fin,l)
      	else Query.selectOpt(tloc loc,Pair (e,cst_nil),l,condi)
    ]];
   
  cond:
      [ [ a = expr -> 
	  (match a with
             | LocatedExpr(_, Atom at) ->
	         (match U.get_str at with
                  | "true" -> Query.True
                  | "false" -> Query.False
                  | _ -> Query.Varb a)
             | _ -> Query.Varb a)
       |"not"; a = cond -> Query.Not(a)
       | a = cond ; "or" ; b = cond -> Query.Ou(a,b)
       | a = cond ; "and" ; b = cond -> Query.Et(a,b)
       | "(" ; a=cond ; ")" -> a
      ]
    ];


  keyword: [ [ a = [ "select" | "from" ] -> a ] ];
END




