#load "pa_extend.cmo";;
#load "q_MLast.cmo";;
open Deriving

(* The code currently generated is somewhat imperative, and not as
   efficient as a hand-written functional version (since it doesn't
   maximise sharing).  Also, the code generated has silly redexes:

     (\x.e)(v)

   where v is a variable name.  I'd expect the OCaml optimiser to pick
   those up, though.
*)

(* TODO: much of this code is in common with, or similar to,
   functor_class.ml.  Refactor! 

   Also, very incomplete: it doesn't currently handle no type aliases,
   polymorphic variants, records, etc.
*)

let rec contains_type (ltype, lmod) = function
| c when ltype_of_ctyp c = Some ltype  -> true
| <:ctyp< [ $list:ctors$ ] >>  -> List.exists (fun (_,_,cs) -> (List.exists (contains_type (ltype, lmod)) cs)) ctors
| <:ctyp< $t1$ $t2$ >>         -> (contains_type (ltype,lmod)) t1 || (contains_type (ltype,lmod)) t2
| <:ctyp< $lid:id$ >>          -> false
| <:ctyp< $uid:m$ . $t2$ >>    -> false
| <:ctyp< { $list:fields$ } >> -> List.exists (fun (_, _, _, c) -> (contains_type (ltype,lmod)) c) fields
| <:ctyp< ( $list:params$ ) >> -> List.exists (contains_type (ltype,lmod)) params
| <:ctyp< [= $list:row$ ] >>   -> List.exists (function
                                                   | MLast.RfTag (_, _, targs) -> List.exists (contains_type (ltype,lmod)) targs
                                                   | MLast.RfInh _ -> false)
                                      row
| <:ctyp< '$a$ >>              -> false
| _                            -> failwith "Failed to generate rewriter instance [1]"

let rec gen_expr_rewriter_function loc (ltype, lmod) = function
                                                             | c when not (contains_type (ltype,lmod) c) -> None
| c when ltype_of_ctyp c = Some ltype -> Some <:expr< adapted >>
| <:ctyp< $lid:tc$ $t$ >>       -> 
  (match gen_expr_rewriter_function loc (ltype,lmod) t with
  | None   -> None
  | Some x -> Some <:expr< $uid:"Functor_" ^ tc$ . map ( $x$ ) >>)
| <:ctyp< ( $list:types$ ) >> -> 
  let (patts, exprs) = List.split (gen_n loc (ltype,lmod) types) in
    Some (<:expr< fun [ ($list:patts$) -> ($list:exprs$) ] >>)
| _                             -> failwith "Failed to generate functor instance [0]"
and gen_code (loc:MLast.loc) (vname:string) (ltype, lmod) ctype = 
    match gen_expr_rewriter_function loc (ltype,lmod) ctype with
      | None   -> <:expr< $lid:vname$ >>
      | Some x -> <:expr<  $x$ ( $lid:vname$) >>

(* given n types, return n patterns and expressions *) 
and gen_n loc (ltype, lmod) types = 
    let vnames = List.map (Printf.sprintf "v%d") (range 0 (List.length types - 1)) in
      List.map2 
        (fun vname ctype -> (<:patt< $lid:vname$ >>, gen_code loc vname (ltype,lmod) ctype))
        vnames
        types

let gen_case (ltype,lmod) (loc, name, params) = 
  match params with
    | [] -> (<:patt< ($uid:name$ as x) >>, None, <:expr< x >>)
    | [x] -> 
        if contains_type (ltype, lmod) x then
          (<:patt< ($uid:name$ x)  >>, None, <:expr< $uid:name$ ($gen_code loc "x" (ltype,lmod) x$) >>)
        else
          (<:patt< ($uid:name$ _ as p) >>, None, <:expr< p >>)
    | _  -> let (patts, exprs) = List.split (gen_n loc (ltype,lmod) params) in
              (<:patt< $uid:name$ ( $list:patts$ ) >>,
              None,
              <:expr< $uid:name$ ( $list:exprs$ ) >>)

let gen_process_children loc (ltype,lmod) tname = function
  | <:ctyp< [ $list:ctors$ ]  >> ->  <:str_item< value process_children' rewriter v = 
                                                   let changed = ref False in
                                                   let adapted x = 
                                                     match rewriter x with 
                                                       [ Some x -> do { changed.contents := True; x }
                                                       | None   -> x ]
                                                   in let rv = match v with [ $list:List.map (gen_case (ltype,lmod)) ctors$ ] in (rv, changed.contents) >>
  | _ -> error loc ("Cannot (yet?) generate instance for " ^ tname)

let gen_type_a loc = 
    List.fold_left
      (fun t (_,(_,mname)) -> <:ctyp< $t$ $uid:mname$ . a >>)

let gen_instance loc tname params ctype =
  let params = param_names params in
  let atype = gen_type_a  loc  <:ctyp< $lid:tname$ >> params in 
  let ltype = gen_type_l tname params 
  and lmod = "Rewriter_" ^ tname in
  let mbody = <:module_expr< Rewriter.RewriterDefaults
                              (struct
                                 type t = $atype$ ;
                                 type rewriter = t -> option t;
                                 $gen_process_children loc (ltype,lmod) tname ctype$;
                               end) >> in
  let fnctor = gen_functor loc "Type" params mbody in
  let applied_functor = 
    List.fold_right (fun (_,(_,mname)) m ->
                       <:module_expr< $m$ $uid:mname$ >>) params fnctor in
  let fnctor_2 = gen_functor loc "Type" params <:module_expr< Rewrite.Rewrite (Rewrite.SimpleRewrite ( $applied_functor$ ))>> in
<:str_item< 
declare
  open Rewriter;
  open Functor;
  module $uid:"Rewrite_" ^ tname$ = $fnctor_2$;
  end
>>

let gen_instances loc : instantiator = function
 | [((loc, tname),params, ctype,_(*constraints*))] -> gen_instance loc tname params ctype
 | _ -> error loc ("Cannot currently generate rewriter instances for mutually recursive types")

let _ = 
  instantiators := ("Rewriter", gen_instances):: !instantiators
