#load "pa_extend.cmo";;
#load "q_MLast.cmo";;
open Deriving

(* TODO: handle records. *)

(* recurse over the type structure.
   
   Every section that consists of type constructors applied to
   type variables gets mapped to the map methods of the
   corresponding functor instances applied to the image of the
   mapped function.

   Tuples and records must be handled specially.
 *)

let rec contains_tvars = function
| <:ctyp< [ $list:ctors$ ] >>  -> List.exists (fun (_,_,cs) -> (List.exists contains_tvars cs)) ctors
| <:ctyp< $t1$ $t2$ >>         -> contains_tvars t1 || contains_tvars t2
| <:ctyp< $lid:id$ >>          -> false
| <:ctyp< $uid:m$ . $t2$ >>    -> false
| <:ctyp< { $list:fields$ } >> -> List.exists (fun (_, _, _, c) -> contains_tvars c) fields
| <:ctyp< ( $list:params$ ) >> -> List.exists contains_tvars params
| <:ctyp< [= $list:row$ ] >>   -> List.exists (function 
                                                  | MLast.RfTag (_, _, targs) -> List.exists contains_tvars targs
                                                  | MLast.RfInh _ -> false)
                                    row
| <:ctyp< '$a$ >>              -> true
| _                            -> failwith "Failed to generate functor instance [1]"



let rec gen_function loc = function
| c when not (contains_tvars c) -> None
| <:ctyp< '$_$  >>              -> Some <:expr< f >>
| <:ctyp< $lid:tc$ $t$ >>       -> (match gen_function loc t with
                                    | None   -> None
                                    | Some x -> 
                                        Some <:expr< $uid:"Functor_" ^ tc$ . map ( $x$ ) >>)
| <:ctyp< ( $list:types$ ) >> -> 
  let (patts, exprs) = List.split (gen_n loc types) in
  Some (<:expr< fun [ ($list:patts$) -> ($list:exprs$) ] >>)
| _                             -> failwith "Failed to generate functor instance [0]"
and gen_code loc vname ctype = 
  match gen_function loc ctype with
    | None   -> <:expr< $lid:vname$ >>
    | Some x -> <:expr< $x$ ( $lid:vname$) >>

(* given n types, return n patterns and expressions *) 
and gen_n loc types = 
    let vnames = List.map (Printf.sprintf "v%d") (range 0 (List.length types - 1)) in
      List.map2 
        (fun vname ctype -> (<:patt< $lid:vname$ >>, gen_code loc vname ctype))
        vnames
        types

let gen_case (loc, name, params) = 
match params with
| [] -> (<:patt< $uid:name$ >>, None, <:expr< $uid:name$ >>)
| [x] -> (<:patt< $uid:name$ x  >>, None, <:expr< $uid:name$ $gen_code loc "x" x$ >>)
| _  -> let (patts, exprs) = List.split (gen_n loc params) in
           (<:patt< $uid:name$ ( $list:patts$ ) >>,
             None,
            <:expr< $uid:name$ ( $list:exprs$ ) >>)


let gen_vcase loc = function
| MLast.RfTag (name, _, params) -> (match params with
    | [] -> (<:patt< `$uid:name$ >>, None, <:expr< `$uid:name$ >>)
    | [x] -> (<:patt< `$uid:name$ x  >>, None, <:expr< `$uid:name$ $gen_code loc "x" x$ >>)
    | _  -> let (patts, exprs) = List.split (gen_n loc params) in
              (<:patt< `$uid:name$ ( $list:patts$ ) >>,
              None,
              <:expr< `$uid:name$ ( $list:exprs$ ) >>))
| MLast.RfInh (<:ctyp< $lid:tname$ >>) ->  (<:patt< (# $[tname]$ as x) >>, None, <:expr< x >>)
| MLast.RfInh _ -> error loc ("Cannot generate functor instance")


let gen_map loc tname = function
 | <:ctyp< [ $list:ctors$ ]  >> ->  <:str_item< value map f = fun [ $list:List.map gen_case ctors$ ] >>
 | <:ctyp< [= $list:ctors$ ] >> ->  <:str_item< value map f = fun [ $list:List.map (gen_vcase loc) ctors$ ] >>
 | _ -> failwith "Failed to generate functor instance [2]"

    
let gen_instance loc tname param ctype =
    let mtype = <:module_type< Functor with type f 'a = $lid:tname$ 'a >>
    and mbody = <:module_expr< struct
                                 type f 'a = $lid:tname$ 'a;
                                 $gen_map loc tname ctype$;
                               end >> in
<:str_item< 
        declare
          open Functor;
          module rec $list:[(("Functor_" ^ tname), mtype, mbody)]$;
        end
>>

let gen_instances loc : instantiator =  function
 | [((loc, tname),[param], ctype,_(*constraints*))] -> gen_instance loc tname param ctype
 | l when List.length l > 1 -> error loc ("Cannot currently generate functor instances for mutually recursive types")
 | _ -> error loc ("Types with functor instances must have exactly one type parameter")
     (* Hmm. We could generate one instance for each parameter.  That
        could be pretty neat, especially for types with a
        parameterized fixpoint.  *)


let _ = 
  instantiators :=   ("Functor", gen_instances):: !instantiators
