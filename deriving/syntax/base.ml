(*pp camlp4of *)
open Utils
open Types
open Camlp4.PreCast

type context = {
  loc : Loc.t;
  (* mapping from type parameters to functor arguments *)
  argmap : name NameMap.t;
  (* ordered list of type parameters *)
  params : param list;
  (* type names *)
  tnames : NameSet.t;
}


exception Underivable of string
exception NoSuchClass of string


(* display a fatal error and exit *)
let error loc (msg : string) =
  Syntax.print_warning loc msg;
  exit 1

(*
module type Context = sig val context: context end
module type TContext = sig include Context val tcontext : type_context end
*)
module type Loc = sig val loc : Loc.t end

module InContext(L : Loc) =
struct
  include L
  module Untranslate = Untranslate(L)

  let instantiate (lookup : name -> expr) : expr -> expr = 
  object 
    inherit transform as super
    method expr = function
      | `Param (name, _) -> lookup name
      | e                -> super # expr e
  end # expr

  let instantiate_modargs ctxt t =
    let lookup var = 
      try 
        `Constr ([NameMap.find var ctxt.argmap; "a"], [])
      with Not_found ->
        failwith ("Unbound type parameter '" ^ var)
    in instantiate lookup t

  let contains_tvars : expr -> bool = 
    (object
       inherit [bool] fold as default
       method crush = List.exists (fun x -> x)
       method expr = function
         | `Param _ -> true
         | e -> default#expr e
     end) # expr

  let substitute env =
    (object
       inherit transform as default
       method expr = function
         | `Param (p,v) when NameMap.mem p env -> 
             `Param (NameMap.find p env,v)
         | e -> default# expr e
     end) # expr

  let cast_pattern ctxt ?(param="x") t = 
    let t = Untranslate.expr (instantiate_modargs ctxt t) in
      (<:patt< $lid:param$ >>,
       <:expr<
         let module M = 
             struct
               type t = $t$
               let test = function #t -> true | _ -> false
             end in M.test $lid:param$ >>,
       <:expr<
         (let module M = 
              struct
                type t = $t$
                let cast = function #t as t -> t | _ -> assert false
              end in M.cast $lid:param$ )>>)

  let seq l r = <:expr< $l$ ; $r$ >>

  let record_pattern ?(prefix="") (fields : Types.field list) : Ast.patt = 
    <:patt<{$list:
              (List.map (fun (label,_,_) -> <:patt< $lid:label$ = $lid:prefix ^ label$ >>) 
                      fields) $}>>

  let record_expr : (string * Ast.expr) list -> Ast.expr = 
    fun fields ->
      let fs = 
      List.fold_left1 
        (fun l r -> <:rec_binding< $l$ ; $r$ >>)
        (List.map (fun (label, exp) -> <:rec_binding< $lid:label$ = $exp$ >>) 
           fields) in
        Ast.ExRec (loc, fs, Ast.ExNil loc)


  let record_expression ?(prefix="") : Types.field list -> Ast.expr = 
    fun fields ->
      let es = List.fold_left1
        (fun l r -> <:rec_binding< $l$ ; $r$ >>)
        (List.map (fun (label,_,_) -> <:rec_binding< $lid:label$ = $lid:prefix ^ label$ >>) 
           fields) in
        Ast.ExRec (loc, es, Ast.ExNil loc)

  let expr_list : Ast.expr list -> Ast.expr = 
    fun exprs ->
      List.fold_right 
        (fun car cdr -> <:expr< $car$ :: $cdr$ >>)
        exprs
      <:expr< [] >>

  let tuple_expr : Ast.expr list -> Ast.expr = function
    | [] -> <:expr< () >>
    | [x] -> x
    | x::xs -> let cs l r = <:expr< $l$, $r$ >> in
        <:expr< $List.fold_left cs x xs$ >>

  let tuple ?(param="v") n : Ast.patt * Ast.expr =
    let v n = Printf.sprintf "%s%d" param n in
      match n with
        | 0 -> <:patt< () >>, <:expr< () >>
        | 1 -> <:patt< $lid:v 0$ >>, <:expr< $lid:v 0$ >>
        | n -> 
            let patts, exprs = 
              (* At time of writing I haven't managed to write anything
                 using quotations that generates an n-tuple *)
              List.fold_left 
                (fun (p, e) (patt, expr) -> Ast.PaCom (loc, p, patt), Ast.ExCom (loc, e, expr))
                (<:patt< >>, <:expr< >>)
                (List.map (fun n -> <:patt< $lid:v n$ >>, <:expr< $lid:v n $ >>)
                   (List.range 0 n))
            in
              Ast.PaTup (loc, patts), Ast.ExTup (loc, exprs)


(*            <:patt< ( $patts$ ) >>, <:expr< ( $exprs$ ) >>*)

(* let tuple_expr loc = function  (\* 0-tuples and 1-tuples are invalid *\) *)
(*   | []  -> <:expr< () >> *)
(*   | [x] -> x *)
(*   | xs  -> <:expr< ( $list:xs$ ) >> *)

  let rec modname_from_qname ~qname ~classname =
    match qname with 
      | [] -> invalid_arg "modname_from_qname"
      | [t] -> <:ident< $uid:classname ^ "_"^ t$ >>
      | t::ts -> <:ident< $uid:t$.$modname_from_qname ~qname:ts ~classname$ >>

  let apply_functor (f : Ast.module_expr) (args : Ast.module_expr list) : Ast.module_expr =
      List.fold_left (fun f p -> <:module_expr< $f$ $p$ >>) f args
          
  class make_module_expr ~classname ~variant ~record ~sum =
  object (self)

    method mapply ctxt (funct : Ast.module_expr) args =
      apply_functor funct (List.map (self#expr ctxt) args)

    method variant = variant
    method sum = sum
    method record = record

    method param ctxt (name, variance) =
      <:module_expr< $uid:NameMap.find name ctxt.argmap$ >>

    method underscore _  = raise (Underivable (classname ^ " cannot be derived for types with `_'"))
    method object_   _ o = raise (Underivable (classname ^ " cannot be derived for object types"))
    method class_    _ c = raise (Underivable (classname ^ " cannot be derived for class types"))
    method label     _ l = raise (Underivable (classname ^ " cannot be derived for label types"))
    method function_ _ f = raise (Underivable (classname ^ " cannot be derived for function types"))

    method constr ctxt (qname, args) = 
      match qname with
        | [name] when NameSet.mem name ctxt.tnames ->
            <:module_expr< $uid:Printf.sprintf "%s_%s" classname name$ >>
        | _ -> 
            let f = (modname_from_qname ~qname ~classname) in
              self#mapply ctxt (Ast.MeId (loc, f)) args

    method tuple ctxt = function
        | [] -> <:module_expr< $uid:Printf.sprintf "%s_unit" classname$ >>
        | [a] -> self#expr ctxt a
        | args -> 
            let f = <:module_expr< $uid:Printf.sprintf "%s_%d" 
                                   classname (List.length args)$ >> in
              self#mapply ctxt f args

    method expr (ctxt : context) : expr -> Ast.module_expr = function
      | `Param p    -> self#param      ctxt p
      | `Underscore -> self#underscore ctxt
      | `Object o   -> self#object_    ctxt o
      | `Class c    -> self#class_     ctxt c
      | `Label l    -> self#label      ctxt l 
      | `Function f -> self#function_  ctxt f
      | `Constr c   -> self#constr     ctxt c
      | `Tuple t    -> self#tuple      ctxt t

    method rhs ctxt (tname, params, rhs, constraints  as decl : Types.decl) : Ast.module_expr = 
      match rhs with
        | `Fresh (None, Sum summands) -> self#sum ctxt decl summands
        | `Fresh (None, Record fields) -> self#record ctxt decl fields
        | `Expr e -> self#expr ctxt e
        | `Variant v -> self# variant ctxt decl v
  end

  let atype ctxt (name, params, rhs, _) = 
    match rhs with 
      | `Fresh _ | `Variant _ ->
          Untranslate.expr (`Constr ([name],
                                     List.map (fun (p,_) -> `Constr ([NameMap.find p ctxt.argmap; "a"],[])) params))
      | `Expr e -> Untranslate.expr (instantiate_modargs ctxt e)

  let generate ~context ~decls ~make_module_expr ~classname ?default_module () =
    (* plan: 
       set up an enclosing recursive module
       generate functors for all types in the clique
       project out the inner modules afterwards.
       
       later: generate simpler code for simpler cases:
       - where there are no type parameters
       - where there's only one type
       - where there's no recursion
       - etc.
    *)
      (*    let _ = ensure_no_polymorphic_recursion in *)
    let wrapper_name = Printf.sprintf "%s_%s" classname (random_id 32)  in
    let make_functor = 
      List.fold_right 
        (fun (p,_) rhs -> 
           let arg = NameMap.find p context.argmap in
             <:module_expr< functor ($arg$ : $uid:classname$.$uid:classname$) -> $rhs$ >>)
        context.params in
    let apply_defaults mexpr = match default_module with
      | None -> mexpr
      | Some default -> <:module_expr< $uid:classname$.$uid:default$ ($mexpr$) >> in
    let mbinds =
      List.map 
        (fun (name,params,rhs,constraints as decl) -> 
           <:module_binding< 
             $uid:classname ^ "_"^ name$
             : $uid:classname$.$uid:classname$ with type a = $atype context decl$
          = $apply_defaults (make_module_expr context decl)$ >>)
        decls in
    let mrec =
      <:str_item< open $uid:classname$ open Primitives module rec $list:mbinds$ >> in
    let fixed = make_functor <:module_expr< struct $mrec$ end >> in
    let applied = apply_functor <:module_expr< $uid:wrapper_name$ >> 
                                (List.map (fun (p,_) -> <:module_expr< $uid:NameMap.find p context.argmap$>>) 
                                      context.params) in
    let projected = (* TODO: apply the functor here *)
      List.map (fun (name,params,rhs,constraints) -> 
                  let modname = classname ^ "_"^ name in
                  let rhs = <:module_expr< struct module P = $applied$ include P.$uid:modname$ end >> in
                    <:str_item< module $uid:modname$ = $make_functor rhs$>>)
        decls in
    let m = <:str_item< module $uid:wrapper_name$ = $fixed$ >> in
      <:str_item< $m$ $List.hd projected$ >>
end
   
let extract_params = 
  let has_params params (_, ps, _, _) = ps = params in
    function
      | [] -> invalid_arg "extract_params"
      | (_,params,_,_)::rest
          when List.for_all (has_params params) rest ->
          params
      | (_,_,rhs,_)::_ -> 
          (* all types in a clique must have the same parameters *)
          raise (Underivable ("Instances can only be derived for "
                             ^"recursive groups where all types\n"
                             ^"in the group have the same parameters."))

let setup_context loc tdecls : context =
  let params = extract_params tdecls in
  let argmap = 
    List.fold_right
      (fun (p,_) m -> NameMap.add p (Printf.sprintf "V_%s" p) m)
      params
      NameMap.empty in 
    { loc = loc;
      argmap = argmap;
      params = params; 
      tnames = NameSet.fromList (List.map (fun (name,_,_,_) -> name) tdecls) }
      
type deriver = Loc.t * context * Types.decl list -> Ast.str_item
let derivers : (name, deriver) Hashtbl.t = Hashtbl.create 15
let register = Hashtbl.add derivers
let find classname = 
  try Hashtbl.find derivers classname
  with Not_found -> raise (NoSuchClass classname)
