(*pp camlp4of *)
open Utils
open Type
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

  let instantiate, instantiate_repr =
    let o lookup = object 
      inherit transform as super
      method expr = function
        | `Param (name, _) -> lookup name
        | e                -> super # expr e
    end in
      (fun (lookup : name -> expr) -> (o lookup)#expr),
      (fun (lookup : name -> expr) -> (o lookup)#repr)

  let instantiate_modargs, instantiate_modargs_repr =
    let lookup ctxt var = 
      try 
        `Constr ([NameMap.find var ctxt.argmap; "a"], [])
      with Not_found ->
        failwith ("Unbound type parameter '" ^ var)
    in (fun ctxt -> instantiate (lookup ctxt)),
       (fun ctxt -> instantiate_repr (lookup ctxt))

  let contains_tvars : expr -> bool = 
    (object
       inherit [bool] fold as default
       method crush = List.exists F.id
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

  let record_pattern ?(prefix="") (fields : Type.field list) : Ast.patt = 
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

  let record_expression ?(prefix="") : Type.field list -> Ast.expr = 
    fun fields ->
      let es = List.fold_left1
        (fun l r -> <:rec_binding< $l$ ; $r$ >>)
        (List.map (fun (label,_,_) -> <:rec_binding< $lid:label$ = $lid:prefix ^ label$ >>) 
           fields) in
        Ast.ExRec (loc, es, Ast.ExNil loc)

  let mproject mexpr name = 
    match mexpr with
      | <:module_expr< $id:m$ >> -> <:expr< $id:m$.$lid:name$ >>
      | _ -> <:expr< let module M = $mexpr$ in M.$lid:name$ >>

  let expr_list : Ast.expr list -> Ast.expr = 
      (fun exprs ->
         List.fold_right 
           (fun car cdr -> <:expr< $car$ :: $cdr$ >>)
           exprs
         <:expr< [] >>)

  let patt_list : Ast.patt list -> Ast.patt = 
      (fun patts ->
         List.fold_right 
           (fun car cdr -> <:patt< $car$ :: $cdr$ >>)
           patts
         <:patt< [] >>)

  let tuple_expr : Ast.expr list -> Ast.expr = function
    | [] -> <:expr< () >>
    | [x] -> x
    | x::xs -> Ast.ExTup (loc, List.fold_left (fun e t -> Ast.ExCom (loc, e,t)) x xs)

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

  let rec modname_from_qname ~qname ~classname =
    match qname with 
      | [] -> invalid_arg "modname_from_qname"
      | [t] -> <:ident< $uid:classname ^ "_"^ t$ >>
      | t::ts -> <:ident< $uid:t$.$modname_from_qname ~qname:ts ~classname$ >>

  let apply_functor (f : Ast.module_expr) (args : Ast.module_expr list) : Ast.module_expr =
      List.fold_left (fun f p -> <:module_expr< $f$ $p$ >>) f args
          
  class virtual make_module_expr ~classname ~allow_private =
  object (self)

    method mapply ctxt (funct : Ast.module_expr) args =
      apply_functor funct (List.map (self#expr ctxt) args)

    method virtual variant : context -> decl -> variant -> Ast.module_expr
    method virtual sum     : ?eq:expr -> context -> decl -> summand list -> Ast.module_expr
    method virtual record  : ?eq:expr -> context -> decl -> field list -> Ast.module_expr
    method virtual tuple   : context -> expr list -> Ast.module_expr

    method param ctxt (name, variance) =
      <:module_expr< $uid:NameMap.find name ctxt.argmap$ >>

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

    method expr (ctxt : context) : expr -> Ast.module_expr = function
      | `Param p    -> self#param      ctxt p
      | `Object o   -> self#object_    ctxt o
      | `Class c    -> self#class_     ctxt c
      | `Label l    -> self#label      ctxt l 
      | `Function f -> self#function_  ctxt f
      | `Constr c   -> self#constr     ctxt c
      | `Tuple t    -> self#tuple      ctxt t

    method rhs ctxt (tname, params, rhs, constraints, _ as decl : Type.decl) : Ast.module_expr = 
      match rhs with
        | `Fresh (_, _, (`Private : [`Private|`Public])) when not allow_private ->
            raise (Underivable ("The class "^ classname ^" cannot be derived for private types"))
        | `Fresh (eq, Sum summands, _) -> self#sum ?eq ctxt decl summands
        | `Fresh (eq, Record fields, _) -> self#record ?eq ctxt decl fields
        | `Expr e -> self#expr ctxt e
        | `Variant v -> self# variant ctxt decl v
        | `Nothing -> <:module_expr< >>
  end

  let atype_expr ctxt expr = 
    Untranslate.expr (instantiate_modargs ctxt expr)

  let atype ctxt (name, params, rhs, _, _) = 
    match rhs with 
      | `Fresh _ | `Variant _ | `Nothing ->
          Untranslate.expr (`Constr ([name],
                                     List.map (fun (p,_) -> `Constr ([NameMap.find p ctxt.argmap; "a"],[])) params))
      | `Expr e -> atype_expr ctxt e

  let make_safe (decls : (decl * Ast.module_binding) list) : Ast.module_binding list =
    (* re-order a set of mutually recursive modules in an attempt to
       make initialization problems less likely *) 
    List.map snd
      (List.sort 
         (fun ((_,_,lrhs,_,_), _) ((_,_,rrhs,_,_), _) -> match (lrhs : rhs), rrhs with
            (* aliases to types in the group score higher than
               everything else.

               In general, things that must come first receive a
               positive score when they occur on the left and a
               negative score when they occur on the right. *)
            | (`Fresh _|`Variant _), (`Fresh _|`Variant _) -> 0
            | (`Fresh _|`Variant _), _ -> -1
            | _, (`Fresh _|`Variant _) -> 1
            | (`Nothing, `Nothing) -> 0
            | (`Nothing, _) -> 1
            | (_, `Nothing) -> -1
            | `Expr l, `Expr r -> 
                let module M = 
                    struct
                      type low = 
                          [`Param of param
                          |`Tuple of expr list]
                    end in
                  match l, r with
                    | #M.low, _ -> 1
                    | _, #M.low -> -1
                    | _         -> 0)
         decls)
      
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
        (fun (name,_,_,_,_ as decl) -> 
           (decl,
            <:module_binding< 
              $uid:classname ^ "_"^ name$
              : $uid:classname$.$uid:classname$ with type a = $atype context decl$
             = $apply_defaults (make_module_expr context decl)$ >>))
        decls in
    let sorted_mbinds = make_safe mbinds in
    let mrec =
      <:str_item< open $uid:classname$ module rec $list:sorted_mbinds$ >> in
      match context.params with
        | [] -> mrec
        | _ ->
           let fixed = make_functor <:module_expr< struct $mrec$ end >> in
           let applied = apply_functor <:module_expr< $uid:wrapper_name$ >> 
                                       (List.map (fun (p,_) -> <:module_expr< $uid:NameMap.find p context.argmap$>>) 
                                             context.params) in
           let projected =
             List.map (fun (name,params,rhs,_,_) -> 
                         let modname = classname ^ "_"^ name in
                         let rhs = <:module_expr< struct module P = $applied$ include P.$uid:modname$ end >> in
                           <:str_item< module $uid:modname$ = $make_functor rhs$>>)
               decls in
           let m = <:str_item< module $uid:wrapper_name$ = $fixed$ >> in
             <:str_item< $m$ $list:projected$ >>
       
    let gen_sig ~classname ~context (tname,params,_,_,generated as decl) = 
      if generated then <:sig_item< >> 
      else
        let t = List.fold_right 
          (fun (p,_) m -> <:module_type< functor ($NameMap.find p context.argmap$ : $uid:classname$.$uid:classname$) -> $m$ >>) 
          params
          <:module_type< $uid:classname$.$uid:classname$ with type a = $atype context decl$ >> in
          <:sig_item< module $uid:Printf.sprintf "%s_%s" classname tname$ : $t$ >>

    let gen_sigs ~classname ~context ~decls =
      <:sig_item< $list:List.map (gen_sig ~classname ~context) decls$ >>
end
   
let find_non_regular params tnames decls : name list =
  List.concat_map
    (object 
       inherit [name list] fold as default
       method crush = List.concat
       method expr = function
         | `Constr ([t], args) 
             when NameSet.mem t tnames ->
             (List.concat_map2
                (fun (p,_) a -> match a with
                   | `Param (q,_) when p = q -> []
                   | _ -> [t])
                params
                args)
         | e -> default#expr e
     end)#decl decls

let extract_params = 
  let has_params params (_, ps, _, _, _) = ps = params in
    function
      | [] -> invalid_arg "extract_params"
      | (_,params,_,_,_)::rest
          when List.for_all (has_params params) rest ->
          params
      | (_,_,rhs,_,_)::_ -> 
          (* all types in a clique must have the same parameters *)
          raise (Underivable ("Instances can only be derived for "
                             ^"recursive groups where all types\n"
                             ^"in the group have the same parameters."))

let setup_context loc (tdecls : decl list) : context =
  let params = extract_params tdecls 
  and tnames = NameSet.fromList (List.map (fun (name,_,_,_,_) -> name) tdecls) in
    match find_non_regular params tnames tdecls with
      | _::_ as names -> 
          failwith ("The following types contain non-regular recursion:\n   "
                   ^String.concat ", " names
                   ^"\nderiving does not support non-regular types")
      | [] ->
          let argmap = 
            List.fold_right
              (fun (p,_) m -> NameMap.add p (Printf.sprintf "V_%s" p) m)
              params
              NameMap.empty in 
            { loc = loc;
              argmap = argmap;
              params = params; 
              tnames = tnames }
      
type deriver = Loc.t * context * Type.decl list -> Ast.str_item
and sigderiver = Loc.t * context * Type.decl list -> Ast.sig_item
let derivers : (name, (deriver * sigderiver)) Hashtbl.t = Hashtbl.create 15
let register = Hashtbl.add derivers
let find classname = 
  try Hashtbl.find derivers classname
  with Not_found -> raise (NoSuchClass classname)
