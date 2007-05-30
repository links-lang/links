open Util
open Types
open Camlp4.PreCast
module NameMap = Map.Make(String)
module NameSet = Set.Make(String)

type context = {
  loc : Loc.t;
  (* mapping from type parameters to functor arguments *)
  argmap : name NameMap.t;
  (* name of this type *)
  tname : name;
  (* the type name plus any type parameters *)
  ltype : name * name list;
  (* The rhs instantiated with modularized parameters, e.g. (V0.a, V1.a) t  *)
  atype : expr;
  (* The rhs of the type declaration *)
  rtype :  [`Fresh of expr option * repr | `Alias of expr];
  (* all the types in this recursive clique *)
  tnames : NameSet.t; 
  (* the original definition *)
  tdec : decl;
}

exception Underivable of (string * Types.expr)

(* display a fatal error and exit *)
let error loc (msg : string) =
  Syntax.print_warning loc msg;
  exit 1

module type Context = sig val context: context end

module InContext(C : Context) =
struct
  open C
  let loc = C.context.loc
  module Untranslate = Untranslate(struct let loc = loc end)

  let instantiate (lookup : name -> expr) : expr -> expr =
    let rec inst = function
      | Param (name, _) -> lookup name 
      | Underscore      -> Underscore
      | Function (l, r) -> Function (inst l, inst r)
      | Constr (c, ts)  -> Constr (c, List.map inst ts)
      | Tuple es        -> Tuple (List.map inst es)
      | Alias (e, n)    -> Alias (inst e, n)
      | Variant (v, ts) -> Variant (v, List.map inst_tag ts)
      | _ -> assert false
    and inst_tag = function
      | Tag (n, Some t) ->  Tag (n, Some (inst t))
      | Tag _ as t -> t
      | Extends t -> Extends (inst t)
    in inst

  let instantiate_modargs t =
    let lookup var = 
      try 
        Constr ([NameMap.find var context.argmap; "a"], [])
      with Not_found ->
        failwith ("Unbound type parameter '" ^ var)
    in instantiate lookup t

  let contains_tvars : expr -> bool = 
    (object
       inherit [bool] fold as default
       method crush = List.exists (fun x -> x)
       method expr = function
         | Param _ -> true
         | e -> default#expr e
     end) # expr

  let substitute env =
    (object
       inherit transform as default
       method expr = function
         | Param (p,v) when NameMap.mem p env -> 
             Param (NameMap.find p env,v)
         | e -> default# expr e
     end) # expr

  let cast_pattern ?(param="x") t = 
    let t = instantiate_modargs t in
      (<:patt< $lid:param$ >>,
       <:expr<
         let module M = 
             struct
               type t = $Untranslate.expr t$
               let test = function #t -> True | _ -> False
             end in M.test $lid:param$ >>,
       <:expr<
         (let module M = 
              struct
                type t = $Untranslate.expr t$
                let cast = function #t as t -> t | _ -> assert False
              end in M.cast $lid:param$ )>>)

  let random_id length = 
    let idchars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'" in
    let nidchars = String.length idchars in
    let s = String.create length in 
      for i = 0 to length - 1 do 
        s.[i] <- idchars.[Random.int nidchars]
      done;
      s

  let seq l r = <:expr< $l$ ; $r$ >>

  let record_pattern ?(prefix="") : Types.field list -> Ast.patt = 
    fun fields ->
      List.fold_left1
        (fun l r -> <:patt< $l$ ; $r$ >>)
        (List.map (fun (label,_,_) -> <:patt< $lid:label$ = $lid:prefix ^ label$ >>) 
           fields)

  let record_expr : (string * Ast.expr) list -> Ast.expr = 
    fun fields ->
      List.fold_left1 
        (fun l r -> <:expr< $l$ ; $r$ >>)
        (List.map (fun (label, exp) -> <:expr< $lid:label$ = $exp$ >>) 
           fields)


  let record_expression ?(prefix="") : Types.field list -> Ast.expr = 
    fun fields ->
      List.fold_left1
        (fun l r -> <:expr< $l$ ; $r$ >>)
        (List.map (fun (label,_,_) -> <:expr< $lid:label$ = $lid:prefix ^ label$ >>) 
           fields)

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
        | n -> List.fold_right1
            (fun (p1,e1) (p2,e2) -> <:patt< $p1$, $p2$ >>, <:expr< $e1$, $e2$ >>)
              (List.map 
                 (fun n -> <:patt< $lid:v n$ >>, <:expr< $lid:v n$ >>)
                 (List.range 0 n))

  let rec modname_from_qname ~qname ~classname =
    match qname with 
      | [] -> invalid_arg "modname_from_qname"
      | [t] -> <:ident< $uid:classname ^ "_"^ t$ >>
      | t::ts -> <:ident< $uid:t$.$modname_from_qname ~qname:ts ~classname$ >>
          
  class make_module_expr ~classname ~variant ~record ~sum =
  object (self)

    method mapply (funct : Ast.module_expr) args =
      List.fold_left
        (fun funct param -> <:module_expr< $funct$ $self#expr param$ >>)
        funct
        args

    method variant = variant
    method sum = sum
    method record = record

    method param (name, variance) =
      <:module_expr< $uid:NameMap.find name context.argmap$ >>

    method underscore  = raise (Underivable (classname, Underscore))
    method object_   o = raise (Underivable (classname, Object o))
    method class_    c = raise (Underivable (classname, Class c))
    method alias     a = raise (Underivable (classname, Alias a))
    method label     l = raise (Underivable (classname, Label l))
    method function_ f = raise (Underivable (classname, Function f))

    method constr (qname, args) = 
      let f = (modname_from_qname ~qname ~classname) in
        self # mapply (Ast.MeId (loc, f)) args

    method tuple args =
      let f = <:module_expr< $uid:Printf.sprintf "%s_%d" 
        classname (List.length args)$ >> in
        self # mapply f args

    method expr : expr -> Ast.module_expr = function
      | Param p    -> self # param p
      | Underscore -> self # underscore
      | Object o   -> self # object_ o
      | Class c    -> self # class_ c
      | Alias a    -> self # alias a
      | Label l    -> self # label l 
      | Function f -> self # function_ f
      | Constr c   -> self # constr c
      | Tuple t    -> self # tuple t
      | Variant v  -> self # variant v

    method rhs : Types.rhs -> Ast.module_expr = function
      | `Fresh (None, Sum summands) -> self # sum summands
      | `Fresh (None, Record fields) -> self # record fields
      | `Alias e -> self # expr e
  end

(*  let expr_class ~classname ~variant ~record ~sum =
  object (self)
    inherit make_module_expr ~classname
    method variant = variant
    method record = record
    method sum = sum
  end
*)

  let extract_params classname = 
    let has_params params (_, ps, _, _) = ps = params in
      function
        | [] -> invalid_arg "extract_params"
        | (_,params,_,_)::rest
            when List.for_all (has_params params) rest ->
            params
        | (_,_,rhs,_)::_ -> 
            (* all types in a clique must have the same parameters *)
            raise (Underivable (classname, rhs))
            

  let generate ~csts ~make_module_expr ~classname ~default_module =
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
    let params = extract_params classname (List.map snd csts) in
(*    let _ = ensure_no_polymorphic_recursion in *)
    let wrapper_name = Printf.sprintf "%s_%s" classname (random_id 32)  in
    let make_functor = 
      List.fold_right 
        (fun (p,_) rhs -> 
           let arg = NameMap.find p context.argmap in
             <:module_expr< functor ($arg$ : $uid:classname$) -> $rhs$ >>)
        params in
    let apply_defaults mexpr = match default_module with
      | None -> mexpr
      | Some default -> <:module_expr< $uid:default$ ($mexpr$) >> in
    let mbinds =
      List.map 
        (fun (context, (name,params,rhs,constraints)) -> 
           <:module_binding< 
             $uid:classname ^ "_"^ context.tname$
           : $uid:classname$ with type a = $Untranslate.expr context.atype$
           = $apply_defaults (make_module_expr rhs)$
             >>)
        csts in
    let mrec =
      <:str_item< module rec $list:mbinds$ >> in
    let fixed = make_functor <:module_expr< struct $mrec$ end >> in
    let projected =
      List.map (fun (ctxt, (name,params,rhs,constraints)) -> 
                  let modname = classname ^ "_"^ context.tname in
                  let rhs = <:module_expr< $uid:wrapper_name$ . $uid:modname$ >> in
                    <:str_item< module $uid:modname$ = $make_functor rhs$>>)
        csts in
    let m = <:str_item< module $uid:wrapper_name$ = $fixed$ >> in
      <:str_item< $m$ $List.hd projected$ >>
end

let setup_contexts loc types =
  let tdecls = List.map Translate.decl types in
  let tnames = List.fold_right (fun (n,_,_,_) ns -> 
                                  NameSet.add n ns) 
                    tdecls NameSet.empty in
    List.map 
      (fun (name,params,rhs,constraints as dec) -> 
         let argmap = 
           List.fold_right
             (fun (p,_) m -> NameMap.add p (Printf.sprintf "V_%s" p) m)
             params
             NameMap.empty
         and ltype  = 
           name, List.map fst params in
         let atype  = 
           let param (p,_) = Constr ([NameMap.find p argmap; "a"], []) in
             Constr ([name], List.map param params)
         in { loc = loc;
              argmap = argmap;
              tname  = name;
              ltype  = ltype;
              atype  = atype;
              rtype  = rhs;
              tnames = tnames;
              tdec   = dec}, dec)
      tdecls

type deriver = (context * decl) list -> Ast.str_item
let derivers : (name, deriver) Hashtbl.t = Hashtbl.create 15
let register = Hashtbl.add derivers
let find = Hashtbl.find derivers
