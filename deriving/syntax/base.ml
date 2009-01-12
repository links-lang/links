(*pp camlp4of *)
open Utils
open Type
open Camlp4.PreCast

exception Underivable of Loc.t * string
exception NoSuchClass of string

(* Let's use a function to move from type variables to
   function names rather than a map. *)
let tvar_name tvar = String.capitalize tvar

let record_pattern ~loc ?(prefix="") (fields : Type.field list) : Ast.patt = 
  <:patt<{$list:
            (List.map (fun (label,_,_) -> <:patt< $lid:label$ = $lid:prefix ^ label$ >>) 
                    fields) $}>>

let record_expression ~loc ?(prefix="") : Type.field list -> Ast.expr = 
  fun fields ->
    let es = List.fold_left1
      (fun l r -> <:rec_binding< $l$ ; $r$ >>)
      (List.map (fun (label,_,_) -> <:rec_binding< $lid:label$ = $lid:prefix ^ label$ >>) 
         fields) in
      Ast.ExRec (loc, es, Ast.ExNil loc)

let seq ~loc l r = <:expr< $l$ ; $r$ >>

let contains_tvars, contains_tvars_decl =
  let o = object
     inherit [bool] fold as default
     method crush = List.exists F.id
     method expr = function
       | `Tyvar _ -> true
       | e -> default#expr e
  end in (o#expr, o#decl)

let record_expr ~loc : (string * Ast.expr) list -> Ast.expr = 
  fun fields ->
    let fs = 
    List.fold_left1 
      (fun l r -> <:rec_binding< $l$ ; $r$ >>)
      (List.map (fun (label, exp) -> <:rec_binding< $lid:label$ = $exp$ >>) 
         fields) in
      Ast.ExRec (loc, fs, Ast.ExNil loc)

let expr_list ~loc : Ast.expr list -> Ast.expr = 
    (fun exprs ->
       List.fold_right 
         (fun car cdr -> <:expr< $car$ :: $cdr$ >>)
         exprs
       <:expr< [] >>)

let patt_list ~loc : Ast.patt list -> Ast.patt = 
    (fun patts ->
       List.fold_right 
         (fun car cdr -> <:patt< $car$ :: $cdr$ >>)
         patts
       <:patt< [] >>)

let tuple_expr ~loc : Ast.expr list -> Ast.expr = function
  | [] -> <:expr< () >>
  | [x] -> x
  | x::xs -> Ast.ExTup (loc, List.fold_left (fun e t -> Ast.ExCom (loc, e,t)) x xs)

let tuple ~loc ?(param="v") n : Ast.patt * Ast.expr =
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

let rec modname_from_qname ~loc ~qname ~classname =
  match qname with 
    | [] -> invalid_arg "modname_from_qname"
    | [t] -> <:ident< $uid:classname ^ "_"^ t$ >>
    | t::ts -> <:ident< $uid:t$.$modname_from_qname ~loc ~qname:ts ~classname$ >>

let make_safe : (rhs * Ast.module_binding) list -> Ast.module_binding list =
  fun decls ->
  (* re-order a set of mutually recursive modules in an attempt to
     make initialization problems less likely *) 
    (* TODO: this needs looking at again *)
  List.map snd
    (List.sort
       (fun (lrhs, _) (rrhs, _) -> match lrhs, rrhs with
          (* aliases to types in the group score higher than
             everything else.

             In general, things that must come first receive a
             positive score when they occur on the left and a
             negative score when they occur on the right. *)
          | (`Fresh _|`Variant _), (`Fresh _|`Variant _) -> 0
          | (`Fresh _|`Variant _), _ -> -1
          | _, (`Fresh _|`Variant _) -> 1
          | (#expr as l), (#expr as r) -> 
              let module M = 
                  struct
                    type low = 
                        [`Tyvar of name
                        |`Tuple of atomic list]
                  end in
                match l, r with
                  | #M.low, _ -> 1
                  | _, #M.low -> -1
                  | _         -> 0)
       decls)

let apply_functor ~loc (f : Ast.module_expr) (args : Ast.ident list) : Ast.module_expr =
    List.fold_left (fun f p -> <:module_expr< $f$ ($id:p$) >>) f args
    
class virtual deriver ?default ~loc ~classname ~allow_private =
object (self)

  val loc = loc
  val classname = classname

  method mapply (funct : Ast.module_expr) (args : atomic list) =
    apply_functor ~loc funct (List.map self#atomic args)

  method atype (name, params : name * param list) : Ast.ctyp =
    List.fold_left
      (fun f (p, _) -> <:ctyp< $uid:tvar_name p$.a $f$ >>)
      <:ctyp< $lid:name$ >>
      params

  method superclasses : name list = []
  method virtual variant : name * param list ->  [`Gt | `Lt | `Eq ] * tagspec list -> Ast.module_expr
  method virtual sum     : name * param list -> ?eq:expr -> summand list -> Ast.module_expr
  method virtual record  : name * param list -> ?eq:expr -> field list -> Ast.module_expr
  method virtual tuple   : name * param list -> atomic list -> Ast.module_expr

  method decls params (decls : (is_generated * rhs) NameMap.t list) : Ast.str_item =
    let () = List.iter (fun m -> assert (not (NameMap.is_empty m))) decls in

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
    let wrapper_name = Printf.sprintf "%s_%s" classname (random_id 32)  in
    let make_functor = 
      List.fold_right 
        (fun (p,_) rhs -> 
           let arg = tvar_name p in
             <:module_expr< functor ($arg$ : $uid:classname$.$uid:classname$) -> $rhs$ >>)
        params in
    let apply_defaults mexpr = match default with
      | None -> mexpr
      | Some default -> <:module_expr< $default$ ($mexpr$) >> in
    let mbinds : (Type.rhs * Ast.module_binding) list list =
      List.map
        (fun decl -> 
           NameMap.fold
             (fun name (_, rhs) binds -> 
                (rhs,
                 <:module_binding< $uid:classname ^ "_"^ name$
                                 : $uid:classname$.$uid:classname$ with type a = $self#atype (name, params)$
                                 = $apply_defaults (self#rhs (name, params) rhs)$ >>) :: binds)
             decl
             [])
        decls in
    let sorted_mbinds : Ast.module_binding list list = List.map make_safe mbinds in
    let mrecs
        = List.fold_right (fun m ms -> <:str_item< module rec $list:m$ $ms$ >>) sorted_mbinds <:str_item< >> in
    let mrecs =
      <:str_item< open $uid:classname$ $mrecs$ >> in
      match params with
        | [] -> mrecs
        | _ ->
            let fixed = make_functor <:module_expr< struct $mrecs$ end >> in
            let applied = apply_functor ~loc
                            <:module_expr< $uid:wrapper_name$ >> 
                            (List.map (fun (p,_) -> Ast.IdUid (loc, tvar_name p)) params) in
            let names : NameMap.key list = List.concat_map (fun map -> NameMap.fold (fun i _ is -> i :: is) map []) decls in
            let projected =
              List.map
                (fun name -> 
                   let modname = classname ^ "_"^ name in
                   let rhs = <:module_expr< struct module P = $applied$ include P.$uid:modname$ end >> in
                     (<:str_item< module $uid:modname$ = $make_functor rhs$>>))
                names
            in
            let m = <:str_item< module $uid:wrapper_name$ = $fixed$ >> in
              <:str_item< $m$ $list:projected$ >>


  method signature (params, names : sigdecl) : Ast.sig_item =
    (* TODO: distinguish generated names *)
    let mktype (n : name) : Ast.module_type =
      List.fold_right 
        (fun (p,_) m -> <:module_type< functor ($tvar_name p$ : $uid:classname$.$uid:classname$) -> $m$ >>) 
        params
      <:module_type< $uid:classname$.$uid:classname$ with type a = $self#atype (n, params)$ >> 
    in
      NameMap.fold
        (fun name rhs si ->
           <:sig_item< module $uid:classname ^ "_" ^ name$ : $mktype name$
                       $si$ >>)
        names
        <:sig_item< >>
    
  method tyvar tvar =
    Ast.IdUid (loc, tvar_name tvar)

  method ctor : qname -> Ast.ident =
    fun qname -> modname_from_qname ~loc ~qname ~classname    

  method atomic : atomic -> Ast.ident = function
    | `Local l -> self#local l
    | `Tyvar t -> self#tyvar t

  method function_ f = raise (Underivable (loc, classname ^ " cannot be derived for function types"))

  method constr (qname, args) = 
    self#mapply <:module_expr< $id:self#ctor qname$ >> args

  method local (n : name) : Ast.ident =
    Ast.IdUid (loc, classname ^ "_" ^ n)

  method rhs : name * param list -> rhs -> Ast.module_expr = fun ctyp -> function 
    | `Fresh (_, _, (`Private : [`Private|`Public])) when not allow_private ->
        raise (Underivable (loc, "The class "^ classname ^" cannot be derived for private types"))
    | `Fresh (eq, `Sum summands, _) -> self#sum ctyp ?eq summands
    | `Fresh (eq, `Record fields, _) -> self#record ctyp ?eq fields
    | `Tyvar p    -> <:module_expr< $id:self#tyvar p$ >>
    | `Function f -> self#function_  f
    | `Appl c     -> self#constr     c
    | `Tuple t    -> self#tuple      ctyp t
    | `Local l    -> <:module_expr< $id:self#local l$ >>
    | `Variant v  -> self#variant    ctyp v
end

type generator = loc:Ast.Loc.t -> deriver

let derivers : (name, generator) Hashtbl.t = Hashtbl.create 15

let register = Hashtbl.add derivers

let find classname = 
  try Hashtbl.find derivers classname
  with Not_found -> raise (NoSuchClass classname)

let is_registered (classname : name) : bool =
  try let _ = find classname in true
  with NoSuchClass _ -> false
