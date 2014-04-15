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

let rec modname_from_qname ~loc ~typename qname =
  match qname with 
    | [] -> invalid_arg "modname_from_qname"
    | [t] -> <:ident< $lid:typename ^ "_"^ t$ >>
    | t::ts -> <:ident< $uid:t$.$modname_from_qname ~loc ~typename ts$ >>

let make_safe : (name * rhs * Ast.binding) list -> (Ast.binding) list * [`Recursive | `Nonrecursive] =
  fun decls ->
  (* re-order a set of mutually recursive modules in an attempt to
     make initialization problems less likely *) 
    (* TODO: this needs looking at again *)
    let sorted =     (List.sort
       (fun (_, lrhs, _) (_, rrhs, _) -> match lrhs, rrhs with
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
       decls) in
      match sorted with
        | [(name, rhs, mbind)] -> 
            let recursive = object
              inherit [bool] fold as default
              method crush = List.exists F.id
              method localtype (`Local (lname, _)) = (lname = name)
            end # rhs rhs in
              [mbind], if recursive then `Recursive else `Nonrecursive
        | _ -> (List.map (fun (_,_,b) -> b) sorted, `Recursive)


let apply_functor ~loc (f : Ast.expr) (args : Ast.expr list) : Ast.expr =
    List.fold_left (fun f p -> <:expr< $f$ $p$ >>) f args
    
module rec Deriver :
sig
class virtual deriver :
  loc:Camlp4.PreCast.Ast.loc ->
  classname:string ->
  allow_private:bool ->
  object
    val classname : string
    val loc : Camlp4.PreCast.Ast.loc
    val virtual methods : Type.name list
    val superclasses : Type.name list
    val typename : string
    method atomic : Type.atomic -> Camlp4.PreCast.Ast.expr
    method atype : Type.name * Type.param list -> Camlp4.PreCast.Ast.ctyp
    method constr : Type.qname * Type.atomic list -> Camlp4.PreCast.Ast.expr
    method decls :
      Type.param list ->
      (Type.is_generated * Type.rhs) Type.NameMap.t list ->
      Camlp4.PreCast.Ast.str_item
    method expand : Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.expr
    method function_ : Type.atomic * Type.atomic -> Camlp4.PreCast.Ast.expr
    method local : Type.name * Type.name list -> Camlp4.PreCast.Ast.expr
    method mapply :
      Camlp4.PreCast.Ast.expr -> Type.atomic list -> Camlp4.PreCast.Ast.expr
    method mktype :
      Type.NameMap.key -> Type.param list -> Camlp4.PreCast.Ast.ctyp
    method virtual record :
      Type.name * Type.param list ->
      ?eq:Type.expr -> Type.field list -> Camlp4.PreCast.Ast.expr
    method rhs :
      Type.name * Type.param list -> Type.rhs -> Camlp4.PreCast.Ast.expr
    method signature : Type.sigdecl -> Camlp4.PreCast.Ast.sig_item
    method virtual sum :
      Type.name * Type.param list ->
      ?eq:Type.expr -> Type.summand list -> Camlp4.PreCast.Ast.expr
    method superclasses : Type.name list
    method virtual tuple :
      Type.name * Type.param list ->
      Type.atomic list -> Camlp4.PreCast.Ast.expr
    method tyvar : Type.name -> Camlp4.PreCast.Ast.expr
    method virtual variant :
      Type.name * Type.param list ->
      [ `Eq | `Gt | `Lt ] * Type.tagspec list -> Camlp4.PreCast.Ast.expr
  end
type generator = loc:Camlp4.PreCast.Ast.Loc.t -> deriver
val derivers : (Type.name, generator) Hashtbl.t
val register : Type.name -> generator -> unit
val find : Type.name -> generator
val is_registered : Type.name -> bool
end =
struct
  type generator = loc:Ast.Loc.t -> Deriver.deriver
  
  let derivers : (name, generator) Hashtbl.t = Hashtbl.create 15
  
  let register c = Hashtbl.add derivers c
  
  let find classname = 
    try Hashtbl.find derivers classname
    with Not_found -> raise (NoSuchClass classname)
  
  let is_registered (classname : name) : bool =
    try let _ = find classname in true
    with NoSuchClass _ -> false

  class virtual deriver ~loc ~classname ~allow_private =
  object (self)
  
    val loc = loc
    val typename = String.lowercase classname
    val classname = classname
    val superclasses : name list = []
    val virtual methods : name list
  
    method superclasses = superclasses
  
    method mapply (funct : Ast.expr) (args : atomic list) : Ast.expr =
      apply_functor ~loc funct (List.map self#atomic args)
  
    method atype (name, params : name * param list) : Ast.ctyp =
      List.fold_left
        (fun f (p, _) -> <:ctyp< '$p$ $f$ >>)
        <:ctyp< $lid:name$ >>
        params
  
    method virtual variant : name * param list ->  [`Gt | `Lt | `Eq ] * tagspec list -> Ast.expr
    method virtual sum     : name * param list -> ?eq:expr -> summand list -> Ast.expr
    method virtual record  : name * param list -> ?eq:expr -> field list -> Ast.expr
    method virtual tuple   : name * param list -> atomic list -> Ast.expr
  
    method mktype name params : Ast.ctyp =
      let maker, typ =
        List.fold_left
          (fun (maker, typ) (name, _) -> 
             ((fun p -> maker <:ctyp< '$name$ $lid:typename$ -> $p$ >>),
              <:ctyp< '$name$ $typ$ >>))
          ((fun x -> x), <:ctyp< $lid:name$ >>)
          params in
        maker <:ctyp< ($typ$ $lid:typename$) >>
  
    method decls params (decls : (is_generated * rhs) NameMap.t list) : Ast.str_item =
      let () = List.iter (fun m -> assert (not (NameMap.is_empty m))) decls in
      let mk_outer_fun =
        List.fold_right 
          (fun (param, _) rhs ->
             <:expr< fun ($lid:param$ : '$param$ $lid:typename$ ) -> $rhs$ >>)
          params in
      let mbinds : (name * Type.rhs * Ast.binding) list list =
        List.map
          (fun decl -> 
             NameMap.fold
               (fun name (_, rhs) binds -> 
                  (name,
                   rhs,
                   <:binding< $lid:typename ^ "_"^ name$
                            : $self#mktype name params$
                            = $mk_outer_fun (self#rhs (name, params) rhs)$ >>) :: binds)
               decl
               [])
          decls in
      let sorted_mbinds : (Ast.binding list * _) list = List.map make_safe mbinds in
      let mrecs 
          = List.fold_right (fun (m,recp) (ms : Ast.str_item) -> 
                               match m, recp with
                                 | [ <:binding< $lid:u$ : $signature$ = $impl$ >> ], `Nonrecursive ->
                                     <:str_item< let $lid:u$ : $signature$ = $impl$ ;; $ms$ >>
                                 | _ ->
                                     <:str_item< let rec $list:m$ $ms$ >>) sorted_mbinds <:str_item< >> in
  
  
        <:str_item< open $uid:classname$ $mrecs$ >> 
  
    method signature (params, names : sigdecl) : Ast.sig_item =
      (* TODO: distinguish generated names *)
        NameMap.fold
          (fun name rhs si ->
             <:sig_item< $si$ ;; val $lid:typename ^ "_" ^ name$ : $self#mktype name params$ >>)
          names
          <:sig_item< open $uid:classname$ >>
      
    method tyvar tvar =
      <:expr< $lid:tvar$ >>
  
    method atomic : atomic -> Ast.expr = function
      | `Local l -> self#local l
      | `Tyvar t -> self#tyvar t
      | `Appl c  -> self#constr c
  
    method function_ f = raise (Underivable (loc, classname ^ " cannot be derived for function types"))
  
    method constr (qname, args) = 
      self#mapply <:expr< $id:modname_from_qname ~loc ~typename qname$ >> args
  
    method local (n, params: name * name list) : Ast.expr =
      self#mapply (<:expr< $lid:typename ^ "_" ^ n$ >>)
        (List.map (fun p -> `Tyvar p) params)
  
    method rhs : name * param list -> rhs -> Ast.expr = fun ctyp -> function 
      | `Fresh (_, _, (`Private : [`Private|`Public])) when not allow_private ->
          raise (Underivable (loc, "The class "^ classname ^" cannot be derived for private types"))
      | `Fresh (eq, `Sum summands, _) -> self#sum ctyp ?eq summands
      | `Fresh (eq, `Record fields, _) -> self#record ctyp ?eq fields
      | `Tyvar p    -> self#tyvar      p
      | `Function f -> self#function_  f
      | `Appl c     -> self#expand (self#constr     c)
      | `Tuple t    -> self#tuple      ctyp t
      | `Local l    -> self#expand (self#local      l)
      | `Variant v  -> self#variant    ctyp v
  
    method expand : Ast.expr -> Ast.expr = 
      fun expr ->
        let superclass_fields =
          List.map (fun cl -> 
                      let field = "_" ^ cl in
                      let impl = find cl ~loc in
                      let superexpr = <:expr< $expr$.$lid:field$ >> in
                        <:rec_binding< $lid:field$ = $impl#expand superexpr$ >>) superclasses
        and method_fields = 
          List.map (fun meth -> <:rec_binding< $lid:meth$ = fun etavar -> ($expr$.$lid:meth$  etavar) >>) methods
        in
        let all_fields = superclass_fields @ method_fields in
        let fields = 
          List.fold_right
            (fun r1 r2 -> <:rec_binding< $r1$ ; $r2$ >>)
            (List.tl all_fields)
            (List.hd all_fields) in
          Ast.ExRec (loc, fields, <:expr< >>)
  end
  
end

include Deriver
