open Util
open Types
open Camlp4.PreCast
module NameMap = Map.Make(String)
module NameSet = Set.Make(String)

type context = {
  loc : Loc.t;
  (* mapping from type parameters to functor arguments *)
  argmap : (name * name) NameMap.t;
  (* name of this type *)
  tname : name;
  (* the type name plus any type parameters *)
  ltype : name * name list;
  (* The type name instantiated with modularized parameters, e.g. (V0.a, V1.a) t  *)
  atype : expr;
  (* The rhs of the type declaration *)
  rtype :  [`Fresh of expr option * repr | `Alias of expr];
  (* all the types in this recursive clique *)
  tnames : NameSet.t; 
}

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

let instantiate_modargs ({loc=loc} as c) t =
  let lookup var = 
    try 
      Constr ([snd (NameMap.find var c.argmap); "a"], [])
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

let cast_pattern ({loc=loc} as c) ?(param="x") t = 
  let t = instantiate_modargs c t in
    (<:patt< $lid:param$ >>,
     <:expr<
            let module M = 
             struct
              type t = $Untranslate.expr loc t$
              let test = function #t -> True | _ -> False
             end in M.test $lid:param$ >>,
     <:expr<
       (let module M = 
            struct
              type t = $Untranslate.expr loc t$
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

let record_pattern loc : Types.field list -> Ast.patt = 
  fun fields ->
    List.fold_left1
      (fun l r -> <:patt< $l$ ; $r$ >>)
      (List.map (fun (label,_,_) -> <:patt< $lid:label$ = $lid:label$ >>) 
         fields)

let tuple_expr loc : Ast.expr list -> Ast.expr = function
  | [] -> <:expr< () >>
  | [x] -> x
  | x::xs -> let cs l r = <:expr< $l$, $r$ >> in
      <:expr< $List.fold_left cs x xs$ >>

let tuple loc ?(param="v") n : Ast.patt * Ast.expr =
  let v n = Printf.sprintf "%s%d" param n in
    match n with
      | 0 -> <:patt< () >>, <:expr< () >>
      | 1 -> <:patt< $lid:v 0$ >>, <:expr< $lid:v 0$ >>
      | n -> List.fold_right1
          (fun (p1,e1) (p2,e2) -> <:patt< $p1$, $p2$ >>, <:expr< $e1$, $e2$ >>)
            (List.map 
               (fun n -> <:patt< $lid:v n$ >>, <:expr< $lid:v n$ >>)
               (List.range 0 n))

(* display a fatal error and exit *)
let error loc (msg : string) =
  Syntax.print_warning loc msg;
  exit 1

let rec modname_from_qname loc =
  fun ~(qname:Types.qname) ~(classname:string) -> 
    match qname with 
      | [] -> invalid_arg "modname_from_qname"
      | [t] -> <:ident< $uid:classname ^ "_"^ t$ >>
      | t::ts -> <:ident< $uid:t$.$modname_from_qname loc ~qname:ts ~classname$ >>

exception Underivable of (string * Types.expr)

class virtual make_module_expr ~context ~classname =
object (self)
  val loc = context.loc

  method mapply (funct : Ast.module_expr) args =
    List.fold_left
      (fun funct param -> <:module_expr< $funct$ $self#expr param$ >>)
      funct
      args

  method virtual variant : Types.variant -> Ast.module_expr
  method virtual sum : Types.summand list -> Ast.module_expr
  method virtual record : Types.field list -> Ast.module_expr

  method param (name, variance) =
    <:module_expr< $uid:snd (NameMap.find name context.argmap)$ >>

  method underscore  = raise (Underivable (classname, Underscore))
  method object_   o = raise (Underivable (classname, Object o))
  method class_    c = raise (Underivable (classname, Class c))
  method alias     a = raise (Underivable (classname, Alias a))
  method label     l = raise (Underivable (classname, Label l))
  method function_ f = raise (Underivable (classname, Function f))

  method constr (qname, args) = 
    let f = (modname_from_qname loc ~qname ~classname) in
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
