(* More convenient representation for types, and translation from the
Camlp4 representation *)


(* auxiliary definitions *)
type ('a,'b) either = Left of 'a | Right of 'b
type name = string
type qname = name list

type param = name * [`Plus | `Minus] option

(* no support for private types yet *)
type decl = name * param list
    * [`Fresh of expr option (* "equation" *) * repr | `Alias of expr]
    * constraint_ list
and repr = 
    Sum of summand list
  | Record of field list
and field = name * poly_expr * [`Mutable | `Immutable]
and summand = name * expr list
and constraint_ = name * expr
and expr =  (* elements that can be nested *)
    Param of param
  | Underscore
  | Label of ([`Optional|`NonOptional] * name * expr * expr)
  | Function of (expr * expr)
  | Constr of (qname * expr list)
  | Tuple of expr list
  | Alias of (expr * name)
  | Variant of variant
  | Object of [`NYI]
  | Class of [`NYI]
and poly_expr = param list * expr
(* no support for < > variants yet.
   no support for '&' yet.
*)
and variant = [`Gt | `Lt | `Eq] * tagspec list
and tagspec = Tag of name * expr option 
            | Extends of expr


open Camlp4.PreCast

module Translate =
struct

  (* TODO:
     - finish implementing toplevel
     - handle identifiers properly
     - translate constraints
  *)
  
  let param = function
    | Ast.TyQuP (loc, name) -> name, Some `Plus
    | Ast.TyQuM (loc, name) -> name, Some `Minus
    | Ast.TyQuo (loc, name)  -> name, None
    | _ -> assert false

  let params = List.map param

  let split_and = function
    | Ast.TyAnd (_,l,r) -> Left (l,r)
    | t -> Right t

  let split_comma = function
    | Ast.TyCom (_,l,r) -> Left (l,r)
    | t -> Right t

  let split_semi = function
    | Ast.TySem (_,l,r) -> Left (l,r)
    | t -> Right t

  let split_or = function
    | Ast.TyOr (_,l,r) -> Left (l,r)
    | t -> Right t

  let split_amp = function
    | Ast.TyAmp (_,l,r) -> Left (l,r)
    | t -> Right t

  let split_ofamp = function
    | Ast.TyOfAmp (_,l,r) -> Left (l,r)
    | t -> Right t

  let split_star = function
    | Ast.TySta (_,l,r) -> Left (l,r)
    | t -> Right t

  let list (one : Ast.ctyp -> 'a) (split : Ast.ctyp -> (Ast.ctyp * Ast.ctyp, Ast.ctyp) either) : Ast.ctyp -> 'a list = 
    let rec aux = function
      | Ast.TyNil _ -> []
      | ctyp ->
          match split ctyp with
            | Left (l,r) -> aux l @ aux r
            | Right item -> [one item]
    in aux

  let ident : Ast.ident -> name = failwith "nyi"

  let qident : Ast.ident -> qname = failwith "nyi"

  let rec expr = function
    | Ast.TyObj _ -> Object `NYI
    | Ast.TyCls _ -> Class `NYI
    | Ast.TyQuP (_,_)
    | Ast.TyQuM (_,_)
    | Ast.TyQuo (_,_) as p -> Param (param p)
    | Ast.TySum _
    | Ast.TyRec _ -> failwith "top level element found nested"
    | Ast.TyAny _ -> Underscore
    | Ast.TyArr (_,f,t) -> Function (expr f,
                                     expr t)
    | Ast.TyApp _ as app -> Constr (application app)
    | Ast.TyTup (_, t) -> Tuple (list expr split_star t)
    | Ast.TyVrnEq (_, t)  -> Variant (`Eq, list tagspec split_or t)
    | Ast.TyVrnSup (_, t) -> Variant (`Gt, list tagspec split_or t)
    | Ast.TyVrnInf (_, t) -> Variant (`Lt, list tagspec split_or t)
    | Ast.TyVrnInfSup (_, _, _) -> failwith "nyi"
    | _ -> assert false
  and tagspec = function
    | Ast.TyVrn (_,tag)                  -> Tag (tag, None)
    | Ast.TyOf (_, Ast.TyVrn (_,tag), t) -> Tag (tag, Some (Tuple (list expr split_comma t)))
    | t                                  -> Extends (expr t)
  and application = function
    | Ast.TyApp (_, (Ast.TyApp _ as a), t) -> 
        let tcon, args = application a in
          tcon, args @ [expr t]
    | Ast.TyApp (_, (Ast.TyId (_, tcon)), t) -> (qident tcon, [expr t])
    | _ -> assert false

  let rec polyexpr : Ast.ctyp -> poly_expr = function
    | Ast.TyPol (_, ps, t) -> 
        let ps', t' = polyexpr t in
          list param split_comma ps @ ps', t'
    | t -> [], expr t


  let field : Ast.ctyp -> field = function 
    | Ast.TyMut (_, Ast.TyCol (_, Ast.TyId (_,name), t)) -> 
        (ident name, polyexpr t, `Mutable)
    | Ast.TyCol (_, Ast.TyId (_,name), t) ->
        (ident name, polyexpr t, `Immutable)
    | _ -> assert false

  let summand = function 
    | Ast.TyId (_, c)                  -> ident c, []
    | Ast.TyOf (_, Ast.TyId (_, c), t) -> ident c, list expr split_comma t
    | _                                -> assert false

  let toplevel = function
    | Ast.TyRec (loc, fields) -> Record (list field split_semi fields)
    | Ast.TySum (loc, summands) -> Sum (list summand split_or summands)
    | Ast.TyPrv _ -> failwith "nyi"
        (* what to do here? *)

  let constraints _ = failwith "nyi"

  let decl : Ast.ctyp -> decl = function
    | Ast.TyDcl (loc, name, ps, rhs, cs) ->
        (name,
         params ps,
         (match expr rhs with (* should handle `TyMan' in here somewhere *)
            | _ -> failwith "nyi"
         ),
         constraints cs)
    | _ -> assert false


  (* Not yet implemented in the translation:
   * label types (TyOlb, TyLab)
   * Aliases (TyAli)
   * Equations (TyMan)
   * constraints
   *)

end
