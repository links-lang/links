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
type rhs = [`Fresh of expr option * repr |`Alias of expr]

class virtual ['result] fold = 
object (self : 'self)
  method virtual crush : 'result list -> 'result

  method decl (d:decl) =
    self#crush (match d with
                  | (_, _, `Fresh (Some e, r), cs) ->
                      self#expr e :: self#repr r :: List.map self#constraint_ cs
                  | (_, _, `Fresh (None, r), cs) ->
                      self#repr r :: List.map self#constraint_ cs
                  | (_, _, `Alias e, cs) ->
                      self#expr e :: List.map self#constraint_ cs)

  method repr r =
    self#crush (match r with
                    | Sum summands ->
                        List.map self#summand summands
                    | Record fields ->
                        List.map self#field fields)
    
  method field (name, pexpr, flag) =
    self#crush [self#poly_expr pexpr]
      
  method summand (_,es) =
    self#crush (List.map self#expr es)

  method constraint_ (_,e) =
    self#crush [self#expr e]

  method expr e = 
    self#crush (match e with
                    Param _
                  | Underscore
                  | Object _
                  | Class _ -> []
                  | Alias (expr, name) -> [self#expr expr]
                  | Label (_, _, e1, e2) 
                  | Function (e1, e2) -> [self#expr e1; self#expr e2]
                  | Constr (_, exprs)
                  | Tuple exprs  -> List.map self#expr exprs
                  | Variant v -> [self#variant v])

  method poly_expr (params,e) =
    self#crush [self#expr e]

  method variant (_,tagspecs) =
    self#crush (List.map self#tagspec tagspecs)

  method tagspec t =
    self#crush (match t with
                    Tag (_, None) -> []
                  | Tag (_, Some e)
                  | Extends e -> [self#expr e])
end

module Translate =
struct
  open Camlp4.PreCast

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

  let ident : Ast.ident -> name = function
    | Ast.IdAcc _
    | Ast.IdAnt _
    | Ast.IdApp _ -> assert false
    | Ast.IdLid (_, i)
    | Ast.IdUid (_, i) -> i

  let rec qident : Ast.ident -> qname = function
    | Ast.IdAcc (_,l,r) -> qident l @ qident r
    | Ast.IdAnt _
    | Ast.IdApp _ -> assert false
    | Ast.IdLid _
    | Ast.IdUid _ as i -> [ident i]

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
    | Ast.TyRec (loc, fields) -> Left (Record (list field split_semi fields))
    | Ast.TySum (loc, summands) -> Left (Sum (list summand split_or summands))
    | Ast.TyPrv _ -> failwith "private types nyi"
    | t -> Right (expr t)

  let constraints _ = failwith "constraints nyi"

  let decl : Ast.ctyp -> decl = function
    | Ast.TyDcl (loc, name, ps, rhs, cs) ->
        (name,
         params ps,
         (match toplevel rhs with
            | Left dec -> `Fresh (None, dec)
            | Right expr -> `Alias expr
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

module Untranslate =
struct
  open Camlp4.PreCast

  let param loc = function
    | p, None        -> <:ctyp<  '$lid:p$ >>
    | p, Some `Plus  -> <:ctyp< +'$lid:p$ >>
    | p, Some `Minus -> <:ctyp< -'$lid:p$ >>

  let rec qname loc = function
    | [] -> assert false
    | [x] -> <:ident< $lid:x$ >>
    | x::xs -> <:ident< $uid:x$.$qname loc xs$ >>

  let unlist loc join items translate = 
    List.fold_right (join loc) (List.map translate items) (Ast.TyNil loc)

  let pair loc l r = <:ctyp< $l$ * $r$ >>
  let bar loc l r = <:ctyp< $l$ | $r$ >>
  let semi loc l r = <:ctyp< $l$ ; $r$ >>
  let comma loc l r = <:ctyp< $l$ , $r$ >>

  let expr loc = 
    let rec expr = function
        Param p -> param loc p
      | Underscore -> <:ctyp< _ >>
      | Function (f, t) -> <:ctyp< $expr f$ -> $expr t$ >>
      | Tuple ts -> unlist loc pair ts expr
      | Constr (tcon, args) -> app (Ast.TyId (loc, qname loc tcon)) args
      | Variant (`Eq, tags) -> <:ctyp< [  $unlist loc bar tags tagspec$ ] >>
      | Variant (`Gt, tags) -> <:ctyp< [> $unlist loc bar tags tagspec$ ] >>
      | Variant (`Lt, tags) -> <:ctyp< [< $unlist loc bar tags tagspec$ ] >>
    and app f = function
      | []    -> f
      | [x]   -> <:ctyp< $f$ $expr x$ >>
      | x::xs -> app (<:ctyp< $f$ $expr x$ >>) xs
    and tagspec = function
      | Tag (c, None) -> <:ctyp< `$c$ >>
      | Tag (c, Some t) -> <:ctyp< `$c$ of $expr t$ >>
      | Extends t -> <:ctyp< $expr t$ >>
    in expr

  let poly loc (params, t) =
    List.fold_right
      (fun (p : param) (t : Ast.ctyp) -> 
         Ast.TyPol (loc, param loc p, t))
      params
      (expr loc t)

  let rhs loc = 
    let summand (name, (args : expr list)) =
      let args = unlist loc comma args (expr loc) in
        <:ctyp< $uid:name$ of $args$ >> in
    let field ((name, t, mut) : field) = 
      match mut with
        | `Mutable   -> <:ctyp< mutable $lid:name$ : $poly loc t$ >>
        | `Immutable -> <:ctyp<         $lid:name$ : $poly loc t$ >> in
    let repr = function
      | Sum summands  -> unlist loc bar summands summand
      | Record fields -> unlist loc semi fields field
    in function
      | `Fresh (None, t) -> repr t
      | `Alias t         -> expr loc t

  let decl loc (name, params, rhs, constraints) =
    Ast.TyDcl (loc, name, List.map (param loc) params,
               rhs, []) (* TODO: constraints *)
end
