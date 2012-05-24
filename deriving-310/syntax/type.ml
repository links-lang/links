(*pp camlp4orf *)
(* More convenient representation for types, and translation from the
   Camlp4 representation *)

exception Unsupported of Camlp4.PreCast.Ast.loc * string
exception Unexpected of Camlp4.PreCast.Ast.loc * string

open Utils

(* auxiliary definitions *)
type name = string
type qname = name list
module NameMap = StringMap
module NameSet = Set.Make(String)


(* Things not handled:
     records with polymorphic field types
     constraints 
     object types
     classes
     types with non-regular recursion
     types called 'a' (!)
*)


(* A type parameter with an optional variance annotation *)
type param = name * [`Plus | `Minus] option

(* A reference to one of the type constructors bound in the same
   declaration.  e.g. within the declaration

     type t1 = e1
      and ...
      and tn = en

   this refers to one of the ti.
*)
type localtype = [`Local of name * name list ]

(* A reference to a type parameter *)
type tyvar = [`Tyvar of name ]

(* An "atomic" expression.  These are the only things allowed to
   appear as subexpressions. *)
type atomic = [ localtype | tyvar | `Appl of qname * atomic list ]

(* A polymorphic variant declaration *)
type tagspec = [`Tag     of name * atomic option 
               | localtype | `Appl of qname * atomic list ]
type variant = [`Variant of [`Gt | `Lt | `Eq ] * tagspec list ]

(* A record type *)
type field = name * atomic * [`Mutable | `Immutable ]
type record = [`Record of field list]

(* A sum type *)
type summand = name * atomic list
type sum = [`Sum of summand list ]

(* A generative type *)
type fresh = [ sum | record ]

(* A type expression *)
type expr = [ `Function of (atomic * atomic)
            | `Tuple    of atomic list
            | variant
            | atomic ]

(* The right hand side of a declaration *)
type rhs = [`Fresh of expr option * fresh * [`Private|`Public] 
           | expr]

type sigrhs = [ rhs | `Nothing]

type is_generated = bool

(* A type declaration group *)
type decl = param list * (is_generated * rhs) NameMap.t

(* A type declaration group in a signature *)
type sigdecl = param list * (is_generated * sigrhs) NameMap.t

class virtual ['result] fold = 
object (self : 'self)
  method virtual crush : 'result list -> 'result

  method param (_ : param) = 
    self#crush []

  method localtype (_ : localtype) =
    self#crush []

  method tyvar (_ : tyvar) =
    self#crush []

  method atomic (a : atomic) =
    self#crush (match a with
                  | #localtype as l -> [self#localtype l]
                  | #tyvar     as t -> [self#tyvar t]
                  | `Appl (_, ats)  -> List.map self#atomic ats)

  method tagspec (t : tagspec) =
    self#crush (match t with
                  | `Tag (_, None)    -> []
                  | `Tag (_, Some at) -> [self#atomic at]
                  | #localtype as at  -> [self#localtype at]
                  | `Appl (_, ats)    -> List.map self#atomic ats)

  method variant (`Variant (_, tags) : variant) =
    self#crush (List.map self#tagspec tags)

  method field (_, at, _ : field) =
    self#crush [self#atomic at]

  method record (`Record fields : record) =
    self#crush (List.map self#field fields)

  method summand (_, ats) = 
    self#crush (List.map self#atomic ats)
      
  method sum (`Sum summands : sum) =
    self#crush (List.map self#summand summands)

  method fresh (f : fresh) =
    self#crush (match f with
                  | #sum    as s -> [self#sum s]
                  | #record as r -> [self#record r])

  method expr (e : expr) =
    self#crush (match e with
                  | `Function (l, r) -> [self#atomic l ; self#atomic r]
                  | `Tuple ats -> List.map self#atomic ats
                  | #variant as v -> [self#variant v]
                  | #atomic as a -> [self#atomic a] )

  method rhs (r : rhs) =
    self#crush (match r with
                  | `Fresh (None, fresh, _) -> [self#fresh fresh]
                  | `Fresh (Some e, fresh, _) -> [self#expr e; self#fresh fresh]
                  | #expr as e -> [self#expr e])

  method sigrhs (r : sigrhs) =
    self#crush (match r with
                  | #rhs as r -> [self#rhs r]
                  | `Nothing  -> [])

  method decl (params, rhs : decl) =
    self#crush (List.map self#param params
                @ (NameMap.fold 
                     (fun _ (_, rhs) rs -> self#rhs rhs :: rs)
                     rhs
                     []))

  method sigdecl (params, rhs : sigdecl) =
    self#crush (List.map self#param params
                @ (NameMap.fold 
                     (fun _ (_, rhs) rs -> self#sigrhs rhs :: rs)
                     rhs
                     []))
end




class transform = 
object (self : 'self)

  method param (p : param) = p

  method localtype (l : localtype) = l

  method tyvar (t : tyvar) = t

  method atomic : atomic -> atomic = function 
    | # localtype as l -> (self#localtype l :> atomic)
    | # tyvar     as t -> (self#tyvar t     :> atomic)
    | `Appl (q, ats)   -> `Appl (q, List.map self#atomic ats)

  method tagspec : tagspec -> tagspec = function
    | `Tag (n, None)    -> `Tag (n, None)
    | `Tag (n, Some at) -> `Tag (n, Some (self#atomic at))
    | #localtype as at  -> (self#localtype at :> tagspec)
    | `Appl (q, ats)    -> `Appl (q, List.map self#atomic ats)
 
  method variant : variant -> variant = function
    | `Variant (spec, tagspecs) -> `Variant (spec, List.map self#tagspec tagspecs)

  method field (name, at, mut) = (name, self#atomic at, mut)

  method record : record -> record = function
    | `Record fields ->`Record (List.map self#field fields)

  method summand (name, ats) = (name, List.map self#atomic ats)

  method sum : sum -> sum = function
    | `Sum summands -> `Sum (List.map self#summand summands)

  method fresh : fresh -> fresh = function
    | #sum    as s -> (self#sum s    :> fresh)
    | #record as r -> (self#record r :> fresh)

  method expr : expr -> expr = function
    | `Function (lat, rat)         -> `Function (self#atomic lat, self#atomic rat)
    | `Tuple ats                   -> `Tuple (List.map self#atomic ats)
    | #variant as v                -> (self#variant v :> expr)
    | #atomic  as a                -> (self#atomic a  :> expr)

  method rhs : rhs -> rhs = function
    | `Fresh (None, fresh, priv)   -> `Fresh (None,               self#fresh fresh, priv)
    | `Fresh (Some e, fresh, priv) -> `Fresh (Some (self#expr e), self#fresh fresh, priv)
    | #expr as e                   -> (self#expr e :> rhs)

  method sigrhs : sigrhs -> sigrhs = function
    | #rhs as r                    -> (self#rhs r :> sigrhs)
    | `Nothing                     -> `Nothing

  method decl (params, rhss) : decl= 
    (List.map self#param params,
     NameMap.map (fun (g, r) -> (g, self#rhs r)) rhss)

  method sigdecl (params, rhss) : sigdecl = 
    (List.map self#param params,
     NameMap.map (fun (g, r) -> (g, self#sigrhs r)) rhss)
end


module Translate =
struct
  open Camlp4.PreCast

  let unsupported loc msg = raise (Unsupported (loc, msg))
  let unexpected loc msg = raise (Unexpected (loc, msg))

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

  type vmap = (name * variant * name option) list

  let fresh_name, set_name_prefix
    = 
    let name_prefix = ref "" in
    let counter = ref 0 in
      ((fun () -> 
        incr counter;
          "deriving_" ^ !name_prefix ^ "_" ^ string_of_int !counter),
       (fun name -> name_prefix := name; counter := 0))

  (* map from type constructors to their definitions *) 
  type bindings = (name * (is_generated * sigrhs)) list
  (* map from as-variables to the type constructors that replace them *) 
  type alias_map = name NameMap.t
  (* everything generated during translation *)
  type env = { bindings : bindings;
               aliases  : alias_map }

  let empty_env : env = { bindings = []; aliases = NameMap.empty }
  let (++) l r = { bindings = l.bindings @ r.bindings ;
                   aliases  = NameMap.union_disjoint2 l.aliases r.aliases }
  let join_envs envs = List.fold_right (++) envs empty_env
  let bind_binding n generated rhs env = { env with bindings = (n, (generated, rhs)) :: env.bindings }
  let bind_alias n rhs env = { env with aliases = NameMap.add n rhs env.aliases }

  (* Accept a binding list of the form
  
     ('a1, ... 'an) t1 = e1
     ...
     ('a1, ... 'an) tn = en
  
     and return
  
     (t1, [[e1]]) ... (tn, [[en]]), (s1, [[f1]]), ... (sm, [[fm]])
  
     where each si = fi is a new type definitions created from a
     nested type expression in one of the ei.
  
     Fail if any of the occurrences of the ti in the ei is applied to
     something other than 'a1...'an
  *)
  let rhss (params : name list) (ctyps : (name * Ast.ctyp) list) : env = 
    let module M = struct
      let locals = NameSet.fromList (List.map fst ctyps)

      let rec expr : Ast.ctyp -> expr * env =
        fun ctyp ->
          let loc = Ast.loc_of_ctyp ctyp in
          match ctyp with
            (* Variant *)
            | Ast.TyVrnEq (_, t)  -> (variant t `Eq :> expr * _)
            | Ast.TyVrnSup (_, t) -> (variant t `Gt :> expr * _)
            | Ast.TyVrnInf (_, t) -> (variant t `Lt :> expr * _)
            | Ast.TyVrnInfSup _   -> unsupported loc "< > variant types"
            | Ast.TyNil _         -> unsupported loc "empty type declarations"
            | Ast.TyObj _         -> unsupported loc "object types"
            | Ast.TyCls _         -> unsupported loc "class types"
            | Ast.TyQuP (_,_)
            | Ast.TyQuM (_,_)     -> unexpected loc "variance annotation on non-parameter"
            | Ast.TyQuo (_,name)  -> `Tyvar name, empty_env
            | Ast.TySum _
            | Ast.TyRec _         -> unexpected loc "nested toplevel element"
            | Ast.TyAny _         -> unsupported loc "`_' in type definitions"
            | Ast.TyLab _         -> unsupported loc "label types"
            | Ast.TyAli (loc, _, Ast.TyQuo (_, name)) 
                when List.mem name params ->
                unsupported loc "alias names that coincide with parameter names"
            | Ast.TyAli (_, t, Ast.TyQuo (_, name)) -> 
                let name' = fresh_name () in
                let e, binds = expr t in
                  (`Local (name', params),
                   (bind_alias name name'
                      (bind_binding name' true (e :> sigrhs) binds)))
            | Ast.TyArr (_,f, t)  -> 
                let f, binds = atomic f and t, binds' = atomic t in
                  `Function (f, t), binds ++ binds'
            | Ast.TyApp _ as t    -> 
                begin match application t  with
                  | `Appl ([id], args), env 
                      when NameSet.mem id locals ->
                      if args = List.map (fun n -> `Tyvar n) params
                      then `Local (id, params), env
                      else unsupported loc "non-regular recursion"
                  | `Appl (tcon, args), env -> `Appl (tcon, args), env
                end
            | Ast.TyId (loc, i) ->
                begin match qident i with 
                  | ["a"] -> unsupported loc "types called 'a' (!)"
                  | [id] when NameSet.mem id locals ->
                      if params = []
                      then `Local (id, params), empty_env
                      else unsupported loc "non-regular recursion"
                  | qid -> (*`Ctor qid*)`Appl (qid, []), empty_env
                end
            | Ast.TyTup (_, t) ->
                let es, binds = List.split (list atomic split_star t) in 
                  `Tuple es, join_envs binds
            | t -> unexpected loc ("the type expression : " ^ PPAst.ctyp t)
      and variant : Ast.ctyp -> [`Gt | `Lt | `Eq ] -> variant * env =
        fun tags spec ->
          let tags, binds = List.split (list tagspec split_or tags) in
            `Variant (spec, tags), join_envs binds
      and application : Ast.ctyp -> _ * env = function
        | Ast.TyApp (_, (Ast.TyApp _ as a), t) -> 
            let `Appl (tcon, args), binds = application a in
            let e, binds' = atomic t in
              `Appl (tcon, args @ [e]), binds ++ binds'
        | Ast.TyApp (_, (Ast.TyId (_, tcon)), t) -> 
            let e, binds = atomic t in
              `Appl (qident tcon, [e]), binds
        | _ -> assert false
      and tagspec = function
        | Ast.TyVrn (_,tag)                  -> `Tag (tag, None), empty_env
        | Ast.TyOf (_, Ast.TyVrn (_,tag), t) -> 
            let spec, binds = atomic t in
              `Tag (tag, Some spec), binds
        | t                                  -> 
            match atomic t with
              | (#localtype as typ, binds)
              | (`Appl _    as typ, binds) -> typ, binds
              | _                          -> assert false
      and atomic t : atomic * env = 
        match expr t with
          | # atomic as a, binds -> a, binds
          | e            , binds -> 
              let name = fresh_name () in
              let e, binds = expr t in
                (`Local (name, params), bind_binding name true (e :> sigrhs) binds)

      let field : Ast.ctyp -> field * env = function 
        | Ast.TyCol (loc, Ast.TyId _, Ast.TyMut (_, Ast.TyPol _)) 
        | Ast.TyCol (loc, Ast.TyId _, Ast.TyPol _) -> 
            unsupported loc "polymorphic record fields"
        | Ast.TyCol (_, Ast.TyId (_,name), Ast.TyMut (_, t)) ->
            let p, v = atomic t in (ident name, p, `Mutable), v
        | Ast.TyCol (_, Ast.TyId (_,name), t) ->
            let p, v = atomic t in (ident name, p, `Immutable), v
        | _ -> assert false

      let record (t : Ast.ctyp) : record * env =
        let fields, vs = List.split (list field split_semi t) in 
          `Record fields, join_envs vs

      let summand : Ast.ctyp -> summand * env = function 
        | Ast.TyId (_, c)                  -> (ident c, []), empty_env
        | Ast.TyOf (_, Ast.TyId (_, c), t) -> 
            let es, binds = List.split (list atomic split_and t) in
              (ident c, es), join_envs binds
        | _                                -> assert false

      let sum (t : Ast.ctyp) : sum * env =
        let summands, binds = List.split (list summand split_or t) in
          `Sum summands, join_envs binds

      (* Accept a type declaration right hand side 
         ('a1, ... 'an) t = e
  
         and return
  
           (t, [[e]]), (s1, [[f1]]), ... (sm, [[fm]])
  
         where each si = fi is a new type definitions created from a
         nested type expression in e.
  
         Fail if any of the occurrences of `locals' in the e is applied to
         something other than 'a1...'an 
      *)
      let rhs ~(name:name) : Ast.ctyp -> env = 
        let () = set_name_prefix name in
        let bind = bind_binding name false in function
        (* Record types (private, public, with and without equations) *)
        | Ast.TyPrv (_, (Ast.TyRec (loc, t))) ->
            let r, binds = record t in
              bind ( `Fresh (None, (r :> fresh), `Private)) binds
        | Ast.TyMan (_, eq, Ast.TyPrv (_, (Ast.TyRec (loc, t)))) ->
            let r, rbinds = record t 
            and e, ebinds = expr eq in
              bind (`Fresh (Some e, (r :> fresh), `Private)) (rbinds ++ ebinds)
        | Ast.TyRec (loc, t) ->
            let r, binds = record t in
              bind ( `Fresh (None, (r :> fresh), `Public)) binds
        | Ast.TyMan (_, eq, (Ast.TyRec (loc, t))) ->
            let r, rbinds = record t 
            and e, ebinds = expr   eq in
              bind (`Fresh (Some e, (r :> fresh), `Public)) (rbinds ++ ebinds)

        (* Sum types (private, public, with and without equations) *)
        | Ast.TyPrv (_, (Ast.TySum (loc, t))) -> 
            let s, binds = sum t in
              bind (`Fresh (None, (s :> fresh), `Private)) binds
        | Ast.TyMan (_, eq, Ast.TyPrv (_, (Ast.TySum (loc, t)))) ->
            let r, sbinds = sum t 
            and e, ebinds = expr eq in
              bind (`Fresh (Some e, (r :> fresh), `Private)) (sbinds ++ ebinds)
        | Ast.TySum (loc, t) -> 
            let s, binds = sum t in
              bind (`Fresh (None, (s :> fresh), `Public)) binds
        | Ast.TyMan (_, eq, (Ast.TySum (loc, t))) ->
            let r, sbinds = sum t 
            and e, ebinds = expr eq in
              bind (`Fresh (Some e, (r :> fresh), `Public)) (sbinds ++ ebinds)

        (* Other private type *)
        | Ast.TyPrv (loc, _) -> 
            unsupported loc "private types other than records and sums"

        (* Nothing *)
        | Ast.TyNil _ ->
            bind `Nothing empty_env

        (* Type aliases *)
        | t -> 
            let e, binds = expr t in
              bind (e :> sigrhs) binds

      let results = 
        List.fold_right 
          (fun (name, t) results -> rhs ~name t ++ results)
          ctyps
          empty_env
      end
      in M.results
 
   let extract_params : Ast.ctyp -> param list * (name * Ast.ctyp) = function
     | Ast.TyDcl (loc, _   , _ , _  , _::_) -> raise (Unsupported (loc, "type constraints"))
     | Ast.TyDcl (loc, name, ps, rhs, []  ) -> (params ps, (name, rhs))
     | _                                    -> assert false

   let check_params loc : param list list -> param list = 
     let check_eq ps' ps =
       if ps' <> ps
       then unsupported loc "different parameters for types within the same group"
       else ps in
     fun ps -> List.fold_right check_eq ps (List.hd ps)

   let substitute_aliases params : alias_map -> sigdecl -> sigdecl = fun map ->
   object
     inherit transform as super
     method atomic = function
       | `Tyvar p when NameMap.mem p map ->
           `Local (NameMap.find p map, List.map fst params)
       | e -> super#atomic e
   end # sigdecl

   let sigdecl (ctyp : Ast.ctyp) : sigdecl =
     let decls         : Ast.ctyp list     = list F.id split_and ctyp in
     let params, decls : param list list * (name * Ast.ctyp) list = List.split (List.map extract_params decls) in
     let params        : param list        = check_params (Ast.loc_of_ctyp ctyp) params in
     let decls         : env               = rhss (List.map fst params) decls in 
       substitute_aliases params decls.aliases
         (params, NameMap.fromList decls.bindings)

   let decl (ctyp : Ast.ctyp) : decl =
     let params, bindings = sigdecl ctyp in
       (params, NameMap.map (function 
                               | _, #rhs as d -> d
                               | _ -> unsupported (Ast.loc_of_ctyp ctyp) 
                                   "type declarations with empty right hand sides")
                           bindings)
end

module Untranslate =
struct
  open Camlp4.PreCast

  let param ~loc = function
    | p, None        -> <:ctyp<  '$lid:p$ >>
    | p, Some `Plus  -> <:ctyp< +'$lid:p$ >>
    | p, Some `Minus -> <:ctyp< -'$lid:p$ >>

  let tyvar ~loc p = <:ctyp<  '$lid:p$ >>

  let rec qname ~loc = function
    | [] -> assert false
    | [x] -> <:ident< $lid:x$ >>
    | x::xs -> <:ident< $uid:x$.$qname ~loc xs$ >>
        
  let unlist ~loc join items translate =
      List.fold_right join (List.map translate items) (Ast.TyNil loc)

  let pair  ~loc l r = Ast.TySta (loc, l,r)
  let bar   ~loc l r = <:ctyp< $l$ | $r$ >>
  let semi  ~loc l r = <:ctyp< $l$ ; $r$ >>
  let comma ~loc l r = <:ctyp< $l$ , $r$ >>
  let and_  ~loc l r = <:ctyp< $l$ and $r$ >>

  let rec localtype ~loc params : name -> Ast.ctyp =
    fun name -> app ~loc <:ctyp< $lid:name$ >> 
      (List.map (fun v -> `Tyvar v) params)

  and atomic ~loc : atomic -> Ast.ctyp = function
    | `Local (l, params) -> localtype ~loc params l
    | `Tyvar t -> tyvar ~loc t    
    | `Appl (tcon, args) -> app ~loc (Ast.TyId (loc, qname ~loc tcon)) args

  and app ~loc f = function
    | []    -> f
    | [x]   -> <:ctyp< $f$ $atomic ~loc x$ >>
    | x::xs -> app ~loc (<:ctyp< $f$ $atomic ~loc x$ >>) xs

  let tagspec ~loc : tagspec -> _ = function
    | `Tag (c, None) -> <:ctyp< `$c$ >>
    | `Tag (c, Some t) -> <:ctyp< `$c$ of $atomic ~loc t$ >>
    | `Local (a, params) -> localtype ~loc params a
    | `Appl (tcon, args) -> app ~loc (Ast.TyId (loc, qname ~loc tcon)) args

  let expr ~loc : expr -> Ast.ctyp = 
    let atomic = atomic ~loc in
    function
      | #atomic as a -> atomic a
      | `Function (f, t) -> <:ctyp< ($atomic f$ -> $atomic t$) >>
      | `Tuple [t] -> atomic t
      | `Tuple ts -> Ast.TyTup (loc, unlist ~loc (pair ~loc) ts atomic)
      | `Variant (`Eq, tags) -> <:ctyp< [= $unlist ~loc (bar ~loc) tags (tagspec ~loc)$ ] >>
      | `Variant (`Gt, tags) -> <:ctyp< [> $unlist ~loc (bar ~loc) tags (tagspec ~loc)$ ] >>
      | `Variant (`Lt, tags) -> <:ctyp< [< $unlist ~loc (bar ~loc) tags (tagspec ~loc)$ ] >>


  let summand ~loc (name, args) =
    let args = unlist ~loc (and_ ~loc) args (atomic ~loc) in
      <:ctyp< $uid:name$ of $args$ >> 

  let field ~loc ((name, t, mut) : field) = match mut with
    | `Mutable   -> <:ctyp< $lid:name$ : mutable $atomic ~loc t$ >> (* mutable l : t doesn't work; perhaps a camlp4 bug *)
    | `Immutable -> <:ctyp< $lid:name$ : $atomic ~loc t$ >>

  let repr ~loc = function
    | `Sum summands  -> Ast.TySum (loc, unlist ~loc (bar ~loc) summands (summand ~loc))
    | `Record fields -> <:ctyp< { $unlist ~loc (semi ~loc) fields (field ~loc)$ }>>

  let rhs ~loc : rhs -> Ast.ctyp = function
    | `Fresh (None, t, `Private) -> <:ctyp< private $repr ~loc t$ >>
    | `Fresh (None, t, `Public) -> repr ~loc t
    | `Fresh (Some e, t, `Private) -> <:ctyp< $expr ~loc e$ == private $repr ~loc t$ >>
    | `Fresh (Some e, t, `Public) -> Ast.TyMan (loc, expr ~loc e, repr ~loc t)
    | #expr as t          -> expr ~loc t

  let rrhs ~loc : sigrhs -> Ast.ctyp = function
    | #rhs as r -> rhs ~loc r
    | `Nothing  -> <:ctyp< >>
          
  let rec app_params ~loc f = function
    | []    -> f
    | [x]   -> <:ctyp< $x$ $f$ >>
    | x::xs -> app_params ~loc (<:ctyp< $x$ $f$ >>) xs

  let sigdecl ~loc (params, bindings : sigdecl) : Ast.ctyp list =
    let params' = List.map (param ~loc) params in
    NameMap.fold
      (fun name (_, r) dcl ->
         let rhs = rrhs ~loc r in
           Ast.TyDcl (loc, name, params', rhs, []):: dcl)
      bindings
      []

  let decl = (sigdecl :> loc:_ -> _ * (_ * rhs) NameMap.t -> _)
end
