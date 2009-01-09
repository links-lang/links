(*pp camlp4of *)

open Base
open Utils
open Type
open Camlp4.PreCast

let tuple_functors = [2;3;4;5;6]

let classname = "Pickle"

let bind ~loc = let bindop = ">>=" in <:expr< $lid:bindop$ >>
let seq  ~loc = let seqop  = ">>"  in <:expr< $lid:seqop$  >>


let wrap ~loc ~atype ~tymod ~eqmod ~picklers ~unpickler =
  <:module_expr< struct open Eq 
                        open Typeable
                        module T = $tymod$
                        module E = $eqmod$
                        type a = $atype$
                        open Write
                        let pickle = let module W = Utils(T)(E) in function $list:picklers$
                        open Read
                        let unpickle = let module W = Utils(T) in $unpickler$
  end >>

(*
  Replace each var 'a ... 'z with the corresponding functor argument
  type A.a ... Z.a
*)
let instantiate_vars = 
object
  inherit Ast.map as default
  method ctyp = function
    | Ast.TyQuo (loc, id) -> <:ctyp< $uid:tvar_name id$.a >>
    | e                   -> default#ctyp e 
end

class pickle ~loc =
object (self)
  inherit Base.deriver ~loc  ~classname ~allow_private:false ~default:<:module_expr< Pickle.Defaults >>

  method private extension tname ts : Ast.match_case =
    (* Try each extension in turn.  If we get an UnknownTag failure,
       try the next one.  This is

       * safe because any two extensions that define the same tag
       must be compatible at that point

       * fast because we can tell on the first integer comparison
       whether we've picked the right path or not.
    *)
      let inner = List.fold_right 
        (fun t exp -> 
           let m = match t with
                     | `Ctor t -> self#ctor t
                     | `Local t -> self#local t in
             <:expr< try $exp$
                         with UnknownTag _ -> ($id:m$.unpickle id :> a Read.m) >>)
        ts
        <:expr< raise (UnknownTag (n, ($str:"Unexpected tag encountered during unpickling of "
                                       ^tname$))) >>
    in <:match_case< n,_ -> $inner$ >>

    method private polycase tagspec : Ast.match_case = match tagspec with
    | `Tag (name, None) -> <:match_case<
        (`$name$ as obj) ->
          W.allocate obj
              (fun thisid -> 
                 W.store_repr thisid
                    (Repr.make ~constructor:$`int:(tag_hash name)$ [])) >>
    | `Tag (name, Some t) -> <:match_case<
        (`$name$ v1 as obj) ->
           W.allocate obj
            (fun thisid ->
             $bind ~loc$ ($id:self#atomic t$.pickle v1)
                    (fun mid -> 
                    (W.store_repr thisid
                        (Repr.make ~constructor:$`int:(tag_hash name)$ [mid])))) >>
    | `Local t -> 
        <:match_case< (# $lid:t$) as obj -> $id:self#local t$.pickle obj >>

    method private polycase_un tagspec : Ast.match_case = match tagspec with
    | (name, None)   -> <:match_case< $`int:(tag_hash name)$, [] -> return `$name$ >>
    | (name, Some t) -> <:match_case< $`int:(tag_hash name)$, [x] -> 
      $bind ~loc$ ($id:self#atomic t$.unpickle x) (fun o -> return (`$name$ o)) >>

    method variant (tname, params as atype) (_, tagspec) : Ast.module_expr =
      let unpickler = 
        let tags, extensions = either_partition
          (function (`Tag (name,t)) -> Left (name,t) | (`Local _) as t -> Right t) tagspec in
        let tag_cases : Ast.match_case list = List.map self#polycase_un tags in
        let extension_case : Ast.match_case = self#extension tname extensions in
          <:expr< fun id -> W.sum (function $list:tag_cases @ [extension_case]$) id >>
      in
        wrap
          ~loc
          ~atype:(self#atype atype)
          ~tymod:(self#typeable_instance atype)
          ~eqmod:(self#eq_instance atype)
          ~picklers:(List.map self#polycase tagspec)
          ~unpickler


    method tuple (name, params) ts : Ast.module_expr = 
      let nts = List.length ts in
        if List.mem nts tuple_functors then
          apply_functor ~loc <:module_expr< $uid:Printf.sprintf "Pickle_%d" nts$ >> 
            (List.map self#atomic ts)
        else
          let ids = (List.mapn (fun t n -> (Printf.sprintf "id%d" n, t)) ts) in
          let eidlist = expr_list ~loc (List.map (fun (id,_) -> <:expr< $lid:id$ >>) ids) in
          let pidlist = patt_list ~loc (List.map (fun (id,_) -> <:patt< $lid:id$ >>) ids) in
          let tpatt,texpr = tuple ~loc ~param:"id" nts in
          let tymod = self#typeable_instance (name, params)
          and eqmod = self#eq_instance (name, params)
          and picklers =
            let inner = 
              List.fold_right
                (fun (id,t) expr -> 
                   <:expr< $bind ~loc$ ($id:self#atomic t$.pickle $lid:id$) 
                              (fun $lid:id$ -> $expr$) >>)
                ids
              <:expr< W.store_repr this (Repr.make $eidlist$) >> in
              [ <:match_case< ($tpatt$ as obj) -> 
                               W.allocate obj (fun this -> $inner$) >>]
                
          and unpickler = 
            let msg = "unexpected object encountered unpickling "^string_of_int nts^"-tuple" in
            let inner = 
              List.fold_right 
                (fun (id,t) expr ->
                   <:expr< $bind ~loc$ ($id:self#atomic t$.unpickle $lid:id$) (fun $lid:id$ -> $expr$) >>)
                ids
              <:expr< return $texpr$ >> in
              <:expr< W.tuple
                      (function 
                         | $pidlist$ -> $inner$
                         | _ -> raise (UnpicklingError $str:msg$)) >>
          and atype = self#atype (name, params) in
            wrap ~loc ~atype ~tymod ~eqmod ~picklers ~unpickler

    method private case (name, params' : summand) (n : int) : Ast.match_case * Ast.match_case = 
      let nparams = List.length params' in
      let ids = List.map (fun n ->  <:expr< $lid:Printf.sprintf "id%d" n$ >>) (List.range 0 nparams) in
      let exp = 
        List.fold_right2
          (fun p n tail -> 
             <:expr< $bind ~loc$ ($id:self#atomic p$.pickle $lid:Printf.sprintf "v%d" n$)
                            (fun $lid:Printf.sprintf "id%d" n$ -> $tail$)>>)
          params'
          (List.range 0 nparams)
          <:expr< W.store_repr thisid (Repr.make ~constructor:$`int:n$ $expr_list ~loc ids$) >> in
        match params' with
          | [] -> <:match_case< $uid:name$ as obj -> 
                                W.allocate obj (fun thisid -> $exp$) >>,
                  <:match_case< $`int:n$, [] -> return $uid:name$ >>
          | _  -> <:match_case< $uid:name$ $fst (tuple ~loc ~param:"v" nparams)$ as obj -> 
                                W.allocate obj (fun thisid -> $exp$) >>,
      let _, tuple = tuple ~loc ~param:"id" nparams in
      let patt, exp = 
        List.fold_right2 
          (fun n t (pat, exp) ->
             let id = Printf.sprintf "id%d" n in
             <:patt< $lid:id$ :: $pat$ >>,
             <:expr< $bind ~loc$ ($id:self#atomic t$.unpickle $lid:id$) (fun $lid:id$ -> $exp$) >>)
          (List.range 0 nparams)
          params'
        (<:patt< [] >>, <:expr< return ($uid:name$ $tuple$) >>) in
        <:match_case< $`int:n$, $patt$ -> $exp$ >>


    method sum (tname, params) ?eq (summands : summand list) : Ast.module_expr =
      let picklers, unpicklers = List.split (List.mapn self#case summands) in
      wrap
        ~loc
        ~atype:(self#atype (tname, params))
        ~tymod:(self#typeable_instance (tname, params))
        ~eqmod:(self#eq_instance (tname, params))
        ~picklers
        ~unpickler:<:expr< fun id -> 
                           let f = function
                                    $list:unpicklers$ 
                                   | n,_ -> raise (UnpicklingError ($str:"Unexpected tag when unpickling "
                                                                    ^tname^": "$^ string_of_int n))
                           in W.sum f id >>

    method record (tname, params) ?eq (fields : Type.field list) : Ast.module_expr =
      let picklers =
        let inner =
          List.fold_right 
            (fun (id,t,_) e ->
               <:expr< $bind ~loc$ ($id:self#atomic t$.pickle $lid:id$) (fun $lid:id$ -> $e$) >>)
            fields
          <:expr< (W.store_repr this
                     (Repr.make
                        $expr_list ~loc (List.map (fun (id,_,_) -> <:expr< $lid:id$ >>) fields)$)) >>
        in
          [ <:match_case< ($record_pattern ~loc fields$ as obj) ->
            W.allocate obj (fun this -> $inner$) >> ] in

      let unpickle_record_bindings e = 
        (* TODO: 

           This is a small problem: we need to instantiate
           type variables in the field types with functor arguments.

           However, the field types are constrained to atomics, which
           doesn't include qualified names.

           Probably the proper thing to do is to have some alternative 
           translation mechanism.
        *)
 
        let mutables = List.map (fun (name, rhs, _) -> (name, rhs, `Mutable)) fields in
        let mutrec = instantiate_vars#ctyp (Untranslate.repr ~loc params (`Record mutables)) in
          <:expr< let module Mutable = struct type t = $mutrec$ end in $e$ >> in
                 
      let unpickler = 
        let msg = "unexpected object encountered unpickling "^tname in
        let assignments = 
          List.fold_right
            (fun (id,_,_) exp ->
         <:expr< this.Mutable.$lid:id$ <- $lid:id$; $exp$ >>)
            fields
          <:expr< return self >> in
        let inner = 
          List.fold_right
            (fun (id,t,_) exp ->
               <:expr< $bind ~loc$ ($id:self#atomic t$.unpickle $lid:id$) (fun $lid:id$ -> $exp$) >>)
            fields
            assignments in
        let idpat = patt_list ~loc (List.map (fun (id,_,_) -> <:patt< $lid:id$ >>) fields) in
          unpickle_record_bindings
            (<:expr< W.record
               (fun self -> function
                  | $idpat$ -> let this = (Obj.magic self : Mutable.t) in $inner$
                  | _ -> raise (UnpicklingError $str:msg$)) $`int:List.length fields$ >>)
      in 
      wrap ~loc ~picklers ~unpickler
           ~atype:(self#atype (tname, params))
           ~tymod:(self#typeable_instance (tname, params))
           ~eqmod:(self#eq_instance (tname, params))

    method private typeable_instance (name, params) =
      let args = List.map (fun (a, _) -> `Tyvar a) params in
        apply_functor ~loc
        <:module_expr< $uid:"Typeable_"^ name$ >>  
          (List.map (fun a -> <:ident< $id:self#atomic a$.T >>) args)

    method private eq_instance (name, params) =
      let args = List.map (fun (a, _) -> `Tyvar a) params in
        apply_functor ~loc
        <:module_expr< $uid:"Eq_"^ name$ >>  
          (List.map (fun a -> <:ident< $id:self#atomic a$.E >>) args)
end

let () = Base.register classname (new pickle)

