(*pp camlp4of *)
module InContext (L : Base.Loc) =
struct
  open Base
  open Utils
  open Types
  open Camlp4.PreCast
  include Base.InContext(L)

  let classname = "Shelve"
  let bind, seq = 
    let bindop = ">>=" and seqop = ">>" in
      <:expr< $lid:bindop$ >>, <:expr< $lid:seqop$ >>

  let wrap ctxt tname decl shelvers unshelver =
    let typs, eqs = List.split 
      (List.map (fun (p,_) -> <:module_expr< $uid:NameMap.find p ctxt.argmap$.Typeable >>, 
                              <:module_expr< $uid:NameMap.find p ctxt.argmap$.Eq >>)
        ctxt.params) in
    let tymod = apply_functor <:module_expr< $uid:"Typeable_" ^ tname$ >>  typs
    and eqmod = apply_functor <:module_expr< $uid:"Eq_" ^ tname$ >> eqs in
    <:module_expr< struct module Typeable = $tymod$ 
                          module Eq = $eqmod$ 
                          module Comp = Dynmap.Comp(Typeable)(Eq)
                          open Shelvehelper
                          type a = $atype ctxt decl$
                          let shelve = function $list:shelvers$
                          open Shelvehelper.Input
                          module W = Whizzy(Typeable)
                          let unshelve = $unshelver$
    end >>

  let rec expr t = (Lazy.force obj) # expr t and rhs t = (Lazy.force obj) # rhs t
  and obj = lazy (new make_module_expr ~classname ~variant ~record ~sum ~allow_private:false)
    
  and polycase ctxt tagspec n : Ast.match_case = match tagspec with
    | Tag (name, None) -> <:match_case<
        (`$name$ as obj) ->
           allocate_store_return (Typeable.makeDynamic obj) Comp.eq
                                 (make_repr ~constructor:$`int:n$ []) >>
    | Tag (name, Some t) -> <:match_case< 
        (`$name$ v1 as obj) ->
           let module M = $expr ctxt t$ in 
           $bind$ (allocate_id (Typeable.makeDynamic obj) Comp.eq) (fun (thisid, freshp) -> 
           if freshp then
             $bind$ (M.shelve v1) (fun mid -> 
             $seq$  (store_repr thisid (make_repr ~constructor:$`int:n$ [mid]))
                    (return thisid))
           else return thisid) >>  

    | Extends t -> 
        let patt, guard, cast = cast_pattern ctxt t in <:match_case<
         ($patt$ as obj) when $guard$ ->
           let module M = $expr ctxt t$ in
           $bind$ (allocate_id (Typeable.makeDynamic obj) Comp.eq) (fun (thisid, freshp) ->
           if freshp then 
             $bind$ (M.shelve $cast$) (fun mid ->
             $seq$  (store_repr thisid (make_repr ~constructor:$`int:n$ [mid]))
                    (return thisid))
           else return thisid) >>

  and polycase_un ctxt tagspec n : Ast.match_case = match tagspec with
    | Tag (name, None) -> <:match_case< $`int:n$, [] -> return `$name$ >>
    | Tag (name, Some t) -> <:match_case< $`int:n$, [x] -> 
      let module M = $expr ctxt t$ in $bind$ (M.unshelve x) (fun o -> return (`$name$ o)) >>
    | Extends t -> <:match_case< $`int:n$, [x] -> let module M = $expr ctxt t$ in (M.unshelve x : M.a n :> a n) >>

  and variant ctxt (tname,_,_,_ as decl) (_, tags) = 
    let ns = (List.range 0 (List.length tags)) in
      wrap ctxt tname decl 
        (List.map2 (polycase ctxt) tags ns)
        <:expr< fun id -> 
                 let f = function $list:List.map2 (polycase_un ctxt) tags ns$
                 in W.whizzySum f id >>

  and case ctxt (name, params') n : Ast.match_case * Ast.match_case = 
    let nparams = List.length params' in
    let ids = List.map (fun n ->  <:expr< $lid:Printf.sprintf "id%d" n$ >>) (List.range 0 nparams) in
    let exp = 
      List.fold_right2
        (fun p n tail -> 
           <:expr< let module M = $expr ctxt p$ in
                       $bind$ (M.shelve $lid:Printf.sprintf "v%d" n$)
                         (fun $lid:Printf.sprintf "id%d" n$ -> $tail$)>>)
        params'
        (List.range 0 nparams)
        <:expr< $seq$ (store_repr thisid (make_repr ~constructor:$`int:n$ $expr_list ids$))
                      (return thisid) >> in
      match params' with
        | [] -> <:match_case< $uid:name$ as obj -> 
                              $bind$ (allocate_id (Typeable.makeDynamic obj) Comp.eq) (fun (thisid,freshp) -> 
                              if freshp then $exp$ else return thisid) >>,
                <:match_case< $`int:n$, [] -> return $uid:name$ >>
        | _  -> <:match_case< $uid:name$ $fst (tuple ~param:"v" nparams)$ as obj -> 
                              $bind$ (allocate_id (Typeable.makeDynamic obj) Comp.eq) (fun (thisid,freshp) ->
                              if freshp then $exp$ else return thisid) >>,
  let _, tuple = tuple ~param:"id" nparams in
  let patt, exp = 
    List.fold_right2 
      (fun n t (pat, exp) ->
         let m = Printf.sprintf "M%d" n and id = Printf.sprintf "id%d" n in
         <:patt< $lid:id$ :: $pat$ >>,
         <:expr< let module $uid:m$ = $expr ctxt t$
                  in $bind$ ($uid:m$.unshelve $lid:id$) (fun $lid:id$ -> $exp$) >>)
      (List.range 0 nparams)
      params'
    (<:patt< [] >>, <:expr< return ($uid:name$ $tuple$) >>) in
    <:match_case< $`int:n$, $patt$ -> $exp$ >>

  and sum ?eq ctxt (tname,_,_,_ as decl) summands =
    let shelvers, unshelvers = List.split (List.map2 (case ctxt) summands (List.range 0 (List.length summands))) in
    wrap ctxt tname decl
      shelvers
      <:expr< fun id -> 
        let f = function $list:unshelvers$ in W.whizzySum f id >>

  and record ?eq ctxt (tname,_,_,_ as decl) (fields : Types.field list) = 
    let patt = record_pattern fields in 
    let tuplemod = 
      expr ctxt
        (`Tuple (List.map (function
                             | (_,([],t),_) -> t
                             | _ -> raise (Underivable
                                             ("Shelve cannot be derived for record types"
                                              ^" with polymorphic fields")))
                   fields)) in
    let nametup = tuple_expr (List.map (fun (f,_,_) -> <:expr< $lid:f$ >>) fields) in
    wrap ctxt tname decl
      [ <:match_case< ($patt$ as obj) -> let module M = $tuplemod$ in M.shelve $nametup$  >> ]
      (let names = List.map (Printf.sprintf "id%d") (List.range 0 (List.length fields)) in
       let patt = 
         List.fold_right
           (fun name patt -> <:patt< $lid:name$::$patt$>>) names <:patt< [] >> in
       let rexp = 
         record_expr (List.map2
                        (fun (label,_,_) name -> (label, <:expr< $lid:name$ >>))
                        fields names) in
       let exp = 
         List.fold_right2
           (fun name (_,(_,t),_) exp -> 
              <:expr< let module M = $expr ctxt t$ 
                       in $bind$ (M.unshelve $lid:name$) 
                               (fun $lid:name$ -> $exp$) >>)
           names fields <:expr< return $rexp$ >> in
      <:expr< fun id -> let f = fun $patt$ -> $exp$ in W.whizzyNoCtor f id >>)
end

let _ = Base.register "Shelve"
  ((fun (loc, context, decls) -> 
      let module M = InContext(struct let loc = loc end) in
        M.generate ~context ~decls ~make_module_expr:M.rhs ~classname:M.classname
          ~default_module:"Shelve_defaults" ()),
   (fun (loc, context, decls) -> 
      let module M = InContext(struct let loc = loc end) in
        M.gen_sigs ~context ~decls ~classname:M.classname))
