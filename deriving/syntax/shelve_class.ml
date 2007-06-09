(*pp camlp4of *)
module InContext (L : Base.Loc) =
struct
  open Base
  open Utils
  open Types
  open Camlp4.PreCast
  include Base.InContext(L)

  let typeable_defaults t = <:module_expr< Typeable.Typeable_defaults($t$) >>

  module Typeable = Typeable_class.InContext(L)
  module Eq       = Eq_class.InContext(L)

  let classname = "Shelve"
  let bind, seq = 
    let bindop = ">>=" and seqop = ">>" in
      <:expr< $lid:bindop$ >>, <:expr< $lid:seqop$ >>


  let typeable_instance ctxt tname =
    <:module_expr< Typeable.Typeable_defaults(
    $apply_functor <:module_expr< $uid:"Typeable_" ^ tname$ >> 
      (List.map (fun (p,_) -> <:module_expr< $uid:NameMap.find p ctxt.argmap$.T >>)
         ctxt.params)$) >>

  let eq_instance ctxt tname =
    apply_functor <:module_expr< $uid:"Eq_" ^ tname$ >> 
      (List.map (fun (p,_) -> <:module_expr< $uid:NameMap.find p ctxt.argmap$.E >>)
         ctxt.params)

  let rebind_params ctxt name : Ast.str_item = 
    NameMap.fold
      (fun _ param s -> <:str_item< $s$ module $uid:param$ = $uid:param$.$uid:name$ >>)
      ctxt.argmap
      <:str_item< >>

  let wrap ~ctxt ~atype ~tymod ~eqmod ~shelvers ~unshelver =
    <:module_expr< struct open Eq open Typeable
                          module T = $tymod$
                          module E = $eqmod$
                          module Comp = Dynmap.Comp(T)(E)
                          open Shelvehelper
                          type a = $atype$
                          let shelve = function $list:shelvers$
                          open Shelvehelper.Input
                          module W = Whizzy(T)
                          let unshelve = $unshelver$
    end >>

    let instance = object (self)
    inherit make_module_expr ~classname ~allow_private:false

    method tuple ctxt ts = 
      let nts = List.length ts in
      let ids = (List.mapn (fun t n -> (Printf.sprintf "id%d" n, t)) ts) in
      let eidlist = expr_list (List.map (fun (id,_) -> <:expr< $lid:id$ >>) ids) in
      let pidlist = patt_list (List.map (fun (id,_) -> <:patt< $lid:id$ >>) ids) in
      let tpatt,texpr = tuple ~param:"id" nts in
      let tymod = Typeable.tup ctxt ts <:expr< M.T.typeRep >> (self#expr)
      and eqmod = Eq.tup ctxt ts <:expr< M.E.eq >> (self#expr)
      and shelvers =
        let inner = 
          List.fold_right
            (fun (id,t) expr -> 
               <:expr< let module M = $self#expr ctxt t$
                        in $bind$ (M.shelve $lid:id$) 
                                  (fun $lid:id$ -> $expr$) >>)
            ids
            <:expr< store_repr this (make_repr $eidlist$) >> in
          [ <:match_case< ($tpatt$ as obj) -> 
                  $bind$ (allocate_id (T.makeDynamic obj) Comp.eq) (fun (this, freshp) ->
                         if freshp then ($inner$; return this)
                         else return this) >>]
      and unshelver = 
        let msg = "unexpected object encountered unshelving "^string_of_int nts^"-tuple" in
        let inner = 
          List.fold_right 
            (fun (id,t) expr ->
               <:expr< let module M = $self#expr ctxt t$ 
                        in $bind$ (M.unshelve $lid:id$) (fun $lid:id$ -> $expr$) >>)
            ids
            <:expr< return $texpr$ >> in
          <:expr< W.whizzyNoCtor
            (function
               | $pidlist$ -> $inner$
               | _ -> raise (UnshelvingError $str:msg$)) >>
      and atype = atype_expr ctxt (`Tuple ts) in
        wrap ~ctxt ~atype ~tymod ~eqmod ~shelvers ~unshelver

    method polycase ctxt tagspec n : Ast.match_case = match tagspec with
    | Tag (name, None) -> <:match_case<
        (`$name$ as obj) ->
           $bind$ (allocate_id (T.makeDynamic obj) Comp.eq) (fun (thisid, freshp) -> 
                   if freshp then 
                     $seq$ (store_repr thisid (make_repr ~constructor:$`int:n$ []))
                           (return thisid)
                   else return thisid) >>
    | Tag (name, Some t) -> <:match_case< 
        (`$name$ v1 as obj) ->
           let module M = $self#expr ctxt t$ in 
           $bind$ (allocate_id (T.makeDynamic obj) Comp.eq) (fun (thisid, freshp) -> 
           if freshp then
             $bind$ (M.shelve v1) (fun mid -> 
             $seq$  (store_repr thisid (make_repr ~constructor:$`int:n$ [mid]))
                    (return thisid))
           else return thisid) >>  

    | Extends t -> 
        let patt, guard, cast = cast_pattern ctxt t in <:match_case<
         ($patt$ as obj) when $guard$ ->
           let module M = $self#expr ctxt t$ in
           $bind$ (allocate_id (T.makeDynamic obj) Comp.eq) (fun (thisid, freshp) ->
           if freshp then 
             $bind$ (M.shelve $cast$) (fun mid ->
             $seq$  (store_repr thisid (make_repr ~constructor:$`int:n$ [mid]))
                    (return thisid))
           else return thisid) >>

    method polycase_un ctxt tagspec n : Ast.match_case = match tagspec with
    | Tag (name, None) -> <:match_case< $`int:n$, [] -> return `$name$ >>
    | Tag (name, Some t) -> <:match_case< $`int:n$, [x] -> 
      let module M = $self#expr ctxt t$ in $bind$ (M.unshelve x) (fun o -> return (`$name$ o)) >>
    | Extends t -> <:match_case< $`int:n$, [x] -> let module M = $self#expr ctxt t$ in (M.unshelve x : M.a n :> a n) >>

    method variant ctxt (tname,_,_,_ as decl) (_, tags) = 
      wrap ~ctxt ~atype:(atype ctxt decl) ~tymod:(typeable_instance ctxt tname)
        ~eqmod:(eq_instance ctxt tname)
        ~shelvers:(List.mapn (self#polycase ctxt) tags)
        ~unshelver:<:expr< fun id -> 
                 let f = function $list:List.mapn (self#polycase_un ctxt) tags$
                 in W.whizzySum f id >>

    method case ctxt (name, params') n : Ast.match_case * Ast.match_case = 
    let nparams = List.length params' in
    let ids = List.map (fun n ->  <:expr< $lid:Printf.sprintf "id%d" n$ >>) (List.range 0 nparams) in
    let exp = 
      List.fold_right2
        (fun p n tail -> 
           <:expr< let module M = $self#expr ctxt p$ in
                       $bind$ (M.shelve $lid:Printf.sprintf "v%d" n$)
                         (fun $lid:Printf.sprintf "id%d" n$ -> $tail$)>>)
        params'
        (List.range 0 nparams)
        <:expr< $seq$ (store_repr thisid (make_repr ~constructor:$`int:n$ $expr_list ids$))
                      (return thisid) >> in
      match params' with
        | [] -> <:match_case< $uid:name$ as obj -> 
                              $bind$ (allocate_id (T.makeDynamic obj) Comp.eq) (fun (thisid,freshp) -> 
                              if freshp then $exp$ else return thisid) >>,
                <:match_case< $`int:n$, [] -> return $uid:name$ >>
        | _  -> <:match_case< $uid:name$ $fst (tuple ~param:"v" nparams)$ as obj -> 
                              $bind$ (allocate_id (T.makeDynamic obj) Comp.eq) (fun (thisid,freshp) ->
                              if freshp then $exp$ else return thisid) >>,
    let _, tuple = tuple ~param:"id" nparams in
    let patt, exp = 
      List.fold_right2 
        (fun n t (pat, exp) ->
           let m = Printf.sprintf "M%d" n and id = Printf.sprintf "id%d" n in
           <:patt< $lid:id$ :: $pat$ >>,
           <:expr< let module $uid:m$ = $self#expr ctxt t$
                    in $bind$ ($uid:m$.unshelve $lid:id$) (fun $lid:id$ -> $exp$) >>)
        (List.range 0 nparams)
        params'
      (<:patt< [] >>, <:expr< return ($uid:name$ $tuple$) >>) in
      <:match_case< $`int:n$, $patt$ -> $exp$ >>

  method sum ?eq ctxt (tname,_,_,_ as decl) summands =
    let shelvers, unshelvers = List.split (List.mapn (self#case ctxt) summands) in
    wrap ~ctxt ~atype:(atype ctxt decl)
      ~tymod:(typeable_instance ctxt tname)
      ~eqmod:(eq_instance ctxt tname)
      ~shelvers
      ~unshelver:<:expr< fun id -> 
        let f = function $list:unshelvers$ in W.whizzySum f id >>

  method record ?eq ctxt (tname,_,_,_ as decl) (fields : Types.field list) = 
    let patt = record_pattern fields in 
    let tuplemod = 
      self#expr ctxt
        (`Tuple (List.map (function
                             | (_,([],t),_) -> t
                             | _ -> raise (Underivable
                                             ("Shelve cannot be derived for record types"
                                              ^" with polymorphic fields")))
                   fields)) in
    let nametup = tuple_expr (List.map (fun (f,_,_) -> <:expr< $lid:f$ >>) fields) in
    let shelvers, unshelver = 
      [ <:match_case< ($patt$ as obj) -> let module M = $tuplemod$ in M.shelve $nametup$  >> ],
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
              <:expr< let module M = $self#expr ctxt t$ 
                       in $bind$ (M.unshelve $lid:name$) 
                               (fun $lid:name$ -> $exp$) >>)
           names fields <:expr< return $rexp$ >> in
      <:expr< fun id -> let f = fun $patt$ -> $exp$ in W.whizzyNoCtor f id >>) in
    wrap ~ctxt ~atype:(atype ctxt decl) ~shelvers ~unshelver
      ~tymod:(typeable_instance ctxt tname)
      ~eqmod:(eq_instance ctxt tname)
  end
end

let _ = Base.register "Shelve"
  ((fun (loc, context, decls) -> 
      let module M = InContext(struct let loc = loc end) in
        M.generate ~context ~decls ~make_module_expr:M.instance#rhs ~classname:M.classname
          ~default_module:"Shelve_defaults" ()),
   (fun (loc, context, decls) -> 
      let module M = InContext(struct let loc = loc end) in
        M.gen_sigs ~context ~decls ~classname:M.classname))
