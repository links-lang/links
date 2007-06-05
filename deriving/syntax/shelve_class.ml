(*pp camlp4of *)
module InContext (L : Base.Loc) =
struct
  open Base
  open Utils
  open Types
  open Camlp4.PreCast
  include Base.InContext(L)

  let classname = "Shelve"
  let bindop = ">>="

  let wrap ctxt tname t shelvers =
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
                          type a = $t$
                          let shelve = function $list:shelvers$
    end >>

  let rec expr t = (Lazy.force obj) # expr t and rhs t = (Lazy.force obj) # rhs t
  and obj = lazy (new make_module_expr ~classname ~variant ~record ~sum)
    
  and polycase ctxt tagspec n : Ast.match_case = 
    failwith "nyi"

  and variant ctxt (_, tags) = 
    failwith "nyi"

  and case ctxt (name, params') n : Ast.match_case = 
    let ids = List.map (fun n ->  <:expr< $lid:Printf.sprintf "id%d" n$ >>) (List.range 0 (List.length params')) in
    let expr = 
      List.fold_right2
        (fun p n tail -> 
           <:expr< let module M = $expr ctxt p$ in
                       $lid:bindop$ (M.shelve $lid:Printf.sprintf "v%d" n$)
                         (fun $lid:Printf.sprintf "id%d" n$ -> $tail$)>>)
        params'
        (List.range 0 (List.length params'))
        <:expr< allocate_store_return (Typeable.makeDynamic obj)
                Comp.eq (make_repr ~constructor:$`int:n$ $expr_list ids$) >> in
      match params' with
        | [] -> <:match_case< $uid:name$ as obj -> $expr$ >>
        | _  -> <:match_case< $uid:name$ $fst (tuple ~param:"v" (List.length params'))$ as obj -> $expr$ >>

  and sum ctxt (tname,_,_,_ as decl) summands =
    wrap ctxt tname (atype ctxt decl)
      (List.map2 (case ctxt) summands (List.range 0 (List.length summands)))

  and record ctxt (tname,_,_,_ as decl) (fields : Types.field list) = 
    let patt = record_pattern fields in 
    let tuplemod = 
      expr ctxt
        (Tuple (List.map (function
                            | (_,([],t),_) -> t
                            | _ -> raise (Underivable
                                            ("Shelve cannot be derived for record types"
                                             ^" with polymorphic fields")))
                                       (fields : Types.field list))) in
    let nametup = tuple_expr (List.map (fun (f,_,_) -> <:expr< $lid:f$ >>) fields) in
    wrap ctxt tname (atype ctxt decl)
      [ <:match_case< ($patt$ as obj) -> let module M = $tuplemod$ in M.shelve $nametup$  >> ]
end

let _ = Base.register "Shelve"
  (fun (loc, context, decls) -> 
     let module M = InContext(struct let loc = loc end) in
       M.generate ~context ~decls ~make_module_expr:M.rhs ~classname:M.classname
         ~default_module:"Shelve_defaults" ())
