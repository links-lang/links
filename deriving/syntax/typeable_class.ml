(*pp camlp4of *)
module InContext (L : Base.Loc) =
struct
  open Base
  open Camlp4.PreCast
  include Base.InContext(L)

  let classname = "Typeable"

  let gen ctxt ((tname,_,_,_) as decl : Types.decl) _ = 
    let paramList = 
      List.fold_right 
        (fun (p,_) cdr ->
             <:expr< $uid:NameMap.find p ctxt.argmap$.typeRep()::$cdr$ >>)
        ctxt.params
      <:expr< [] >>
    in <:module_expr< struct type a = $atype ctxt decl$
          let typeRep = 
            (* computed lazily to avoid recursive-module initialization issues. *)
            let rep = ref None in 
            fun _ -> 
             match rep.contents with
               None -> let t = TypeRep (Tag.fresh(), $paramList$) in 
                         rep := Some t; 
                         t
             | Some r -> r end >>

  let rec expr t = (Lazy.force obj) # expr t and rhs t = (Lazy.force obj) # rhs t
  and obj = lazy (new make_module_expr ~classname ~variant ~record ~sum)
  and sum = gen and record = gen
  and variant _ _ = raise (Underivable ("Typeable cannot currently be derived for polymorphic variant types"))
end

let _ = Base.register "Typeable" 
  (fun (loc, context, decls) -> 
     let module M = InContext(struct let loc = loc end) in
       M.generate ~context ~decls ~make_module_expr:M.rhs ~classname:M.classname
         ~default_module:"Typeable_defaults" ())
