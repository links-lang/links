(*pp camlp4of *)
module InContext (L : Base.Loc) =
struct
  open Base
  open Utils
  open Types
  open Camlp4.PreCast
  include Base.InContext(L)

  let classname = "Bounded"

  let rec expr t = (Lazy.force obj) # expr t and rhs t = (Lazy.force obj) # rhs t
  and obj = lazy (new make_module_expr ~classname ~variant ~record ~sum ~allow_private:false)
    
  and sum ?eq ctxt ((tname,_,_,_) as decl) summands = 
    let names = ListLabels.map summands
        ~f:(function
              | (name,[]) -> name
              | (name,_) -> raise (Underivable ("Bounded cannot be derived for the type "^
                                                  tname ^" because the constructor "^
                                                  name^" is not nullary"))) in
        <:module_expr< struct type a = $atype ctxt decl$
                       let minBound = $uid:List.hd names$ 
                       and maxBound = $uid:List.last names$ end >>

  and variant ctxt decl (_, tags) = 
    let names = ListLabels.map tags
        ~f:(function
              | Tag (name, None) -> name
             | Tag (name, _) -> raise (Underivable ("Bounded cannot be derived because the tag "^
                                                      name^" is not nullary"))
             | _ -> raise (Underivable ("Bounded cannot be derived for this "
                                        ^"polymorphic variant type"))) in
      <:module_expr< struct type a = $atype ctxt decl$
                     let minBound = `$List.hd names$ 
                     and maxBound = `$List.last names$ end >>

  (* should perhaps implement this one *)
  and record ?eq _ (tname,_,_,_) = raise (Underivable ("Bounded cannot be derived for record types (i.e. "^
                                                     tname^")"))
end

let _ = Base.register "Bounded" 
  ((fun (loc, context, decls) -> 
     let module M = InContext(struct let loc = loc end) in
       M.generate ~context ~decls ~make_module_expr:M.rhs ~classname:M.classname ()),
   (fun (loc, context, decls) -> 
      let module M = InContext(struct let loc = loc end) in
        M.gen_sigs ~context ~decls ~classname:M.classname))
