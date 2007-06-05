(*pp camlp4of *)
module InContext (L : Base.Loc) =
struct
  open Base
  open Utils
  open Types
  open Camlp4.PreCast
  include Base.InContext(L)

  let classname = "Enum"

  let rec expr t = (Lazy.force obj) # expr t and rhs t = (Lazy.force obj) # rhs t
  and obj = lazy (new make_module_expr ~classname ~variant ~record ~sum)

  and sum ctxt ((tname,_,_,_) as decl) summands =
    let numbering = 
      List.fold_right2
        (fun n ctor rest -> 
           match ctor with
             | (name, []) -> <:expr< ($uid:name$, $`int:n$) :: $rest$ >>
             | (name,_) -> raise (Underivable ("Enum cannot be derived for the type "^
                                  tname ^" because the constructor "^
                                  name^" is not nullary")))
        (List.range 0 (List.length summands))
        summands
        <:expr< [] >> in
      <:module_expr< struct type a = $atype ctxt decl$ let numbering = $numbering$ end >>

  and variant ctxt decl (_, tags) = 
    let numbering = 
      List.fold_right2
        (fun n tagspec rest -> 
           match tagspec with
             | Tag (name, None) -> <:expr< (`$name$, $`int:n$) :: $rest$ >>
             | Tag (name, _) -> raise (Underivable ("Enum cannot be derived because the tag "^
                                                      name^" is not nullary"))
             | _ -> raise (Underivable ("Enum cannot be derived for this "
                                        ^"polymorphic variant type")))
        (List.range 0 (List.length tags))
        tags
        <:expr< [] >> in
      <:module_expr< struct type a = $atype ctxt decl$ let numbering = $numbering$ end >>

  and record _ (tname,_,_,_) = raise (Underivable ("Enum cannot be derived for record types (i.e. "^
                                                     tname^")"))
end

let _ = Base.register "Enum" 
  (fun (loc, context, decls) -> 
     let module M = InContext(struct let loc = loc end) in
       M.generate ~context ~decls ~make_module_expr:M.rhs ~classname:M.classname
         ~default_module:"EnumDefaults" ())
