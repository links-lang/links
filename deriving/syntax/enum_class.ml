(*pp camlp4of *)
module InContext (L : Base.Loc) =
struct
  open Base
  open Utils
  open Type
  open Camlp4.PreCast
  include Base.InContext(L)

  let classname = "Enum"

  let instance = object(self)
    inherit make_module_expr ~classname ~allow_private:false

    method sum ?eq ctxt ((tname,_,_,_,_) as decl) summands =
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
      <:module_expr< Enum.Defaults(struct type a = $atype ctxt decl$ let numbering = $numbering$ end) >>

    method variant ctxt decl (_, tags) = 
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
      <:module_expr< Enum.Defaults(struct type a = $atype ctxt decl$ let numbering = $numbering$ end) >>

    method tuple context _ = raise (Underivable "Enum cannot be derived for tuple types")
    method record ?eq _ (tname,_,_,_,_) = raise (Underivable
                                                 ("Enum cannot be derived for record types (i.e. "^
                                                    tname^")"))
  end
end

let _ = Base.register "Enum" 
  ((fun (loc, context, decls) -> 
     let module M = InContext(struct let loc = loc end) in
       M.generate ~context ~decls ~make_module_expr:M.instance#rhs ~classname:M.classname ()),
   (fun (loc, context, decls) -> 
      let module M = InContext(struct let loc = loc end) in
        M.gen_sigs ~context ~decls ~classname:M.classname))
