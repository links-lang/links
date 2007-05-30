module InContext (C : Base.Context) =
struct
  open C
  open Base
  open Camlp4.PreCast
  include Base.InContext(C)

  let classname = "Typeable"

  let gen = 
    let paramList = 
      List.fold_right 
        (fun p cdr ->
             <:expr< $uid:NameMap.find p context.argmap$.typeRep()::$cdr$ >>)
        (snd context.ltype)
      <:expr< [] >>
    in <:module_expr< struct type a = $Untranslate.expr context.atype$
          let typeRep = 
            (* computed lazily to avoid recursive-module initialization issues. *)
            let rep = ref None in 
            fun _ -> 
             match rep.contents with
               None -> let t = TypeRep (Tag.fresh(), $paramList$) in 
                         rep.contents := Some t; 
                         t
             | Some r -> r end >>

  let rec expr t = (new make_module_expr ~classname ~variant ~record ~sum) # expr t
  and sum _ = gen and variant _ = gen and record _ = gen
end

let generate context csts = 
  let module M = InContext(struct let context = context end) in
    M.generate ~csts ~make_module_expr:M.expr
      ~classname:M.classname ~default_module:(Some "Typeable_defaults")
