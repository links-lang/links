module InContext (C : Base.Context) =
struct
  open C
  open Base
  open Util
  open Types
  open Camlp4.PreCast
  include Base.InContext(C)

  let classname = "Bounded"

  let rec expr t = (new make_module_expr ~classname ~variant ~record ~sum) # expr t
    
  and sum summands = 
    let names = ListLabels.map summands
        ~f:(function
              | (name,[]) -> name
              | _ -> raise (Underivable (classname, context.atype))) in
        <:module_expr< struct type a = $Untranslate.expr context.atype$
                       let minBound = $uid:List.hd names$ 
                       and maxBound = $uid:List.last names$ end >>

  and variant (_, tags) = 
    let names = ListLabels.map tags
        ~f:(function
              | Tag (name, None) -> name
              | _ -> raise (Underivable (classname, context.atype))) in
      <:module_expr< struct type a = $Untranslate.expr context.atype$
                     let minBound = `$List.hd names$ 
                     and maxBound = `$List.last names$ end >>

  (* should perhaps implement this one *)
  and record _ = raise (Underivable (classname, context.atype))
end

let generate context csts = 
  let module M = InContext(struct let context = context end) in
    M.generate ~csts ~make_module_expr:M.expr
      ~classname:M.classname ~default_module:None
