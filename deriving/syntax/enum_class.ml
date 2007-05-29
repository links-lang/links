module InContext (C : Base.Context) =
struct
  open C
  open Base
  open Util
  open Types
  open Camlp4.PreCast
  include Base.InContext(C)

  let classname = "Enum"

  let rec expr t = (new make_module_expr ~classname ~variant ~record ~sum) # expr t

  and sum summands =
    let numbering = 
      List.fold_right2
        (fun n ctor rest -> 
           match ctor with
             | (name, []) -> <:expr< ($uid:name$, $`int:n$) :: $rest$ >>
             | _ -> raise (Underivable (classname, context.atype)))
        (List.range 0 (List.length summands))
        summands
        <:expr< [] >> in
      <:module_expr< struct let numbering = $numbering$ end >>

  and variant (_, tags) = 
    let numbering = 
      List.fold_right2
        (fun n tagspec rest -> 
           match tagspec with
             | Tag (name, None) -> <:expr< (`$name$, $`int:n$) :: $rest$ >>
             | _ -> raise (Underivable (classname, context.atype)))
        (List.range 0 (List.length tags))
        tags
        <:expr< [] >> in
      <:module_expr< struct let numbering = $numbering$ end >>

  and record _ = raise (Underivable (classname, context.atype))
end

let generate context csts = 
  let module M = InContext(struct let context = context end) in
    M.generate ~csts ~make_module_expr:M.expr
      ~classname:M.classname ~default_module:(Some "Enum_defaults")
