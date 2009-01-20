(*pp camlp4of *)

open Base
open Utils
open Type
open Camlp4.PreCast

let classname = "Enum"
class enum ~loc =
object (self)
  inherit Base.deriver ~loc  ~classname ~allow_private:false ?default:None

    method sum (name, params) ?eq summands =
    let numbering = 
      List.fold_right2
        (fun n ctor rest -> 
           match ctor with
             | (name, []) -> <:expr< ($uid:name$, $`int:n$) :: $rest$ >>
             | (name,_) -> raise (Underivable (loc, "Enum cannot be derived for the type "^
                                               name ^" because the constructor "^
                                  name^" is not nullary")))
        (List.range 0 (List.length summands))
        summands
        <:expr< [] >> in
      <:module_expr< Enum.Defaults(struct type a = $self#atype (name, params)$ 
                                          let numbering = $numbering$ end) >>

    method variant atype (_, tags) = 
    let numbering = 
      List.fold_right2
        (fun n tagspec rest -> 
           match tagspec with
             | `Tag (name, None) -> <:expr< (`$name$, $`int:n$) :: $rest$ >>
             | `Tag (name, _) -> raise (Underivable (loc, "Enum cannot be derived because the tag "^
                                                      name^" is not nullary"))
             | _ -> raise (Underivable (loc, "Enum cannot be derived for this "
                                        ^"polymorphic variant type")))
        (List.range 0 (List.length tags))
        tags
        <:expr< [] >> in
      <:module_expr< Enum.Defaults(struct type a = $self#atype atype$ 
                                          let numbering = $numbering$ end) >>

    method tuple _ _ = raise (Underivable (loc, "Enum cannot be derived for tuple types"))
    method record _ ?eq = raise (Underivable (loc, "Enum cannot be derived for record types"))
end

let _ = Base.register classname (new enum)
