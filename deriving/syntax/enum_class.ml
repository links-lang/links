(*pp camlp4of *)

open Base
open Utils
open Type
open Camlp4.PreCast

let classname = "Enum"
class enum ~loc =
object (self)
  inherit Base.deriver ~loc  ~classname ~allow_private:false

    val methods = [ "succ";
                    "pred";
                    "to_enum";
                    "from_enum";
                    "enum_from";
                    "enum_from_then";
                    "enum_from_to";
                    "enum_from_then_to"]

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
      <:expr< Enum.from_numbering $numbering$ >>

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
      <:expr< Enum.from_numbering $numbering$ >>

    method tuple _ _ = raise (Underivable (loc, "Enum cannot be derived for tuple types"))
    method record _ ?eq = raise (Underivable (loc, "Enum cannot be derived for record types"))
end

let _ = Base.register classname (new enum)
