(*pp camlp4of *)

open Base
open Utils
open Type
open Camlp4.PreCast

let classname = "Bounded"

class bounded ~loc = 
object (self)
  inherit Base.deriver ~allow_private:false ~loc ~classname ?default:None

    method tuple atype ts = 
    let minBounds, maxBounds = 
      List.split (List.map
                    (fun t -> let e = self#atomic t in 
                       <:expr< $id:e$.min_bound >>,
                       <:expr< $id:e$.max_bound >>) ts) in
    <:module_expr< struct type a = $self#atype atype$
                          let min_bound = $tuple_expr ~loc minBounds$ 
                          let max_bound = $tuple_expr ~loc maxBounds$ end >>

    method sum (name, params) ?eq summands = 
    let names = ListLabels.map summands
        ~f:(function
              | (name,[]) -> name
              | (name,_) -> raise (Underivable (loc, "Bounded cannot be derived for the type "^
                                                  name ^" because the constructor "^
                                                  name^" is not nullary"))) in
        <:module_expr< struct type a = $self#atype (name, params)$
                       let min_bound = $uid:List.hd names$ 
                       and max_bound = $uid:List.last names$ end >>

    method variant atype (_, tags) = 
    let names = ListLabels.map tags
        ~f:(function
              | `Tag (name, None) -> name
              | `Tag (name, _) -> raise (Underivable (loc, "Bounded cannot be derived because the tag "^
                                                      name^" is not nullary"))
              | _ -> raise (Underivable (loc, "Bounded cannot be derived for this "
                                        ^"polymorphic variant type"))) in
      <:module_expr< struct type a = $self#atype atype$
                     let min_bound = `$List.hd names$ 
                     and max_bound = `$List.last names$ end >>

  (* should perhaps implement this one *)
  method record (name, _) ?eq _ = 
    raise (Underivable (loc, "Bounded cannot be derived for record types (i.e. "^
                          name^")"))
end

let _ = Base.register classname (new bounded)
