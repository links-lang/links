(*pp camlp4of *)

open Base
open Utils
open Type
open Camlp4.PreCast

let classname = "Bounded"

class bounded ~loc = 
object (self)
  inherit Base.deriver ~allow_private:false ~loc ~classname

    val methods = [
      "min_bound";
      "max_bound"
    ]
    method tuple atype ts = 
    let minBounds, maxBounds = 
      List.split (List.map
                    (fun t -> let e = self#atomic t in 
                       <:expr< $e$.min_bound >>,
                       <:expr< $e$.max_bound >>) ts) in
    <:expr< { min_bound = $tuple_expr ~loc minBounds$ ;
              max_bound = $tuple_expr ~loc maxBounds$ } >>

    method sum (name, params) ?eq summands = 
    let names = ListLabels.map summands
        ~f:(function
              | (name,[]) -> name
              | (name,_) -> raise (Underivable (loc, "Bounded cannot be derived for the type "^
                                                  name ^" because the constructor "^
                                                  name^" is not nullary"))) in
        <:expr< { min_bound = $uid:List.hd names$ ;
                  max_bound = $uid:List.last names$ } >>

    method variant atype (_, tags) = 
    let names = ListLabels.map tags
        ~f:(function
              | `Tag (name, None) -> name
              | `Tag (name, _) -> raise (Underivable (loc, "Bounded cannot be derived because the tag "^
                                                      name^" is not nullary"))
              | _ -> raise (Underivable (loc, "Bounded cannot be derived for this "
                                        ^"polymorphic variant type"))) in
      <:expr< { min_bound = `$List.hd names$ ;
                max_bound = `$List.last names$ } >>

  (* should perhaps implement this one *)
  method record (name, _) ?eq _ = 
    raise (Underivable (loc, "Bounded cannot be derived for record types (i.e. "^
                          name^")"))
  method expand e = e
end

let _ = Base.register classname (new bounded)
