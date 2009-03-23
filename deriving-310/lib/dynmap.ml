(* Finite maps : t -> dynamic *)

open Hash
open Typeable

let comp (t : 'a typeable) (e : 'a Hash.hash) : dynamic -> dynamic -> bool =
  let adjust_comparator : ('a -> 'a -> bool) -> dynamic -> dynamic -> bool 
    = fun comparator d1 d2 ->
      match cast t d1, cast t d2 with
        | Some l, Some r -> comparator l r
        | _ -> assert false in
    adjust_comparator e.Hash._Eq.Eq.eq

module TypeMap = Map.Make(TypeRep)
type comparator = dynamic -> dynamic -> bool
module Int =
struct
  type t = int
  let compare = compare
end
module HashcodeMap = Map.Make(Int)
type 'v t = (('v * dynamic) list HashcodeMap.t * comparator) TypeMap.t
let empty = TypeMap.empty

let add hsh typeable key value map =
  let dynamic : dynamic = mk typeable key 
  and typeRep : TypeRep.t = type_rep typeable 
  and hashCode : int = hash hsh key in
  let monomap, comparator = try TypeMap.find typeRep map with Not_found -> HashcodeMap.empty, comp typeable hsh in 
  let values = try HashcodeMap.find hashCode monomap with Not_found -> [] in
    TypeMap.add typeRep ((HashcodeMap.add hashCode ((value, dynamic) :: values) monomap), comparator) map
      
let find hsh typeable key map =
  let dynamic : dynamic = mk typeable key 
  and typeRep : TypeRep.t = type_rep typeable 
  and hashCode : int = hash hsh key in
    try
      let monomap, comparator = TypeMap.find typeRep map in
        Some (fst (List.find
                     (fun (_, k) -> comparator dynamic k)
                     (HashcodeMap.find hashCode monomap)))
    with Not_found -> None
