(* Finite maps : t -> dynamic *)

open Typeable

let comp (t : 'a typeable) (e : 'a Eq.eq) =
  let adjust_comparator : ('a -> 'a -> bool) -> dynamic -> dynamic -> bool 
    = fun comparator d1 d2 ->
      match cast t d1, cast t d2 with
        | Some l, Some r -> comparator l r
        | _ -> assert false in
    adjust_comparator e.Eq.eq

module DynMap =
struct
  module TypeMap = Map.Make(TypeRep)
  type comparator = dynamic -> dynamic -> bool
  type 'value t = (((dynamic * 'value) list * comparator) TypeMap.t)

  let empty = TypeMap.empty

  let add dynamic value comparator map =
    let typeRep = tagOf dynamic in
    let monomap = 
      try (List.filter
             (fun (k,_) -> not (comparator k dynamic))
             (fst (TypeMap.find typeRep map)))
      with Not_found -> []
    in
      TypeMap.add
        typeRep 
        (((dynamic,value)::monomap), comparator)
        map

  let mem dynamic map =
    try let monomap, comparator = TypeMap.find (tagOf dynamic) map in
      (List.exists 
         (fun (k,_) -> (comparator dynamic k)) 
         monomap)
    with Not_found -> false

  let find dynamic map =
    try 
      let monomap, comparator = TypeMap.find (tagOf dynamic) map in
        Some (snd (List.find
                     (fun (k,_) -> comparator dynamic k)
                     monomap))
    with Not_found -> None

  let iter : (dynamic -> 'a -> unit) -> 'a t -> unit
    = fun f -> 
      TypeMap.iter 
        (fun _ (monomap,_) -> List.iter (fun (k, v) -> f k v) monomap)
end
