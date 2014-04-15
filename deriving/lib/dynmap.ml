(* Finite maps : t -> dynamic *)

open Eq
open Hash
open Typeable

module OrdType :
sig
  type t = int * TypeRep.t
  val compare : t -> t -> int
end =
struct
  type t = int * TypeRep.t
  let compare : t -> t -> int
    = fun (lhash, lrep) (rhash, rrep) ->
      let hashcmp = compare lhash rhash in
        if hashcmp <> 0 then hashcmp
        else TypeRep.compare lrep rrep
end

type eqtest = dynamic -> dynamic -> bool

let comp : 'a typeable -> ('a -> 'a -> bool) -> (dynamic -> dynamic -> bool) =
  fun typeable eq d1 d2 ->
    match cast typeable d1, cast typeable d2 with
      | Some l, Some r -> eq l r
      | _ -> assert false

module CombinedMap = Map.Make(OrdType)

type 'v t = (('v * dynamic) list * eqtest) CombinedMap.t
let empty = CombinedMap.empty
  
let add hsh typeable key value map =
let mapKey = hash hsh key, type_rep typeable  in
let matches, eqtest = (try CombinedMap.find mapKey map 
                           with Not_found ->  [], comp typeable hsh._Eq.eq) in
  CombinedMap.add mapKey (((value, mk typeable key) :: matches), eqtest) map

let find hsh typeable key map =
  let mapKey = hash hsh key, type_rep typeable  in 
    try let candidates, eqtest = CombinedMap.find mapKey map in
      Some (fst (List.find (fun (_, k) -> eqtest (mk typeable key) k) candidates))
    with Not_found -> None

