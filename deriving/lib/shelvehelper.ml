(*pp deriving *)
open Typeable

module MapByType = 
  Map.Make(TypeRep)

module Id :
sig
  type t
  val initial : t
  val compare : t -> t -> int
  val next : t -> t  
  module Pickle_t : Pickle.Pickle with type a = t
  module Show_t : Show.Show with type a = t
  module Eq_t : Eq.Eq with type a = t
end =
struct
  type t = int
  let initial = 0
  let compare = compare
  let next = succ
  module Pickle_t = Pickle.Pickle_int
  module Show_t = Primitives.Show_int
  module Eq_t = Eq.Eq_int
end
module IdMap = Map.Make (Id)
type id = Id.t

type repr = Bytes of string | CApp of int option * Id.t list
  deriving (Typeable, Eq, Show, Pickle)

module Pickle_Ids = Pickle.Pickle_list(Id.Pickle_t)

let make_repr : ?constructor:int -> Id.t list -> repr
  = fun ?constructor ids ->
(*    match constructor with 
        (* This doesn't seem reliable.  How can we know whether
           there's a constructor there when we unpickle?  A better way
           might be to store a list of ints which the client can
           decode *)
      | Some tag -> Pickle.Pickle_int.pickleS tag ^ Pickle_Ids.pickleS ids
      | None -> Pickle_Ids.pickleS ids
*)
    CApp (constructor, ids)

type output_state = {
  nextid : Id.t; (* the next id to be allocated *)
  obj2id : Id.t Dynmap.DynMap.t; (* map from typerep to id cache for the corresponding type *)
  id2rep : repr IdMap.t;
} 

include Monad.Monad_state
  (struct type state = output_state end)
  
let allocate_id : dynamic -> Dynmap.DynMap.comparator -> Id.t m
  = fun obj comparator -> 
    get >>= fun ({nextid=nextid;obj2id=obj2id} as t) ->
      match Dynmap.DynMap.find obj obj2id with
        | Some id -> return id
        | None -> 
            let id, nextid = nextid, Id.next nextid in
            put {t with
                   obj2id=Dynmap.DynMap.add obj id comparator obj2id;
                   nextid=nextid} >>
            return id
                
let store_repr id repr = 
  get >>= fun state ->
    put {state with id2rep = IdMap.add id repr state.id2rep}
      
let initial_output_state = {
  nextid = Id.initial;
  obj2id = Dynmap.DynMap.empty;
  id2rep = IdMap.empty;
}
  
let allocate_store_return dynamic eq (repr:repr) =
  allocate_id dynamic eq >>= fun id ->
  store_repr id repr >>
  return id

open Printf
let dump_state {
  nextid = nextid;
  obj2id = obj2id;
  id2rep = id2rep
} =
  let printf x = Printf.fprintf stderr x in
  printf "nextid : %d\n" ((Obj.magic nextid):int);
  printf "obj2id : \n";
  Dynmap.DynMap.iter (fun _ v -> printf "  ? => %d\n" ((Obj.magic v):int)) obj2id;
  printf "id2rep : \n";
  IdMap.iter (fun k v -> printf "  %s => %s\n" (Id.Show_t.show k) (Show_repr.show v)) id2rep;
  flush stdout

  let rec unduplicate equal = function
    | [] -> []
    | elem :: elems -> (let _, others = List.partition (equal elem) elems in
                          elem :: unduplicate equal others)


module Shelver =
struct
  let doShelveB : id m -> Buffer.t -> unit
    = fun m buffer ->
      let _, {id2rep=id2rep} = runState m initial_output_state in
      let ids : (Id.t * repr) list  = IdMap.fold (fun k v output -> (k,v)::output) id2rep [] in
        Printf.fprintf stderr  "%d ids\n" (List.length ids);
      let ids' = unduplicate (=) (List.map snd ids) in
        Printf.fprintf stderr  "~ %d ids?\n" (List.length ids');
      let module Pickle_Ids = Pickle.Pickle_list(Pickle.Pickle_2(Id.Pickle_t)(Pickle_repr)) in
        Pickle_Ids.pickle buffer ids
  let doShelveS : id m -> string
    = fun m ->
      let buffer = Buffer.create 127 in 
        begin
          doShelveB m buffer;
          Buffer.contents buffer
        end
end

include Shelver

let repr_of_string s = Bytes s
