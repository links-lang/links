(*pp derivingpp *)
open Typeable

module MapByType = 
  Map.Make(TypeRep)

module Id :
sig
  type t deriving (Show, Pickle, Eq)
  val initial : t
  val compare : t -> t -> int
  val next : t -> t  
end =
struct
  type t = int deriving (Show, Pickle, Eq)
  let initial = 0
  let compare = compare
  let next = succ
end
module IdMap = Map.Make (Id)
type id = Id.t deriving (Show, Pickle)

type repr = Bytes of string | CApp of (int option * Id.t list)
  deriving (Typeable, Eq, Show, Pickle)

module Pickle_Ids = Pickle.Pickle_list(Id.Pickle_t)

let make_repr : ?constructor:int -> Id.t list -> repr
  = fun ?constructor ids ->
    CApp (constructor, ids)

type output_state = {
  nextid : Id.t; (* the next id to be allocated *)
  obj2id : Id.t Dynmap.DynMap.t; (* map from typerep to id cache for the corresponding type *)
  id2rep : repr IdMap.t;
} 

include Monad.Monad_state
  (struct type state = output_state end)
  
let allocate_id : dynamic -> Dynmap.DynMap.comparator -> (Id.t * bool) m
  = fun obj comparator -> 
    get >>= fun ({nextid=nextid;obj2id=obj2id} as t) ->
      match Dynmap.DynMap.find obj obj2id with
        | Some id -> return (id,false)
        | None -> 
            let id, nextid = nextid, Id.next nextid in
            put {t with
                   obj2id=Dynmap.DynMap.add obj id comparator obj2id;
                   nextid=nextid} >>
            return (id, true)
                
let store_repr id repr = 
  get >>= fun state ->
    put {state with id2rep = IdMap.add id repr state.id2rep}
      
let initial_output_state = {
  nextid = Id.initial;
  obj2id = Dynmap.DynMap.empty;
  id2rep = IdMap.empty;
}
  
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
  type ids = (Id.t * repr) list
      deriving (Pickle)

  let doShelveB : id m -> Buffer.t -> unit
    = fun m buffer ->
      let _, {id2rep=id2rep} = runState m initial_output_state in
      let ids : (Id.t * repr) list  = IdMap.fold (fun k v output -> (k,v)::output) id2rep [] in
        Printf.fprintf stderr  "%d ids\n" (List.length ids);
      let ids' = unduplicate (=) (List.map snd ids) in
        Printf.fprintf stderr  "~ %d ids?\n" (List.length ids');
        Pickle_ids.pickle buffer ids
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
let string_of_repr = function
  | Bytes s -> s
  | _ -> invalid_arg "string_of_repr"

module Input =
struct
  type input_state = (repr * (dynamic option)) IdMap.t
      
  include Monad.Monad_state(struct type state = input_state end)

  let ctor_repr : repr -> int option * id list = function 
    | CApp arg -> arg
    | _ -> assert false

  let decode_repr_ctor = function
    | CApp (Some c, ids) -> (c, ids)
    | _ -> invalid_arg "decode_repr_ctor"

  let decode_repr_noctor = function
    | CApp (None, ids) -> ids
    | _ -> invalid_arg "decode_repr_ctor"
  
  let find_by_id id =
    get >>= fun state ->
      return (IdMap.find id state)
        
  let update_map id dynamic =
    get >>= fun state -> 
      match IdMap.find id state with 
        | (repr, None) ->     
            put (IdMap.add id (repr, Some dynamic) state)
        | (repr, Some dyn) -> 
            return ()
              (* Checking for id already present causes unshelving to fail 
                 when there is circularity involving immutable values (even 
                 if the recursion wholly depends on mutability).

                 For example, consider the code

                    type t = A | B of t ref deriving (Typeable, Eq, Shelve)
                    let s = ref A in
                    let r = B s in
                       s := r;
                       let shelved = Shelve_t.shelveS r in
                       Shelve_t.unshelveS r

                 which results in the value
                    B {contents = B {contents = B { ... }}}

                 During deserialization the following steps occur:
                     1. lookup "B {...}" in the dictionary (not there)
                     2. unshelve the contents of B:
                        3. lookup the contents in the dictionary (not there)
                        4. create a blank reference, insert it into the dictionary
                        5. unshelve the contents of the reference:
                           6. lookup ("B {...}") in the dictionary (not there)
                           7. unshelve the contents of B:
                               8. lookup the contents in the dictionary (there)
                           9. insert "B{...}" into the dictionary.
                     10. insert "B{...}" into the dictionary.
              *)

  module Whizzy (T : Typeable.Typeable) =
  struct
    let whizzy f id decode =
      find_by_id id >>= fun (repr, dynopt) ->
        match dynopt with 
          | None ->
              f (decode repr) >>= fun obj ->
                update_map id (T.makeDynamic obj) >>
                return obj
          | Some obj -> 
              match T.cast obj with
                | Some obj -> return obj
                | None -> assert false

    let whizzySum f id = 
      whizzy f id decode_repr_ctor
    let whizzyNoCtor f id = whizzy f id decode_repr_noctor
  end
end

type dumpable = id * (id * repr) list
    deriving (Show, Pickle)

module Do (S : sig
    type a
    val shelve : a -> id m
    val unshelve : id -> a Input.m
  end) =
struct
  type a = S.a

  let decode_shelved_string : string -> id * Input.state =
    fun s -> 
      let (id, state : dumpable) = Pickle_dumpable.unpickleS s in
        id, (List.fold_right 
               (fun (id,repr) map -> IdMap.add id (repr,None) map)
               state
               IdMap.empty)
    
  let encode_shelved_string : id * output_state -> string  =
    fun (id,state) ->
      let input_state = 
        id, IdMap.fold (fun id repr output -> (id,repr)::output)
          state.id2rep [] in
        Pickle_dumpable.pickleS input_state

  let doShelve v = 
    let id, state = runState (S.shelve v) initial_output_state in
      encode_shelved_string (id, state)

  let doUnshelve string = 
    let id, initial_input_state = decode_shelved_string string in  
    let value, state = Input.runState (S.unshelve id) initial_input_state in
      value
end

