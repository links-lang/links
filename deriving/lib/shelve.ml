(*pp derivingpp *)
(*
  Idea: 
  1. every object receives a serializable id.
  2. an object is serialized using the ids of its subobjects
*)
module Shelve =
struct
exception UnshelvingError of string

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

module Repr : sig
  (* Break abstraction for the sake of efficiency for now *)
  type t = Bytes of string | CApp of (int option * Id.t list) deriving (Pickle, Show)
  val of_string : string -> t
  val to_string : t -> string
  val make : ?constructor:int -> id list -> t
  val unpack_ctor : t -> int option * id list
end =
struct
  type t = Bytes of string | CApp of (int option * Id.t list) deriving (Pickle, Show)
  let of_string s = Bytes s
  let to_string = function
    | Bytes s -> s
    | _ -> invalid_arg "string_of_repr"
  let make ?constructor ids = CApp (constructor, ids)
  let unpack_ctor = function 
    | CApp arg -> arg
    | _ -> assert false
end
type repr = Repr.t

module Write : sig
  type s = {
    nextid : Id.t; (* the next id to be allocated *)
    obj2id : Id.t Dynmap.DynMap.t; (* map from typerep to id cache for the corresponding type *)
    id2rep : repr IdMap.t;
  }
  val initial_output_state : s
  include Monad.Monad_state_type with type state = s
  val allocate_id : Typeable.dynamic -> Dynmap.DynMap.comparator -> (id * bool) m
  val allocate : Typeable.dynamic -> Dynmap.DynMap.comparator -> (id -> unit m) -> id m
  val store_repr : id -> Repr.t -> unit m
end =
struct
  type s = {
    nextid : Id.t; (* the next id to be allocated *)
    obj2id : Id.t Dynmap.DynMap.t; (* map from typerep to id cache for the corresponding type *)
    id2rep : repr IdMap.t;
  }
  let initial_output_state = {
    nextid = Id.initial;
    obj2id = Dynmap.DynMap.empty;
    id2rep = IdMap.empty;
  }
  include Monad.Monad_state (struct type state = s end)

  let allocate_id obj comparator =
    get >>= fun ({nextid=nextid;obj2id=obj2id} as t) ->
    match Dynmap.DynMap.find obj obj2id with
      | Some id -> return (id,false)
      | None -> 
          let id, nextid = nextid, Id.next nextid in
            put {t with
                   obj2id=Dynmap.DynMap.add obj id comparator obj2id;
                   nextid=nextid} >>
            return (id, true)

  let allocate dynamic eq f : id m =
    allocate_id dynamic eq >>= fun (id,freshp) ->
      if freshp then
        f id >>
          return id
      else
        return id

  let store_repr id repr =
    get >>= fun state ->
    put {state with id2rep = IdMap.add id repr state.id2rep}
end

module Read : sig
  type s = (repr * (Typeable.dynamic option)) IdMap.t
  include Monad.Monad_state_type with type state = s
  val find_by_id : id -> (Repr.t * Typeable.dynamic option) m
  val update_map : id -> Typeable.dynamic -> unit m
  module Utils (T : Typeable.Typeable) : sig
    val whizzySum : (int * id list -> T.a m) -> id -> T.a m
    val whizzyNoCtor : (id list -> T.a m) -> (id -> T.a m)
    val whizzyRecord : id -> (id list -> T.a m) -> T.a m
  end
end =
struct
  type s = (repr * (Typeable.dynamic option)) IdMap.t
  include Monad.Monad_state (struct type state = s end)

  let find_by_id id =
    get >>= fun state ->
    return (IdMap.find id state)

  let update_map id dynamic =
      get >>= fun state -> 
        match IdMap.find id state with 
          | (repr, None) ->     
              put (IdMap.add id (repr, Some dynamic) state)
          | (_, Some _) -> 
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
  module Utils (T : Typeable.Typeable) = struct
    let decode_repr_ctor c = match Repr.unpack_ctor c with
      | (Some c, ids) -> (c, ids)
      | _ -> invalid_arg "decode_repr_ctor"

    let decode_repr_noctor c = match Repr.unpack_ctor c with
      | (None, ids) -> ids
      | _ -> invalid_arg "decode_repr_ctor"

    let whizzy f id decode =
      find_by_id id >>= fun (repr, dynopt) ->
      match dynopt with 
        | None ->
            f (decode repr) >>= fun obj ->
            update_map id (T.makeDynamic obj) >>
            return obj
        | Some obj -> return (T.throwingCast obj)

    let whizzySum f id = whizzy f id decode_repr_ctor
    let whizzyNoCtor f id = whizzy f id decode_repr_noctor
    let whizzyRecord id (f : id list -> T.a m) =
      find_by_id id >>= fun (repr, obj) ->
        match obj with
          | None ->
              f (decode_repr_noctor repr)
          | Some obj -> return (T.throwingCast obj)


  end
end

module Run
  (S : sig
     type a
     val shelve : a -> id Write.m
     val unshelve : id -> a Read.m
   end) : sig
  type a = S.a
  val doShelve : a -> string
  val doUnshelve : string -> a
end =
struct
  type a = S.a

  type ids = (Id.t * Repr.t) list
      deriving (Pickle, Show)

  type dumpable = id * ids
      deriving (Show, Pickle)

  type ('a,'b) pair = 'a * 'b deriving (Pickle)
  type capp = int option * Id.t list deriving (Pickle)

  (* We don't serialize ids of each object at all: we just use the
     ordering in the output file to implicitly record the ids of
     objects.

     This can (and should) all be written much more efficiently.
  *)
  type discriminated = 
      (Id.t * string) list
    * (Id.t * (int * Id.t list)) list
    * (Id.t * (Id.t list)) list
        deriving (Pickle, Show)

  type discriminated_ordered = 
      string list
      * (int * Id.t list) list
      * (Id.t list) list
        deriving (Pickle, Show)

  let reorder : Id.t * discriminated -> Id.t * discriminated_ordered =
    fun (root,(a,b,c)) ->
      let collect_ids items (map,counter) =
        List.fold_left 
          (fun (map,counter) (id,_) ->
             IdMap.add id counter map, Id.next counter) 
          (map,counter) items in

      let map, _ = 
        collect_ids c
          (collect_ids b
             (collect_ids a
                (IdMap.empty, Id.initial))) in
      let lookup id = IdMap.find id map in
        (lookup root,
         (List.map snd a,
          List.map (fun (_,(c,l)) -> c, List.map lookup l) b,
          List.map (fun (_,l) -> List.map lookup l) c))

  let unorder : Id.t * discriminated_ordered -> Id.t * discriminated
    = fun (root,(a,b,c)) ->
      let number_sequentially id items =
        List.fold_left
          (fun (id,items) item -> 
             (Id.next id, (id,item)::items))
          (id,[]) items in
      let id = Id.initial in
      let id, a = number_sequentially id a in
      let id, b = number_sequentially id b in
      let  _, c = number_sequentially id c in
        (root, (a,b,c))

  type ('a,'b) either = Left of 'a | Right of 'b
  let either_partition (f : 'a -> ('b, 'c) either) (l : 'a list)
      : 'b list * 'c list =
    let rec aux (lefts, rights) = function
      | [] -> (List.rev lefts, List.rev rights)
      | x::xs ->
          match f x with 
            | Left l  -> aux (l :: lefts, rights) xs
            | Right r -> aux (lefts, r :: rights) xs
    in aux ([], []) l

  type discriminated_dumpable = Id.t * discriminated deriving (Pickle) 

  let discriminate : (Id.t * Repr.t) list -> discriminated
    = fun input ->
      let bytes, others = 
        either_partition
          (function
             | id, (Repr.Bytes s) -> Left (id,s)
             | id, (Repr.CApp c) -> Right (id,c))
          input in
      let ctors, no_ctors =
        either_partition
          (function
             | id, (Some c, ps) -> Left (id, (c,ps))
             | id, (None, ps) -> Right (id,ps))
          others in
        (bytes, ctors, no_ctors)

  let undiscriminate : discriminated -> (Id.t * Repr.t) list
    = fun (a,b,c) ->
      List.map (fun (id,s) -> (id,Repr.Bytes s)) a
      @ List.map (fun (id,(c,ps)) -> (id,Repr.CApp (Some c,ps))) b
      @ List.map (fun (id,(ps)) -> (id,Repr.CApp (None,ps))) c

  type do_pair = Id.t * discriminated_ordered 
      deriving (Show, Pickle)

  let write_discriminated : Id.t * (Id.t * Repr.t) list -> string
    = fun (root,map) -> 
      let dmap = discriminate map in 
      let rmap = reorder (root,dmap) in
        (*prerr_endline ("dmap : " ^ Show_discriminated.show dmap);*)
        Pickle_do_pair.pickleS rmap

  let read_discriminated : string -> Id.t * (Id.t * Repr.t) list
    = fun s -> 
      let rmap = Pickle_do_pair.unpickleS s in
      let (root,dmap) = unorder rmap in
        (root, undiscriminate dmap)

  open Write

  let decode_shelved_string : string -> Id.t * Read.s =
    fun s -> 
      let (id, state : dumpable) = 
        
(*Pickle_dumpable.unpickleS s *)
(*        Marshal.from_string s 0*)
        read_discriminated s
in
        id, (List.fold_right 
               (fun (id,repr) map -> IdMap.add id (repr,None) map)
               state
               IdMap.empty)

  let encode_shelved_string : id * Write.s -> string  =
    fun (id,state) ->
      let input_state = 
        id, IdMap.fold (fun id repr output -> (id,repr)::output)
          state.id2rep [] in
(*
        Pickle_dumpable.pickleS input_state
*)
(*        Marshal.to_string input_state []*)
        write_discriminated input_state

  let rec unduplicate equal = function
    | [] -> []
    | elem :: elems -> (let _, others = List.partition (equal elem) elems in
                          elem :: unduplicate equal others)

  let doShelveB : id m -> Buffer.t -> unit
    = fun m buffer ->
      let _, {id2rep=id2rep} = runState m initial_output_state in
      let ids : (Id.t * repr) list  = IdMap.fold (fun k v output -> (k,v)::output) id2rep [] in
        Printf.fprintf stderr  "%d ids\n" (List.length ids);
        let ids' = unduplicate (=) (List.map snd ids) in
          Printf.fprintf stderr  "~ %d ids?\n" (List.length ids');
          Pickle_ids.pickle buffer ids
  let doShelveS : id Write.m -> string
    = fun m ->
      let buffer = Buffer.create 127 in 
        begin
          doShelveB m buffer;
          Buffer.contents buffer
        end
  let doShelve v = 
    let id, state = runState (S.shelve v) initial_output_state in
      encode_shelved_string (id, state)

  let doUnshelve string = 
    let id, initial_input_state = decode_shelved_string string in  
    let value, _ = Read.runState (S.unshelve id) initial_input_state in
      value
end





module type Shelve =
sig
  type a
  module T : Typeable.Typeable with type a = a
  module E : Eq.Eq with type a = a
  val shelve : a -> id Write.m
  val unshelve : id -> a Read.m
  val shelveS : a -> string
  val unshelveS : string -> a
end

module Shelve_defaults
  (S : sig
     type a
     module T : Typeable.Typeable with type a = a
     module E : Eq.Eq with type a = a
     val shelve : a -> id Write.m
     val unshelve : id -> a Read.m
   end) =
struct
  include S
  module M = Run(S)
  let shelveS = M.doShelve
  and unshelveS = M.doUnshelve
end

module Shelve_from_pickle
  (P : Pickle.Pickle)
  (E : Eq.Eq with type a = P.a)
  (T : Typeable.Typeable with type a = P.a)
  : Shelve with type a = P.a
           and type a = T.a = Shelve_defaults
  (struct
     type a = T.a
     module T = T
     module E = E
     module Comp = Dynmap.Comp(T)(E)
     open Write
     let shelve obj = 
       allocate_id (T.makeDynamic obj) Comp.eq >>= fun (id, freshp) ->
         if freshp then
           store_repr id (Repr.of_string (P.pickleS obj)) >>
             return id
         else return id
     open Read
     let unshelve id = 
       find_by_id id >>= fun (repr, dynopt) ->
         match dynopt with
           | None -> 
               let obj : a = P.unpickleS (Repr.to_string repr) in
                 update_map id (T.makeDynamic obj) >> 
                   return obj
           | Some obj -> return (T.throwingCast obj)
   end)

module Shelve_unit : Shelve with type a = unit = Shelve_from_pickle(Pickle.Pickle_unit)(Eq.Eq_unit)(Typeable.Typeable_unit)
module Shelve_bool = Shelve_from_pickle(Pickle.Pickle_bool)(Eq.Eq_bool)(Typeable.Typeable_bool)
module Shelve_int = Shelve_from_pickle(Pickle.Pickle_int)(Eq.Eq_int)(Typeable.Typeable_int)
module Shelve_char = Shelve_from_pickle(Pickle.Pickle_char)(Eq.Eq_char)(Typeable.Typeable_char)
module Shelve_float = Shelve_from_pickle(Pickle.Pickle_float)(Eq.Eq_float)(Typeable.Typeable_float)
module Shelve_num = Shelve_from_pickle(Pickle.Pickle_num)(Eq.Eq_num)(Typeable.Typeable_num)
module Shelve_string = Shelve_from_pickle(Pickle.Pickle_string)(Eq.Eq_string)(Typeable.Typeable_string) 

module Shelve_option (V0 : Shelve) : Shelve with type a = V0.a option = Shelve_defaults(
  struct
    module T = Typeable.Typeable_option (V0.T)
    module E = Eq.Eq_option (V0.E)
    module Comp = Dynmap.Comp (T) (E)
    open Write
    type a = V0.a option
    let rec shelve =
      function
          None as obj ->
            allocate_id (T.makeDynamic obj) Comp.eq >>= fun (id,freshp) ->
              if freshp then
                store_repr id (Repr.make ~constructor:0 []) >>
                  return id
              else
                return id
        | Some v0 as obj ->
            allocate_id (T.makeDynamic obj) Comp.eq >>= fun (thisid,freshp) ->
              if freshp then
                V0.shelve v0 >>= fun id0 ->
                  store_repr thisid (Repr.make ~constructor:1 [id0]) >>
                    return thisid
              else
                return thisid
    open Read
    let unshelve = 
      let module W = Utils(T) in
      let f = function
        | 0, [] -> return None
        | 1, [id] -> V0.unshelve id >>= fun obj -> return (Some obj)
        | n, _ -> raise (UnshelvingError
                           ("Unexpected tag encountered unshelving "
                            ^"option : " ^ string_of_int n)) in
        W.whizzySum f
  end)


module Allocate =
struct
  open Write

  let allocate dynamic eq f =
    allocate_id dynamic eq >>= fun (id,freshp) ->
      if freshp then
        f id >>
          return id
      else
        return id
end

module Shelve_list (V0 : Shelve)
  : Shelve with type a = V0.a list = Shelve_defaults (
struct
  module T = Typeable.Typeable_list (V0.T)
  module E = Eq.Eq_list (V0.E)
  module Comp = Dynmap.Comp (T) (E)
  type a = V0.a list
  open Write
  let rec shelve = function
      [] as obj ->
        Allocate.allocate (T.makeDynamic obj) Comp.eq 
          (fun this -> store_repr this (Repr.make ~constructor:0 []))
    | (v0::v1) as obj ->
        Allocate.allocate (T.makeDynamic obj) Comp.eq
          (fun this -> V0.shelve v0 >>= fun id0 ->
                          shelve v1 >>= fun id1 ->
                            store_repr this (Repr.make ~constructor:1 [id0; id1]))
  open Read
  module W = Utils (T)
  let rec unshelve id = 
    let f = function
      | 0, [] -> return []
      | 1, [car;cdr] -> 
          V0.unshelve car >>= fun car ->
             unshelve cdr >>= fun cdr ->
               return (car :: cdr)
      | n, _ -> raise (UnshelvingError
                         ("Unexpected tag encountered unshelving "
                          ^"option : " ^ string_of_int n)) in
      W.whizzySum f id
end)
end
include Shelve  


type 'a ref = 'a Pervasives.ref = { mutable contents : 'a }
    deriving (Shelve)

(* Idea: compress the representation portion (id2rep) of the
   output_state before serialization, allowing objects of different
   types to share their serialized representation. *)

(* Idea: keep pointers to values that we've serialized in a global
   weak hash table so that we can share structure with them if we
   deserialize any equal values in the same process *)

(* Idea: serialize small objects (bools, chars) in place rather than
   using the extra level of indirection (and space) introduced by ids
*)

(* Idea: when serializing the repr list (the type `Run.ids') at the very end we could save space 
   by serializing two maps:

      (Id.t * string) list
      (Id.t * (int option * Id.t list)) list
   instead of 
      (Id.t * Repr.t) list
   since we'd wouldn't have an extra byte overhead for every item in the list.
*)

(* Idea: bitwise output instead of bytewise.  Probably a bit much to
   implement now, but should have a significant impact (e.g. one using
   bit instead of one byte for two-constructor sums) *)

(* 
   Should we use a different representation for lists? 
   i.e. write out the length followed by the elements?
   we could no longer claim sharing maximization, but it would
   actually be more efficient in most cases.
*)
