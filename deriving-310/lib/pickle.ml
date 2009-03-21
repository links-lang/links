(*pp deriving *)
(*
  Idea: 
  1. every object receives a serializable id.
  2. an object is serialized using the ids of its subobjects
*)
module Pickle =
struct
exception UnknownTag of int * string
exception UnpicklingError of string

module Id :
sig
  type t deriving (Show, Dump, Eq)
  val initial : t
  val compare : t -> t -> int
  val next : t -> t  
end =
struct
  type t = int deriving (Show, Dump, Eq)
  let initial = 0
  let compare = compare
  let next = succ
end
module IdMap = Map.Make (Id)
type id = Id.t deriving (Show, Dump)

module Repr : sig
  (* Break abstraction for the sake of efficiency for now *)
  type t = Bytes of string | CApp of (int option * Id.t list) deriving (Dump, Show)
  val of_string : string -> t
  val to_string : t -> string
  val make : ?constructor:int -> id list -> t
  val unpack_ctor : t -> int option * id list
end =
struct
  type t = Bytes of string | CApp of (int option * Id.t list) deriving (Dump, Show)
  let of_string s = Bytes s
  let to_string = function
    | Bytes s -> s
    | _ -> invalid_arg "string_of_repr"
  let make ?constructor ids = 
    match constructor with
      | Some n -> CApp (Some n, ids)
      | None   -> CApp (None, ids)
  let unpack_ctor = function 
    | CApp arg -> arg
    | _ -> raise (UnpicklingError "Error unpickling constructor")
end
type repr = Repr.t

type write_state = {
  nextid : Id.t;
  obj2id : Id.t Dynmap.DynMap.t;
  id2rep : repr IdMap.t;
}

let initial_output_state : write_state = {
    nextid = Id.initial;
    obj2id = Dynmap.DynMap.empty;
    id2rep = IdMap.empty;
  }

module Write = Monad.Monad_state (struct type state = write_state end)

open Write

let allocate typeable hash o f =
  let comparator = Dynmap.comp typeable hash.Hash._Eq in
      
  let obj = Typeable.make_dynamic typeable o in
    get >>= fun ({nextid=nextid;obj2id=obj2id} as t) ->
      match Dynmap.DynMap.find obj obj2id with
        | Some id -> return id
        | None -> 
            let id, nextid = nextid, Id.next nextid in
              put {t with
                     obj2id = Dynmap.DynMap.add obj id comparator obj2id;
                     nextid = nextid} >>
                f id >> return id
                  
let store_repr id repr =
  get >>= fun state ->
    put {state with id2rep = IdMap.add id repr state.id2rep}

type read_state = (repr * (Typeable.dynamic option)) IdMap.t

module Read = Monad.Monad_state (struct type state = read_state end)

open Read

let find_by_id id =
  get >>= fun state -> return (IdMap.find id state)

let decode_repr_ctor c = match Repr.unpack_ctor c with
  | (Some c, ids) -> (c, ids)
  | _ -> invalid_arg "decode_repr_ctor"
      
let decode_repr_noctor c = match Repr.unpack_ctor c with
  | (None, ids) -> ids
  | _ -> invalid_arg "decode_repr_ctor"
      
let update_map typeable id obj =
  let dynamic = Typeable.make_dynamic typeable obj in
    get >>= fun state -> 
      match IdMap.find id state with 
        | (repr, None) ->     
            put (IdMap.add id (repr, Some dynamic) state)
        | (_, Some _) -> 
            return ()
              (* Checking for id already present causes unpickling to fail 
                 when there is circularity involving immutable values (even 
                 if the recursion wholly depends on mutability).
                 
                 For example, consider the code

                 type t = A | B of t ref deriving (Typeable, Eq, Pickle)
                 let s = ref A in
                 let r = B s in
                 s := r;
                 let pickled = Pickle_t.pickleS r in
                   Pickle_t.unpickleS r
                 
                 which results in the value
                 B {contents = B {contents = B { ... }}}
                 
                 During deserialization the following steps occur:
                 1. lookup "B {...}" in the dictionary (not there)
                 2. unpickle the contents of B:
                 3. lookup the contents in the dictionary (not there)
                 4. create a blank reference, insert it into the dictionary
                 5. unpickle the contents of the reference:
                 6. lookup ("B {...}") in the dictionary (not there)
                 7. unpickle the contents of B:
                 8. lookup the contents in the dictionary (there)
                 9. insert "B{...}" into the dictionary.
                 10. insert "B{...}" into the dictionary.
              *)

let whizzy typeable f id decode =
  find_by_id id >>= fun (repr, dynopt) ->
    match dynopt with 
      | None ->
          f (decode repr) >>= fun obj ->
            update_map typeable id obj >>
              return obj
      | Some obj -> return (Typeable.throwing_cast typeable obj)

let sum typeable f id = whizzy typeable f id decode_repr_ctor
let tuple typeable f id = whizzy typeable f id decode_repr_noctor
let record_tag = 0
let record typeable f size id =
  find_by_id id >>= fun (repr, obj) ->
    match obj with
      | None ->
          let this = Obj.magic  (Obj.new_block record_tag size) in
            update_map typeable id this >>
              f this (decode_repr_noctor repr) >>
              return this
      | Some obj -> return (Typeable.throwing_cast typeable obj)


type 'a pickle = {
  _Typeable : 'a Typeable.typeable ;
  _Hash     : 'a Hash.hash ;
  pickle : 'a -> id Write.m ;
  unpickle : id -> 'a Read.m 
}

type ids = (Id.t * Repr.t) list
    deriving (Dump, Show)

type dumpable = id * ids
    deriving (Show, Dump)

type ('a,'b) pair = 'a * 'b deriving (Dump)
type capp = int option * Id.t list deriving (Dump)

(* We don't serialize ids of each object at all: we just use the
   ordering in the output file to implicitly record the ids of
   objects.
   
   Also, we don't serialize the repr constructors.  All values with
   a particular constructor are grouped in a single list.

   This can (and should) all be written much more efficiently.
*)
type discriminated = 
    (Id.t * string) list
  * (Id.t * (int * Id.t list)) list
  * (Id.t * (Id.t list)) list
      deriving (Dump, Show)

type discriminated_ordered = 
      string list
    * (int * Id.t list) list
    * (Id.t list) list
      deriving (Dump, Show)

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
       
type discriminated_dumpable = Id.t * discriminated deriving (Dump) 
    
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
    deriving (Show, Dump)
    
let write_discriminated f
    = fun (root,map) ->
      let dmap = discriminate map in
      let rmap = reorder (root,dmap) in
        f rmap
          
let read_discriminated (f : 'b -> 'a) : 'b -> Id.t * (Id.t * Repr.t) list
  = fun s -> 
    let rmap = f s in
    let (root,dmap) = unorder rmap in
      (root, undiscriminate dmap)
        
open Write
        
let decode_pickled_string (f : 'a -> Id.t * discriminated_ordered) : 'b -> Id.t * read_state =
  fun s -> 
    let (id, state : dumpable) = 
      read_discriminated f s
    in
      id, (List.fold_right 
             (fun (id,repr) map -> IdMap.add id (repr,None) map)
             state
             IdMap.empty)
        
let encode_pickled_string f =
  fun (id,state) ->
    let input_state =
      id, IdMap.fold (fun id repr output -> (id,repr)::output)
        state.id2rep [] in
      write_discriminated f input_state
        
let doPickle s f v : 'a = 
  let id, state = runState (s.pickle v) initial_output_state in
    encode_pickled_string f (id, state)
      
let doUnpickle s f input = 
  let id, initial_input_state = decode_pickled_string f input in  
  let value, _ = Read.runState (s.unpickle id) initial_input_state in
    value
      
let from_channel s = doUnpickle s (Dump.from_channel dump_do_pair)
let from_string s = doUnpickle s (Dump.from_string dump_do_pair)
let from_stream s = doUnpickle s (Dump.from_stream dump_do_pair)
let to_channel s channel = doPickle s (Dump.to_channel dump_do_pair channel)
let to_buffer s buffer = doPickle s (Dump.to_buffer dump_do_pair buffer)
let to_string s = doPickle s (Dump.to_string dump_do_pair)

let pickle_from_dump
    (p : 'a Dump.dump)
    (hash : 'a Hash.hash)
    (typeable : 'a Typeable.typeable)
    : 'a pickle =
  let comp = Dynmap.comp typeable hash.Hash._Eq in
(*  let w = Read.utils typeable eq in
  let u = Write.utils typeable in*)
    { _Typeable = typeable;
      _Hash = hash;
      pickle = (fun obj ->
                  allocate typeable hash obj
                    (fun id -> store_repr id (Repr.of_string (Dump.to_string p obj))));
      unpickle = (fun id ->
                    Read.(>>=) (find_by_id id) (fun (repr, dynopt) ->
                                                  match dynopt with
                                                    | None ->
                                                        let obj : 'a = Dump.from_string p (Repr.to_string repr) in
                                                          Read.(>>)
                                                            (update_map typeable id obj) (Read.return obj)
                                                    | Some obj -> Read.return (Typeable.throwing_cast typeable obj)))
    }

let pickle_unit   = pickle_from_dump Dump.dump_unit   Hash.hash_unit   Typeable.typeable_unit
let pickle_bool   = pickle_from_dump Dump.dump_bool   Hash.hash_bool   Typeable.typeable_bool
let pickle_int    = pickle_from_dump Dump.dump_int    Hash.hash_int    Typeable.typeable_int
let pickle_char   = pickle_from_dump Dump.dump_char   Hash.hash_char   Typeable.typeable_char
let pickle_float  = pickle_from_dump Dump.dump_float  Hash.hash_float  Typeable.typeable_float
let pickle_num    = pickle_from_dump Dump.dump_num    Hash.hash_num    Typeable.typeable_num
let pickle_string = pickle_from_dump Dump.dump_string Hash.hash_string Typeable.typeable_string

let pickle_option (v0 : 'a pickle) : 'a option pickle =
  let typeable = Typeable.typeable_option v0._Typeable in
  let hash = Hash.hash_option v0._Hash  in
  let comp = Dynmap.comp typeable hash.Hash._Eq in
  let rec pickle =
      function
          None as obj ->
            allocate typeable hash obj
              (fun id -> store_repr id (Repr.make ~constructor:0 []))
        | Some v as obj ->
            allocate typeable hash obj
              (fun thisid ->
                 v0.pickle v >>= fun id0 ->
                   store_repr thisid (Repr.make ~constructor:1 [id0])) in
  let unpickle =
    let f = function
      | 0, [] -> Read.return None
      | 1, [id] -> Read.(>>=) (v0.unpickle id) (fun obj -> Read.return (Some obj))
        | n, _ -> raise (UnpicklingError
                           ("Unexpected tag encountered unpickling "
                            ^"option : " ^ string_of_int n)) in
      sum typeable f
  in 
  { _Typeable = typeable ; _Hash = hash ; pickle = pickle ; unpickle = unpickle }

let pickle_list (pickler : 'a pickle) : 'a list pickle =
  let typeable = Typeable.typeable_list pickler._Typeable in
  let hash = Hash.hash_list pickler._Hash in
  let comp = Dynmap.comp typeable hash.Hash._Eq in
  let rec pickle = function
      [] as obj ->
        allocate typeable hash obj
          (fun this -> store_repr this (Repr.make ~constructor:0 []))
    | (v0::v1) as obj ->
        allocate typeable hash obj
          (fun this -> pickler.pickle v0 >>= fun id0 ->
                               pickle v1 >>= fun id1 ->
                                 store_repr this (Repr.make ~constructor:1 [id0; id1])) in
  let rec unpickle id =
    let module M = struct
      open Read
      let f = function
        | 0, [] -> return []
        | 1, [car;cdr] ->
            pickler.unpickle car >>= fun car ->
              unpickle cdr >>= fun cdr ->
                return (car :: cdr)
              | n, _ -> raise (UnpicklingError
                                 ("Unexpected tag encountered unpickling "
                                  ^"option : " ^ string_of_int n)) 
    end in
      sum typeable M.f id
  in
    { _Typeable = typeable ; _Hash = hash ; pickle = pickle ; unpickle = unpickle }

 end 
 include Pickle 

type 'a ref = 'a Pervasives.ref = { mutable contents : 'a } 
    deriving (Pickle) 

let pickle_6 (a1 : 'a1 pickle) (a2 : 'a2 pickle) (a3 : 'a3 pickle) (a4 : 'a4 pickle) (a5 : 'a5 pickle) (a6 : 'a6 pickle)
    :  ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) pickle =
  let typeable = Typeable.typeable_6 a1._Typeable a2._Typeable a3._Typeable a4._Typeable a5._Typeable a6._Typeable in
  let hash = Hash.hash_6 a1._Hash a2._Hash a3._Hash a4._Hash a5._Hash a6._Hash in
  let comp = Dynmap.comp typeable hash.Hash._Eq in
  let rec pickle (a1, a2, a3, a4, a5, a6) = assert false in
  let rec unpickle =
    let module M = struct
      open Read
      let v = 
        tuple typeable
        (function
           | [v1;v2;v3;v4;v5;v6] ->
               (a1.unpickle v1 >>= fun b1 ->
                  a2.unpickle v2 >>= fun b2 ->
                    a3.unpickle v3 >>= fun b3 ->
                      a4.unpickle v4 >>= fun b4 ->
                        a5.unpickle v5 >>= fun b5 ->
                          a6.unpickle v6 >>= fun b6 ->
                            return (b1, b2, b3, b4, b5, b6))
           | l -> raise (UnpicklingError
                           ("Unexpected number of elements encountered when unpickling "
                            ^"6-tuple : " ^ string_of_int (List.length l))))
    end in M.v
  in
    { _Typeable = typeable ; _Hash = hash ; pickle = pickle ; unpickle = unpickle }

let pickle_5 (a1 : 'a1 pickle) (a2 : 'a2 pickle) (a3 : 'a3 pickle) (a4 : 'a4 pickle) (a5 : 'a5 pickle)
    :  ('a1 * 'a2 * 'a3 * 'a4 * 'a5) pickle =
  let typeable = Typeable.typeable_5 a1._Typeable a2._Typeable a3._Typeable a4._Typeable a5._Typeable in
  let hash = Hash.hash_5 a1._Hash a2._Hash a3._Hash a4._Hash a5._Hash in
  let comp = Dynmap.comp typeable hash.Hash._Eq in
  let rec pickle (a1, a2, a3, a4, a5) = assert false in
  let rec unpickle =
    let module M = struct
      open Read
      let v = 
        tuple typeable
        (function
           | [v1;v2;v3;v4;v5] ->
               (a1.unpickle v1 >>= fun b1 ->
                  a2.unpickle v2 >>= fun b2 ->
                    a3.unpickle v3 >>= fun b3 ->
                      a4.unpickle v4 >>= fun b4 ->
                        a5.unpickle v5 >>= fun b5 ->
                            return (b1, b2, b3, b4, b5))
           | l -> raise (UnpicklingError
                           ("Unexpected number of elements encountered when unpickling "
                            ^"5-tuple : " ^ string_of_int (List.length l))))
    end in M.v
  in
    { _Typeable = typeable ; _Hash = hash ; pickle = pickle ; unpickle = unpickle }

let pickle_4 (a1 : 'a1 pickle) (a2 : 'a2 pickle) (a3 : 'a3 pickle) (a4 : 'a4 pickle) 
    :  ('a1 * 'a2 * 'a3 * 'a4) pickle =
  let typeable = Typeable.typeable_4 a1._Typeable a2._Typeable a3._Typeable a4._Typeable in
  let hash = Hash.hash_4 a1._Hash a2._Hash a3._Hash a4._Hash in
  let comp = Dynmap.comp typeable hash.Hash._Eq in
  let rec pickle (a1, a2, a3, a4) = assert false in
  let rec unpickle =
    let module M = struct
      open Read
      let v = 
        tuple typeable
        (function
           | [v1;v2;v3;v4] ->
               (a1.unpickle v1 >>= fun b1 ->
                  a2.unpickle v2 >>= fun b2 ->
                    a3.unpickle v3 >>= fun b3 ->
                      a4.unpickle v4 >>= fun b4 ->
                            return (b1, b2, b3, b4))
           | l -> raise (UnpicklingError
                           ("Unexpected number of elements encountered when unpickling "
                            ^"4-tuple : " ^ string_of_int (List.length l))))
    end in M.v
  in
    { _Typeable = typeable ; _Hash = hash ; pickle = pickle ; unpickle = unpickle }

let pickle_3 (a1 : 'a1 pickle) (a2 : 'a2 pickle) (a3 : 'a3 pickle) :  ('a1 * 'a2 * 'a3) pickle =
  let typeable = Typeable.typeable_3 a1._Typeable a2._Typeable a3._Typeable in
  let hash = Hash.hash_3 a1._Hash a2._Hash a3._Hash in
  let comp = Dynmap.comp typeable hash.Hash._Eq in
  let rec pickle (a1, a2, a3) = assert false in
  let rec unpickle =
    let module M = struct
      open Read
      let v = 
        tuple typeable
        (function
           | [v1;v2;v3] ->
               (a1.unpickle v1 >>= fun b1 ->
                  a2.unpickle v2 >>= fun b2 ->
                    a3.unpickle v3 >>= fun b3 ->
                            return (b1, b2, b3))
           | l -> raise (UnpicklingError
                           ("Unexpected number of elements encountered when unpickling "
                            ^"3-tuple : " ^ string_of_int (List.length l))))
    end in M.v
  in
    { _Typeable = typeable ; _Hash = hash ; pickle = pickle ; unpickle = unpickle }

let pickle_2 (a1 : 'a1 pickle) (a2 : 'a2 pickle) :  ('a1 * 'a2) pickle =
  let typeable = Typeable.typeable_2 a1._Typeable a2._Typeable in
  let hash = Hash.hash_2 a1._Hash a2._Hash in
  let comp = Dynmap.comp typeable hash.Hash._Eq in
  let rec pickle (a1, a2) = assert false in
  let rec unpickle =
    let module M = struct
      open Read
      let v = 
        tuple typeable
        (function
           | [v1;v2] ->
               (a1.unpickle v1 >>= fun b1 ->
                  a2.unpickle v2 >>= fun b2 ->
                    return (b1, b2))
           | l -> raise (UnpicklingError
                           ("Unexpected number of elements encountered when unpickling "
                            ^"2-tuple : " ^ string_of_int (List.length l))))
    end in M.v
  in
    { _Typeable = typeable ; _Hash = hash ; pickle = pickle ; unpickle = unpickle }

(* Idea: keep pointers to values that we've serialized in a global
   weak hash table so that we can share structure with them if we
   deserialize any equal values in the same process *)

(* Idea: serialize small objects (bools, chars) in place rather than
   using the extra level of indirection (and space) introduced by ids
*)

(* Idea: bitwise output instead of bytewise.  Probably a bit much to
   implement now, but should have a significant impact (e.g. one using
   bit instead of one byte for two-constructor sums) *)

(* Should we use a different representation for lists?  i.e. write out
   the length followed by the elements?  we could no longer claim
   sharing maximization, but it would actually be more efficient in
   most cases.
*)
