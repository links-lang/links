(*
  Idea: 
  1. every object receives a serializable id.
  2. an object is serialized using the ids of its subobjects
  3. we also serialize a mapping from ids to offsets.
*)
exception UnshelvingError of string

open Shelvehelper

type 'a m = 'a Shelvehelper.m
type id = Shelvehelper.id
type 'a n = 'a Shelvehelper.Input.m

module type Shelve =
sig
  type a
  module T : Typeable.Typeable with type a = a
  module E : Eq.Eq with type a = a
  val shelve : a -> id m
  val unshelve : id -> a n
  val shelveS : a -> string
  val unshelveS : string -> a
end

module Shelve_defaults
  (S : sig
     type a
     module T : Typeable.Typeable with type a = a
     module E : Eq.Eq with type a = a
     val shelve : a -> id m
     val unshelve : id -> a Input.m
   end) =
struct
  include S
  module M = Do(S)
  let shelveS = M.doShelve
  and unshelveS = M.doUnshelve
end

module Shelve_primtype
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
     let shelve obj = 
       allocate_id (T.makeDynamic obj) Comp.eq >>= fun (id, freshp) ->
         if freshp then
           store_repr id (repr_of_string (P.pickleS obj)) >>
             return id
         else return id
     open Shelvehelper.Input
     let unshelve id = 
       find_by_id id >>= fun (repr, dynopt) ->
         match dynopt with
           | None -> 
               let obj : a = P.unpickleS (string_of_repr repr) in
                 update_map id (T.makeDynamic obj) >> 
                   return obj
           | Some obj -> return (T.throwingCast obj)
   end)

module Shelve_unit : Shelve with type a = unit = Shelve_primtype(Pickle.Pickle_unit)(Eq.Eq_unit)(Typeable.Typeable_unit)
module Shelve_bool = Shelve_primtype(Pickle.Pickle_bool)(Eq.Eq_bool)(Typeable.Typeable_bool)
module Shelve_int = Shelve_primtype(Pickle.Pickle_int)(Eq.Eq_int)(Typeable.Typeable_int)
module Shelve_char = Shelve_primtype(Pickle.Pickle_char)(Eq.Eq_char)(Typeable.Typeable_char)
module Shelve_float = Shelve_primtype(Pickle.Pickle_float)(Eq.Eq_float)(Typeable.Typeable_float)
module Shelve_num = Shelve_primtype(Pickle.Pickle_num)(Eq.Eq_num)(Typeable.Typeable_num)

module Shelve_option (V0 : Shelve) : Shelve with type a = V0.a option = Shelve_defaults(
  struct
    module T = Typeable.Typeable_option (V0.T)
    module E = Eq.Eq_option (V0.E)
    module Comp = Dynmap.Comp (T) (E)
    open Shelvehelper
    type a = V0.a option
    let rec shelve =
      function
          None as obj ->
            allocate_id (T.makeDynamic obj) Comp.eq >>= fun (id,freshp) ->
              if freshp then
                store_repr id (make_repr ~constructor:0 []) >>
                  return id
              else
                return id
        | Some v0 as obj ->
            allocate_id (T.makeDynamic obj) Comp.eq >>= fun (thisid,freshp) ->
              if freshp then
                V0.shelve v0 >>= fun id0 ->
                  store_repr thisid (make_repr ~constructor:1 [id0]) >>
                    return thisid
              else
                return thisid
    open Shelvehelper.Input
    let unshelve = 
      let module W = Whizzy(T) in
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
  let rec shelve = function
      [] as obj ->
        Allocate.allocate (T.makeDynamic obj) Comp.eq 
          (fun this -> store_repr this (make_repr ~constructor:0 []))
    | (v0::v1) as obj ->
        Allocate.allocate (T.makeDynamic obj) Comp.eq
          (fun this -> V0.shelve v0 >>= fun id0 ->
                          shelve v1 >>= fun id1 ->
                            store_repr this (make_repr ~constructor:1 [id0; id1]))
  open Shelvehelper.Input
  module W = Whizzy (T)
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
  

(* Is this right for mutable strings?  I think so, because of Eq, but it should be checked *)
module Shelve_string = Shelve_primtype(Pickle.Pickle_string)(Eq.Eq_string)(Typeable.Typeable_string) 

module Shelve_ref (S : Shelve) = Shelve_defaults(
  struct
    module E = Eq.Eq_ref(S.E)
    module T = Typeable.Typeable_ref(S.T)
    module Comp = Dynmap.Comp(T)(E)
    type a = S.a ref
    let shelve : a -> id m =
      fun r -> (* exactly what we'd generate (even for immutable types) *)
        Allocate.allocate (T.makeDynamic r) Comp.eq 
          (fun this -> (S.shelve r.contents >>= fun content_id ->
                          (store_repr this (make_repr [content_id]))))

    open Shelvehelper.Input
    let record_tag = 0
    let ref_size = 1
    let unshelve : id -> a m =
      fun id ->
        find_by_id id >>= fun (repr, obj) ->
          match obj with
            | None ->
                flush stderr;
                let this = Obj.new_block record_tag ref_size in
                  update_map id (T.makeDynamic (Obj.magic this)) >>= 
                    (fun _ ->
                       begin match ctor_repr repr with
                         | None, [x] -> 
                             S.unshelve x >>= fun contents ->
                               let obj : S.a ref = Obj.magic this in
                                 obj.contents <- contents;
                                 return obj
                             | _ -> assert false
                       end)
            | Some obj -> return (T.throwingCast obj)
  end)

(* Idea: compress the representation portion (id2rep) of the
   output_state before serialization, allowing objects of different
   types to share their serialized representation. *)

(* Idea: keep pointers to values that we've serialized in a global
   weak hash table so that we can share structure with them if we
   deserialize any equal values in the same process *)
