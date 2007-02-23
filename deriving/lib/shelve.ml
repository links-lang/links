(*
  Idea: 
  1. every object receives a serializable id.
  2. an object is serialized using the ids of its subobjects
  3. we also serialize a mapping from ids to offsets.
*)
open Shelvehelper

module type Shelve =
sig
  type a
  module Typeable : Typeable.Typeable with type a = a
  module Eq : Eq.Eq with type a = a
  val shelve : a -> id m
end

module Shelve_defaults (S : Shelve) = S

module Shelve_primtype
  (P : Pickle.Pickle)
  (E : Eq.Eq with type a = P.a)
  (T : Typeable.Typeable with type a = P.a)
  : Shelve with type a = P.a
           and type a = T.a =
struct
  type a = T.a
  module Typeable = T
  module Eq = E
  module Comp = Dynmap.Comp(T)(Eq)
  let shelve obj = 
    allocate_store_return (T.makeDynamic obj) Comp.eq (repr_of_string (P.pickleS obj))
end

module Shelve_unit : Shelve with type a = unit = Shelve_primtype(Pickle.Pickle_unit)(Eq.Eq_unit)(Typeable.Typeable_unit)
module Shelve_bool = Shelve_primtype(Pickle.Pickle_bool)(Eq.Eq_bool)(Typeable.Typeable_bool)
module Shelve_int = Shelve_primtype(Pickle.Pickle_int)(Eq.Eq_int)(Typeable.Typeable_int)
module Shelve_char = Shelve_primtype(Pickle.Pickle_char)(Eq.Eq_char)(Typeable.Typeable_char)
module Shelve_float = Shelve_primtype(Pickle.Pickle_float)(Eq.Eq_float)(Typeable.Typeable_float)
module Shelve_num = Shelve_primtype(Pickle.Pickle_num)(Eq.Eq_num)(Typeable.Typeable_num)

module Shelve_option (V0 : Shelve) : Shelve with type a = V0.a option =
  (struct
     module Typeable = Typeable.Typeable_option (V0.Typeable)
     module Eq = Eq.Eq_option (V0.Eq)
     module Comp = Dynmap.Comp (Typeable) (Eq)
     open Shelvehelper
     type a = V0.a option
     let rec shelve =
       function
           None as obj ->
             allocate_store_return (Typeable.makeDynamic obj) Comp.eq
               (make_repr ~constructor:0 [])
         | Some v0 as obj ->
             let module M = V0
             in
               ( >>= ) (M.shelve v0)
                 (fun id0 ->
                    allocate_store_return (Typeable.makeDynamic obj) Comp.eq
                      (make_repr ~constructor:1 [id0]))
   end :
     Shelve with type a = V0.a option)


module Shelve_list (V0 : Shelve)
  : Shelve with type a = V0.a list =
struct
  module Typeable = Typeable.Typeable_list (V0.Typeable)
  module Eq = Eq.Eq_list (V0.Eq)
  module Comp = Dynmap.Comp (Typeable) (Eq)
  type a = V0.a list
  let rec shelve = function
      [] as obj ->
        allocate_store_return (Typeable.makeDynamic obj) Comp.eq
          (make_repr ~constructor:0 [])
    | (v0::v1) as obj ->
        let module M = V0
        in
          M.shelve v0 >>= fun id0 ->
            shelve v1 >>= fun id1 ->
              allocate_store_return
                (Typeable.makeDynamic obj)
                Comp.eq
                (make_repr ~constructor:1 [id0; id1])
end 
  

(* Is this right for mutable strings?  I think so, because of Eq, but it should be checked *)
module Shelve_string = Shelve_primtype(Pickle.Pickle_string)(Eq.Eq_string)(Typeable.Typeable_string) 

module Shelve_1 (S1 : Shelve) = S1

module Shelve_2
  (S1 : Shelve)
  (S2 : Shelve) 
  : Shelve with type a = S1.a * S2.a =
struct
  type a = S1.a * S2.a
  module Typeable = Typeable.Typeable_2(S1.Typeable)(S2.Typeable)
  module Eq = Eq.Eq_2(S1.Eq)(S2.Eq)
  module Comp = Dynmap.Comp(Typeable)(Eq)
  let shelve : a -> id m =
    fun ((obj1, obj2) as obj) ->
      S1.shelve obj1 >>= fun id1 ->
      S2.shelve obj2 >>= fun id2 ->
      allocate_store_return (Typeable.makeDynamic obj) Comp.eq (make_repr [id1;id2])
end

module Shelve_3
  (S1 : Shelve)
  (S2 : Shelve)
  (S3 : Shelve)
  : Shelve with type a = S1.a * S2.a * S3.a =
struct
  type a = S1.a * S2.a * S3.a
  module Typeable = Typeable.Typeable_3(S1.Typeable)(S2.Typeable)(S3.Typeable)
  module Eq = Eq.Eq_3(S1.Eq)(S2.Eq)(S3.Eq)
  module Comp = Dynmap.Comp(Typeable)(Eq)
  let shelve : a -> id m =
    fun ((obj1, obj2, obj3) as obj) ->
      S1.shelve obj1 >>= fun id1 ->
      S2.shelve obj2 >>= fun id2 ->
      S3.shelve obj3 >>= fun id3 ->    
      allocate_store_return (Typeable.makeDynamic obj) Comp.eq (make_repr [id1;id2;id3])
end


module Shelve_4
  (S1 : Shelve)
  (S2 : Shelve)
  (S3 : Shelve)
  (S4 : Shelve)
  : Shelve with type a = S1.a * S2.a * S3.a * S4.a =
struct
  type a = S1.a * S2.a * S3.a * S4.a
  module Typeable = Typeable.Typeable_4(S1.Typeable)(S2.Typeable)(S3.Typeable)(S4.Typeable)
  module Eq = Eq.Eq_4(S1.Eq)(S2.Eq)(S3.Eq)(S4.Eq)
  module Comp = Dynmap.Comp(Typeable)(Eq)
  let shelve : a -> id m =
    fun ((obj1, obj2, obj3, obj4) as obj) ->
      S1.shelve obj1 >>= fun id1 ->
      S2.shelve obj2 >>= fun id2 ->
      S3.shelve obj3 >>= fun id3 ->
      S4.shelve obj4 >>= fun id4 ->    
      allocate_store_return (Typeable.makeDynamic obj) Comp.eq (make_repr [id1;id2;id3;id4])
end


module Shelve_5
  (S1 : Shelve)
  (S2 : Shelve)
  (S3 : Shelve)
  (S4 : Shelve)
  (S5 : Shelve)
  : Shelve with type a = S1.a * S2.a * S3.a * S4.a * S5.a =
struct
  type a = S1.a * S2.a * S3.a * S4.a * S5.a
  module Typeable = Typeable.Typeable_5(S1.Typeable)(S2.Typeable)(S3.Typeable)(S4.Typeable)(S5.Typeable)
  module Eq = Eq.Eq_5(S1.Eq)(S2.Eq)(S3.Eq)(S4.Eq)(S5.Eq)
  module Comp = Dynmap.Comp(Typeable)(Eq)
  let shelve : a -> id m =
    fun ((obj1, obj2, obj3, obj4, obj5) as obj) ->
      S1.shelve obj1 >>= fun id1 ->
      S2.shelve obj2 >>= fun id2 ->
      S3.shelve obj3 >>= fun id3 ->
      S4.shelve obj4 >>= fun id4 ->
      S5.shelve obj5 >>= fun id5 ->    
      allocate_store_return (Typeable.makeDynamic obj) Comp.eq (make_repr [id1;id2;id3;id4;id5])
end


module Shelve_6
  (S1 : Shelve)
  (S2 : Shelve)
  (S3 : Shelve)
  (S4 : Shelve)
  (S5 : Shelve)
  (S6 : Shelve)
  : Shelve with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a =
struct
  type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a
  module Typeable = Typeable.Typeable_6(S1.Typeable)(S2.Typeable)(S3.Typeable)(S4.Typeable)(S5.Typeable)(S6.Typeable)
  module Eq = Eq.Eq_6(S1.Eq)(S2.Eq)(S3.Eq)(S4.Eq)(S5.Eq)(S6.Eq)
  module Comp = Dynmap.Comp(Typeable)(Eq)
  let shelve : a -> id m =
    fun ((obj1, obj2, obj3, obj4, obj5, obj6) as obj) ->
      S1.shelve obj1 >>= fun id1 ->
      S2.shelve obj2 >>= fun id2 ->
      S3.shelve obj3 >>= fun id3 ->
      S4.shelve obj4 >>= fun id4 ->
      S5.shelve obj5 >>= fun id5 ->
      S6.shelve obj6 >>= fun id6 ->    
      allocate_store_return (Typeable.makeDynamic obj) Comp.eq (make_repr [id1;id2;id3;id4;id5;id6])
end


module Shelve_7
  (S1 : Shelve)
  (S2 : Shelve)
  (S3 : Shelve)
  (S4 : Shelve)
  (S5 : Shelve)
  (S6 : Shelve)
  (S7 : Shelve)
  : Shelve with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a =
struct
  type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a
  module Typeable = Typeable.Typeable_7(S1.Typeable)(S2.Typeable)(S3.Typeable)(S4.Typeable)(S5.Typeable)(S6.Typeable)(S7.Typeable)
  module Eq = Eq.Eq_7(S1.Eq)(S2.Eq)(S3.Eq)(S4.Eq)(S5.Eq)(S6.Eq)(S7.Eq)
  module Comp = Dynmap.Comp(Typeable)(Eq)
  let shelve : a -> id m =
    fun ((obj1, obj2, obj3, obj4, obj5, obj6, obj7) as obj) ->
      S1.shelve obj1 >>= fun id1 ->
      S2.shelve obj2 >>= fun id2 ->
      S3.shelve obj3 >>= fun id3 ->
      S4.shelve obj4 >>= fun id4 ->
      S5.shelve obj5 >>= fun id5 ->
      S6.shelve obj6 >>= fun id6 ->
      S7.shelve obj7 >>= fun id7 ->    
      allocate_store_return (Typeable.makeDynamic obj) Comp.eq (make_repr [id1;id2;id3;id4;id5;id6;id7])
end


module Shelve_8
  (S1 : Shelve)
  (S2 : Shelve)
  (S3 : Shelve)
  (S4 : Shelve)
  (S5 : Shelve)
  (S6 : Shelve)
  (S7 : Shelve)
  (S8 : Shelve)
  : Shelve with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a * S8.a =
struct
  type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a * S8.a
  module Typeable = Typeable.Typeable_8(S1.Typeable)(S2.Typeable)(S3.Typeable)(S4.Typeable)(S5.Typeable)(S6.Typeable)(S7.Typeable)(S8.Typeable)
  module Eq = Eq.Eq_8(S1.Eq)(S2.Eq)(S3.Eq)(S4.Eq)(S5.Eq)(S6.Eq)(S7.Eq)(S8.Eq)
  module Comp = Dynmap.Comp(Typeable)(Eq)
  let shelve : a -> id m =
    fun ((obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8) as obj) ->
      S1.shelve obj1 >>= fun id1 ->
      S2.shelve obj2 >>= fun id2 ->
      S3.shelve obj3 >>= fun id3 ->
      S4.shelve obj4 >>= fun id4 ->
      S5.shelve obj5 >>= fun id5 ->
      S6.shelve obj6 >>= fun id6 ->
      S7.shelve obj7 >>= fun id7 ->
      S8.shelve obj8 >>= fun id8 ->    
      allocate_store_return (Typeable.makeDynamic obj) Comp.eq (make_repr [id1;id2;id3;id4;id5;id6;id7;id8])
end


module Shelve_9
  (S1 : Shelve)
  (S2 : Shelve)
  (S3 : Shelve)
  (S4 : Shelve)
  (S5 : Shelve)
  (S6 : Shelve)
  (S7 : Shelve)
  (S8 : Shelve)
  (S9 : Shelve)
  : Shelve with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a * S8.a * S9.a =
struct
  type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a * S8.a * S9.a
  module Typeable = Typeable.Typeable_9(S1.Typeable)(S2.Typeable)(S3.Typeable)(S4.Typeable)(S5.Typeable)(S6.Typeable)(S7.Typeable)(S8.Typeable)(S9.Typeable)
  module Eq = Eq.Eq_9(S1.Eq)(S2.Eq)(S3.Eq)(S4.Eq)(S5.Eq)(S6.Eq)(S7.Eq)(S8.Eq)(S9.Eq)
  module Comp = Dynmap.Comp(Typeable)(Eq)
  let shelve : a -> id m =
    fun ((obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9) as obj) ->
      S1.shelve obj1 >>= fun id1 ->
      S2.shelve obj2 >>= fun id2 ->
      S3.shelve obj3 >>= fun id3 ->
      S4.shelve obj4 >>= fun id4 ->
      S5.shelve obj5 >>= fun id5 ->
      S6.shelve obj6 >>= fun id6 ->
      S7.shelve obj7 >>= fun id7 ->
      S8.shelve obj8 >>= fun id8 ->
      S9.shelve obj9 >>= fun id9 ->    
      allocate_store_return (Typeable.makeDynamic obj) Comp.eq (make_repr [id1;id2;id3;id4;id5;id6;id7;id8;id9])
end


(* Idea: compress the representation portion (id2rep) of the
   output_state before serialization, allowing objects of different
   types to share their serialized representation. *)




(* Idea: keep pointers to values that we've serialized in a global
   weak hash table so that we can share structure with them if we
   deserialize any equal values in the same process *)
