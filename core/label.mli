open CommonTypes

module Uid : sig
  type t = Id of Int.t | Free
end

type local = Name.t * Uid.t
type global = Name.t
(* type unresolved = Name.t *)

type t =
        | Lcl of local
        | Gbl of global
        (* | Unr of unresolved *)
(* type t = string *)
      [@@deriving show]

val mk_local : Name.t -> t
val mk_global : Name.t -> t
(* val mk_unresolved : Name.t -> t *)
val make : ?local:bool -> Name.t -> t

val mk_int : Int.t -> t
val to_int : t -> Int.t
val name : t -> Name.t

val compare : t -> t -> int
val eq : t -> t -> bool
val eq_name : t -> Name.t -> bool

val is_local : t -> bool
val is_global : t -> bool
val is_free : t -> bool
(* val is_resolved : t -> bool *)

val uid : t -> Uid.t
val bind_local : ?bind_with:t -> t -> t


val one : t
val two : t
val return : t

(* val resolve_local : t -> t *)
(* val resolve_global : t -> t *)


module type LABELMAP = Utility.Map with type key = t
module Map : LABELMAP

module type LABELSET = Utility.Set with type elt = t
module Set : LABELSET

val string_to_label_map : 'a Utility.StringMap.t -> 'a Map.t
val label_to_string_map : 'a Map.t -> 'a Utility.StringMap.t
val string_to_label_set : Utility.StringSet.t -> Set.t
val label_to_string_set : Set.t -> Utility.StringSet.t
