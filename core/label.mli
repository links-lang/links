val show_unique_labels_idents : bool Settings.setting

module Uid : sig
  type t = Id of Int.t | Free
end

type local = string * Uid.t
type global = string

type t =
    | Local of local
    | Global of global
    | Number of int
   [@@deriving show]
type label = t

val make_local : ?uid:Uid.t -> string -> t
val make_global : string -> t
val make : string -> t

val of_int : Int.t -> t
val to_int : t -> Int.t
val name : t -> string

val compare : t -> t -> int
val equal : t -> t -> bool
val textual_equal : t -> t -> bool
val name_is : t -> string -> bool

val is_local : t -> bool
val is_global : t -> bool
val is_free : t -> bool

val uid : t -> Uid.t
val bind_local : ?bind_with:t -> t -> t


val one : t
val two : t
val return : t


module type LABELMAP = Utility.Map with type key = t
module Map : LABELMAP

module type LABELSET = Utility.Set with type elt = t
module Set : LABELSET

val string_to_label_map : 'a Utility.StringMap.t -> 'a Map.t
val label_to_string_map : 'a Map.t -> 'a Utility.StringMap.t
val string_to_label_set : Utility.StringSet.t -> Set.t
val label_to_string_set : Set.t -> Utility.StringSet.t

module Env : sig
    type t

    val pp : Format.formatter -> t -> unit

    val empty : t

    val extend : t -> t -> t

    val bind : t -> label -> t
    val unbind : t -> label -> t

    val bind_labels : label list -> t -> t
    val unbind_labels : label list -> t -> t

    val find_homonyms : label -> t -> label list
end
