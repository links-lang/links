include module type of List

module Seq = Lens_seq

val mem : 'a list -> 'a -> equal:('a -> 'a -> bool) -> bool

val find : 'a list -> f:('a -> bool) -> 'a option

val find_exn : 'a list -> f:('a -> bool) -> 'a

val for_all2_exn : 'a list -> 'b list -> f:('a -> 'b -> bool) -> bool

val for_all : 'a list -> f:('a -> bool) -> bool

val for_all_or_error : 'a list -> f:('a -> bool) -> error:('a -> 'b) -> (unit, 'b) result

val map : 'a list -> f:('a -> 'b) -> 'b list

val map_if : 'a list -> b:('a -> bool) -> f:('a -> 'a) -> 'a list

val filter_opt : 'a option list -> 'a list

val filter_map : 'a list -> f:('a -> 'b option) -> 'b list

val unzip3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list

(** Take up to the first [n] elements of a list. *)
val take : 'a list -> n:int -> 'a list

(** Remove the first [n] elements of a list. *)
val drop : 'a list -> n:int -> 'a list

(** Like [zip], but in the case of length mismatch it zips as many
    elements as possible and then ends. *)
val zip_nofail : 'a list -> 'b list -> ('a * 'b) list

(** Turn a list into a lazily evaluated sequence. This is for ocaml 4.06 compatibility. *)
val to_seq : 'a list -> 'a Seq.t

(** Create a tuple using the elements of two lists. Requires lists to be of equal length or fail. *)
val zip_exn : 'a list -> 'b list -> ('a * 'b) list
