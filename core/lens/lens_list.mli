open Utility

include module type of List

val filter_map : 'a list -> f:('a -> 'b option) -> 'b list

val unzip3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list

(** Take up to the first [n] elements of a list. *)
val take : 'a list -> n:int -> 'a list

(** Remove the first [n] elements of a list. *)
val skip : 'a list -> n:int -> 'a list

(** Like [zip], but in the case of length mismatch it zips as many
    elements as possible and then ends. *)
val zip_nofail : 'a list -> 'b list -> ('a * 'b) list
