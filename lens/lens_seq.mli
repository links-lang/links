(** This is the lazy evaluated sequence module for OCaml 4.06 compatability. The signatures and comments are based on the standard OCaml library. *)

type 'a t = unit -> 'a node

and 'a node = Nil | Cons of 'a * 'a t

(** The empty sequence, containing no elements. *)
val empty : 'a t

(** The singleton sequence containing only the given element. *)
val return : 'a -> 'a t

(** [map f seq] returns a new sequence whose elements are the elements of [seq], transformed by [f]. This
    transformation is lazy, it only applies when the result is traversed. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Remove from the sequence the elements that do not satisfy the given predicate. This transformation is lazy, it
    only applies when the result is traversed. *)
val filter : ('a -> bool) -> 'a t -> 'a t

(** Apply the function to every element; if [f x = None] then [x] is dropped; if [f x = some y] then [y] is
    returned. This transformation is lazy, it only applies when the result is traversed. *)
val filter_map : ('a -> 'b option) -> 'a t -> 'b t

(** Traverse the sequence from left to right, combining each element with the accumulator using the given
    function. The traversal happens immediately and will not terminate on infinite sequences.

    Also see [List.fold_left] *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(** Iterate on the sequence, calling the (imperative) function on every element. The traversal happens
    immediately and will not terrminate on infinite sequences. *)
val iter : ('a -> unit) -> 'a t -> unit
