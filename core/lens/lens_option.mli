type 'a t = 'a option

val bind : 'a t -> f:('a -> 'b t) -> 'b t

val map : 'a t -> f:('a -> 'b) -> 'b t

val map2 : 'a option -> 'b option -> f:('a -> 'b -> 'c) -> 'c option

(** If only one of the inputs has a value, that value is returned. Otherwise
    the result of [f] applied to the underlying values is returned. *)
val combine : 'a t -> 'a t -> f:('a -> 'a -> 'a) -> 'a option

(** Get the underlying value of an option type or return [default] *)
val value : 'a t -> default:'a -> 'a
