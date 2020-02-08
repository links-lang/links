type 'a t = 'a option

val iter : 'a t -> f:('a -> unit) -> unit

val bind : 'a t -> f:('a -> 'b t) -> 'b t

val map : 'a t -> f:('a -> 'b) -> 'b t

val map2 : 'a option -> 'b option -> f:('a -> 'b -> 'c) -> 'c option

(** If only one of the inputs has a value, that value is returned. Otherwise
    the result of [f] applied to the underlying values is returned. *)
val combine : 'a t -> 'a t -> f:('a -> 'a -> 'a) -> 'a option

(** Get the underlying value of an option type or return [default] *)
val value : 'a t -> default:'a -> 'a

(** Unpack the option value or throw an exception if it is [None]. *)
val value_exn : ?exn:exn -> 'a t -> 'a

(** Return a constant value as a [Some]. *)
val return : 'a -> 'a t

(** Return true if the option value contains a value. *)
val is_some : 'a t -> bool

(** Return true if the option value is none. *)
val is_none : 'a t -> bool

val negate : 'b option -> value:'a -> 'a option

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
