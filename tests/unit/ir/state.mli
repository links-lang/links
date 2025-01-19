type ('a, 'st) t

val return : 'a -> ('a, 'st) t

val apply : ('a -> 'b, 'st) t -> ('a, 'st) t -> ('b, 'st) t

val map : ('a -> 'b) -> ('a, 'st) t -> ('b, 'st) t

val bind : ('a -> ('b, 'st) t) -> ('a, 'st) t -> ('b, 'st) t

val product : ('a, 'st) t -> ('b, 'st) t -> ('a * 'b, 'st) t

val run_state : init:'st -> ('a, 'st) t -> 'a * 'st

val state_context :
  ('st -> 'st') -> ('st -> 'st' -> 'st) -> ('a, 'st') t -> ('a, 'st) t

val map_state : ('st -> 'st) -> (unit, 'st) t

val get : ('st, 'st) t

val put : 'st -> (unit, 'st) t

val push_context : 'st' -> ('a, 'st' * 'st) t -> ('a, 'st) t

val pop_context : ('a, 'st) t -> ('a, 'st * 'st') t

val skip_context : ('a, 'st') t -> ('a, 'st * 'st') t

val for_loop : f:(int -> (unit, 'st) t) -> int -> int -> (unit, 'st) t

module Hack : sig
  (* allows the use of state in impure contexts *)
  val in_hack : ((('a, 'st) t -> 'a) -> 'b) -> ('b, 'st) t
end

module List : sig
  val iter : f:('a -> (unit, 'st) t) -> 'a list -> (unit, 'st) t

  val fold :
    init:'acc -> f:('acc -> 'a -> ('acc, 'st) t) -> 'a list -> ('acc, 'st) t

  val map : f:('a -> ('b, 'st) t) -> 'a list -> ('b list, 'st) t

  val lift : ('a, 'st) t list -> ('a list, 'st) t

  val filter : f:('a -> (bool, 'st) t) -> 'a list -> ('a list, 'st) t

  module Result : sig
    val iter :
      f:('a -> ((unit, 'e) result, 'st) t) ->
      'a list ->
      ((unit, 'e) result, 'st) t
  end
end

module Option : sig
  val bind : f:('a -> ('b option, 'st) t) -> 'a option -> ('b option, 'st) t

  val merge :
    f:('a -> 'a -> ('a option, 'st) t) ->
    'a option ->
    'a option ->
    ('a option, 'st) t
end

module Result : sig
  val map : f:('a -> 'b) -> (('a, 'e) result, 'st) t -> (('b, 'e) result, 'st) t

  val bind :
    f:('a -> (('b, 'e) result, 'st) t) ->
    (('a, 'e) result, 'st) t ->
    (('b, 'e) result, 'st) t

  val try_revert : (('a, 'e) result, 'st) t -> (('a, 'e) result, 'st) t

  module Syntax : sig
    (** Map syntax. *)
    val ( let$+ ) :
      (('a, 'e) result, 'st) t -> ('a -> 'b) -> (('b, 'e) result, 'st) t

    (** Bind syntax. *)
    val ( let$* ) :
      (('a, 'e) result, 'st) t ->
      ('a -> (('b, 'e) result, 'st) t) ->
      (('b, 'e) result, 'st) t

    val return_ok : 'a -> (('a, 'e) result, 'st) t
  end
end

module Syntax : sig
  val ( <*> ) : ('a -> 'b, 'st) t -> ('a, 'st) t -> ('b, 'st) t

  val ( let* ) : ('a, 'st) t -> ('a -> ('b, 'st) t) -> ('b, 'st) t

  val ( let+ ) : ('a, 'st) t -> ('a -> 'b) -> ('b, 'st) t

  val ( and+ ) : ('a, 'st) t -> ('b, 'st) t -> ('a * 'b, 'st) t

  val return : 'a -> ('a, 'st) t
end
