(** Environments. *)

module type S = sig
  type name
  (** The type of names. *)

  type 'a t
    [@@deriving show]
  (** The type of environments. *)

  val empty : 'a t
  (** The empty environment. *)

  val singleton : name -> 'a -> 'a t
  (** Create an environment with a single entry. *)

  val bind : name -> 'a -> 'a t -> 'a t
  (** Extend an environment with a new entry. *)

  val unbind : name -> 'a t -> 'a t
  (** Remove an entry from an environment. *)

  val extend : 'a t -> 'a t -> 'a t
  (** Extend an environment with another.  Bindings from the right
      shadow bindings from the left. *)

  val has : name -> 'a t -> bool
  (** Whether a particular name is in an environment *)

  val find : name -> 'a t -> 'a
  (** Look up a name in an environment.  Raise [NotFound name] if the
     name is not present. *)

  val find_opt : name -> 'a t -> 'a option
  (** Look up a name in an environment.  Return [None] if the name
      is not present. *)

  module Dom : Utility.Set.S
  (** Sets of names. *)

  val domain : 'a t -> Dom.t
  (** The domain of an environment *)

  val range : 'a t -> 'a list
  (** The range of an environment *)

  val bindings : 'a t -> (name * 'a) list

  val map : ('a -> 'b) -> 'a t -> 'b t

  val iter : (name -> 'a -> unit) -> 'a t -> unit

  val fold : (name -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val filter : (name -> 'a -> bool) -> 'a t -> 'a t

  val filter_map : (name -> 'a -> 'b option) -> 'a t -> 'b t

  val complement : 'a t -> 'a t -> 'a t
  (** Computes the relative complement B \ A *)
end
(** Output signature of the functor {!Env.Make}. *)

module Make (Ord : Utility.OrderedShow)
: S with type name = Ord.t
    and module Dom = Utility.Set.Make(Ord)
(** Functor building an implementation of the env structure given a
    totally-ordered, showable type. *)

(** Some pre-built environments with common key types *)

module String : S
  with type name = string
  and module Dom = Utility.Set.Make(Utility.String)
  and module Dom = Utility.StringSet
(** Pre-built environment with strings for names *)

module Int : S
  with type name = int
  and module Dom = Utility.Set.Make(Utility.Int)
  and module Dom = Utility.IntSet
(** Pre-built environment with integers for names *)
