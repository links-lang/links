(** Environments. *)

type 'a t
(** The type of environments. *)

type name = string
(** The type of names. *)

val empty : 'a t
(** The empty environment. *)

val bind : name -> 'a -> 'a t -> 'a t
(** Bind a name in an environment *)

val has : 'a t -> name -> bool
(** Whether a particular name is in an environment *)

val find : name -> 'a t -> 'a
(** Look up a name in an environment.  Raise [NotFound name] if the
    name is not present. *)

val lookup : name -> 'a t -> 'a option
(** Look up a name in an environment.  Return [None] if the 
    name is not present. *)

val extend : 'a t -> 'a t -> 'a t
(** Extend an environment with another.  Bindings from the right
    shadow bindings from the left. *)

val domain : 'a t -> Utility.StringSet.t
(** The domain of an environment *)

val range : 'a t -> 'a list
(** The range of an environment *)

module Show_t (A : Show.Show) 
  : Show.Show with type a = A.a t
(** Printing for environments *)

module Pickle_t (A : Pickle.Pickle) 
  : Pickle.Pickle with type a = A.a t
(** Serialisation for environments *)

module Typeable_t (A : Typeable.Typeable) 
  : Typeable.Typeable with type a = A.a t
(** Dynamic typing for environments *)
