open Types
open Sugartypes

type t = lens_phrase

(** Create a variable phrase *)
val var : string -> t

(** Create a logical and between two phrases *)
val and' : t -> t -> t

(** Create a logical or between two phrases *)
val or' : t -> t -> t

(** Create an equality comparison of two phrases *)
val equal : t -> t -> t

(** Logical not of a phrase *)
val not' : t -> t

(** Create a tuple of a list of phrases *)
val tuple : t list -> t

(** Create a tuple of a single phrase *)
val tuple_singleton : t -> t

(** Convert a links syntax phrase into a lens phrase. *)
val of_phrase : phrase -> t

(** Traverse a lens phrase, applying [dosth] to each nod and then replacing the result *)
val traverse : t -> (t -> t) -> t

(** Get a list of variables in the expression *)
val get_vars : t -> Utility.StringSet.t

(** Calculate the vale of an expression given a lookup function for variables. *)
val eval : t -> (string -> Value.t) -> Value.t

(** Rename all variables with an entry in the given map *)
val rename_var : t -> replace:string Lens_alias.Map.t -> t

(** Replace all variable nodes with nodes by the given map *)
val replace_var : t -> replace:Value.t Lens_alias.Map.t -> t

module Constant : sig

  (** Create a constant bool phrase *)
  val bool : bool -> t

  (** Create a constant int phrase *)
  val int : int -> t

  (** Create a constant of a links value *)
  val of_value : Value.t -> t
end

module Option : sig
  type elt = t
  type t = elt option

  (** Combine two option phrases with a logical and *)
  val combine_and : t -> t -> t

  (** Combine two option phrases with a logical or *)
  val combine_or : t -> t -> t

  (** Construct an in expression phrase option  *)
  val in_expr : string list -> Value.t list list -> t
end

module List : sig
  type elt = t
  type t = elt list

  (** Fold a list to an option phrase *)
  val fold_and : t -> Option.t

  (** Fold a list of option phrases to an option phrase combining with ands *)
  val fold_and_opt : Option.t list -> Option.t

  (** Fold a list to an option phrase *)
  val fold_or : t -> Option.t

  (** Fold a list of option phrases to an option phrase combining with ands *)
  val fold_or_opt : Option.t list -> Option.t
end

module Record : sig
  type record = Value.t

  val matching_cols_simp : Lens_alias.t list -> Value.t list -> Option.t

  val matching_cols : Lens_alias.Set.t -> Value.t -> Option.t

  (** Evaluate the phrase with a given record *)
  val eval : t -> record -> record
end
