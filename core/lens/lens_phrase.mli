open Types
open SourceCode
open Sugartypes

type t = lens_phrase
type node = lens_phrasenode

(** Get the underlying node without the source code position. *)
val node : t -> node

(** Create a variable phrase. *)
val var : ?pos:Position.t -> string -> t

(** Any infix operator application. *)
val infix : ?pos:Position.t -> Lens_operators.Binary.t -> t -> t -> t

(** Create a logical and between two phrases. *)
val and' : ?pos:Position.t -> t -> t -> t

(** Create a logical or between two phrases. *)
val or' : ?pos:Position.t -> t -> t -> t

(** Create an equality comparison of two phrases. *)
val equal : ?pos:Position.t -> t -> t -> t

(** Logical not of a phrase. *)
val not' : ?pos:Position.t -> t -> t

(** Create a tuple of a list of phrases. *)
val tuple : ?pos:Position.t -> t list -> t

(** Create a tuple of a single phrase. *)
val tuple_singleton : ?pos:Position.t -> t -> t

(** Convert a links syntax phrase into a lens phrase. *)
val of_phrase : phrase -> t

(** Traverse a lens phrase, applying [dosth] to each nod and then replacing the result. *)
val traverse : t -> f:(node -> node) -> t

(** Get a list of variables in the expression. *)
val get_vars : t -> Lens_alias.Set.t

(** Calculate the vale of an expression given a lookup function for variables. *)
val eval : t -> (string -> Value.t) -> Value.t

(** Rename all variables with an entry in the given map. *)
val rename_var : t -> replace:string Lens_alias.Map.t -> t

(** Replace all variable nodes with nodes by the given map. *)
val replace_var : t -> replace:Value.t Lens_alias.Map.t -> t

module Constant : sig

  (** Create a constant bool phrase *)
  val bool : ?pos:Position.t -> bool -> t

  (** Create a constant int phrase *)
  val int : ?pos:Position.t -> int -> t

  (** Create a constant of a links value *)
  val of_value : ?pos:Position.t -> Value.t -> t
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


(** This module contains operators and short hand constructors for producing phrase expressions.

  Example:

  {[
    let phrase =
      let open Lens_phrase.O in
      v "A" < i 20 && v "B" > i 45 in
    ...
  ]}

*)
module O : sig

  (** Greater than comparison. *)
  val (>) : t -> t -> t

  (** Less than comparison. *)
  val (<) : t -> t -> t

  (** Equality comparison. *)
  val (=) : t -> t -> t

  (** Logical and operator. *)
  val (&&) : t -> t -> t

  (** Logical or operator. *)
  val (||) : t -> t -> t

  (** Variable reference. *)
  val v : string -> t

  (** Integer constant phrase. *)
  val i : int -> t

  (** Boolean constant phrase. *)
  val b : bool -> t
end


(** This module is a simple algorithm for determining which variables affect each other during
    execution. *)
module Grouped_variables : sig
  include Lens_set.S with type elt = Lens_alias.Set.t

  (** Generate a grouped type variable value from a list of lists of column names.
      This is mainly useful for debugging. *)
  val of_lists : string list list -> t

  (** Calculate the grouped type variables from a phrase. *)
  val gtv : lens_phrase -> t

  (** Determine if the grouped type variables contains a group which
      only partially overlaps with the specified set of [cols].

      Example:

      When called on the set `{ A; A B; C D; }` with the cols `A B`, the
      result is false because `A B` only occurs in groups without further
      variables. If it is called with cols `C`, then it returns true, because
      the group `C D` contains the column `D` in addition to the column `C`. *)
  val has_partial_overlaps : t -> cols:elt -> bool
end
