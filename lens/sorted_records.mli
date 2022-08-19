(** This module provides an efficient method of performing calculations on delta sets. This is done by storing
    the records in a sorted order and by keeping track of the columns of the whole set of records rather than in
    each record itself. *)

open Lens_utility

module Simple_record : sig
  type t = Phrase_value.t list

  val compare_val : Phrase_value.t -> Phrase_value.t -> int

  val compare : t -> t -> int

  (** Find to find the index of [record] in an array of records. *)
  val find_index : t array -> record:t -> int option

  (** Find the actual record in an array of records which partially matches on [record]. *)
  val find_record : t array -> record:t -> t option

  (** Find the index range of records matching [record]. *)
  val find_all_index : t array -> record:t -> int * int

  (** Get the array subset of records matching [record]. *)
  val find_all_record : t array -> record:t -> t array

  val equal : t -> t -> bool
end

module Inconsistent_columns_error : sig
  exception E of string list * string list
end

type t

val construct : records:Phrase_value.t list -> t

val construct_cols : columns:string list -> records:Phrase_value.t list -> t

val construct_full :
  columns:string list ->
  plus:Simple_record.t list ->
  neg:Simple_record.t list ->
  t

val columns : t -> string list

val plus_rows : t -> Simple_record.t array

val neg_rows : t -> Simple_record.t array

val map_values : f:(Phrase_value.t -> Phrase_value.t) -> t -> t

(** The record set does not contain negative entries. *)
val is_positive : t -> bool

(** The total number of records in the set. *)
val total_size : t -> int

val pp : t Format.fmt_fn

(** Print the set in tabular form. *)
val pp_tabular : t Format.fmt_fn

(** Returns true if the record set contains [record] as a positive row. *)
val find : t -> record:Simple_record.t -> bool

(** Generate a function which maps a set of columns to a subset of those columns
    specified by [columns].

    Example:

    {[
      let sr = (* sorted records with columns A, B, C, D *) in
      let map = get_cols_map sr ~columns:["B"; "D"] in
      let proj = map [1; 2; 3; 4] in
      (* proj = [2; 4] *)
    ]} *)
val get_cols_map : t -> columns:string list -> 'a list -> 'a list

val sort : t -> unit

(** Project the record set onto the given list of column names. *)
val project_onto : t -> columns:string list -> t

(** Project the record set onto the columns of [onto]. *)
val project_onto_set : t -> onto:t -> t

(** Filter out all records that don't match predicate. *)
val filter : t -> predicate:Phrase.t -> t

(** Perform a delta merge (\oplus) between two record sets. *)
val merge : t -> t -> t

module Reorder_error : sig
  type t = Not_subset of { first : string list; cols : string list }

  val pp : t Format.fmt_fn
end

(** Reorder the columns, so that the columns [first] appear at the beginning. *)
val reorder : t -> first:string list -> (t, Reorder_error.t) result

val reorder_exn : t -> first:string list -> t

module Join_error : sig
  type elt = t
  type t =
    | Reorder_error of { error : Reorder_error.t; left : elt; right : elt }

  val pp : t Format.fmt_fn
end

(** Perform a relational join on two sets. [on] consists out of tuples with the order:
    - output name
    - left input name
    - right input name *)
val join :
  t -> t -> on:(string * string * string) list -> (t, Join_error.t) result

val join_exn : t -> t -> on:(string * string * string) list -> t

(** Swap positive and negative entries. *)
val negate : t -> t

(** Get only the negative subset of the record set. *)
val negative : t -> t

(** Subtract one delta set from another. Both sets must be positive only. *)
val minus : t -> t -> t

(** Convert a positive delta set into a value type. *)
val to_value : t -> Phrase_value.t list

val project_fun_dep :
  t ->
  fun_dep:Fun_dep.t ->
  (string list * string list)
  * (Simple_record.t * Simple_record.t) array
  * (Simple_record.t * Simple_record.t) array

type changelist =
  ((string list * string list) * (Simple_record.t * Simple_record.t) array) list

val calculate_fd_changelist : t -> fun_deps:Fun_dep.Set.t -> changelist

val pp_changelist_pretty : changelist Format.fmt_fn

val relational_update : t -> fun_deps:Fun_dep.Set.t -> update_with:t -> t

val relational_merge : t -> fun_deps:Fun_dep.Set.t -> update_with:t -> t

(** Extend the given set of sorted records by the column [by] and populate it using
    the relational data [data] containing the functional dependency [key] -> [by].
    Use the default value specified if no entry in [data] is found. *)
val relational_extend :
  t ->
  key:string ->
  by:string ->
  data:t ->
  default:Phrase_value.t ->
  (t, Reorder_error.t) result

val relational_extend_exn :
  t -> key:string -> by:string -> data:t -> default:Phrase_value.t -> t

(** Get all distinct values of both positive and negative records in this set in a sorted list. *)
val all_values : t -> Simple_record.t list

val to_diff :
  t ->
  key:string list ->
  ( string list
    * (Simple_record.t list * Simple_record.t list * Simple_record.t list),
    Reorder_error.t )
  result

val to_diff_exn :
  t ->
  key:string list ->
  string list
  * (Simple_record.t list * Simple_record.t list * Simple_record.t list)

val force_positive : t -> t
