(** This module provides an efficient method of performing calculations on delta sets. This is done by storing
    the records in a sorted order and by keeping track of the columns of the whole set of records rather than in
    each record itself. *)

open Lens_utility

module Simple_record : sig
  type t = Value.t list

  val compare_val: Value.t -> Value.t -> int

  val compare :  t -> t -> int

  (** Find to find the index of [record] in an array of records. *)
  val find_index : t array -> record:t -> int option

  (** Find the actual record in an array of records which partially matches on [record]. *)
  val find_record : t array -> record:t -> t option

  (** Find the index range of records matching [record]. *)
  val find_all_index : t array -> record:t -> int * int

  (** Get the array subset of records matching [record]. *)
  val find_all_record : t array -> record:t -> t array
end

module Inconsistent_columns_error : sig
  exception E of string list * string list
end

type t

val construct : records:Value.t -> t

val construct_cols : cols:string list -> records:Value.t -> t

val columns : t -> string list

(** The record set does not contain negative entries. *)
val is_positive : t -> bool

(** The total number of records in the set. *)
val total_size : t -> int

val pp : t Format.fmt_fn

(** Print the set in tabular form. *)
val pp_tabular : t Format.fmt_fn

(** Returns true if the record set contains [record] as a positive row. *)
val find : t -> record:Simple_record.t -> bool

(** Project the record set onto the given list of column names. *)
val project_onto : t -> columns:string list -> t

(** Project the record set onto the columns of [onto]. *)
val project_onto_set : t -> onto:t -> t

(** Filter out all records that don't match predicate. *)
val filter : t -> predicate:Lens_phrase.t -> t

(** Perform a delta merge (\oplus) between two record sets. *)
val merge : t -> t -> t

(** Perform a relational join on two sets. *)
val join : t -> t -> on:(string * string * string) list -> t

(** Convert a positive delta set into a value type. *)
val to_value : t -> Value.t

val project_fun_dep : t -> fun_dep:Lens_fun_dep.t -> (string list * string list) * (Value.t * Value.t) array
