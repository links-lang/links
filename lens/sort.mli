type t [@@deriving show, sexp]

(** Get the functional dependencies *)
val fds : t -> Fun_dep.Set.t

(** Get the predicate which defines all valid possible tuples *)
val predicate : t -> Phrase.t option

(** Get the predicate which is used for querying the database. *)
val query : t -> Phrase.t option

(** Get the list of columns belonging to the lens sort *)
val cols : t -> Column.List.t

(** Gets a list of the aliases of all present columns *)
val cols_present_aliases : t -> string list

(** Get a list of present aliases as a set. *)
val cols_present_aliases_set : t -> Alias.Set.t

(** Get the columns as a set *)
val colset : t -> Column.Set.t

(** Get a set of the present columns *)
val present_colset : t -> Column.Set.t

(** Construct a lens sort *)
val make :
  ?fds:Fun_dep.Set.t ->
  ?predicate:Phrase.t option ->
  ?query:Phrase.t option ->
  Column.t list ->
  t

(** Find the column with the specified alias *)
val find_col_alias : t -> alias:string -> Column.t option

(** Replace the predicate *)
val update_predicate :
  t -> query:Phrase.t option -> predicate:Phrase.t option -> t

(** Update all columns with a table name *)
val update_table_name : t -> table:string -> t

(** Change the specified columns to be of serial type. *)
val set_serial : t -> columns:Alias.Set.t -> t

(** Determines if the lenses should be swapped, because the right lens defines
    the left lens. *)
val join_lens_should_swap : t -> t -> on:string list -> bool

module Lens_sort_error : sig
  type t = UnboundColumns of Alias.Set.t [@@deriving eq]
end

val lens_sort :
  fds:Fun_dep.Set.t -> columns:Column.List.t -> (t, Lens_sort_error.t) result

module Select_sort_error : sig
  type t =
    | PredicateDoesntIgnoreOutputs of {
        fds : Fun_dep.Set.t;
        columns : Alias.Set.t;
      }
        (** The underlying lens predicate doesn't ignore the outputs of the
            functional dependencies. *)
    | TreeFormError of { error : Fun_dep.Tree.Tree_form_error.t }
        (** The functional dependencies are not in tree form. *)
    | UnboundColumns of Alias.Set.t
        (** The specified columns are not bound by the lens. *)
  [@@deriving show]

  val equal : t -> t -> bool
end

(** Create a selection lens sort without checking the predicate. *)
val select_lens_sort_dynamic : t -> (t, Select_sort_error.t) result

(** Create a selection lens using the specified predicate to filter records. *)
val select_lens_sort :
  t -> predicate:Phrase.t -> (t, Select_sort_error.t) result

module Drop_sort_error : sig
  type t =
    | UnboundColumns of Alias.Set.t
        (** The columns could not be found in the lens sort. *)
    | DefiningFDNotFound of Alias.Set.t
        (** A functional dependency X -> Y with Y = drop could not be found. *)
    | DropNotDefinedByKey
        (** A functional dependency could not be found which
          defines the columns to be dropped by the key.*)
    | DefaultDropMismatch
        (** The number of columns to drop does not match the number
          of default values specified. *)
    | DefaultDoesntMatchPredicate
        (** The default value a for A does not satisfy P[A]. *)
    | DropTypeError of {
        column : Alias.t;
        default_type : Phrase_type.t;
        column_type : Phrase_type.t;
      }
        (** The type of [column] is [column_type] does not match the
          type [default_type] of the default value. *)
    | NotIndependent of Alias.Set.t
        (** It is not possible to tell that P = P[A] join P[U-A]. *)
  [@@deriving show]

  val equal : t -> t -> bool
end

module Join_sort_error : sig
  type t =
    | UnboundColumn of Alias.Set.t
    | AlreadyBound of Alias.Set.t
    | TreeFormError of { error : Fun_dep.Tree.Tree_form_error.t }
  [@@deriving eq]
end

(** Create a drop lens sort. *)
val drop_lens_sort :
  t ->
  drop:Alias.t list ->
  default:Phrase.Value.t list ->
  key:Alias.Set.t ->
  (t, Drop_sort_error.t) result

(** Create a sort as the join of two other sorts on the columns specified by [on] *)
val join_lens_sort :
  t ->
  t ->
  on:(string * string * string) list ->
  (t * (string * string * string) list, Join_sort_error.t) result

(** Convert the sort into a phrase type. *)
val record_type : t -> Phrase_type.t

val equal : t -> t -> bool
