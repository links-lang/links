[@@@ocamlformat "doc-comments=before"]

type t
  [@@deriving show]

(** Get the functional dependencies *)
val fds : t -> Fun_dep.Set.t

(** Get the predicate which defines all valid possible tuples *)
val predicate : t -> Phrase.t option

(** Get the list of columns belonging to the lens sort *)
val cols : t -> Column.List.t

(** Gets a list of the aliases of all present columns *)
val cols_present_aliases : t -> string list

(** Get the columns as a set *)
val colset : t -> Column.Set.t

(** Get a set of the present columns *)
val present_colset : t -> Column.Set.t

(** Construct a lens sort *)
val make :
     ?fds:Fun_dep.Set.t
  -> ?predicate:Phrase.t option
  -> Column.t list
  -> t

(** Find the column with the specified alias *)
val find_col_alias : t -> alias:string -> Column.t option

(** Replace the predicate *)
val update_predicate : t -> predicate:Phrase.t option -> t

(** Update all columns with a table name *)
val update_table_name : t -> table:string -> t

(** Determines if the lenses should be swapped, because the right lens defines the left lens. *)
val join_lens_should_swap : t -> t -> on:string list -> bool

(** Create a selection lens using the specified predicate to filter records. *)
val select_lens_sort : t -> predicate:Phrase.t -> t

(** Create a drop lens sort. *)
val drop_lens_sort : t -> drop:Alias.Set.t -> key:Alias.Set.t -> t

(** Create a sort as the join of two other sorts on the columns specified by [on] *)
val join_lens_sort :
  t -> t -> on:string list -> t * (string * string * string) list

(** Convert the sort into a phrase type. *)
val record_type : t -> Phrase_type.t

val equal : t -> t -> bool
