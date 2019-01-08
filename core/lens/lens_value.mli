type t = Value.t

val sort : t -> Lens_sort.t

val is_memory_lens : t -> bool

val columns : t -> Lens_column.List.t

(** returns the aliases of all present columns. *)
val cols_present_aliases : t -> string list

val colset : t -> Lens_column.Set.t

val fundeps : t -> Lens_fun_dep.Set.t

val predicate : t -> Lens_phrase.t option

val get_primary_key : t -> Lens_alias.Set.t

val generate_query : t -> Lens_database.Select.t

(** Fetch the records of a lens from the database. *)
val lens_get : t -> Value.t

(** Construct a select lens using the specified underlying lens and select predicate. *)
val lens_select : t -> predicate:Lens_phrase.t -> t

(** Generate a select lens from the specified lens and query its results. *)
val lens_get_select_opt : t -> predicate:Lens_phrase.t option -> Value.t

val query_exists : t -> Lens_phrase.t -> bool
