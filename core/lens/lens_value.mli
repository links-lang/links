type t =
  | Lens of {table: Lens_database.Table.t; database: Lens_database.t; sort: Lens_sort.t}
  | LensMem of {records: Lens_phrase_value.t list; sort: Lens_sort.t}
  | LensSelect of {lens: t; predicate: Lens_phrase.t; sort: Lens_sort.t}
  | LensJoin of
      { left: t
      ; right: t
      ; on: (string * string * string) list
      ; del_left: Lens_phrase.t
      ; del_right: Lens_phrase.t
      ; sort: Lens_sort.t }
  | LensDrop of
      { lens: t
      ; drop: string
      ; key: string
      ; default: Lens_phrase_value.t
      ; sort: Lens_sort.t }
      [@@deriving show]

val string_of_value : t -> string

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
val lens_get : t -> Lens_phrase_value.t list

(** Construct a select lens using the specified underlying lens and select predicate. *)
val lens_select : t -> predicate:Lens_phrase.t -> t

(** Generate a select lens from the specified lens and query its results. *)
val lens_get_select_opt : t -> predicate:Lens_phrase.t option -> Lens_phrase_value.t list

val query_exists : t -> Lens_phrase.t -> bool
