type t = Value.t

val sort : t -> Lens_sort.t

val is_memory_lens : t -> bool

val columns : t -> Lens_column.List.t

(** returns the aliases of all present columns. *)
val cols_present_aliases : t -> string list

val colset : t -> Lens_column.Set.t

val fundeps : t -> Lens_fun_dep.Set.t

val predicate : t -> Lens_phrase.t option
