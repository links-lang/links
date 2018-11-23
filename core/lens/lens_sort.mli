open Types

type t


(** Get the functional dependencies *)
val fds : t -> Lens_fun_dep.Set.t

(** Get the predicate which defines all valid possible tuples *)
val predicate : t -> lens_phrase option

(** Get the list of columns belonging to the lens sort *)
val cols : t -> Lens_column.List.t

(** Gets a list of the aliases of all present columns *)
val cols_present_aliases : t -> string list

(** Construct a lens sort *)
val make : ?fds:Lens_fun_dep.Set.t -> ?predicate:Types.lens_phrase option -> Types.lens_col list -> t

val find_col_alias : t -> alias:string -> Types.lens_col

val find_col_alias_opt : t -> alias:string -> Types.lens_col option

val update_predicate : t -> predicate:lens_phrase option -> t
