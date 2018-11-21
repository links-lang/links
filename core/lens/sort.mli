type t

(** Get the functional dependencies *)
val fds : t -> LensUtility.fundepset

(** Get the predicate which defines all valid possible tuples *)
val predicate : t -> lens_phrase option

(** Get the list of columns belonging to the lens sort *)
val cols : t -> Column.t list

(** Gets a list of the aliases of all present columns *)
val cols_present_aliases : t -> string list

