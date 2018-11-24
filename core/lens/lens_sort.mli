open Types

type t = Types.lens_sort


(** Get the functional dependencies *)
val fds : t -> Lens_fun_dep.Set.t

(** Get the predicate which defines all valid possible tuples *)
val predicate : t -> lens_phrase option

(** Get the list of columns belonging to the lens sort *)
val cols : t -> Lens_column.List.t

(** Gets a list of the aliases of all present columns *)
val cols_present_aliases : t -> string list

(** Get the columns as a set *)
val colset : t -> Lens_column.Set.t

(** Get a set of the present columns *)
val present_colset : t -> Lens_column.Set.t

(** Construct a lens sort *)
val make : ?fds:Lens_fun_dep.Set.t -> ?predicate:Types.lens_phrase option -> Types.lens_col list -> t

(** Find the column with the specified alias *)
val find_col_alias : t -> alias:string -> Types.lens_col option

(** Replace the predicate *)
val update_predicate : t -> predicate:lens_phrase option -> t

(** Update all columns with a table name *)
val update_table_name : t -> table:string -> t

(** Get the record type *)
val record_type : t -> Types.typ
