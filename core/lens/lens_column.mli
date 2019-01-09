type t = Types.lens_col

(** Return the name of the column as it would be accessible in links. *)
val alias : t -> string

(** Return the name of the column correspending to the name in the database table. *)
val name : t -> string

(** Return the name of the database table. *)
val table : t -> string

(** Return the column type. *)
val typ : t -> Types.typ

(** Determine if the column is present. *)
val present : t -> bool

(** Get the table name *)
val table : t -> string

val hide : t -> t

val rename : t -> alias:string -> t

val equal : t -> t -> bool

module Set : sig
  include Lens_set.S with type elt = t

  (** Generate a dummy column with a given alias *)
  val dummy_alias : string -> elt

  (** Determine if there is a column with the specified alias *)
  val mem_alias : t -> alias:string -> bool

  (** Convert to an alias set. *)
  val alias_set : t -> Lens_alias.Set.t

  (** Find the column with the given alias. *)
  val find_alias : t -> alias:string -> elt

  (** Find the column with the given alias and return [None] if it is not found. *)
  val find_alias_opt : t -> alias:string -> elt option
end

module List : sig
  type elt = t
  type t = elt list

  (** filter out all non present columns **)
  val present : t -> t

  (** get a list of column aliases **)
  val aliases : t -> string list

  (** get the aliases of all present columns **)
  val present_aliases : t -> string list

  (** determine if the calumn [alias] is present *)
  val mem_alias : t -> alias:string -> bool

  (** Convert the list of columns into a set *)
  val colset : t -> Set.t

  (** Convetr the list of columns into a map from the alias to the column *)
  val colmap : t -> elt Lens_alias.Map.t

  (** find the column with [alias] *)
  val find_alias : t -> alias:string -> elt option

  (** Convert a list of records to a links type *)
  val record_type : t -> Types.typ
end

