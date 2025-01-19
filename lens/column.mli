type t [@@deriving show, sexp]

val make :
  table:string ->
  name:string ->
  alias:string ->
  typ:Phrase_type.t ->
  present:bool ->
  t

(** Return the name of the column as the column would be bound in the user program. *)
val alias : t -> string

(** Return the name of the column corresponding to the name in the database table. *)
val name : t -> string

(** Return the name of the database table. *)
val table : t -> string

(** Return the column type. *)
val typ : t -> Phrase_type.t

(** Change a column type. *)
val set_typ : typ:Phrase_type.t -> t -> t

(** Determine if the column is present. *)
val present : t -> bool

val hide : t -> t

val rename : t -> alias:string -> t

val equal : t -> t -> bool

val set_table : t -> table:string -> t

module Set : sig
  include Lens_set.S with type elt = t

  (** Generate a dummy column with a given alias. *)
  val dummy_alias : string -> elt

  (** Determine if there is a column with the specified alias. *)
  val mem_alias : t -> alias:string -> bool

  (** Convert to an alias set. *)
  val alias_set : t -> Alias.Set.t

  (** Find the column with the given alias. *)
  val find_alias : t -> alias:string -> elt

  (** Find the column with the given alias and return [None] if it is not found. *)
  val find_alias_opt : t -> alias:string -> elt option
end

module List : sig
  type elt = t [@@deriving eq]

  type t = elt list [@@deriving eq]

  (** Filter out all non present columns. *)
  val present : t -> t

  (** Get a list of column aliases. *)
  val aliases : t -> string list

  (** Get the aliases of all present columns. *)
  val present_aliases : t -> string list

  (** Return the set of all present aliases. *)
  val present_aliases_set : t -> Alias.Set.t

  (** determine if the calumn [alias] is present. *)
  val mem_alias : t -> alias:string -> bool

  (** Convert the list of columns into a set. *)
  val colset : t -> Set.t

  (** Convert the list of columns into a map from the alias to the column. *)
  val colmap : t -> elt Alias.Map.t

  (** Find the column with [alias]. *)
  val find_alias : t -> alias:string -> elt option

  (** Convert a list of records to a Links type. *)
  val record_type : t -> Phrase_type.t
end
