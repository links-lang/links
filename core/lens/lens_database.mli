type t = Value.database

val fmt_col : db:t -> Format.formatter -> Lens_column.t -> unit

(** Format a pair of table and alias values *)
val fmt_table : db:t -> Format.formatter -> string * string -> unit

(** Format a list table and alias pairs *)
val fmt_tables : db:t -> Format.formatter -> (string * string) list -> unit

(** Format a list of columns *)
val fmt_cols : db:t -> Format.formatter -> Lens_column.t list -> unit

(** Format a phrase *)
val fmt_phrase : db:t -> map:(string -> string) -> Format.formatter -> Lens_phrase.t -> unit

(** Formats a phrase using a dummy db driver. This should only be used for debugging. *)
val fmt_phrase_dummy : Format.formatter -> Lens_phrase.t -> unit

(** Convert the phrase to a string using a dummy db driver. This should only be used for debugging. *)
val to_string_dummy : Lens_phrase.t -> string

module Select : sig
  type db = t

  type t = {
      tables : (string * string) list;
      cols : Lens_column.t list;
      predicate: Lens_phrase.Option.t;
      db : db;
  }

  (** Construct a select query from a lens sort. *)
  val of_sort : db -> sort:Lens_sort.t -> t

  val fmt : Format.formatter -> t -> unit
end
