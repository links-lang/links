type t =
  { driver_name: unit -> string
  ; escape_string: string -> string
  ; quote_field: string -> string
  ; execute: string -> unit
  ; execute_select:
         string
      -> field_types:(string * Phrase_type.t) list
      -> Phrase_value.t list }

module Table : sig
  type t = {name: string; keys: string list list}

  val name : t -> string
end

val fmt_col : db:t -> Format.formatter -> Column.t -> unit

(** Format a pair of table and alias values. *)
val fmt_table : db:t -> Format.formatter -> string * string -> unit

(** Format a list table and alias pairs. *)
val fmt_tables : db:t -> Format.formatter -> (string * string) list -> unit

(** Format a list of columns. *)
val fmt_cols : db:t -> Format.formatter -> Column.t list -> unit

(** Format a phrase. *)
val fmt_phrase :
  db:t -> map:(string -> string) -> Format.formatter -> Phrase.t -> unit

(** Formats a phrase using a dummy db driver. This should only be used for debugging. *)
val fmt_phrase_dummy : Format.formatter -> Phrase.t -> unit

(** Convert the phrase to a string using a dummy db driver. This should only be used for debugging. *)
val to_string_dummy : Phrase.t -> string

module Select : sig
  type db = t

  type t =
    { tables: (string * string) list
    ; cols: Column.t list
    ; predicate: Phrase.Option.t
    ; db: db }

  (** Add a further selection criterion to an existing predicate. *)
  val select : t -> predicate:Phrase.Option.t -> t

  (** Construct a select query from a lens sort. *)
  val of_sort : db -> sort:Sort.t -> t

  val fmt : Format.formatter -> t -> unit

  val execute :
       t
    -> database:db
    -> field_types:(string * Phrase_type.t) list
    -> Phrase_value.t list

  val query_exists : t -> database:db -> bool
end

module Delete : sig
  type db = t

  type t = {table: string; predicate: Phrase.Option.t; db: db}

  val fmt : Format.formatter -> t -> unit
end

module Update : sig
  type db = t

  type t =
    { table: string
    ; predicate: Phrase.Option.t
    ; set: (string * Phrase_value.t) list
    ; db: db }

  val fmt : Format.formatter -> t -> unit
end

module Insert : sig
  type db = t

  type t =
    { table: string
    ; columns: string list
    ; values: Phrase_value.t list list
    ; db: db }

  val fmt : Format.formatter -> t -> unit
end
