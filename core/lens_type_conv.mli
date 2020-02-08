type 'a die = string -> 'a

(** Lookup a type alias in the typing environment context. *)
val lookup_alias : Types.tycon_environment -> alias:string -> Types.typ

val lens_type_of_type : die:Lens.Type.t die -> Types.typ -> Lens.Type.t

(** Convert a native Links language type to a lens phrase type. *)
val type_of_lens_phrase_type :
  context:Types.tycon_environment -> Lens.Phrase.Type.t -> Types.typ

(** Convert a lens phrase type to a native Links type. *)
val lens_phrase_type_of_type : Types.typ -> Lens.Phrase.Type.t

(** Extract the relational lens columns of a table type. Use the specified table name. *)
val sort_cols_of_table : Types.typ -> table:string -> Lens.Column.t list
