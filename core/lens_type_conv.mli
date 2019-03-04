
(** Convert a native Links language type to a lens phrase type. *)
val type_of_lens_phrase_type : Lens_phrase_type.t -> Types.typ

(** Convert a lens phrase type to a native Links type. *)
val lens_phrase_type_of_type : Types.typ -> Lens_phrase_type.t
