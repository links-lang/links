type t = Types.lens_col

(** Return the name of the column as it would be accessible in links. *)
val alias : t -> string

(** Return the name of the column correspending to the name in the database table. *)
val name : t -> string

(** Return the name of the database table. *)
val table : t -> string

(** Return the column type. *)
val typ : t -> string

(** Determine if the column is present. *)
val present : t -> bool

val hide : t -> t

val rename : t -> alias:name -> t

val equal : t -> t -> bool



