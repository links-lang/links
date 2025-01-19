type t =
  | Lens of { table : Database.Table.t; sort : Sort.t }
  | LensMem of { records : Phrase_value.t list; sort : Sort.t }
  | LensSelect of { lens : t; predicate : Phrase.t; sort : Sort.t }
  | LensJoin of {
      left : t;
      right : t;
      on : (string * string * string) list;
      del_left : Phrase.t;
      del_right : Phrase.t;
      sort : Sort.t;
    }
  | LensDrop of {
      lens : t;
      drop : string;
      key : string;
      default : Phrase_value.t;
      sort : Sort.t;
    }
[@@deriving show, sexp]

(** Serialization using s-expression. *)
val serialize : t -> string

(** Deserialization using s-expression. *)
val deserialize : string -> t

val string_of_value : t -> string

val sort : t -> Sort.t

val is_memory_lens : t -> bool

val columns : t -> Column.List.t

(** returns the aliases of all present columns. *)
val cols_present_aliases : t -> string list

val colset : t -> Column.Set.t

val fundeps : t -> Fun_dep.Set.t

val predicate : t -> Phrase.t option

val get_primary_key : t -> Alias.Set.t

val generate_query : t -> Database.Select.t

(** Fetch the records of a lens from the database. *)
val lens_get : db:Database.t -> t -> Phrase_value.t list

(** Construct a select lens using the specified underlying lens and select
   predicate. This should not be used to construct lenses in general, as it may
   have a bad put direction. *)
val lens_select_internal : t -> predicate:Phrase.t -> t

(** Generate a select lens from the specified lens and query its results. *)
val lens_get_select_opt :
  db:Database.t -> t -> predicate:Phrase.t option -> Phrase_value.t list

val query_exists : db:Database.t -> t -> Phrase.t -> bool

(** Change the type of the specified columns to be of serial type. Only
   primitive lenses supported. *)
val set_serial : t -> columns:Alias.Set.t -> t
