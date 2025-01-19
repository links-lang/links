type t =
  | ConcreteLens of Sort.t  (** A statically fully checked lens. *)
  | AbstractLens of { checked : bool; sort : Sort.t }
      (** The type of a lens where not all checks have been performed.
          The predicate value is a dummy value (None) and not the actual predicate. *)
[@@deriving show]

(** Get the sort of the lens. *)
val sort : t -> Sort.t

val equal : t -> t -> bool

val type_lens_fun_dep :
  fds:(string list * string list) list ->
  columns:Column.List.t ->
  (t, Sort.Lens_sort_error.t) result

(** Change the specified columns to be of serial type. *)
val set_serial : t -> columns:Alias.Set.t -> t

module Select_lens_error : sig
  type 'a t =
    | SortError of Sort.Select_sort_error.t
    | PredicateTypeError of 'a Phrase_typesugar.error
    | PredicateNotBoolean of Phrase_type.t
end

val type_select_lens :
  t -> predicate:'a Phrase_sugar.phrase -> (t, 'a Select_lens_error.t) result

val type_select_lens_dynamic : t -> (t, 'a Select_lens_error.t) result

module Drop_lens_error : sig
  type t = Sort.Drop_sort_error.t
end

val type_drop_lens :
  t ->
  drop:Alias.t list ->
  default:Phrase_type.t list ->
  key:Alias.Set.t ->
  (t, Drop_lens_error.t) result

module Join_lens_error : sig
  type lens = Left | Right

  type 'a t =
    | PredicateTypeError of lens * 'a Phrase_typesugar.error
    | PredicateNotBoolean of lens * Phrase_type.t
    | SortError of Sort.Join_sort_error.t
end

val type_join_lens :
  t ->
  t ->
  on:(string * string * string) list ->
  del_left:'a Phrase_sugar.phrase ->
  del_right:'a Phrase_sugar.phrase ->
  (t, 'a Join_lens_error.t) result

val record_type : t -> Phrase_type.t

module Unchecked_lens_error : sig
  type t = UncheckedLens
end

(** Returns true if the lens is an abstract lens. *)
val is_abstract : t -> bool

(** Returns true if an abstract lens has been checked or the lens is a concrete lens. *)
val is_checked : t -> bool

(** Ensures that an abstract lens type has explicitly been checked. *)
val ensure_checked : t -> (unit, Unchecked_lens_error.t) result

(** Set an abstract lens as explicitly checked.*)
val make_checked : t -> t
