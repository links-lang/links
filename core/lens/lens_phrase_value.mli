
type t =
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Tuple of t list
  | Record of (string * t) list
[@@deriving show]

val equal : t -> t -> bool

val box_bool : bool -> t

val unbox_bool : t -> bool

val box_int : int -> t

val unbox_int : t -> int

val box_float : float -> t

val unbox_float : t -> float

val box_string : string -> t

val unbox_string : t -> string

val box_tuple : t list -> t

val unbox_tuple : t -> t list

val box_record : (string * t) list -> t

val unbox_record : t -> (string * t) list

module Record : sig
  (** Get a record values field [key]. Returns [None] if the field is not found. *)
  val get : t -> key:string -> t option

  (** Get a record values field [key]. Throw an exception if the field is not found. *)
  val get_exn : t -> key:string -> t

  (** Set a record values [field] to [value]. *)
  val set : t -> key:string -> value:t -> t

  (** Determine if two records have the same values for the fields specified in [on]. *)
  val match_on : t -> t -> on:string list -> bool
end

