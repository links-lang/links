open Lens_format

module Operator_not_found_exception : sig
  type t

  val to_string : t -> string

  val get_op : t -> string
end

module Unary : sig
  type t = Minus | Not [@@deriving show, sexp]

  val to_string : t -> string

  val of_string : string -> (t, Operator_not_found_exception.t) result

  (** Do not use. *)
  val of_string_exn : string -> t

  val fmt : t fmt_fn
end

module Binary : sig
  type t =
    | Plus
    | Minus
    | Multiply
    | Divide
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    | Equal
    | LogicalAnd
    | LogicalOr
  [@@deriving show, sexp]

  val to_string : t -> string

  val of_string : string -> (t, Operator_not_found_exception.t) result

  (** Do not use. *)
  val of_string_exn : string -> t

  val fmt : t fmt_fn
end
