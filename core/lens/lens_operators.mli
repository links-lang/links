open Lens_format

module Unary : sig
  type t = Minus | Not [@@deriving show]

  val to_string : t -> string

  val of_string : string -> t

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
  [@@deriving show]

  val to_string : t -> string

  val of_string : string -> t

  val fmt : t fmt_fn
end
