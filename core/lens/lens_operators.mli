open Lens_format

module Unary : sig
  type t = Minus | Not [@@deriving show]

  val of_sugartype_op : Operators.UnaryOp.t -> t option

  val to_string : t -> string

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

  val of_sugartype_op : Operators.BinaryOp.t -> t option

  val to_string : t -> string

  val fmt : t fmt_fn
end
