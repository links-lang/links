(** This is the module for converting / handling links surface syntax to ensure compatability with
    the relational lenses module. *)

open SourceCode

module Error : sig
  type t = Internal_error of string [@@deriving show]
end

(** Convert a Links unary operator to a Relational Lenses unary operator. *)
val unary_of_sugartype_op : Operators.UnaryOp.t -> Lens.Operators.Unary.t option

(** Convert a Links binary operator to a Relational Lenses binary operator. *)
val binary_of_sugartype_op :
  Operators.BinaryOp.t -> Lens.Operators.Binary.t option

(** Extract a columns from a sugar syntax phrase. This is used in the join case where the user
    specifies the join columns in the Links syntax, which results in an expression of the form
    (a, b, c), which is parsed to a [TupleLit] of [Var] elements. *)
val cols_of_phrase : Sugartypes.phrase -> string list

(** Determine if the predicate should dynamically be checked or not. *)
val is_static : Lens.Phrase.Type.t -> Sugartypes.phrase -> bool

val lens_sugar_phrase_of_sugar :
  Sugartypes.phrase -> Position.t Lens.Phrase.Sugar.phrase
