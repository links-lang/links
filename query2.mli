type 'a name_map = 'a Utility.stringmap
type name_set = Utility.stringset

(** ugly hack to store the database used in the query block *)
val used_database : Value.database option ref

(** This module infers the implementation type of expressions and annotates the expression 
    tree with it *)
module Annotate : sig
  type implementation_type = [ `Atom | `List ]

  (** expression tree, annotated with the implementation type on every node *)
  type typed_t =
    [ `For of (typed_t * typed_t list * typed_t) * implementation_type
    | `Lambda of ((Var.var list * typed_t) * implementation_type)
(*    | `GroupBy of ((Var.var * typed_t) * typed_t) * implementation_type *)
    | `If of (typed_t * typed_t * typed_t option) * implementation_type
    | `Table of Value.table * implementation_type
    | `Singleton of typed_t * implementation_type 
    | `Append of typed_t list * implementation_type
    | `Record of typed_t name_map * implementation_type
    | `Project of (typed_t * string) * implementation_type
    | `Erase of (typed_t * name_set) * implementation_type
    | `Extend of (typed_t option * typed_t name_map) * implementation_type
    | `Variant of (string * typed_t) * implementation_type
    | `XML of Value.xmlitem * implementation_type
    | `Apply of (string * typed_t list) * implementation_type
    | `Primitive of string * implementation_type
    | `Var of Var.var * implementation_type
    | `Constant of Constant.constant * implementation_type
    | `Box of typed_t * implementation_type
    | `Unbox of typed_t * implementation_type
    | `Case of (typed_t * (Var.var * typed_t) name_map * (Var.var * typed_t) option) * implementation_type
    | `Wrong of implementation_type ]
      
  val typeof_typed_t : typed_t -> implementation_type
end

type range = NoRange | Value of Value.t * Value.t | Ir of Ir.value * Ir.value

(** compile a IR tree together with the local environment to an expression tree 
    annotated with the implementation type *)
val compile : Value.env -> range -> Ir.computation -> (Annotate.typed_t * Annotate.implementation_type)
