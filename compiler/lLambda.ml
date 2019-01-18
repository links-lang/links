(*pp deriving *)
(** PRIMitive Elaborated IR **)

open Links_core

type identifier = { name : string
                  ; uid  : int
                  }


let string_of_identifier {name=name ; uid=uid} = name ^ (string_of_int uid)

type name = Ir.name


type 'a name_map = 'a Ir.name_map


type constant = Ir.constant


type llambda =
  [ (* values *)
    `Constant of constant
  | `Unit (* special constant *)
  | `Variable  of identifier
  | `Extend    of (llambda name_map * llambda option)
  | `Project   of (name * llambda)
  | `Inject    of (name * llambda * Types.datatype)
  | `Coerce    of (llambda * Types.datatype)
  | `Primitive of primitive
  (* tail computations *)
  | `Apply     of llambda * llambda list
  | `Case      of llambda * (identifier * llambda) name_map * (identifier * llambda) option
  | `If        of llambda * llambda * llambda
  (* bindings *)
  | `Sequence of llambda * llambda
  | `Let      of identifier * llambda * llambda
  | `Fun      of identifier list * llambda
  | `Rec      of (identifier * llambda) list * llambda
  (* specials *)
  | `Wrong
(*)  | `Handle      of llambda * clause name_map * clause * Ir.handler_spec*)
  | `DoOperation of llambda * llambda list
  ]
and primitive =
  [ `Nil
  | `Builtin    of name
  | `FnApply    of name * llambda list
  | `Print      of llambda list
  | `BinOp      of binary_operation * llambda list
  | `Global     of identifier * int * name * [`Get | `Set]
  | `SetOOId    of name * name
  ]
and binary_operation =
  [ `Plus
  | `Minus
  | `Mult
  | `Div
  | `Mod
  | `Lt
  | `Eq
  | `Gt
  | `Le
  | `Ge
  | `Neq
  | `And
  | `Or
  ] * Types.primitive option
and clause = [`Effect of identifier * int | `Exception of int | `Regular of int] * identifier * llambda


type program = llambda

