(*pp deriving *)
(** PRIMitive Elaborated IR **)
module Types = Types_links

type identifier = { name : string
                  ; uid  : int
                  }
   deriving (Show)

let string_of_identifier {name=name ; uid=uid} = name ^ (string_of_int uid)

type name = Ir.name
   deriving (Show)
                                                         
type 'a name_map = 'a Ir.name_map
   deriving (Show)
   
(*type value =
  [ `Constant  of Ir.constant
  | `Variable  of identifier
  | `Extend    of (value name_map * value option)
  | `Project   of (name * value)
  | `Inject    of (name * value * Types.datatype)
  | `Coerce    of (value * Types.datatype)
  | `Primitive of identifier * Types.datatype option
  ]
and tail_computation =
  [ `Return  of value
  | `Apply   of value * value list
  | `Special of special
  | `Case    of (value * (identifier * computation) name_map * (identifier * computation) option)
  | `If      of value * computation * computation
  ]    
and fun_def = identifier * (identifier list * computation)
and binding =
  [ `Let of identifier * tail_computation
  | `Fun of fun_def
  | `Rec of fun_def list
  ]
and special =
  [ `Wrong       of Types.datatype
  | `Handle      of (value * clause name_map * Ir.handler_spec)
  | `DoOperation of (name * value list * Types.datatype)
  ]
and computation = binding list * tail_computation
and clause = [`Effect of identifier | `Exception | `Regular] * identifier * computation
  deriving (Show)

type program = computation
                 deriving (Show)*)

type constant = Ir.constant
   deriving (Show)
   
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
  | `Handle      of llambda * clause name_map * clause * Ir.handler_spec
  | `DoOperation of name * llambda list
  ]
and primitive =
  [ `Nil
  | `Builtin    of name
  | `FnApply    of name * llambda list
  | `BinOp      of binary_operation * llambda list
  | `Global     of identifier * int * name * [`Get | `Set]
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
and clause = [`Effect of identifier | `Exception | `Regular] * identifier * llambda
   deriving (Show)

type program = llambda
   deriving (Show)
