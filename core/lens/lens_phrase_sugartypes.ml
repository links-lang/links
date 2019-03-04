open CommonTypes
open SourceCode
open Lens_operators

type name = string

type t =
  | Constant of Constant.t
  | Var of name
  | UnaryAppl of Unary.t * phrase
  | InfixAppl of Binary.t * phrase * phrase
and phrase =
  t WithPos.t
