open Lens_operators

type name = string

type 'a t =
  | Constant of Lens_phrase_value.t
  | Var of name
  | UnaryAppl of Unary.t * 'a phrase
  | InfixAppl of Binary.t * 'a phrase * 'a phrase
and 'a phrase =
  'a * 'a t
