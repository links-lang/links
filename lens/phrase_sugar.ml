open Operators

type name = string

type 'a t =
  | Constant of Phrase_value.t
  | Var of name
  | UnaryAppl of Unary.t * 'a phrase
  | InfixAppl of Binary.t * 'a phrase * 'a phrase

and 'a phrase = 'a * 'a t

let node (_, v) = v

let pos (pos, _) = pos
