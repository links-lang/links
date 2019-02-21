module Linearity = struct
  type t = Any | Unl
    [@@deriving eq,show]
end

let string_of_linearity = function
  | Linearity.Any -> "Any"
  | Linearity.Unl -> "Unl"

let linUnl = Linearity.Unl
let linAny = Linearity.Any
let isUnl lin = lin == Linearity.Unl

module Restriction = struct
  type t =
    | Any
    | Base
    | Session
    | Effect
    [@@deriving eq,show]

  let isAny = function
    | Any -> true
    | _   -> false

  let isBase = function
    | Base -> true
    | _    -> false

  let isSession = function
    | Session -> true
    | _       -> false
end

let resAny     = Restriction.Any
let resBase    = Restriction.Base
let resSession = Restriction.Session
let resEffect  = Restriction.Effect

let string_of_restriction = function
 | Restriction.Any     -> "Any"
 | Restriction.Base    -> "Base"
 | Restriction.Session -> "Session"
 | Restriction.Effect  -> "Eff"
