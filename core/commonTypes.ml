module Linearity = struct
  type t = Any | Unl
    [@@deriving eq,show]

  let isAny lin = lin == Any
  let isUnl lin = lin == Unl

  let string_of = function
    | Any -> "Any"
    | Unl -> "Unl"
end

let linUnl = Linearity.Unl
let linAny = Linearity.Any

module DeclaredLinearity = struct
  type t = Lin | Unl
    [@@deriving show]

  let isLin lin = lin == Lin
  let isUnl lin = lin == Unl
end

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

  let string_of = function
    | Any     -> "Any"
    | Base    -> "Base"
    | Session -> "Session"
    | Effect  -> "Eff"
end

let resAny     = Restriction.Any
let resBase    = Restriction.Base
let resSession = Restriction.Session
let resEffect  = Restriction.Effect
