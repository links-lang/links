module Linearity = struct
  type t = Any | Unl
    [@@deriving eq,show]

  let isAny lin = lin == Any
  let isUnl lin = lin == Unl

  let string_of = function
    | Any -> "Any"
    | Unl -> "Unl"
end

(* Convenient aliases for constructing values *)
let linUnl = Linearity.Unl
let linAny = Linearity.Any

module DeclaredLinearity = struct
  type t = Lin | Unl
    [@@deriving show]

  let isLin lin = lin == Lin
  let isUnl lin = lin == Unl
end

(* Convenient aliases for constructing values *)
let dlLin = DeclaredLinearity.Lin
let dlUnl = DeclaredLinearity.Unl

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

(* Convenient aliases for constructing values *)
let resAny     = Restriction.Any
let resBase    = Restriction.Base
let resSession = Restriction.Session
let resEffect  = Restriction.Effect

module PrimaryKind = struct
  type t =
    | Type
    | Row
    | Presence
    [@@deriving show,eq]

  let string_of = function
    | Type -> "Type"
    | Row -> "Row"
    | Presence -> "Presence"
end

(* Convenient aliases for constructing values *)
let pkType     = PrimaryKind.Type
let pkRow      = PrimaryKind.Row
let pkPresence = PrimaryKind.Presence
