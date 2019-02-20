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
