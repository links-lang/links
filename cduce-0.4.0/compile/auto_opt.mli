open Patterns
open Patterns.Compile

val make_branches : Types.t -> (node * 'a) list -> dispatcher * 'a rhs array
