open Utility

include module type of String

module Set : Lens_set.S with type elt = string

module Map : module type of Utility.StringMap
