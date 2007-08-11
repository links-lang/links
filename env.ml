open Utility
include StringMap

type name = string

let domain map = fold (fun k _ -> StringSet.add k) map StringSet.empty
let extend = superimpose
let bind = add
let range map = fold (fun _ v l -> v::l) map []
let has env name = mem name env

module Typeable_t = Typeable_stringmap
module Pickle_t = Pickle_stringmap
