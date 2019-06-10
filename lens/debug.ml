let debug = ref false

let set_debug b = debug := b

let print s = if !debug then prerr_endline s
