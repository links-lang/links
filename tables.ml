open Utility

let init_size = 10000

let scopes = (Hashtbl.create init_size : (Var.var, Ir.scope) Hashtbl.t)

let fun_defs = (Hashtbl.create init_size : (Var.var, Ir.eval_fun_def) Hashtbl.t)
let cont_defs = (Hashtbl.create init_size : (Var.var, Ir.computation) Hashtbl.t)

let cont_vars = (Hashtbl.create init_size : (Var.var, intset) Hashtbl.t)

let find = Hashtbl.find
let lookup = Hashtbl.lookup
let mem = Hashtbl.mem
