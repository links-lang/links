open Utility

let fun_map = (Hashtbl.create 10000 : (Var.var, Ir.eval_fun_def) Hashtbl.t)

let find f = Hashtbl.find fun_map f
let lookup f = Hashtbl.lookup fun_map f
let is_fun f = Hashtbl.mem fun_map f
