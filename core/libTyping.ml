(* This file is a workaround for the fact that the module Lib
   needs to be part of the runtime library, but its typing information
   must be accessible in the backend library *)


let dummy _x = assert false


let ref_primitive_name : (Var.var -> string) ref = ref dummy
let ref_is_primitive_var : (Var.var-> bool) ref = ref dummy
let ref_is_pure_primitive : (string -> bool) ref = ref dummy

let set_fun_primitive_name f = ref_primitive_name := f
let set_fun_is_primitive_var f = ref_is_primitive_var := f
let set_fun_is_pure_primitive f = ref_is_pure_primitive := f


let primitive_name (x : Var.var) : string = (!ref_primitive_name) x
let is_primitive_var (x : Var.var) : bool = (!ref_is_primitive_var) x
let is_pure_primitive (x : string) : bool = (!ref_is_pure_primitive) x
