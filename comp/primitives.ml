(* Compiler supported source language constants *)
open Lambda
       
let rec zip xs ys =
  match xs, ys with
  | x :: xs, y :: ys -> (x,y) :: zip xs ys
  | _, _ -> []

let arith_ops =
  [ "+" 
  ; "-"
  ; "*"
  ; "/"
  ; "^"
  ; "mod"
  ; "+."
  ; "-."
  ; "*."
  ; "/."
  ; "^."
  ]

let lambda_arith_prims =
  [ Paddint
  ; Psubint
  ]

let rel_ops =
  [ "=="
  ; "<>"
  ; "<"
  ; ">"
  ; "<="
  ; ">="
  ]

let lambda_rel_prims =
  [ Ceq
  ; Cneq
  ; Clt
  ; Cgt
  ; Cle
  ; Cge
  ]
    
let string_ops =
  ["^^"]    

let lambda_of_links_primitive =
  function
  | "+" -> Lambda.(Paddint)
  | "-" -> Lambda.(Psubint)
  | _ -> assert false

let is_arithmetic_operation : string -> bool =
  fun op -> List.mem op arith_ops

let is_relational_operation : string -> bool =
  fun op -> List.mem op rel_ops

let is_string_operation : string -> bool =
  fun op -> List.mem op string_ops

let is_primitive : Var.var -> bool = Lib.is_primitive_var
let primitive_name : Var.var -> string option
  = fun var ->
  try Some (Lib.primitive_name var) with
  | _ -> None
  
  
