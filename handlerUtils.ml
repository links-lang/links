open Utility
open TypeUtils

type handler_spec = Sugartypes.handler_spec

let return_case = "Return"
  
let nature = fst
let depth = snd
    
let is_closed spec =
  match nature spec with
    `Closed -> true
  | _       -> false

let is_shallow spec =
  match depth spec with
    `Shallow -> true
  | _        -> false
 

let handles_operation : Types.row -> string -> bool
  = fun (fields,_,_) opname -> StringMap.mem opname fields		   

let allow_wild : Types.row -> Types.row
  = fun row ->
  let fields = StringMap.add "wild" Types.unit_type StringMap.empty in
  Types.extend_row fields row

let make_operations_presence_polymorphic : Types.row -> Types.row
  = fun (signatures,row_var,dual) ->
  let has_wild = StringMap.exists (fun name _ -> String.compare name "wild" == 0) signatures in
  let signatures = StringMap.filter (fun name _ -> String.compare name "wild" <> 0) signatures in
  let signatures = StringMap.map (fun _ -> Types.fresh_presence_variable (`Unl, `Any)) signatures in
  let row = (signatures, row_var, dual) in
  if has_wild then allow_wild row
  else row
  
