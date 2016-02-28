open Utility
open TypeUtils

type handler_spec = Sugartypes.handler_spec
 
let return_case = "Return"
  
let nature = fst
let depth = snd
let spec  = fst
    
let is_closed desc =
  match nature (spec desc) with
    `Closed -> true
  | _       -> false

let is_shallow desc =
  match depth (spec desc) with
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
  
module type Handler = sig
  type t

  val is_closed      : t -> bool
  val is_shallow     : t -> bool
  val specialization : t -> handler_spec
end

module type FrontendHandler = sig
  type t

  include Handler with type t := Sugartypes.hdescriptor

  val update_type_info   : t -> (Types.datatype * Types.row) -> t
  val type_info          : t -> (Types.datatype * Types.row) option
end

module SugarHandler : (FrontendHandler with type t = Sugartypes.hdescriptor) = struct
  type t = Sugartypes.hdescriptor

  let specialization = function
    | (spec, _) -> spec
    
  let is_closed h =
    match nature (specialization h) with
    | `Closed -> true
    | _       -> false
     
  let is_shallow h =
    match depth (specialization h) with
    | `Shallow -> true
    | _        -> false
    
  let update_type_info (spec,_) info = (spec, Some info)
  let type_info = function
    | (_, info) -> info
end

module IrHandler : (Handler with type t = Ir.handler_spec) = struct
  type t = Sugartypes.handler_spec
    
  let specialization = fun s -> s
    
  let is_closed h =
    match nature (specialization h) with
    | `Closed -> true
    | _       -> false
     
  let is_shallow h =
    match depth (specialization h) with
    | `Shallow -> true
    | _        -> false
end
