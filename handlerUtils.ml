module Types = Types_links
open Utility
open TypeUtils

let return_case = "Return"
  
let nature = fst
let depth = snd
    
module type Handler = sig
  type h

  val is_closed      : h -> bool
  val is_shallow     : h -> bool
end


module type Descriptor = sig
  type t

  include Handler with type h := Sugartypes.hdescriptor
    
  val update_type_info   : t -> (Types.datatype * Types.row) -> t
  val type_info          : t -> (Types.datatype * Types.row) option
  val specialization     : t -> Sugartypes.handler_spec    
end
  
module HandlerDescriptor : (Descriptor with type t = Sugartypes.hdescriptor) = struct
  type t = Sugartypes.hdescriptor

  let specialization = function
    | (spec,_) -> spec

  let type_info = function
    | (_,info) -> info

  let update_type_info (spec,_) info = (spec, Some info)

  let is_closed h =
    match nature (specialization h) with
    | `Closed -> true
    | _       -> false
     
  let is_shallow h =
    match depth (specialization h) with
    | `Shallow -> true
    | _        -> false       
end
  
module type FrontendHandler = sig
  type h

  include Handler with type h := Sugartypes.handler

  val update_type_info   : h -> (Types.datatype * Types.row) -> h
  val type_info          : h -> (Types.datatype * Types.row) option
  val make_handler       : Sugartypes.phrase -> (Sugartypes.pattern * Sugartypes.phrase) list -> Sugartypes.hdescriptor -> h
  val descriptor         : h -> Sugartypes.hdescriptor
  val specialization     : h -> Sugartypes.handler_spec
end

module SugarHandler : (FrontendHandler with type h = Sugartypes.handler) = struct
  type h = Sugartypes.handler

  let descriptor = function
    | (_,_,desc) -> desc
    
  let specialization h = HandlerDescriptor.specialization (descriptor h)
    
  let is_closed h =
    match nature (specialization h) with
    | `Closed -> true
    | _       -> false
     
  let is_shallow h =
    match depth (specialization h) with
    | `Shallow -> true
    | _        -> false
    
  let type_info h = HandlerDescriptor.type_info (descriptor h)

  let update_type_info (m, cases, (spec,_)) info = (m, cases, (spec, Some info))

  let make_handler m cases desc = (m, cases, desc)
end

module IrHandler : (Handler with type h = Ir.handler_spec) = struct
  type h = Ir.handler_spec
    
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
