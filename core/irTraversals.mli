open Ir
open Utility
open CommonTypes

module type IR_VISITOR =
sig
  type environment = Types.datatype Env.Int.t

  class visitor : environment ->
  object ('self_type)
    val tyenv : environment

    method lookup_type : var -> Types.datatype
    method constant : Constant.t -> (Constant.t * Types.datatype * 'self_type)
    method optionu :
      'a.
      ('self_type -> 'a -> ('a * 'self_type)) ->
      'a option -> 'a option * 'self_type
    method option :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a option -> 'a option * Types.datatype option * 'self_type
    method list :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a list -> 'a list * Types.datatype list * 'self_type
    method name_map :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a name_map -> 'a name_map * Types.datatype name_map * 'self_type
    method var_map :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a var_map -> 'a var_map * Types.datatype var_map * 'self_type
    method var : var -> (var * Types.datatype * 'self_type)
    (* method closure_var : var -> (var * Types.datatype * 'self_type) *)
    method value : value -> (value * Types.datatype * 'self_type)

    method tail_computation :
      tail_computation -> (tail_computation * Types.datatype * 'self_type)
    method special : special -> (special * Types.datatype * 'self_type)
    method bindings : binding list -> (binding list * 'self_type)
    method computation : computation -> (computation * Types.datatype * 'self_type)
    method binding : binding -> (binding * 'self_type)
    method binder : binder -> (binder * 'self_type)
    (* method closure_binder : binder -> (binder * 'self_type) *)

    method program : program -> (program * Types.datatype * 'self_type)

    method get_type_environment : environment
  end
end

module Transform : IR_VISITOR

module Inline : IrTransform.S
module ElimDeadDefs : IrTransform.S
module ElimBodiesFromMetaTypeVars : IrTransform.S
module CheckForCycles : IrTransform.S
module ElimTypeAliases : IrTransform.S
module InstantiateTypes :
sig
  val computation : Types.datatype Env.Int.t -> Types.type_arg IntMap.t -> computation -> computation
end
