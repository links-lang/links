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
    method constant : Constant.t -> ('self_type * Constant.t * Types.datatype)
    method optionu :
      'a.
      ('self_type -> 'a -> ('self_type * 'a)) ->
      'a option -> 'self_type * 'a option
    method option :
      'a.
      ('self_type -> 'a -> ('self_type * 'a * Types.datatype)) ->
      'a option -> 'self_type * 'a option * Types.datatype option
    method list :
      'a.
      ('self_type -> 'a -> ('self_type * 'a * Types.datatype)) ->
      'a list -> 'self_type * 'a list * Types.datatype list
    method name_map :
      'a.
      ('self_type -> 'a -> ('self_type * 'a * Types.datatype)) ->
      'a name_map -> 'self_type * 'a name_map * Types.datatype name_map
    method var_map :
      'a.
      ('self_type -> 'a -> ('self_type * 'a * Types.datatype)) ->
      'a var_map -> 'self_type * 'a var_map * Types.datatype var_map
    method var : var -> ('self_type * var * Types.datatype)
    method temporal_update : temporal_update -> ('self_type * temporal_update)
    method temporal_deletion : temporal_deletion -> ('self_type * temporal_deletion)

    method value : value -> ('self_type * value * Types.datatype)

    method tail_computation :
      tail_computation -> ('self_type * tail_computation * Types.datatype)
    method special : special -> ('self_type * special * Types.datatype)
    method bindings : binding list -> ('self_type * binding list)
    method computation : computation -> ('self_type * computation * Types.datatype)
    method binding : binding -> ('self_type * binding)
    method binder : binder -> ('self_type * binder)

    method program : program -> ('self_type * program * Types.datatype)

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
