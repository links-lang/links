module type INTERFACE = sig
  type state
  type 'a result

  val name : string

  val program : state ->
                Sugartypes.program ->
                Sugartypes.program result

  val sentence : state ->
                 Sugartypes.sentence ->
                 Sugartypes.sentence result
end

module type UNTYPED = sig
  type state = Context.t
  type 'a result = Result of { program: 'a;
                               state: state }


  val return : state -> 'a -> 'a result
  val context : state -> Context.t

  module type S = sig
    module Untyped: sig
      include INTERFACE with type state := state and type 'a result := 'a result
    end
  end

  module Make: sig
    module Transformer(T : sig
                 val name : string
                 val obj : SugarTraversals.map end): sig
      include INTERFACE with type state := state and type 'a result := 'a result
    end
  end
end

module Untyped : UNTYPED


module type TYPEABLE = sig
  type state =
    { datatype: Types.datatype;
      context: Context.t }

  type 'a result = Result of { program: 'a;
                               state: state }

  val return : state -> 'a -> 'a result
  val with_type : Types.datatype option -> state -> state
  val context : state -> Context.t

  module type S = sig
    module Typeable: sig
      include INTERFACE with type state := state and type 'a result := 'a result
    end
  end

  (* This virtual class helps break the dependency cycle:

     Value -> DesugarDatatypes -> Transform -> TransformSugar -> DesugarDatatypes -> Transform

   *)
  class virtual sugar_transformer:
                  object ('self)
                    method virtual program : Sugartypes.program -> ('self * Sugartypes.program * Types.datatype option)
                    method virtual sentence : Sugartypes.sentence -> ('self * Sugartypes.sentence * Types.datatype option)
                  end

  module Make(T : sig
               val name : string
               val obj : Types.typing_environment -> sugar_transformer end): sig
    include INTERFACE with type state := state and type 'a result := 'a result
  end
end

module Typeable : TYPEABLE

module Identity: sig
  include Untyped.S
  include Typeable.S
end
