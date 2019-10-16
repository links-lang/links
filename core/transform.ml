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
                 val obj : SugarTraversals.map
               end): sig
      include INTERFACE with type state := state and type 'a result := 'a result
    end
  end
end

module Untyped : UNTYPED = struct
  type 'a transformer = 'a -> 'a

  type state = Context.t
  type 'a result = Result of { program: 'a;
                               state: state }
  let context state = state
  let return state program =
    Result { program; state }

  (* Interface for untyped transformations. *)
  module type S = sig
    module Untyped: sig
      include INTERFACE with type state := state and type 'a result := 'a result
    end
  end

  let apply_transformer : state -> 'a transformer -> 'a -> 'a result
    = fun state transform program ->
    return state (transform program)

  module Make = struct
    module Transformer(T : sig
                 val name : string
                 val obj : SugarTraversals.map
               end) = struct

      let name = T.name

      let program state program =
        apply_transformer state T.obj#program program

      let sentence state sentence =
        apply_transformer state T.obj#sentence sentence
    end
  end
end

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
               val obj : Types.typing_environment -> sugar_transformer
             end): sig
    include INTERFACE with type state := state and type 'a result := 'a result
  end

end

module Typeable : TYPEABLE = struct
  class virtual sugar_transformer =
          object (_ : 'self)
            method virtual program : Sugartypes.program -> ('self * Sugartypes.program * Types.datatype option)
            method virtual sentence : Sugartypes.sentence -> ('self * Sugartypes.sentence * Types.datatype option)
          end

  type 'a transformer = 'a -> (sugar_transformer * 'a * Types.datatype option)
  type state =
    { datatype: Types.datatype;
      context: Context.t }

  type 'a result = Result of { program: 'a;
                               state: state }

  let return state program =
    Result { program; state }

  let with_type : Types.datatype option -> state -> state
    = fun t st ->
    match t with
    | None -> st
    | Some t -> { st with datatype = t }

  let context { context; _ } = context

  let apply : state -> 'a transformer -> 'a -> 'a result
    = fun st transform program ->
    let (_, program', t) =
      transform program
    in
    return (with_type t st) program'

  (* Interface for typeability preserving transformations. *)
  module type S = sig
    module Typeable: sig
      include INTERFACE with type state := state and type 'a result := 'a result
    end
  end

  module Make(T : sig
               val name : string
               val obj : Types.typing_environment -> sugar_transformer
             end) = struct

    let name = T.name

    let program state program =
      let open Context in
      apply state (T.obj (typing_environment state.context))#program program

    let sentence state sentence =
      let open Context in
      apply state (T.obj (typing_environment state.context))#sentence sentence
  end
end

(* Identity transformer. *)
module Identity = struct
  module Untyped = struct
    let name = "identity"

    let identity state program =
      Untyped.return state program

    let program = identity
    let sentence = identity
  end

  module Typeable = struct
    let name = "identity"

    let identity state program =
      Typeable.return state program

    let program = identity
    let sentence = identity
  end
end
