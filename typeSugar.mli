module Check :
  sig
    open Sugartypes

    val program : Types.typing_environment
            -> (position -> Syntax.position)
            -> program
            -> program * Types.datatype * Types.typing_environment

    val sentence : Types.typing_environment
                -> (position -> Syntax.position)
                -> sentence
                -> sentence * Types.typing_environment * Types.datatype
  end
