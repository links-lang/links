module Check :
  sig
    open Sugartypes

    val program : Types.typing_environment
               -> program
               -> program * Types.datatype * Types.typing_environment

    val sentence : Types.typing_environment
                -> sentence
                -> sentence * Types.datatype * Types.typing_environment
  end
