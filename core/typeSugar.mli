module Check :
  sig
    open Sugartypes

    val program : Types.FrontendTypeEnv.t
               -> program
               -> program * Types.datatype * Types.FrontendTypeEnv.t

    val sentence : Types.FrontendTypeEnv.t
                -> sentence
                -> sentence * Types.datatype * Types.FrontendTypeEnv.t
  end
