module Check :
  sig
    open Sugartypes

    val program : FrontendTypeEnv.t
               -> program
               -> program * Types.datatype * FrontendTypeEnv.t

    val sentence : FrontendTypeEnv.t
                -> sentence
                -> sentence * Types.datatype * FrontendTypeEnv.t
  end
