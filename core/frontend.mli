type 'a result =
  { program: 'a;
    datatype: Types.datatype;
    context: Context.t }

val program : Context.t -> Sugartypes.program -> Sugartypes.program result
val interactive : Context.t -> Sugartypes.sentence -> Sugartypes.sentence result
