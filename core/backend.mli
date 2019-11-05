type result =
  { program: Ir.program;
    datatype: Types.datatype;
    context: Context.t }

val program : Context.t -> Types.datatype -> Ir.program -> result
