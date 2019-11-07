type 'a result =
  { program_ : 'a;
    context: Context.t }

val load : Context.t -> string -> Sugartypes.program result
