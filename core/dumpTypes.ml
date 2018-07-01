(*
  This function is intended to be used to dump the types and
  positions of all variables in a program. It should be useful for
  enabling Tom's IDE to display typing tooltips for variables.

  Technically the dumper is incorrect because the current environment
  does not necessarily respect lexical scope. However, it should work
  anyway as the type checker ensures the program is well-typed and in
  particular that there are no free variables.
*)
let program =
  fun ({Types.var_env=env; Types.tycon_env=_; effect_row=_} as tyenv) code ->
    let module QualifiedName = Sugartypes.QualifiedName in
    let dumper = object (o)
      inherit SugarTraversals.fold as super

      val env = env
      val vars = []

      method get_vars () = List.rev vars

      method bind (x, t, pos) =
        {< env = Env.String.bind env (x, t); vars = (x, t, pos)::vars >}

      method use (x, t, pos) =
        {< vars = (x, t, pos)::vars >}

      method bound x = Env.String.has env x

      method lookup x =
        Env.String.lookup env x

      method! binder =
        fun (x, t, pos) ->
          o#option
            (fun o t -> o#bind (x, t, pos))
            t

      method! phrase =
        function
        | `Var x, pos when o#bound (QualifiedName.unqualify x) ->
           let x = QualifiedName.unqualify x in
              o#use (x, o#lookup x, pos)
          | e -> super#phrase e
    end in
    let program =
      let sugar, pos_context = Parse.parse_string ~pp:(Settings.get_value Basicsettings.pp) Parse.program code in
      let (program, _, _), _ = Frontend.Pipeline.program tyenv pos_context sugar in
        program
    in
      (dumper#program program)#get_vars()
