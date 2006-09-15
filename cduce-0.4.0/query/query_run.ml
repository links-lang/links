let () = Cduce.extra_specs := 
      (  "--noquery-optim", Arg.Set Query.nooptim,
         " do not optimize queries " ) :: 
	!Cduce.extra_specs
