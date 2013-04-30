let pred_db = Lambda([42], 
  ([], Return(Primitive(">", `Bool),
     [ Variable(42); Constant(Int 3)])))
