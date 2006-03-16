function bind(name, val, env) {
  return env.concat([[name, val]]);
}

function lookup(name, env) {
  for (var i = env.length - 1; i >= 0; i--) {
    if (name == env[i][0]) {
      return env[i][1];
    }
  }
  throw "Not found " + name;
}


function interpret(object, env) {
  for (;;) {
    eval:
    switch (typeof (object)) {
      // booleans, integers, floats and strings encoded as literal values
      case "string":
      case "number":
      case "boolean":
      case "function":
           return object;
      case "object":
        switch (object[0]) {
           case "variable":
               return lookup(object[1], env);
           case "application":
               var fun = interpret(object[1], env);
               if (typeof(fun) == "function") { // primitive
                   return fun(interpret(object[2], env))
               }
               else {
                  env = bind(fun.arg, interpret(object[2], env), env);
                  object = fun.body;
                  break eval;
               }
           case "condition":
               var cond = interpet(object[1], env);
               return interpret(object[cond ? 2 : 3], env);      
           case "comparison":
               var l = interpret(object[2], env);
               var r = interpret(object[3], env);
               switch(object[1]) {
                 case "==": return l == r;
                 case "<<" : return l < r;
                 case "<=": return l <= r;
                 case "<>": return l != r;
                 default : throw "Invalid comparison operator: " 
                                + object[1].toString();
               }
           case "function":
               var fun = new Object();
               fun.arg = object[1];
               fun.body = object[2];
               return fun;
           case "let":
               return interpret(object[3], bind(object[1], 
                                                interpret(object[2], env),
                                                env));
           case "rec":
               throw "Not yet implemented (letrec)";
           case "record_empty": 
           case "record_extension": 
           case "record_selection": 
           case "record_selection_empty": 
           case "variant_injection": 
           case "variant_selection": 
           case "variant_selection_empty": 
           case "collection_empty": 
           case "collection_single": 
           case "collection_union": 
           case "collection_extension": 
              throw "Not yet implemented : " + object[1];
           case "xmltree": 
              throw "Not yet implemented (xml)"
           default:
              throw ("Unexpected compound object (beginning " 
                    + object[0].toString() 
                    + ") passed to interpreter");
      }
      default:
        throw "Unexpected " + object.toString() + " passed to interpreter";
    }
  }
}

var initial_env = [["+", function (x) { return function (y) { return x + y; } }]];
var expression = ["let", "x", ["function", "x", ["application", ["application", ["variable", "+"], ["variable", "x"]], 1]], ["application", ["variable", "x"], 4]];
print (interpret( expression, initial_env));
