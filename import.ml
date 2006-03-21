let _ = Curl.global_init Curl.CURLINIT_GLOBALALL 

let grab_url url = 
   let connection = Curl.init () in 
   Curl.set_url connection url; 
   let output = ref "" in 
   Curl.set_writefunction connection (fun s ->  output := s); 
   Curl.perform connection; 
   !output;; 

(* TODO: recursive import: if an imported module also imports modules, handle those too. *)
let rec import_modules : Syntax.expression list -> (string * Result.environment * Kind.environment) list =
  let import (name, url) = 
    let code = grab_url url in 
    let exprs = Parse.parse_string code in
    let typeenv, exprs = Inference.type_program Library.type_env exprs in
    let exprs = Optimiser.optimise_program (typeenv, exprs) in 
    let valenv, _ = (Interpreter.run_program Library.value_env) exprs in 
      name, valenv, typeenv in
    Utility.concat_map (function
                             | Syntax.Directive (Syntax.Namespace (name, url), _) -> [import (name, url)]
                             | _ -> [])
