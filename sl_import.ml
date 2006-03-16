let _ = Curl.global_init Curl.CURLINIT_GLOBALALL 

let grab_url url = 
   let connection = Curl.init () in 
   Curl.set_url connection url; 
   let output = ref "" in 
   Curl.set_writefunction connection (fun s ->  output := s); 
   Curl.perform connection; 
   !output;; 

(* TODO: recursive import: if an imported module also imports modules, handle those too. *)
let rec import_modules : Sl_syntax.expression list -> (string * Sl_result.environment * Sl_kind.environment) list =
  let import (name, url) = 
    let code = grab_url url in 
    let exprs = Parse.parse_string code in
    let typeenv, exprs = Sl_inference.type_program Sl_library.type_env exprs in
    let exprs = Sl_optimiser.optimise_program (typeenv, exprs) in 
    let valenv, _ = (Sl_interpreter.run_program Sl_library.value_env) exprs in 
      name, valenv, typeenv in
    Sl_utility.concat_map (function
                             | Sl_syntax.Directive (Sl_syntax.Namespace (name, url), _) -> [import (name, url)]
                             | _ -> [])
