open Getopt
open Utility

(* Whether to run the interactive loop *)
let interacting = ref true

(* Whether to print types *)
let printing_types = ref true

(* Prompt in interactive mode *)
let ps1 = "links> "

(* Builtin environments *)
let stdenvs = Library.value_env, Library.type_env

(* Run unit tests *)
let run_tests () = 
  interacting := false;
(*   Optimiser.test () ; *)
(*   Js.test () *)
  Js.run_tests ()

(* Print a result, including its type if `printing_types' is true. *)
let print_result rtype result = 
  print_string (Result.string_of_result result);
  print_endline (if !printing_types then " : "^ Kind.string_of_kind rtype
                 else "")

(* Read Links source code, then type, optimize and run it. *)
let rec evaluate ?(handle_errors=Errors.display_errors_fatal stderr) parse (valenv, typeenv) input = 
  interacting := false;
  handle_errors
    (fun input ->
       let exprs =          Performance.measure "parse" parse input in 
       let typeenv, exprs = Performance.measure "type_program" (Inference.type_program typeenv) exprs in
       let exprs =          Performance.measure "optimise_program" Optimiser.optimise_program (typeenv, exprs) in
       let valenv, result = Performance.measure "run_program" (Interpreter.run_program valenv) exprs in
         print_result (Syntax.node_kind (last exprs)) result;
         (valenv, typeenv), result
    ) input


(* Interactive loop *)
let rec interact envs = 
  let error_handler = Errors.display_errors stderr (fun _ -> envs, `Record []) in
    print_string ps1; flush stdout; 
    interact (fst (evaluate ~handle_errors:error_handler Parse.parse_channel envs (stdin, "<stdin>")))
let web_program filename = 
  interacting := false;
  Webif.serve_requests filename 
    

let web_program filename = 
  interacting := false;
  Webif.serve_requests filename 
    
let options : opt list = 
    [
      ('d',     "debug",               set Utility.debugging     true,  None);
      ('O',     "optimize",            set Optimiser.optimising  true,  None);
      (noshort, "measure-performance", set Performance.measuring true,  None);
      ('n',     "no-types",            set printing_types        false, None);
      ('e',     "evaluate",            None,                            Some (ignore -<- evaluate Parse.parse_string stdenvs));
      ('t',     "run-tests",           Some run_tests,                  None);
      ('w',     "web",                 None,                            Some web_program);
    ]

(* main *)
let _ =
  Errors.display_errors_fatal stderr (parse_cmdline options) (ignore -<- evaluate Parse.parse_file stdenvs);
  if !interacting then interact stdenvs
