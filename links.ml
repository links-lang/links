open Performance
open Getopt
open Utility
open List
open Basicsettings

(** type, optimise and evaluate a program *)
let process_program (valenv, typingenv, nenv) program = 
  print_string ((Ir.Show_program.show program)^"\n");
  print_endline

(* Read Links source code, then optimise and run it. *)
let evaluate ?(handle_errors=Errors.display_fatal) parse (_, tyenv, nenv as envs) = 
  handle_errors (measure "parse" parse (nenv, tyenv) ->- process_program envs)

let run_file prelude envs filename =
  Settings.set_value interacting false;
  let parse_and_desugar (nenv, tyenv) filename =
    let (nenv, tyenv), program =
      Errors.display_fatal Loader.load_file (nenv, tyenv) filename
    in
      program
  in
    if Settings.get_value web_mode then 
      failwith "not implemented web mode for the new IR yet"
      (*Webif.serve_request prelude envs filename*)
    else
      ignore (evaluate parse_and_desugar envs filename)

let evaluate_string_in envs v =
  let parse_and_desugar (nenv, tyenv) s = 
    let sugar, pos_context = Parse.parse_string ~pp:(Settings.get_value pp) Parse.program s in
    let program, _, _ = Frontend.Pipeline.program tyenv pos_context sugar in

    let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in

    let program, _nenv = Sugartoir.desugar_program (nenv, tenv) program in
      program
  in
    (Settings.set_value interacting false;
     ignore (evaluate parse_and_desugar envs v))

let load_prelude () = 
  let (nenv, tyenv), (bs, _) =
    (Errors.display_fatal
       Loader.load_file (Lib.nenv, Lib.typing_env) (Settings.get_value prelude_file)) in
  let () = Lib.prelude_env := Some tyenv in

    (* TODO:

        - bump the variable counters
        - run the prelude (need to implement evalir first)
    *)
    
  let envs =
    (Env.String.extend Lib.nenv nenv,
     Types.extend_typing_environment Lib.typing_env tyenv)
  in
    bs, envs

let to_evaluate = Oldlinks.to_evaluate
let config_file = Oldlinks.config_file
let options = Oldlinks.options

let main () =
  begin match Utility.getenv "REQUEST_METHOD" with 
    | Some _ -> Settings.set_value web_mode true
    | None -> ()
  end;
  config_file := (try Some (Unix.getenv "LINKS_CONFIG") with _ -> !config_file);
  let file_list = ref [] in
  Errors.display_fatal_l (lazy 
     (parse_cmdline options (fun i -> push_back i file_list)));
  (match !config_file with None -> () 
     | Some file -> Settings.load_file file);

  if Settings.get_value ir then
    begin
      let prelude, (nenv, tyenv) = load_prelude() in

      let valenv = () in       
      let envs = (valenv, tyenv, nenv) in
      
      let () = Utility.for_each !to_evaluate (evaluate_string_in envs) in
        (* TBD: accumulate type/value environment so that "interact" has access *)

      (*   let () = Utility.for_each !to_precompile (Loader.precompile_cache (snd prelude_envs)) in *)
      (*   let () = if !to_precompile <> [] then Settings.set_value interacting false in *)
          
      let () = Utility.for_each !file_list (run_file prelude envs) in
        ()
      (*     if Settings.get_value interacting then *)
      (*       let () = print_endline (Settings.get_value welcome_note) in *)
      (*         interact prelude_envs *)
    end
  else
    Oldlinks.main file_list

let _ =
  main ()
