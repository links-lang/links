open Performance
open Getopt
open Utility
open List
open Basicsettings

(** type, optimise and evaluate a program *)
let process_program (valenv, typingenv, nenv) (program,_) = 
  print_string ((Ir.Show_program.show program)^"\n");
  print_endline

(* Read Links source code, then type, optimize and run it. *)
let evaluate ?(handle_errors=Errors.display_fatal) parse (_, tyenv, nenv as envs) = 
  handle_errors (measure "parse" parse (nenv, tyenv) ->- process_program envs)

let evaluate_string_in envs v =
  let parse_and_desugar (nenv, tyenv) s = 
    let sugar, pos_context = Parse.parse_string ~pp:(Settings.get_value pp) Parse.program s in
    let (bindings, expr) as program, _, _ = Frontend.Pipeline.program tyenv pos_context sugar in

    (* create a copy of the type environment mapping vars (= ints) to datatypes
       instead of strings to types
    *)
    let tenv =
      Env.String.fold
        (fun name t tenv ->
           Env.Int.bind tenv (Env.String.lookup nenv name, t))
        tyenv.Types.var_env
        Env.Int.empty in
    let program = Sugartoir.desugar_program (nenv, tenv) program in
      program, (bindings, expr)
  in
    (Settings.set_value interacting false;
     ignore (evaluate parse_and_desugar envs v))

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
      let valenv = () in
      let nenv = Lib.nenv in
      let tyenv = Lib.typing_env in
        
      let envs = (valenv, tyenv, nenv) in
      
      let () = Utility.for_each !to_evaluate (evaluate_string_in envs) in
        (* TBD: accumulate type/value environment so that "interact" has access *)
        ()
      (*   let () = Utility.for_each !to_precompile (Loader.precompile_cache (snd prelude_envs)) in *)
      (*   let () = if !to_precompile <> [] then Settings.set_value interacting false in *)
          
      (*   let () = Utility.for_each !file_list (run_file prelude_syntax prelude_envs) in *)
      (*     if Settings.get_value interacting then *)
      (*       let () = print_endline (Settings.get_value welcome_note) in *)
      (*         interact prelude_envs *)
    end
  else
    Oldlinks.main file_list

let _ =
  main ()
