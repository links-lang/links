open Performance
open Getopt
open Utility
open List
open Basicsettings

(** Print a value (including its type if `printing_types' is [true]). *)
let print_value rtype value = 
  print_string (Value.string_of_value value);
  print_endline (if Settings.get_value(printing_types) then
		   " : "^ Types.string_of_datatype rtype
                 else "")

(** optimise and evaluate a program *)
let process_program ?(printer=print_value) (valenv, typingenv, nenv) (program, t) =
  (* TODO: the optimise part *)
(*
  print_string ((Ir.Show_program.show program)^"\n");
  print_endline;
*)
  let valenv, v = lazy (Evalir.run_program valenv program)
    <|measure_as|> "run_program"
  in
    printer t v;
    (valenv, typingenv, nenv), v

(* Read Links source code, then optimise and run it. *)
let evaluate ?(handle_errors=Errors.display_fatal) parse (_, tyenv, nenv as envs) =
    handle_errors (measure "parse" parse (nenv, tyenv) ->- process_program envs)

(* Read Links source code and pretty-print the IR *)
let print_ir ?(handle_errors=Errors.display_fatal) parse (_, tyenv, nenv as envs) =
  let printer (valenv, typingenv, nenv) (program, t) =
    print_endline (Ir.Show_program.show program ^ "\n");
    print_endline (Ir.string_of_ir (Compileir.invert_env nenv) program) in
  handle_errors (measure "parse" parse (nenv, tyenv) ->- printer envs)

let run_file prelude envs filename =
  Settings.set_value interacting false;
  let parse_and_desugar (nenv, tyenv) filename =
    let (nenv, tyenv), (globals, (locals, main), t) =
      Errors.display_fatal Loader.load_file (nenv, tyenv) filename
    in
      ((globals @ locals, main), t)
  in
    if Settings.get_value web_mode then
      let (valenv, tyenv, nenv) = envs in
      let (nenv', tyenv'), (globals, (locals, main), _t) =
        Errors.display_fatal Loader.load_file (nenv, tyenv) filename in
      let closures = Ir.ClosureTable.program (Var.varify_env (nenv, tyenv.Types.var_env)) (globals @ locals, main) in

      let valenv = Evalir.run_defs (Value.with_closures valenv closures) globals in

      let envs =
        (valenv,
         Env.String.extend nenv nenv',
         Types.extend_typing_environment tyenv tyenv')
      in
        Webif.serve_request envs (prelude @ globals, (locals, main))
    else
      if Settings.get_value pretty_print_ir then
        print_ir parse_and_desugar envs filename
      else
        ignore (evaluate parse_and_desugar envs filename)

let evaluate_string_in envs v =
  let parse_and_desugar (nenv, tyenv) s = 
    let sugar, pos_context = Parse.parse_string ~pp:(Settings.get_value pp) Parse.program s in
    let program, t, _ = Frontend.Pipeline.program tyenv pos_context sugar in

    let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in

    let globals, (locals, main), _nenv = Sugartoir.desugar_program (nenv, tenv) program in
      ((globals @ locals, main), t)
  in
    (Settings.set_value interacting false;
     if Settings.get_value pretty_print_ir then
       print_ir parse_and_desugar envs v
     else
       ignore (evaluate parse_and_desugar envs v))

let load_prelude () = 
  let (nenv, tyenv), (globals, _, _) =
    (Errors.display_fatal
       Loader.load_file (Lib.nenv, Lib.typing_env) (Settings.get_value prelude_file)) in
  let () = Lib.prelude_env := Some tyenv in

    (* TODO:

        - bump the variable counters
        - run the prelude (need to implement evalir first)
    *)

  let closures = Ir.ClosureTable.bindings (Var.varify_env (Lib.nenv, Lib.typing_env.Types.var_env)) globals in
  let valenv = Evalir.run_defs (Value.with_closures Value.empty_env closures) globals in
  let envs =
    (valenv,
     Env.String.extend Lib.nenv nenv,
     Types.extend_typing_environment Lib.typing_env tyenv)
  in
    globals, envs

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
      let prelude, (valenv, nenv, tyenv) = load_prelude () in

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
