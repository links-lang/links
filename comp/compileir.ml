(* Compiler driver *)
open Irtolambda

let hello_world : Lambda.lambda =
  Lambda.(
    Compmisc.init_path false;
    let () = Ident.reinit() in
    let print_endline, _ = Env.lookup_value
			     (Longident.(Ldot (Lident "Pervasives", "print_endline")))
			     Env.empty
    in
    let id = Ident.( { name = "Helloworld" ; flags = 1 ; stamp = 0 } ) in
    let body = 
      Lapply (transl_path Env.empty print_endline,
              [Lconst (Const_immstring "Hello, world")],
              no_apply_info)
    in
    Lsequence (body, Lprim (Pmakeblock(0, Immutable), []))
  )


(* (catch
      (let (match/1209 = 1 switcher/1210 =a (-1+ match/1209))
        (if (isout 6 switcher/1210) (exit 1)
          (switch* switcher/1210
           case int 0: 1
           case int 1: 2
           case int 2: 3
           case int 3: (exit 1)
           case int 4: 5
           case int 5: 6
           case int 6: 7)))
     with (1)
      (raise
        (makeblock 0 (global Match_failure/18g)
          [0: "/tmp/tmpvrgltaik/tmpmu0lu0s7.ml" 1 0])))
	 *)
let dump_lambda lam =
  if Settings.get_value Basicsettings.show_lambda_ir
  then Format.fprintf Format.err_formatter "%a@\n@." Printlambda.lambda lam
  else ()      

let bytecomp module_name lambda_program  =
  let objfile = module_name ^ ".cmo" in
  let impl = Bytegen.compile_implementation module_name lambda_program in
  let fd = open_out objfile in
  begin
    Emitcode.to_file fd module_name objfile impl;
    close_out fd
  end (* TODO: Handle errors; add clean-up code, etc... *)

let nativecomp name lambda_program =
  Compmisc.init_path true;
  let modulename = String.capitalize name in

  let env = Compmisc.initial_env() in
  let (_ : Types.signature) = Env.save_signature [] modulename (name ^ ".cmx") in
    (* let () = Env.reset_cache () in *)

  (* Proc.init (); *)
  (* Reg.reset (); *)
  Env.set_unit_name modulename;
  Compilenv.reset modulename;
  Clflags.dump_clambda := true;
  Clflags.dump_cmm := true;
  let impl = Simplif.simplify_lambda lambda_program in
  let () = prerr_endline "simplified lambda" ; flush stderr in
  let () = Asmgen.compile_implementation name Format.err_formatter (1, impl) in
  let () = prerr_endline "compiled implementation" ; flush stderr  in
  let () = Compilenv.save_unit_info (Printf.sprintf "%s.cmx" name) in
  let () = prerr_endline "saved unit info"  ; flush stderr in
  ()(*
  Clflags.dump_clambda := true;
  Clflags.dump_cmm := true;
  let cmxfile = module_name ^ ".cmx" in  
  Compmisc.init_path true;
  (* Proc.init (); *)
  (* Reg.reset (); *)
  Env.set_unit_name (String.capitalize module_name);
  Compilenv.reset (String.capitalize module_name);
  let impl = Simplif.simplify_lambda lambda_program in
  Format.fprintf Format.err_formatter "%a@\n@." Printlambda.lambda impl;
  let () = Asmgen.compile_implementation module_name Format.err_formatter (0, impl) in
  Compilenv.save_unit_info cmxfile*)

let remove_links_extension filename =
  Filename.chop_suffix filename ".links"
			   
let compile parse_and_desugar env prelude filename =
  let program = parse_and_desugar env filename in
  let () = if Settings.get_value Basicsettings.show_compiled_ir
	   then print_endline (Ir.Show_program.show program)
	   else ()
  in    
  let module_name = remove_links_extension filename in
  let lam = lambda_of_ir env module_name program in  
  dump_lambda lam;
  if Settings.get_value Basicsettings.dry_run
  then ()
  else (if Settings.get_value Basicsettings.bytecomp
	then (print_endline "byte code compilation"; bytecomp module_name lam)
	else (print_endline "native code compilation"; nativecomp module_name lam))
