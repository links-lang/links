(* Compiler driver *)
open Irtolambda
open Asmgen

let hello_world : unit -> Lambda.lambda =
  fun () ->
  let env = Compmisc.initial_env () in
  Lambda.(
    let () = Ident.reinit() in
    let print_endline, _ = Env.lookup_value
                           (Longident.(Ldot (Lident "Builtins", "print")))
                           env
    in
    (*let print_endline, _ = Env.lookup_value
			     (Longident.(Ldot (Lident "Pervasives", "print_endline")))
			     env
      in*)
    let id = Ident.( { name = "Hello" ; flags = 1 ; stamp = 0 } ) in
    let body = 
      LambdaDSL.lapply (transl_path Env.empty print_endline)
              ([Lconst (Const_immstring "Hello, world!")]
              )
    in
    Lsequence (body, Lprim (Pmakeblock(0, Asttypes.Immutable, None), [], Location.none))
  )

let hw : unit -> Lambda.lambda = 
  fun () ->
  LambdaDSL.lprim (Psetglobal (Ident.create_persistent "Hw")) [LambdaDSL.lseq (LambdaDSL.lapply (LambdaDSL.lprim (LambdaDSL.field 29) [ LambdaDSL.lgetglobal "Pervasives" ]) [LambdaDSL.lstring "hello_world\n"]) (LambdaDSL.lprim LambdaDSL.box [])]

let empty : unit -> Lambda.lambda =
  fun () -> 
  LambdaDSL.lprim LambdaDSL.box []  

let two : unit -> Lambda.lambda = 
    fun () ->
    Lprim ((Psetglobal (Ident.create_persistent "Const2" )), [(LambdaDSL.lseq (LambdaDSL.linteger 2) (LambdaDSL.lprim LambdaDSL.box []))], Location.none)



let module_ident =  Ident.create_persistent "const2"

let emptyprog : Lambda.program = {module_ident = module_ident ; main_module_block_size = 0; required_globals = Ident.Set.empty ; code = empty () }

let makeprog : Lambda.lambda -> Lambda.program = 
  fun lam ->
  {module_ident = module_ident ; main_module_block_size = 0; required_globals = Ident.Set.empty ; code = lam }



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

let run p arg =
  let dry_run = Settings.get_value Basicsettings.dry_run in
  if dry_run then arg
  else p arg
    
let print_if : 'a. bool -> string -> 'a -> 'a
  = fun cond msg forward ->
    let verbose = Settings.get_value Basicsettings.verbose in
    let chan = stdout in
    let _ =
      if cond || verbose
      then (Printf.fprintf chan "%s\n" msg; flush chan)
      else ()
    in
    forward

let print_verbose : 'a. string -> 'a -> 'a =
  fun msg forward ->
    let verbose = Settings.get_value Basicsettings.verbose in
    print_if verbose msg forward
    
let dependencies () =
  let linkslib  = "LINKS_LIB" in
  let serverlib_subdir = "../../install/default/lib/links" in
  let serverlib_dir =  
  try Filename.concat (Sys.getenv linkslib) serverlib_subdir with
  | Not_found ->
     let guess = Filename.concat (Filename.dirname Sys.executable_name) (Filename.concat "lib" serverlib_subdir) in
     Printf.fprintf stderr "Warning: Cannot locate libraries because environment variable $%s is not set. Optimistically guessing '%s'.\n" linkslib guess; flush stderr; guess
  in
  let _ = print_verbose ("server libraries directory: " ^ serverlib_dir) () in
  let builtins_cmx =
    let builtins_cmx = Filename.concat serverlib_dir "builtins.cmx" in
    if not (Sys.file_exists builtins_cmx) then
      failwith "Error: Cannot locate Builtins module."
    else
      builtins_cmx
  in
  let unix_cmxa =
    let err = "Error: Cannot locate Unix module." in
    try
      let unix_cmxa = Filename.concat (Findlib.package_directory "unix") "unix.cmxa" in
      if not (Sys.file_exists unix_cmxa) then
        failwith err
      else
        unix_cmxa
    with
    | Not_found -> failwith err
  in
  let _ = print_verbose ("Unix module: " ^ unix_cmxa) () in
  List.map CompilationUnit.make_linkable_unit [unix_cmxa ; builtins_cmx]
    
let initialize_ocaml_backend () =
  let dependencies = dependencies () in
  let dep_include_dirs =
    let dep_dirs = Utility.StringSet.from_list (List.map (fun d -> CompilationUnit.Linkable_Unit.locate d |> Filename.dirname) dependencies) in
    Utility.StringSet.fold (fun s acc -> s :: acc) dep_dirs []
  in
  let verbose = Settings.get_value Basicsettings.verbose in
  (*  Clflags.flambda_invariant_checks := true;*)
  (*  Clflags.inlining_report := false;*)
  Clflags.nopervasives := false;
  Clflags.dump_lambda := Settings.get_value Basicsettings.show_lambda_ir || verbose;
  Clflags.dump_clambda := Settings.get_value Basicsettings.show_clambda_ir || verbose;
  Clflags.dump_cmm := false;
  Clflags.keep_asm_file := true;
  Clflags.include_dirs := dep_include_dirs @ !Clflags.include_dirs;
  Clflags.native_code := true;
  Compmisc.init_path true; (* true for native code compilation *)
  Ident.reinit ();
  dependencies

    
let dump_lambda lam =
  if Settings.get_value Basicsettings.show_lambda_ir
  then Format.fprintf Format.err_formatter "%a@\n@." Printlambda.lambda lam
  else ()

let udump_lambda lam = Format.fprintf Format.err_formatter "%a@\n@." Printlambda.lambda lam



(*let bytecomp module_name lambda_program  =
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
  *)

(*let remove_links_extension filename =
  Filename.chop_suffix filename ".links"*)
   
let assemble_executable linkables target =
  print_verbose ("assembling executable " ^ target) ();
  run (fun _ -> Asmlink.link  Format.std_formatter linkables target) ()

   
(*let compile parse_and_desugar envs prelude filename =
  let _ = setup_options () in
  let ((bs,tc) as program, tenv) = parse_and_desugar envs filename in
  let () = if Settings.get_value Basicsettings.show_compiled_ir
	   then print_endline (Ir.Show_program.show program)
	   else ()
  in
  let program =
    if true then
      (prelude @ bs, tc)
    else
      (bs,tc)
  in
  let module_name = remove_links_extension filename in
  let lam = lambda_of_ir (envs,tenv) module_name program in  
  dump_lambda lam;
  if Settings.get_value Basicsettings.dry_run
  then ()
  else (if Settings.get_value Basicsettings.bytecomp
	then (print_endline "byte code compilation"; bytecomp module_name lam)
  else (print_endline "native code compilation"; nativecomp module_name lam))*)


let lambda_of_links_ir ir source =
  let open FileInfo in
  let printq_lambda lam = (Format.fprintf Format.std_formatter "Dumping lambda@\n%a@\n@." Printlambda.lambda lam; lam) in
  let dump_lambda lam =
    if !Clflags.dump_lambda 
    then (Format.fprintf Format.std_formatter "Dumping lambda@\n%a@\n@." Printlambda.lambda lam; lam)
    else lam
  in
  let dump_basic_unit bcu =
    print_verbose ("Basic compilation unit\n" ^ (CompilationUnit.Basic_Compilation_Unit.string_of_basic_unit bcu) ^ "\n") bcu
  in
  let srcfile = CompilationUnit.source_file source in
  let module_name =
    let caml_module_name = Compenv.module_of_filename (Format.std_formatter) (filename srcfile) (fileroot srcfile) in
    Filename.concat "Links_" caml_module_name
  (* FIXME: Prepend 'Links_' to module names to avoid clashes with OCaml modules, e.g. random.links gets the module name '_random' rather than 'random' which would clash with the OCaml random module. We should really come up with a better, more robust scheme for module naming. *)
  in
  ir
  |> fun qo -> (1, empty ())
  |> (fun ((globals,lam) as r) -> dump_lambda lam; r)
  |> (fun (globals,lam)  -> let lam = Simplif.simplify_lambda "" lam in (globals,lam))
  |> (fun lam -> print_if (!Clflags.dump_lambda) "simplified lambda" lam)
  |> (fun ((globals,lam) as r) -> dump_lambda lam; printq_lambda lam ; r)
  |> CompilationUnit.make_basic_compilation_unit source module_name
  |> dump_basic_unit

module NC = CompilationUnit.Native_Compilation_Unit
      
let clean_up comp_unit =
  let files = List.map FileInfo.filename (NC.temporary_files comp_unit) in
  List.fold_left (fun _ f -> Misc.remove_file f) () files
  
let nativecomp : 'a CompilationUnit.basic_comp_unit -> 'a CompilationUnit.native_comp_unit
  = fun basic_comp_unit -> 
    let open FileInfo in
    let open NC in
    let ppf = Format.std_formatter in
    let dump_comp_unit txt comp_unit = print_verbose (txt ^ "\n" ^ (NC.string_of_native_unit comp_unit) ^ "\n") comp_unit in
    let num_globals = CompilationUnit.Basic_Compilation_Unit.count_globals basic_comp_unit in
    let initialize_environment comp_unit =
      let module_name = module_name comp_unit in 
      Env.set_unit_name module_name;
      Compilenv.reset ?packname:!Clflags.for_package module_name;
      comp_unit
    in
    let module_identt = Ident.create_persistent "test" in
    let make_cmi_file comp_unit =
      let cmxfile = cmx_file comp_unit in
      let ign = Env.save_signature [] (module_name comp_unit) (filename cmxfile) in
      let cmifile = with_extension "cmi" cmxfile in
      with_cmi_file cmifile comp_unit
    in
    let asm_compile comp_unit =
      let  lambda = ir comp_unit in
      let srcfile_wo_ext = Misc.chop_extensions (source_file comp_unit |> filename) in
      let (pro: Lambda.program) = {module_ident = module_identt ; main_module_block_size = 0; required_globals = Ident.Set.empty ; code = lambda } in
      let _ = Asmgen.compile_implementation_clambda srcfile_wo_ext ppf  pro in
      comp_unit
    in
    let save_unit_info comp_unit =
      let _ = Compilenv.save_unit_info (cmx_file comp_unit |> filename) in
      comp_unit
    in
    let _ = print_verbose "native code compilation" () in
    let comp_unit = CompilationUnit.make_native_compilation_unit basic_comp_unit in
    try
      comp_unit
  |> dump_comp_unit "+ Compilation unit"
  (*    let module_ident = Ident.create_persistent modulename in*)
  |> initialize_environment 
  |> make_cmi_file
  |> dump_comp_unit "+ Compilation unit after cmi generation"
  |> run asm_compile
  |> (fun comp_unit -> print_verbose "compiled implementation" comp_unit)
  |> run save_unit_info
  |> (fun comp_unit -> print_verbose ("saved unit info " ^ (cmx_file comp_unit |> filename) ^ "\n") comp_unit)
    with exc -> clean_up comp_unit; raise exc

let modularize comp_unit =
  let cmxfile = CompilationUnit.Native_Compilation_Unit.cmx_file comp_unit |> FileInfo.filename in
  CompilationUnit.make_linkable_unit cmxfile

let cmp2 modname prog = 
  LambdatoNative.compile (Ident.create_persistent (Misc.chop_extensions modname))  prog  "a.out"
  
let compile parse_and_desugar filename =
  let ((bs,tc) as program, tenv) = parse_and_desugar filename in
  let () = print_endline (Ir.show_program program)
  in
  (*  hello_world ();*)
  let target = Settings.get_value Basicsettings.output_file |> FileInfo.make_fileinfo |> FileInfo.filename in
  let lam = (snd (lambda_of_ir filename program)) in
  let () = udump_lambda lam in
  let () = LambdatoNative.initialise () in
  let cmpfile = cmp2 filename (makeprog lam) in
  ()



(*let compile parse_and_desugar filename =
  let ((bs,tc) as program, tenv) = parse_and_desugar filename in
  let () = print_endline (Ir.show_program program)
  in
  let dependencies = initialize_ocaml_backend () in
  (*  hello_world ();*)
  let target = Settings.get_value Basicsettings.output_file |> FileInfo.make_fileinfo |> FileInfo.filename in
  let comp_unit =
    CompilationUnit.make_source_desc filename
    |> lambda_of_links_ir program
    |> nativecomp
  in
  let linkable_units =
    List.map CompilationUnit.Linkable_Unit.locate (dependencies @ [modularize comp_unit])
  in
  let res  = assemble_executable linkable_units target in
  clean_up comp_unit*)
