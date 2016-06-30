(* Compiler driver *)
open Irtolambda

(*let builtins =
  Env.read_signature "Builtins" "comp/builtins.cmi"*)
   
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
      Lapply (transl_path Env.empty print_endline,
              [Lconst (Const_immstring "Hello, world!")],
              no_apply_info)
    in
    Lsequence (body, Lprim (Pmakeblock(0, Asttypes.Immutable), []))
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

let remove_links_extension filename =
  Filename.chop_suffix filename ".links"
			   
let printf_if cond printer arg =
  if cond then Printf.printf "%a@" printer arg;
  arg
    

let setup_options () =
  Clflags.native_code := true;
  (*  Clflags.flambda_invariant_checks := true;*)
  Clflags.nopervasives := false;
  Clflags.dump_lambda := Settings.get_value Basicsettings.show_lambda_ir;
  Clflags.dump_cmm := false;
  Clflags.keep_asm_file := false;
  Clflags.include_dirs := "/home/dhil/projects/links/compiler/comp" :: "/home/dhil/.opam/4.02.2+local-git-4.02.2+effects/lib/ocaml" :: !Clflags.include_dirs;
  (*  Clflags.inlining_report := false;*)
  (*  Compenv.(readenv Format.std_formatter (Before_compile "malfunction")); *)
  let _ = Compmisc.init_path true in ()

    
let assemble_executable cmxs target =
  Asmlink.link Format.std_formatter cmxs target
  (*
  let cmxfile = FileInfo.filename (CompilationUnit.cmx_file comp_unit) in
  Sys.command
    (Printf.sprintf "ocamlfind ocamlopt '%s' -o '%s'" cmxfile outputfile)*)

   
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

let clean_up compunit =
  let _ = List.map Misc.remove_file (CompilationUnit.temporary_files compunit)
  in ()
  
let nativecomp : CompilationUnit.comp_unit -> Lambda.lambda -> CompilationUnit.comp_unit
    = fun comp_unit lambda ->
  let open CompilationUnit in
  let open FileInfo in
  let srcfile = source_file comp_unit in
  let cmxfile = cmx_file comp_unit in
  let module_name = module_name comp_unit in
  let comp_unit = ref comp_unit in
  let ppf = Format.std_formatter in
  try
    (*    let source_provenance = Timings.File filename in*)
    let _ = print_endline ("Module name " ^ module_name) in
    (*    let module_ident = Ident.create_persistent modulename in*)
    let cmifile = with_extension "cmi" (cmx_file !comp_unit) in
    comp_unit := with_cmi_file cmifile !comp_unit;
    Env.set_unit_name module_name;
    (*    Compilenv.reset ~source_provenance ?packname:!Clflags.for_package modulename;*)
    Compilenv.reset ?packname:!Clflags.for_package module_name;
    (*    Compenv.(readenv Format.std_formatter (Before_compile "builtins"));*)
    let (_ : Types.signature) = Env.save_signature [] module_name (filename cmxfile) in
(*    ignore
      (match Misc.find_in_path_uncap !Config.load_path cmi with
      | file -> Env.read_signature modulename file
      | exception Not_found ->
         let mlifile = file.basename ^ !Config.interface_suffix in
         if Sys.file_exists mlifile then
           Typemod.(raise(Error(Location.in_file (filename file),
                                Env.empty,
                                Interface_not_compiled cmi)))
         else
           (* hackily generate an empty cmi file *)
           let cmifile = cmi in
           objfiles := { !objfiles with cmifile = Some (make_file cmi) };
           let mlifile = filename (make_file (Filename.concat file.directory (String.uncapitalize_ascii (modulename ^ ".mli")))) in
           let ch = open_out mlifile in
           print_endline ("ocamlc -c " ^ mlifile);
           print_endline cmifile;
           output_string ch "(* autogenerated mli for Links *)\n";
           close_out ch;
           ignore (Sys.command ("ocamlc -c " ^ mlifile));
           Misc.remove_file mlifile;
           if not (Sys.file_exists cmifile) then failwith "Failed to generate empty cmi file";
           Env.read_signature modulename cmifile
      );*)
    let srcfile_wo_ext = Misc.chop_extension_if_any (filename srcfile) in
    let _ = Asmgen.compile_implementation srcfile_wo_ext ppf (0, lambda) in
    let _ = prerr_endline "compiled implementation" ; flush stderr  in
    let _ = Compilenv.save_unit_info (filename cmxfile) in
    let _ = prerr_endline ("saved unit info " ^ (filename cmxfile))  ; flush stderr in
    !comp_unit
  with exc -> clean_up !comp_unit; raise exc

let compile parse_and_desugar envs prelude filename =
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
  setup_options ();
  Ident.reinit ();
  (*  hello_world ();*)
  let comp_unit   = CompilationUnit.make_compilation_unit filename in
  let module_name = CompilationUnit.module_name comp_unit in
  (*  let module_name = remove_links_extension filename in*)
  let lam = lambda_of_ir (envs,tenv) module_name program in
  dump_lambda lam;
  let impl = Simplif.simplify_lambda lam in
  let ()  = prerr_endline "simplified lambda" ; flush stderr in
  if Settings.get_value Basicsettings.dry_run
  then ()
  else
    let _ = print_endline "native code compilation" in
    let comp_unit = nativecomp comp_unit impl in
    let target = None in
    let target = match target with
      | None     -> (Compenv.output_prefix "a") ^ ".out"
      | Some out -> out
    in
    let cmxs = ["/home/dhil/projects/links/compiler/comp/builtins.cmx" ; CompilationUnit.cmx_file comp_unit |> FileInfo.filename] in
    let res  = assemble_executable cmxs target in
    clean_up comp_unit
